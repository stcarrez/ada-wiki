-----------------------------------------------------------------------
--  wiki-parsers-markdown -- Markdown parser operations
--  Copyright (C) 2016 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Containers.Vectors;
with Util.Encoders.URI;
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Common;
with Wiki.Html_Parser;
package body Wiki.Parsers.Markdown is

   use Wiki.Helpers;
   use Wiki.Nodes;
   use Wiki.Strings;
   use type Wiki.Buffers.Buffer_Access;
   use type Wiki.Html_Parser.Entity_State_Type;

   type Marker_Kind is (M_CODE, M_STAR, M_UNDERSCORE, M_LINK, M_TILDE, M_AUTOLINK,
                        M_LINK_REF, M_IMAGE, M_IMAGE_REF, M_END, M_BRACKET, M_BRACKET_IMAGE,
                        M_ENTITY, M_TEXT, M_LINEBREAK, M_INLINE_HTML);

   type Delimiter_Index_Type is new Natural;
   subtype Delimiter_Index is Delimiter_Index_Type range 1 .. Delimiter_Index_Type'Last;
   type Delimiter_Type;

   type Delimiter_Type is record
      Marker    : Marker_Kind := M_END;
      Cursor    : Wiki.Buffers.Cursor;
      Count     : Natural := 0;
      Can_Open  : Boolean := False;
      Can_Close : Boolean := False;
      Space_Before : Boolean := False;
      Space_After  : Boolean := False;
      Associate : Delimiter_Index_Type := 0;
      Close_Count : Natural := 0;
      Skip_Count  : Natural := 0;
   end record;

   package Delimiter_Vectors is
      new Ada.Containers.Vectors (Delimiter_Index, Delimiter_Type);

   subtype Delimiter_Vector is Delimiter_Vectors.Vector;

   function Check_Trailing_Header (Text : in Wiki.Buffers.Cursor) return Boolean;
   function Maybe_Lazy (Parser : in out Parser_Type) return Boolean;

   function Get_Header_Level (Text   : in out Wiki.Buffers.Cursor) return Natural;
   function Is_Thematic_Break (Text      : in Wiki.Buffers.Cursor;
                               Token     : in Wiki.Strings.WChar) return Boolean;
   procedure Get_List_Level (Parser : in out Parser_Type;
                             Text   : in out Wiki.Buffers.Cursor;
                             Level  : out Integer;
                             Indent : out Natural;
                             Marker : out Wiki.Strings.WChar);
   function Is_End_Preformat (Text   : in Wiki.Buffers.Cursor;
                              Expect : in WChar;
                              Length : in Positive) return Boolean;
   procedure Scan_Link_Title (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Cursor;
                              Expect : in Wiki.Strings.WChar;
                              Link   : in out Wiki.Strings.BString;
                              Title  : in out Wiki.Strings.BString);
   procedure Append_Header (Parser : in out Parser_Type;
                            Text   : in out Wiki.Buffers.Cursor);
   procedure Add_Header (Parser : in out Parser_Type;
                         Level  : in Positive;
                         C      : in out Wiki.Strings.WChar);
   procedure Add_Text (Parser   : in out Parser_Type;
                       Text     : in out Wiki.Buffers.Cursor;
                       Limit    : in Wiki.Buffers.Cursor);
   procedure Add_Autolink (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor;
                           Length : in Natural);

   procedure Parse_Table (Parser : in out Parser_Type;
                          Text   : in out Wiki.Buffers.Cursor;
                          Kind   : in Nodes.Row_Kind);
   procedure Get_Delimiter (Text        : in out Wiki.Buffers.Cursor;
                            Before_Char : Strings.WChar;
                            C           : in Strings.WChar;
                            Delim       : in out Delimiter_Type);

   procedure Parse_Link_Label (Parser : in out Parser_Type;
                               Text   : in out Wiki.Buffers.Cursor;
                               Label  : in out Wiki.Strings.BString;
                               Allow_Bracket : in Boolean);

   procedure Parse_Link_Definition (Parser : in out Parser_Type;
                                    Text   : in out Wiki.Buffers.Cursor);

   procedure Scan_Backtick (Parser   : in out Parser_Type;
                            Text     : in out Wiki.Buffers.Cursor;
                            Start    : in out Delimiter_Type;
                            Stop     : in out Delimiter_Type);

   procedure Scan_Link (Text   : in out Wiki.Buffers.Cursor;
                        Expect : in Wiki.Strings.WChar;
                        Link   : in out Wiki.Strings.BString;
                        Valid  : out Boolean);

   procedure Scan_Title (Text   : in out Wiki.Buffers.Cursor;
                         Title  : in out Wiki.Strings.BString;
                         Valid  : out Boolean);
   procedure Scan_Linebreak (Text   : in out Wiki.Buffers.Cursor;
                             Delim  : in out Delimiter_Type);
   procedure Parse_Html (Parser : in out Parser_Type;
                         Text   : in out Wiki.Buffers.Cursor);
   procedure Open_New_Blocks (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Cursor;
                              C      : in out Wiki.Strings.WChar);

   function Is_Escapable (C : in Wiki.Strings.WChar) return Boolean is
     (C in '!' .. '/' | ':' .. '@' | '{' .. '~' | '[' .. '`');

   function Get_Header_Level (Text   : in out Wiki.Buffers.Cursor) return Natural is
      Pos   : Wiki.Buffers.Cursor := Text;
      Count : Natural;
      Space_Count : Natural;
   begin
      Buffers.Count_Occurence (Pos, '#', Count);
      if Count > 6 or else not Buffers.Is_Valid (Pos) then
         return 0;
      end if;
      if not Is_Space_Or_Newline (Buffers.Char_At (Pos)) then
         return 0;
      end if;
      Wiki.Buffers.Skip_Spaces (Pos, Space_Count);
      Text := Pos;
      return Count;
   end Get_Header_Level;

   function Is_Thematic_Break (Text      : in Wiki.Buffers.Cursor;
                               Token     : in Wiki.Strings.WChar) return Boolean is
      Pos   : Wiki.Buffers.Cursor := Text;
      Count : Natural := 1;
      C     : Wiki.Strings.WChar;
   begin
      loop
         C := Buffers.Next (Pos);
         if C = Token then
            Count := Count + 1;
         elsif C in Helpers.NUL | Helpers.CR | Helpers.LF then
            return Count >= 3;
         elsif not Is_Space_Or_Newline (C) then
            return False;
         end if;
      end loop;
   end Is_Thematic_Break;

   --  ------------------------------
   --  Get the setext heading level returning 0, 1 or 2.
   --  ------------------------------
   function Get_Setext_Heading (Parser : in out Parser_Type;
                                Text   : in Wiki.Buffers.Cursor) return Natural is
      Pos   : Wiki.Buffers.Cursor := Text;
      C     : Wiki.Strings.WChar;
      Level : Wiki.Strings.WChar := ' ';
      Has_Spaces : Boolean := False;
      Top : Block_Access;
   begin
      if Parser.Text_Buffer.Length = 0 or else Parser.Line_Count = 0 then
         return 0;
      end if;
      Top := Current (Parser);
      if Top /= null and then Parser.Column < Top.Level then
         return 0;
      end if;
      if Parser.Indent >= 4 then
         return 0;
      end if;
      C := Buffers.Char_At (Pos);
      loop
         case C is
            when '=' | '-' =>
               if (C /= Level and then Level /= ' ') or else Has_Spaces then
                  return 0;
               end if;
               Level := C;
            when ' ' | Helpers.HT =>
               if Level = ' ' then
                  return 0;
               end if;
               Has_Spaces := Level /= ' ';
            when Helpers.LF | Helpers.CR =>
               case Level is
                  when '=' =>
                     return 1;
                  when '-' =>
                     return 2;
                  when others =>
                     return 0;
               end case;
            when others =>
               return 0;
         end case;
         C := Buffers.Next (Pos);
      end loop;
   end Get_Setext_Heading;

   --  Parse a preformatted header block.
   --  Example:
   --    ```
   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Cursor;
                                 Marker : in Wiki.Strings.WChar) is
      Pos   : Wiki.Buffers.Cursor := Text;
      Count : Natural;
   begin
      Buffers.Count_Occurence (Pos, Marker, Count);
      if Count < 3 then
         return;
      end if;

      --  Extract the format either 'Ada' or '[Ada]'
      declare
         Space_Count : Natural;
         C : Wiki.Strings.WChar;
         Has_Bracket : Boolean := False;
      begin
         Wiki.Strings.Clear (Parser.Preformat_Format);
         Buffers.Skip_Spaces (Pos, Space_Count);
         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '[' then
            Buffers.Next (Pos);
            Has_Bracket := True;
         end if;
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when Helpers.Is_Space_Or_Tab (C) or else Helpers.Is_Newline (C);
            if C = '`' and then Marker = C then
               --  The backtick cannot occur in the string for a ``` code block.
               return;
            end if;
            exit when Has_Bracket and C = ']';
            if C = '\' then
               Buffers.Next (Pos);
               if not Buffers.Is_Valid (Pos) then
                  return;
               end if;
               C := Buffers.Char_At (Pos);
            end if;
            Wiki.Strings.Append_Char (Parser.Preformat_Format, C);
            Buffers.Next (Pos);
         end loop;
         if Buffers.Is_Valid (Pos) then
            Buffers.Skip_Spaces (Pos, Space_Count);
         end if;
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            if C = '`' and then Marker = C then
               --  The backtick cannot occur in the string for a ``` code block.
               return;
            end if;
            Buffers.Next (Pos);
            exit when Helpers.Is_Newline (C);
         end loop;
         Text := Pos;
      end;

      --  If we are in a list, it is loose due to the blank line.
      if Parser.Previous_Line_Empty > 0 then
         Documents.Set_Loose (Parser.Document);
      end if;
      Parser.Preformat_Indent := Parser.Indent;
      Parser.Preformat_Fence := Marker;
      Parser.Preformat_Fcount := Count;
      Flush_Block (Parser, Trim => Right);
      Push_Block (Parser, N_PREFORMAT, Parser.Indent);
      Parser.Previous_Line_Empty := 0;
   end Parse_Preformatted;

   procedure Get_List_Level (Parser : in out Parser_Type;
                             Text   : in out Wiki.Buffers.Cursor;
                             Level  : out Integer;
                             Indent : out Natural;
                             Marker : out WChar) is
      Pos   : Wiki.Buffers.Cursor := Text;
      C     : Strings.WChar;
      Limit : Natural := 9;
      Count : Natural;
   begin
      Level := 0;
      Indent := 0;
      Marker := Helpers.NUL;
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if C in '0' .. '9' then
            --  Must not exceed 9 digits
            exit when Limit = 0;
            Level := Level * 10;
            Level := Level + WChar'Pos (C) - WChar'Pos ('0');
            Indent := Indent + 1;
            Limit := Limit - 1;
         elsif C in '.' | ')' then
            Indent := Indent + 1;
            Buffers.Next (Pos);
            exit when not Buffers.Is_Valid (Pos);
            Buffers.Skip_Spaces (Pos, Count);
            exit when Count = 0;

            --  A list that interrupts a paragraph must start with 1.
            exit when Level /= 1
              and then not (Parser.Current_Node in Nodes.N_LIST_ITEM)
              and then Parser.Text_Buffer.Length > 0;

            --  An empty list item cannot interrupt a paragraph.
            exit when not Buffers.Is_Valid (Pos) and then Parser.Text_Buffer.Length > 0;
            Indent := Indent + Count;
            Marker := C;
            Text := Pos;
            return;
         else
            Level := -1;
            return;
         end if;
         Buffers.Next (Pos);
      end loop;
      Level := -1;
   end Get_List_Level;

   procedure Add_Header (Parser : in out Parser_Type;
                         Level  : in Positive;
                         C      : in out Wiki.Strings.WChar) is
      Text  : Wiki.Buffers.Cursor;
   begin
      --  Check for a possible link definition predeceded by the Setext header.
      Text := (Parser.Text_Buffer.First'Unchecked_Access, 1);
      Parse_Link_Definition (Parser, Text);
      if not Buffers.Is_Valid (Text) then
         return;
      end if;

      if not Parser.Context.Is_Hidden then
         Block_Stack.Push (Parser.Blocks);
         declare
            Top   : constant Block_Access := Current (Parser);
         begin
            Top.Kind := N_HEADER;
            Top.Level := Level;
            Parser.Need_Paragraph := False;
            Parser.Current_Node := N_HEADER;
            Parser.Context.Filters.Start_Block (Parser.Document, Nodes.N_HEADER, Level);
            Parse_Inline_Text (Parser, Text);
            Buffers.Clear (Parser.Text_Buffer);
            Pop_Block (Parser);
         end;
      end if;
      Parser.Empty_Line   := True;
      Parser.In_Paragraph := False;
      Buffers.Clear (Parser.Text_Buffer);
      C := Helpers.NUL;
   end Add_Header;

   function Is_End_Preformat (Text   : in Wiki.Buffers.Cursor;
                              Expect : in WChar;
                              Length : in Positive) return Boolean is
      Pos   : Wiki.Buffers.Cursor := Text;
      C     : WChar;
      Count : Natural := 0;
   begin
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if Is_Newline (C) then
            return Count >= Length;
         end if;
         if C /= Expect then
            if Count >= Length and then Is_Space (C) then
               Buffers.Skip_Spaces (Pos, Count);
               return not Buffers.Is_Valid (Pos) or else Is_Newline (Buffers.Char_At (Pos));
            end if;
            return False;
         end if;
         Buffers.Next (Pos);
         Count := Count + 1;
      end loop;
      return False;
   end Is_End_Preformat;

   --  ------------------------------
   --  Flush current block and add an horizontal rule in the document.
   --  ------------------------------
   procedure Add_Horizontal_Rule (Parser : in out Parser_Type) is
   begin
      Flush_Block (Parser, Trim => Right);
      if Parser.Current_Node in N_PARAGRAPH | N_LIST_START then
         Pop_Block (Parser);
      end if;
      Parser.Previous_Line_Empty := 0;
      if not Parser.Context.Is_Hidden then
         Parser.Context.Filters.Add_Node (Parser.Document, Wiki.Nodes.N_HORIZONTAL_RULE);
      end if;
   end Add_Horizontal_Rule;

   procedure Parse_Columns (Text    : in Wiki.Buffers.Cursor;
                            Columns : in out Nodes.Column_Array_Style;
                            Count   : out Natural) is
      Pos   : Wiki.Buffers.Cursor := Text;
      C     : Wiki.Strings.WChar;
      Space_Count : Natural;
      Line_Count  : Natural;
      Bar_Count   : Natural := 0;
      Column      : Natural := 0;
      Style       : Nodes.Align_Style;
      Width       : Natural;
   begin
      Count := 0;
      --  | ------------- | ------------- |
      --  | :----------- | :--------------: | -------------------------: |
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         exit when C /= '|';
         Style := Nodes.ALIGN_DEFAULT;
         Bar_Count := Bar_Count + 1;
         Buffers.Next (Pos);
         Buffers.Skip_Spaces (Pos, Space_Count, Line_Count);
         exit when Line_Count > 0 or else not Buffers.Is_Valid (Pos);
         C := Buffers.Char_At (Pos);
         if C = ':' then
            Buffers.Next (Pos);
            Style := Nodes.ALIGN_LEFT;
         end if;
         Width := 0;
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when C /= '-';
            Buffers.Next (Pos);
            Width := Width + 1;
         end loop;
         if Width = 0 then
            Count := 0;
            return;
         end if;
         if C = ':' then
            Buffers.Next (Pos);
            Style := (if Style = Nodes.ALIGN_LEFT then Nodes.ALIGN_CENTER else Nodes.ALIGN_RIGHT);
         end if;
         Buffers.Skip_Spaces (Pos, Space_Count, Line_Count);
         if Line_Count > 0 or else not Buffers.Is_Valid (Pos) then
            Count := 0;
            return;
         end if;
         Column := Column + 1;
         if Column >= Columns'First and then Column <= Columns'Last then
            Columns (Column).Format := Style;
            Columns (Column).Width := Width;
         end if;
      end loop;
      Count := (if Bar_Count <= 1 then 0 else Column);
   end Parse_Columns;

   --  Parse a markdown table/column.
   --  Example:
   --    | col1 | col2 | ... | colN |
   procedure Parse_Table (Parser : in out Parser_Type;
                          Text   : in out Wiki.Buffers.Cursor;
                          Kind   : in Nodes.Row_Kind) is
      Pos         : Wiki.Buffers.Cursor := Text;
      C           : Wiki.Strings.WChar;
      Skip_Spaces : Boolean := True;
   begin
      Buffers.Next (Pos);
      Parser.Context.Filters.Add_Row (Parser.Document, Kind);
      Wiki.Attributes.Clear (Parser.Attributes);

      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if Skip_Spaces and then Is_Space_Or_Newline (C) then
            Buffers.Next (Pos);
         else
            if Skip_Spaces then
               Skip_Spaces := False;
               Parser.Context.Filters.Add_Column (Parser.Document, Parser.Attributes);
            end if;

            if C = '\' then
               Buffers.Next (Pos);
               exit when not Buffers.Is_Valid (Pos);
               C := Buffers.Char_At (Pos);
               Buffers.Append (Parser.Text_Buffer, C);
            elsif C = '|' then
               Skip_Spaces := True;
               Flush_Block (Parser);
            else
               Buffers.Append (Parser.Text_Buffer, C);
            end if;
            Buffers.Next (Pos);
         end if;
      end loop;
      if not Skip_Spaces then
         Flush_Block (Parser);
      end if;
      Text := Pos;
   end Parse_Table;

   --  Parse a markdown header and column specification.
   --  The column header was saved in the `Parser.Text_Buffer` and the
   --  column specification is passed in the `Text` buffer.
   --  Example:
   --    | col1 | col2 | ... | colN |
   --    | ---- | ---- | ... | ---- |
   procedure Parse_Table_Header (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Cursor;
                                 C      : in out Wiki.Strings.WChar) is
      Count   : Natural;
      Empty   : Nodes.Column_Array_Style (1 .. 0);
   begin
      Parse_Columns (Text, Empty, Count);
      if Count = 0 then
         return;
      end if;

      --  Save the text buffer because we must not write it yet.
      declare
         Pos        : Wiki.Buffers.Cursor := Text;
         Bar_Count  : Natural := 0;
         Char_Count : Natural := 0;
         Header     : Wiki.Buffers.Builder (512);
         Is_Newline : Boolean := True;
         Truncate_Pos : Wiki.Buffers.Cursor;
      begin
         Buffers.Next (Pos);
         Pos := (Parser.Text_Buffer.First'Unchecked_Access, 1);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when Is_Newline and then C = '|';
            Buffers.Next (Pos);
            if Helpers.Is_Newline (C) then
               Is_Newline := True;
            else
               Is_Newline := False;
            end if;
         end loop;
         Truncate_Pos := Pos;

         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            Buffers.Next (Pos);
            Buffers.Append (Header, C);
            if C = '|' then
               Bar_Count := Bar_Count + 1;
            elsif not Helpers.Is_Space_Or_Newline (C) then
               Char_Count := Char_Count + 1;
            end if;
         end loop;

         --  Expect the same number of columns.
         if Bar_Count /= Count + 1 then
            return;
         end if;

         Buffers.Truncate (Parser.Text_Buffer, Truncate_Pos.Block, Truncate_Pos.Pos);

         --  Get the column layout and create the table.
         declare
            Columns : Nodes.Column_Array_Style (1 .. Count);
         begin
            Parse_Columns (Text, Columns, Count);
            Push_Block (Parser, Nodes.N_TABLE);
            Parser.Context.Filters.Add_Table (Parser.Document, Columns);
         end;

         --  The header is not empty, emit the table row header.
         if Char_Count > 0 then
            Pos := (Header.First'Unchecked_Access, 1);
            Parse_Table (Parser, Pos, Nodes.N_ROW_HEADER);
         end if;
         Text := (null, Pos.Pos);
         C := Helpers.NUL;
      end;
   end Parse_Table_Header;

   --  Parse a markdown link definition.
   --  Example:
   --    [label]: url
   --    [label]: url "title"
   procedure Parse_Link_Label (Parser : in out Parser_Type;
                               Text   : in out Wiki.Buffers.Cursor;
                               Label  : in out Wiki.Strings.BString;
                               Allow_Bracket : in Boolean) is
      pragma Unreferenced (Parser);

      Pos   : Wiki.Buffers.Cursor := Text;
      C     : Wiki.Strings.WChar;
      Has_Space : Boolean := False;
      Count_Bracket : Natural := 0;
   begin
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if C = '\' then
            Buffers.Next (Pos);
            exit when not Buffers.Is_Valid (Pos);
            C := Buffers.Char_At (Pos);
            if Is_Escapable (C) then
               Buffers.Next (Pos);
               exit when not Buffers.Is_Valid (Pos);
               Append (Label, '\');
               Append (Label, C);
            else
               Append (Label, '\');
               Append (Label, C);
            end if;
            Has_Space := False;
         elsif C = '[' then
            exit when not Allow_Bracket;
            Count_Bracket := Count_Bracket + 1;
            Has_Space := False;
            Append (Label, C);
            Buffers.Next (Pos);
         elsif C = ']' and then Count_Bracket = 0 then
            Text := Pos;
            return;
         elsif Helpers.Is_Space_Or_Newline (C) then
            if not Has_Space then
               Append (Label, ' ');
            end if;
            Has_Space := True;
            Buffers.Next (Pos);
         else
            if C = '[' then
               Count_Bracket := Count_Bracket + 1;
            elsif C = ']' then
               exit when not Allow_Bracket;
               Count_Bracket := Count_Bracket - 1;
            end if;
            Has_Space := False;
            Append (Label, C);
            Buffers.Next (Pos);
         end if;
      end loop;
      Text := (null, 1);
   end Parse_Link_Label;

   --  Parse a markdown link definition.
   --  Example:
   --    [label]: url
   --    [label]: url "title"
   --    [label]: <url>
   procedure Parse_Link_Definition (Parser : in out Parser_Type;
                                    Text   : in out Wiki.Buffers.Cursor) is
      Pos         : Wiki.Buffers.Cursor := Text;
      Label       : Wiki.Strings.BString (128);
      Link        : Wiki.Strings.BString (128);
      Title       : Wiki.Strings.BString (128);
      Space_Count : Natural;
      Is_Valid    : Boolean;
   begin
      while Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '[' loop
         Buffers.Next (Pos);
         Parse_Link_Label (Parser, Pos, Label, False);
         if Wiki.Strings.Length (Label) = 0 then
            return;
         end if;
         if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= ']' then
            return;
         end if;
         Buffers.Next (Pos);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
         if Buffers.Char_At (Pos) /= ':' then
            return;
         end if;
         Buffers.Next (Pos);
         Buffers.Skip_Spaces (Pos, Space_Count);
         Scan_Link (Pos, ' ', Link, Is_Valid);
         if not Buffers.Is_Valid (Pos) or else not Is_Valid then
            return;
         end if;
         Scan_Title (Pos, Title, Is_Valid);
         exit when not Is_Valid;
         Parser.Document.Set_Link (Strings.To_WString (Label),
                                   Helpers.Encode_URI (Link),
                                   Strings.To_WString (Title));
         Text := Pos;
         Strings.Clear (Label);
         Strings.Clear (Link);
         Strings.Clear (Title);
      end loop;
   end Parse_Link_Definition;

   function Is_List_Item (Text   : in Wiki.Buffers.Cursor;
                          Marker : in Wiki.Strings.WChar) return Boolean is
      Pos   : Wiki.Buffers.Cursor := Text;
      C     : Wiki.Strings.WChar;
   begin
      C := Buffers.Char_At (Pos);
      if C /= Marker then
         return False;
      end if;
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         return False;
      end if;
      return Helpers.Is_Space_Or_Tab (Buffers.Char_At (Pos));
   end Is_List_Item;

   procedure Parse_Block (Parser : in out Parser_Type;
                          Text   : in out Wiki.Buffers.Cursor;
                          C      : out Wiki.Strings.WChar) is

      procedure Process (Nodes : in Block_Stack.Element_Type_Array) is
         Need_Char : Boolean := True;
         Matched   : Boolean;
         I         : Positive := Nodes'First;
         Indent    : Natural := 0;
      begin
         while I <= Nodes'Last loop
            if Need_Char then
               First_Nonspace (Parser, Text, C);
               Need_Char := False;
            end if;

            case Nodes (I).Kind is
               when N_BLOCKQUOTE =>
                  Need_Char := True;
                  if Parser.Indent > 3 then
                     Matched := False;
                  elsif C = '>' or else I = Nodes'Last then
                     Matched := True;
                     Indent := Nodes (I).Level;
                  elsif Nodes (Nodes'Last).Kind = N_PARAGRAPH
                    and then Parser.Previous_Line_Empty = 0 and then Parser.Text_Buffer.Length > 0
                  then
                     Matched := True;
                     Need_Char := False;
                     I := Nodes'Last;
                     Indent := Nodes (I).Level;
                  else
                     Matched := False;
                  end if;

               when N_LIST_START | N_NUM_LIST_START =>
                  Matched := Parser.Is_Blank or else Parser.Indent >= Nodes (I).Level
                    or else (Parser.Indent = 0 and then Parser.Text_Buffer.Length > 0
                             and then Parser.Previous_Line_Empty = 0
                             and then not Is_List_Item (Text, Nodes (I).Marker));
                  if not Matched and then Parser.Need_Paragraph
                    and then Parser.Previous_Line_Empty > 0
                    and then I + 1 <= Nodes'Last
                  then
                     Parser.Need_Paragraph := Nodes (I + 1).Line_Count > 1;
                     Parser.Need_Paragraph := False;
                  end if;
                  if Matched then
                     Indent := Nodes (I).Level;
                     if I + 1 <= Nodes'Last and then Nodes (I + 1).Kind = N_LIST_ITEM then
                        Indent := Nodes (I + 1).Level;
                        if not Parser.Is_Blank
                          and then Parser.Indent < Nodes (I + 1).Level
                          and then (Parser.Text_Buffer.Length = 0
                                    or else Parser.Previous_Line_Empty > 0)
                        then
                           Matched := False;
                           Indent := 0;
                           if Is_List_Item (Text, Nodes (I).Marker) then
                              I := I + 1;
                              Documents.Set_Loose (Parser.Document);
                              Parser.Need_Paragraph := False;
                           end if;
                        else
                           I := I + 1;
                        end if;
                     end if;

                  end if;
                  Need_Char := False;

               when N_PARAGRAPH =>
                  Matched := not Parser.Is_Blank;
                  Need_Char := False;

               when N_PREFORMAT =>
                  if Parser.Preformat_Fence = ' ' then
                     if Parser.Is_Blank then
                        Parser.Previous_Line_Empty := Parser.Previous_Line_Empty + 1;
                        C := Helpers.NUL;
                        return;
                     end if;
                     Matched := Parser.Indent >= Parser.Preformat_Indent;
                     if Matched then
                        Indent := 4;
                     end if;
                  elsif C = Parser.Preformat_Fence
                    and then Is_End_Preformat (Text, C, Parser.Preformat_Fcount)
                    and then Parser.Indent <= 3 + Indent
                  then
                     Matched := False;
                     Need_Char := False;
                     C := Helpers.NUL;
                  else
                     Matched := True;
                  end if;
                  if Matched then
                     if Parser.Preformat_Fence /= ' ' then
                        if Parser.Indent > Indent then
                           Parser.Indent := Parser.Indent - Indent;
                        else
                           Parser.Indent := 0;
                        end if;
                     end if;
                     while Parser.Previous_Line_Empty > 0 loop
                        Strings.Append_Char (Parser.Text, Helpers.LF);
                        Parser.Previous_Line_Empty := Parser.Previous_Line_Empty - 1;
                     end loop;
                     if Parser.Indent > Parser.Preformat_Indent then
                        for I in 1 .. Parser.Indent - Parser.Preformat_Indent loop
                           Strings.Append_Char (Parser.Text, ' ');
                        end loop;
                     end if;
                     Common.Append (Parser.Text, Text);
                     C := Helpers.NUL;
                     Need_Char := False;
                  end if;

               when N_HEADER =>
                  Matched := False;

               when N_TABLE =>
                  Matched := C = '|';

               when others =>
                  Matched := False;
            end case;
            if not Matched then
               if Parser.Indent > Indent then
                  Parser.Indent := Parser.Indent - Indent;
               else
                  Parser.Indent := 0;
               end if;
               while I <= Nodes'Last loop
                  Pop_Block (Parser, Trim => Wiki.Parsers.Right);
                  I := I + 1;
               end loop;
               return;
            end if;
            I := I + 1;
         end loop;
         if Parser.Indent > Indent then
            Parser.Indent := Parser.Indent - Indent;
         else
            Parser.Indent := 0;
         end if;
         if Need_Char then
            C := Buffers.Next (Text);
         end if;
      end Process;

   begin
      if not Block_Stack.Is_Empty (Parser.Blocks) then
         Block_Stack.Read (Parser.Blocks, Process'Access);
      else
         First_Nonspace (Parser, Text, C);
      end if;
   end Parse_Block;

   procedure Add_Text (Parser : in out Parser_Type;
                       Text   : in out Wiki.Buffers.Cursor;
                       C      : in out Wiki.Strings.WChar) is
   begin
      if C in '*' | '-' and then Is_Thematic_Break (Text, C)
        and then Parser.Indent = 0
      then
         Add_Horizontal_Rule (Parser);
         return;
      end if;

      if C = ' ' then
         First_Nonspace (Parser, Text, C);
      end if;

      if Parser.Current_Node in N_BLOCKQUOTE then
         if not Parser.Is_Blank and then Parser.Text_Buffer.Length > 0 then
            declare
               Level : Integer;
            begin
               Level := Get_Setext_Heading (Parser, Text);
               if Level > 0 then
                  Add_Header (Parser, Level, C);
                  if C = Helpers.NUL then
                     return;
                  end if;
               end if;
            end;
         end if;
         if Parser.Column = 0 and then Parser.Is_Blank then
            while Parser.Current_Node = N_BLOCKQUOTE loop
               Pop_Block (Parser);
            end loop;
         elsif Parser.Text_Buffer.Length = 0 then
            Push_Block (Parser, N_PARAGRAPH);
         end if;
      end if;
      declare
         Top   : constant Block_Access := Current (Parser);
      begin
         if Parser.Is_Blank then
            if Parser.Text_Buffer.Length > 0
              and then (Parser.Current_Node /= N_LIST_ITEM
                        or else (Top /= null and then Top.Line_Count > 0))
            then
               Parser.Need_Paragraph := True;
            end if;
            Parser.Line_Count := 0;
            Parser.Previous_Line_Empty := Parser.Previous_Line_Empty + 1;
            if Parser.Column = 0 then
               while Parser.In_Blockquote loop
                  Pop_Block (Parser);
               end loop;
            end if;
         else
            if Parser.Previous_Line_Empty > 0
              and then Parser.Text_Buffer.Length > 0
            then
               Flush_Block (Parser, Trim => Right);
               Parser.Need_Paragraph := True;
               Documents.Set_Loose (Parser.Document);
            end if;

            Parser.Previous_Line_Empty := 0;
            Buffers.Append (Parser.Text_Buffer, Text.Block, Text.Pos);
            Parser.Line_Count := Parser.Line_Count + 1;
         end if;
      end;
   end Add_Text;

   function Maybe_Lazy (Parser : in out Parser_Type) return Boolean is
      Top : constant Block_Access := Current (Parser);
   begin
      if Top /= null then
         return Top.Kind in N_PARAGRAPH | N_BLOCKQUOTE and then Parser.Previous_Line_Empty = 0;
      else
         return Parser.Text_Buffer.Length > 0 and then Parser.Previous_Line_Empty = 0;
      end if;
   end Maybe_Lazy;

   function Check_Trailing_Header (Text : in Wiki.Buffers.Cursor) return Boolean is
      Pos         : Wiki.Buffers.Cursor := Text;
      Space_Count : Natural;
      Line_Count  : Natural;
      Count       : Natural;
   begin
      Buffers.Skip_Spaces (Pos, Space_Count, Line_Count);
      if Pos.Block = null or else Line_Count > 0 then
         return True;
      end if;
      Buffers.Count_Occurence (Pos, '#', Count);
      if Count = 0 then
         return False;
      end if;
      Buffers.Skip_Spaces (Pos, Space_Count, Line_Count);
      return Pos.Block = null or else Line_Count > 0;
   end Check_Trailing_Header;

   procedure Append_Header (Parser : in out Parser_Type;
                            Text   : in out Wiki.Buffers.Cursor) is
      C : Wiki.Strings.WChar;
   begin
      if Check_Trailing_Header (Text) then
         return;
      end if;
      while Buffers.Is_Valid (Text) loop
         C := Buffers.Char_At (Text);
         if Helpers.Is_Space (C) then
            exit when Check_Trailing_Header (Text);
         end if;
         Buffers.Append (Parser.Text_Buffer, C);
         Buffers.Next (Text);
      end loop;
   end Append_Header;

   procedure Parse_Html (Parser : in out Parser_Type;
                         Text   : in out Wiki.Buffers.Cursor) is
      use type Wiki.Html_Parser.State_Type;

      Has_Error : Boolean := False;
      Pos  : Wiki.Buffers.Cursor := Text;
      Kind : Wiki.Html_Parser.State_Type;
   begin
      Buffers.Next (Pos);
      loop
         Wiki.Html_Parser.Parse_Element (Parser.Html, Pos.Block.Content (1 .. Pos.Block.Last),
                                         Pos.Pos, Kind, Pos.Pos);
         if Kind = Wiki.Html_Parser.HTML_ERROR then
            Has_Error := True;
         elsif Kind /= Wiki.Html_Parser.HTML_NONE then
            Process_Html (Parser, Kind, Wiki.Strings.To_WString (Parser.Html.Elt_Name),
                          Parser.Html.Attributes);
         end if;
         if Pos.Pos = Pos.Block.Last + 1 then
            Pos.Block := Pos.Block.Next_Block;
            exit when Pos.Block = null;
            Pos.Pos := 1;
         end if;
         exit when Wiki.Html_Parser.Is_Empty (Parser.Html);
      end loop;
      if Has_Error then
         Buffers.Append (Parser.Text_Buffer, Text.Block, Text.Pos);
         Pos.Block := null;
      end if;
      Text := Pos;
   end Parse_Html;

   ---------------------
   -- Open_New_Blocks --
   ---------------------
   procedure Open_New_Blocks (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Cursor;
                              C      : in out Wiki.Strings.WChar) is
      Is_Indented : Boolean;
      Is_Lazy     : Boolean := Maybe_Lazy (Parser);
      Level       : Integer;
      Need_Char   : Boolean := True;
   begin
      loop
         Is_Indented := Parser.Indent >= 4;
         if not Is_Indented then
            case C is
               when '>' =>
                  Parser.Quote_Level := Parser.Quote_Level + 1;
                  Push_Block (Parser, Nodes.N_BLOCKQUOTE, Parser.Column);
                  if Buffers.Has_Next (Text)
                    and then Wiki.Helpers.Is_Space (Buffers.Next_At (Text))
                  then
                     Parser.Column := Parser.Column + 1;
                     Buffers.Next (Text);
                  end if;
                  Need_Char := True;

               when '#' =>
                  Level := Get_Header_Level (Text);
                  if Level > 0 then
                     Push_Block (Parser, Nodes.N_HEADER, Level);
                     Parser.Previous_Line_Empty := 0;
                     Append_Header (Parser, Text);
                     Parser.Line_Count := Parser.Line_Count + 1;
                     C := Helpers.NUL;
                     return;
                  end if;
                  return;

               when '_' =>
                  if Is_Thematic_Break (Text, C) then
                     Add_Horizontal_Rule (Parser);
                     C := Helpers.NUL;
                  end if;
                  return;

               when '=' =>
                  Level := Get_Setext_Heading (Parser, Text);
                  if Level > 0 then
                     Add_Header (Parser, Level, C);
                  end if;
                  return;

               when '*' | '-' | '+' =>
                  if C in '*' | '-' then
                     declare
                        Is_Break : constant Boolean := Is_Thematic_Break (Text, C);
                     begin
                        Level := Get_Setext_Heading (Parser, Text);
                        if Parser.In_Blockquote and then Parser.Indent = 0 then
                           if Is_Break then
                              while Parser.Current /= null loop
                                 Pop_Block (Parser);
                                 exit when not Parser.In_Blockquote;
                              end loop;
                              Add_Horizontal_Rule (Parser);
                              C := Helpers.NUL;
                              return;
                           end if;
                        else
                           if C = '-' and then Level > 0 then
                              Add_Header (Parser, Level, C);
                              return;
                           end if;
                           if Is_Break then
                              if Parser.Current_Node = N_LIST_ITEM
                                and then Parser.Column /= Get_Current_Level (Parser)
                              then
                                 Pop_List (Parser);
                              elsif Parser.Current_Node = N_BLOCKQUOTE
                                and then Parser.Column /= Get_Current_Level (Parser)
                              then
                                 Pop_Block (Parser);
                                 Parser.Quote_Level := 0;
                              end if;

                              Add_Horizontal_Rule (Parser);
                              C := Helpers.NUL;
                              return;
                           end if;
                        end if;
                     end;
                  end if;

                  if (Buffers.Has_Next (Text)
                      and then Helpers.Is_Space_Or_Tab (Buffers.Next_At (Text)))
                    or else (Buffers.Has_Next (Text)
                             and then Helpers.Is_Newline (Buffers.Next_At (Text))
                             and then Parser.Text_Buffer.Length = 0)
                  then
                     declare
                        Top   : Block_Access := Current (Parser);
                        Count : Natural;
                     begin
                        Buffers.Next (Text);
                        if Buffers.Is_Valid (Text) then
                           Buffers.Skip_Spaces (Text, Count);
                           if not Buffers.Is_Valid (Text)
                             or else Helpers.Is_Newline (Buffers.Char_At (Text))
                           then
                              Count := 0;
                           end if;
                        else
                           Count := 0;
                        end if;
                        Level := Parser.Column + Count;
                        if Top /= null and then Top.Kind /= N_BLOCKQUOTE then
                           if Top.Kind = N_LIST_START then
                              if Top.Level + 1 > Parser.Column
                                or else (Top.Marker /= C and then Top.Level + 1 >= Parser.Column)
                              then
                                 Pop_List (Parser, Parser.Column, C, 0);
                                 Top := Current (Parser);
                              end if;
                           elsif Top.Level + 1 <= Parser.Column
                             and then Top.Kind /= N_LIST_START
                           then
                              Push_Block (Parser, Nodes.N_LIST_START, Parser.Column - 1, C);
                           else
                              if Top.Kind = N_LIST_ITEM then
                                 if Top.Marker /= C then
                                    Pop_List (Parser, Parser.Column, C, 0);
                                    Top := Current (Parser);
                                 else
                                    Pop_Block (Parser, Trim => Right);
                                 end if;
                              end if;
                           end if;
                        end if;
                        if Top = null or else Top.Kind not in N_LIST_ITEM | N_LIST_START then
                           Push_Block (Parser, Nodes.N_LIST_START, Parser.Column - 1, C);
                        else
                           if Parser.Previous_Line_Empty > 0 then
                              Documents.Set_Loose (Parser.Document);
                           end if;
                        end if;
                        Push_Block (Parser, Nodes.N_LIST_ITEM, Level, C);
                        Parser.Previous_Line_Empty := 0;
                        Parser.Need_Paragraph := False;
                        if Buffers.Is_Valid (Text) then
                           C := Buffers.Char_At (Text);
                        else
                           C := Helpers.NUL;
                        end if;
                        if not (C in '#' | '>' | '`') then
                           return;
                        end if;
                        Need_Char := False;
                     end;
                  else
                     return;
                  end if;

               when '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
                  declare
                     Top   : Block_Access := Current (Parser);
                     Indent : Natural;
                     Count  : Integer;
                     Number : Integer;
                     Marker : Wiki.Strings.WChar;
                  begin
                     Get_List_Level (Parser, Text, Number, Indent, Marker);
                     if Number >= 0 then
                        Parser.Previous_Line_Empty := 0;
                        if Buffers.Is_Valid (Text) then
                           Buffers.Skip_Spaces (Text, Count);
                        end if;
                        Level := Parser.Column;
                        if Top /= null then
                           if Top.Level > Indent
                             or else (Top.Marker /= Marker and then Top.Level >= Level)
                           then
                              Pop_List (Parser, Parser.Column, C, 0);
                              Top := Current (Parser);
                           elsif Top.Level < Indent and then Top.Kind /= N_NUM_LIST_START then
                              Push_Block (Parser, Nodes.N_NUM_LIST_START,
                                          Level => Parser.Column - 1, Marker => Marker,
                                          Number => Number);
                           else
                              if Top.Kind = N_LIST_ITEM then
                                 Pop_Block (Parser, Trim => Right);
                              end if;
                           end if;
                        end if;
                        if Top = null or else Top.Kind not in N_LIST_ITEM | N_NUM_LIST_START then
                           Push_Block (Parser, Nodes.N_NUM_LIST_START,
                                       Level => Parser.Column - 1, Marker => Marker,
                                       Number => Number);
                        end if;
                        Push_Block (Parser, Nodes.N_LIST_ITEM,
                                    Level => Indent, Marker => Marker, Number => Number);
                        Parser.Previous_Line_Empty := 0;
                        if Buffers.Is_Valid (Text) then
                           C := Buffers.Char_At (Text);
                        else
                           C := Helpers.NUL;
                        end if;
                        if not (C in '#' | '>' | '`') then
                           return;
                        end if;
                        Need_Char := False;
                     else
                        return;
                     end if;
                  end;

               when '~' | '`' =>
                  Parse_Preformatted (Parser, Text, C);
                  if Parser.Current_Node = Nodes.N_PREFORMAT then
                     Common.Append (Parser.Text, Text);
                     C := Helpers.NUL;
                  end if;
                  return;

               when '|' =>
                  if Parser.Current_Node = N_TABLE then
                     Parse_Table (Parser, Text, Nodes.N_ROW);
                     if not Buffers.Is_Valid (Text) then
                        C := Helpers.NUL;
                     end if;
                     return;
                  end if;
                  if Parser.Text_Buffer.Length > 0 then
                     Parse_Table_Header (Parser, Text, C);
                  end if;
                  return;

               when '<' =>
                  declare
                     Pos       : Wiki.Buffers.Cursor := Text;
                     Count     : Natural;
                  begin
                     loop
                        Parse_Html (Parser, Pos);
                        if not Buffers.Is_Valid (Pos) then
                           C := Helpers.NUL;
                           return;
                        end if;
                        Text := Pos;
                        if Parser.In_Html /= HTML_NONE then
                           Buffers.Skip_Spaces (Pos, Count);
                           if not Buffers.Is_Valid (Pos) then
                              if Parser.In_Html = HTML_BLOCK then
                                 Append_Char (Parser.Text, ' ');
                              end if;
                              C := Helpers.NUL;
                              return;
                           end if;
                           Parser.In_Html := HTML_NONE;
                           C := Pos.Block.Content (Pos.Pos);
                           if C /= '<' then
                              return;
                           end if;
                        else
                           return;
                        end if;
                     end loop;
                  end;

               when Helpers.LF | Helpers.CR =>
                  if Parser.In_Html = HTML_BLOCK then
                     Flush_Text (Parser, Trim => Wiki.Parsers.Both);
                     Parser.In_Html := HTML_NONE;
                     Pop_Block (Parser, Trim => Wiki.Parsers.Left);
                  end if;
                  return;

               when others =>
                  return;

            end case;
         elsif not Is_Lazy and then not Parser.Is_Blank and then C /= Helpers.NUL then
            if Parser.Current_Node /= N_PREFORMAT then
               --  If we are in a list, it is loose due to the blank line.
               if Parser.Previous_Line_Empty > 0 then
                  Documents.Set_Loose (Parser.Document);
               end if;
               Parser.Preformat_Indent := Parser.Column - 1;
               Parser.Preformat_Fence := ' ';
               Parser.Preformat_Fcount := 0;
               Parser.Previous_Line_Empty := 0;
               Push_Block (Parser, N_PREFORMAT);
               for I in 1 .. Parser.Indent - 4 loop
                  Strings.Append_Char (Parser.Text, ' ');
               end loop;
            end if;
            Common.Append (Parser.Text, Text);
            C := Helpers.NUL;
            return;
         else
            return;
         end if;
         if Need_Char then
            First_Nonspace (Parser, Text, C);
         end if;
         Is_Lazy := False;
      end loop;
   end Open_New_Blocks;

   --  Current paragraph
   --  N_BLOCKQUOTE
   --  N_LIST
   --  N_LIST_ITEM
   --  N_PARAGRAPH    *Never nested*
   --  N_PREFORMAT, Fence, Indent level *Never nested*
   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor) is
      Pos    : Wiki.Buffers.Cursor;
      C      : WChar;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Pos := Text;
         Common.Parse_Html_Element (Parser, Pos, Start => False);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
         if Parser.In_Html /= HTML_NONE then
            Common.Parse_Html_Preformatted (Parser, Pos);
            return;
         end if;
      elsif Parser.In_Html /= HTML_NONE then
         Pos := Text;
         if Parser.In_Html = HTML_BLOCK
           and then Wiki.Helpers.Is_Newline (Buffers.Char_At (Pos))
         then
            Parser.In_Html := HTML_NONE;
            Flush_Text (Parser, Trim => Both);
            Flush_Block (Parser);
            Parser.Need_Paragraph := True;
            Parser.Previous_Line_Empty := 1;
            return;
         end if;
         Common.Parse_Html_Preformatted (Parser, Pos);
         return;
      else
         Pos := (Text.Block, Text.Pos - 1);
      end if;

      Parser.Column := 0;
      Parse_Block (Parser, Pos, C);
      Open_New_Blocks (Parser, Pos, C);
      if C = Helpers.NUL then
         return;
      end if;
      Add_Text (Parser, Pos, C);
      if Parser.Current_Node /= Nodes.N_NONE or else Parser.Current_Node = Nodes.N_NONE then
         return;
      end if;
   end Parse_Line;

   procedure Scan_Link (Text   : in out Wiki.Buffers.Cursor;
                        Expect : in Wiki.Strings.WChar;
                        Link   : in out Wiki.Strings.BString;
                        Valid  : out Boolean) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar := Helpers.NUL;
      Space_Count : Natural;
      Paren_Count : Integer := 0;
   begin
      Valid := False;
      Wiki.Strings.Clear (Link);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '<' then
         Buffers.Next (Pos);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when C = '>';
            if C = LF or else C = '<' then
               return;
            end if;
            Buffers.Next (Pos);
            Append (Link, C);
         end loop;
         if Buffers.Is_Valid (Pos) and then C = '>' then
            Buffers.Next (Pos);
            Valid := True;
         end if;
      else
         Buffers.Skip_Ascii_Spaces (Pos, Space_Count);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when C = Expect or else Wiki.Helpers.Is_Space_Or_Newline (C);
            if C = '\' then
               Buffers.Next (Pos);
               exit when not Buffers.Is_Valid (Pos);
               C := Buffers.Char_At (Pos);
               if Is_Escapable (C) then
                  Buffers.Next (Pos);
                  exit when not Buffers.Is_Valid (Pos);
                  Append (Link, C);
               else
                  Append (Link, '\');
                  Append (Link, C);
                  Buffers.Next (Pos);
                  exit when not Buffers.Is_Valid (Pos);
               end if;
            else
               Buffers.Next (Pos);
               Append (Link, C);
               if C = '(' then
                  Paren_Count := Paren_Count + 1;
               elsif C = ')' then
                  Paren_Count := Paren_Count - 1;
               end if;
            end if;
         end loop;
         Valid := Paren_Count = 0;
         if Paren_Count /= 0 then
            Pos.Block := null;
         end if;
      end if;
      Text := Pos;
   end Scan_Link;

   procedure Scan_Title (Text   : in out Wiki.Buffers.Cursor;
                         Title  : in out Wiki.Strings.BString;
                         Valid  : out Boolean) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar := Helpers.NUL;
      Sep    : Wiki.Strings.WChar;
      Space_Count : Natural;
      Line_Count  : Natural;
   begin
      Wiki.Strings.Clear (Title);
      Buffers.Skip_Spaces (Pos, Space_Count, Line_Count);
      Valid := Line_Count > 0 or else not Buffers.Is_Valid (Pos);
      if Valid then
         Text := Pos;
      end if;
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) in '"' | ''' | '(' then
         if Space_Count = 0 and then Line_Count = 0 then
            return;
         end if;
         Sep := Buffers.Char_At (Pos);
         if Sep = '(' then
            Sep := ')';
         end if;
         Buffers.Next (Pos);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when C = Sep;
            if C = '\' then
               Buffers.Next (Pos);
               exit when not Buffers.Is_Valid (Pos);
               C := Buffers.Char_At (Pos);
               if Is_Escapable (C) then
                  Append (Title, C);
                  Buffers.Next (Pos);
               else
                  Append (Title, '\');
                  Append (Title, C);
                  Buffers.Next (Pos);
               end if;
            else
               Buffers.Next (Pos);
               Append (Title, C);
            end if;
         end loop;
         if C /= Sep then
            Wiki.Strings.Clear (Title);
            return;
         end if;
         while Buffers.Is_Valid (Pos) loop
            Buffers.Next (Pos);
            exit when not Buffers.Is_Valid (Pos);
            C := Buffers.Char_At (Pos);
            exit when not Helpers.Is_Space (C) or else Helpers.Is_Newline (C);
         end loop;
         if Buffers.Is_Valid (Pos) and then Helpers.Is_Newline (C) then
            Buffers.Next (Pos);
         end if;
         if not Buffers.Is_Valid (Pos) or else Helpers.Is_Newline (C) then
            Valid := True;
            Text := Pos;
         end if;
      end if;
   end Scan_Title;

   procedure Scan_Link_Title (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Cursor;
                              Expect : in Wiki.Strings.WChar;
                              Link   : in out Wiki.Strings.BString;
                              Title  : in out Wiki.Strings.BString) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar := Helpers.NUL;
      Sep    : Wiki.Strings.WChar;
      Space_Count : Natural;
      Paren_Count : Integer := 0;
   begin
      Wiki.Strings.Clear (Link);
      Wiki.Strings.Clear (Title);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '<' then
         Buffers.Next (Pos);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when C = '>';
            if C = LF or else C = '<' or else C = '\' then
               return;
            end if;
            Buffers.Next (Pos);
            Append (Link, C);
         end loop;
         if Buffers.Is_Valid (Pos) and then C = '>' then
            Buffers.Next (Pos);
         end if;
      else
         Buffers.Skip_Ascii_Spaces (Pos, Space_Count);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when (C = Expect and then Paren_Count = 0)
              or else Wiki.Helpers.Is_Space_Or_Tab (C)
              or else Wiki.Helpers.Is_Newline (C);
            if C = '\' then
               Buffers.Next (Pos);
               exit when not Buffers.Is_Valid (Pos);
               C := Buffers.Char_At (Pos);
               if Is_Escapable (C) then
                  Buffers.Next (Pos);
                  exit when not Buffers.Is_Valid (Pos);
                  Append (Link, C);
               else
                  Append (Link, '\');
                  Append (Link, C);
                  Buffers.Next (Pos);
               end if;
            elsif C = '&' then
               declare
                  Status : Wiki.Html_Parser.Entity_State_Type;
               begin
                  Common.Parse_Entity (Parser, Pos, Status, C);
                  Append (Link, C);
               end;
            else
               Buffers.Next (Pos);
               Append (Link, C);
               if C = '(' then
                  Paren_Count := Paren_Count + 1;
               elsif C = ')' then
                  Paren_Count := Paren_Count - 1;
               end if;
            end if;
         end loop;
         if Paren_Count /= 0 then
            Pos.Block := null;
         end if;
      end if;
      Buffers.Skip_Spaces (Pos, Space_Count);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) in '"' | ''' | '(' then
         Sep := Buffers.Char_At (Pos);
         if Sep = '(' then
            Sep := ')';
         end if;
         Buffers.Next (Pos);
         while Buffers.Is_Valid (Pos) loop
            C := Buffers.Char_At (Pos);
            exit when C = Sep;
            if C = '\' then
               Buffers.Next (Pos);
               exit when not Buffers.Is_Valid (Pos);
               C := Buffers.Char_At (Pos);
               if Is_Escapable (C) then
                  Append (Title, C);
                  Buffers.Next (Pos);
               else
                  Append (Title, '\');
                  Append (Title, C);
                  Buffers.Next (Pos);
               end if;
            else
               Buffers.Next (Pos);
               Append (Title, C);
            end if;
         end loop;
         Buffers.Next (Pos);
         Buffers.Skip_Spaces (Pos, Space_Count);
      end if;
      Text := Pos;
   end Scan_Link_Title;

   procedure Get_Delimiter (Text        : in out Wiki.Buffers.Cursor;
                            Before_Char : Strings.WChar;
                            C           : in Strings.WChar;
                            Delim       : in out Delimiter_Type) is
      Pos           : Wiki.Buffers.Cursor := Text;
      After_Char    : Strings.WChar;
      Numdelims     : Natural := 1;
   begin
      Buffers.Next (Pos);
      if C not in ''' | '"' then
         while Buffers.Is_Valid (Pos) loop
            exit when Buffers.Char_At (Pos) /= C;
            Buffers.Next (Pos);
            Numdelims := Numdelims + 1;
         end loop;
      end if;
      if not Buffers.Is_Valid (Pos) then
         After_Char := LF;
      else
         After_Char := Buffers.Char_At (Pos);
      end if;
      declare
         Before_Space  : constant Boolean := Helpers.Is_Space_Or_Newline (Before_Char);
         Before_Punct  : constant Boolean := Helpers.Is_Symbol_Or_Punctuation (Before_Char);
         After_Space   : constant Boolean := Helpers.Is_Space_Or_Newline (After_Char);
         After_Punct   : constant Boolean := Helpers.Is_Symbol_Or_Punctuation (After_Char);
         Left_Flanking : constant Boolean
            := Numdelims > 0 and then not After_Space
                and then (not After_Punct or else Before_Space or else Before_Punct);
         Right_Flanking : constant Boolean
            := Numdelims > 0 and then not Before_Space
                and then (not Before_Punct or else After_Space or else After_Punct);
      begin
         if C = '_' then
            Delim.Can_Open := Left_Flanking and then (not Right_Flanking or else Before_Punct);
            Delim.Can_Close := Right_Flanking and then (not Left_Flanking or else After_Punct);
         elsif C in ''' | '"' then
            Delim.Can_Open := Left_Flanking
                 and then (not Right_Flanking or else Before_Char in '(' | '[')
                 and then Before_Char not in ']' | ')';
            Delim.Can_Close := Right_Flanking;
         else
            Delim.Can_Open := Left_Flanking;
            Delim.Can_Close := Right_Flanking;
         end if;
         Delim.Space_Before := Before_Char = ' ';
         Delim.Space_After := After_Char = ' ';
         Delim.Count := Numdelims;
         Delim.Cursor := Text;
         Text := Pos;
      end;
   end Get_Delimiter;

   procedure Add_Autolink (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor;
                           Length : in Natural) is
      Pos     : Wiki.Buffers.Cursor := Text;
      Link    : Wiki.Strings.BString (128);
      C       : Wiki.Strings.WChar;
      Is_Mail : Boolean := False;
   begin
      for I in 1 .. Length loop
         C := Buffers.Char_At (Pos);
         Is_Mail := Is_Mail or else C = '@';
         Buffers.Next (Pos);
         Append_Char (Link, C);
      end loop;
      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         if Is_Mail then
            Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR,
                                    "mailto:" & Helpers.Encode_URI (Link));
         else
            Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR,
                                    Helpers.Encode_URI (Link));
         end if;
         Parser.Context.Filters.Add_Link (Parser.Document,
                                          Strings.To_WString (Link),
                                          Parser.Attributes, False);
      end if;
      Buffers.Next (Pos);
      Text := Pos;
   end Add_Autolink;

   procedure Add_Text (Parser   : in out Parser_Type;
                       Text     : in out Wiki.Buffers.Cursor;
                       Limit    : in Wiki.Buffers.Cursor) is
      use type Wiki.Buffers.Cursor;
   begin
      while Buffers.Is_Valid (Text) loop
         declare
            C    : Wiki.Strings.WChar;
         begin
            if Text = Limit then
               return;
            end if;
            C := Buffers.Char_At (Text);
            if C = Wiki.Helpers.CR or else C = Wiki.Helpers.LF then
               if Strings.Length (Parser.Text) > 0 then
                  if Parser.Format (STRONG) or else Parser.Format (EMPHASIS) then
                     Flush_Text (Parser);
                     Append (Parser.Text, ' ');
                  else
                     Append (Parser.Text, ' ');
                  end if;
               else
                  Append (Parser.Text, ' ');
               end if;
            elsif C = '\' then
               Buffers.Next (Text);
               if Text = Limit then
                  Append (Parser.Text, '\');
                  return;
               end if;
               C := Buffers.Char_At (Text);
               if Wiki.Helpers.Is_Newline (C)
                 and then not Parser.Format (CODE)
                 and then (Limit.Block /= null or else Buffers.Has_Next (Text))
               then
                  Flush_Text (Parser);
                  if not Parser.Context.Is_Hidden then
                     Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                  end if;

               elsif Parser.Format (CODE) or else not Is_Escapable (C) then
                  Append (Parser.Text, '\');
                  Append (Parser.Text, C);
               else
                  Append (Parser.Text, C);
               end if;
            else
               Append (Parser.Text, C);
            end if;
         end;
         Buffers.Next (Text);
      end loop;
   end Add_Text;

   procedure Scan_Backtick (Parser   : in out Parser_Type;
                            Text     : in out Wiki.Buffers.Cursor;
                            Start    : in out Delimiter_Type;
                            Stop     : in out Delimiter_Type) is
      pragma Unreferenced (Parser);

      Pos      : Wiki.Buffers.Cursor := Text;
      Count    : Natural;
      C        : Wiki.Strings.WChar;
      Prev     : Wiki.Strings.WChar := Helpers.NUL;
      Prev_Pos : Wiki.Buffers.Cursor := Text;
      Total    : Natural := 0;
   begin
      Start.Cursor := Pos;
      Buffers.Count_Occurence (Pos, '`', Count);
      Text := Pos;
      Start.Count := Count;
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if C = '`' then
            Stop.Cursor := Pos;
            Buffers.Count_Occurence (Pos, '`', Count);

            --  Found a matching occurence, we are done.
            if Count = Start.Count then
               Start.Marker := M_CODE;
               Start.Can_Open := True;
               Start.Can_Close := False;
               Stop.Can_Open := False;
               Stop.Can_Close := True;
               Stop.Space_Before := Prev = ' ' or else Helpers.Is_Newline (Prev);
               Stop.Count := Count;
               if Total > 1 and then Stop.Space_Before and then Start.Space_After then
                  Stop.Cursor := Prev_Pos;
                  Stop.Skip_Count := 1;
               else
                  Stop.Space_Before := False;
                  Start.Space_After := False;
               end if;
               Stop.Marker := M_CODE;
               Text := Pos;
               return;
            end if;
            Total := Total + 1;
         elsif (C = ' ' or else Helpers.Is_Newline (C)) and then Prev = Helpers.NUL then
            Start.Space_After := True;
            Prev_Pos := Pos;
            Buffers.Next (Pos);
         else
            Prev_Pos := Pos;
            Buffers.Next (Pos);
            Total := Total + 1;
         end if;
         Prev := C;
      end loop;

      --  End of buffer reached: we have not found the end marker.
      Start.Count := 0;
   end Scan_Backtick;

   procedure Scan_Linebreak (Text   : in out Wiki.Buffers.Cursor;
                             Delim  : in out Delimiter_Type) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Count  : Natural := 0;
   begin
      C := Buffers.Char_At (Pos);
      Delim.Cursor := Pos;
      Delim.Count := 0;
      if C = '\' then
         C := Buffers.Next (Pos);
         if C /= Helpers.LF or else not Buffers.Has_Next (Pos) then
            Delim.Count := 0;
            return;
         end if;
         Delim.Marker := M_LINEBREAK;
         Delim.Count := 2;
         Text := Pos;
         return;
      end if;

      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         exit when C = Helpers.LF;
         if not Helpers.Is_Space (C) then
            return;
         end if;
         Count := Count + 1;
         Buffers.Next (Pos);
      end loop;
      if Count >= 2 and then Buffers.Has_Next (Pos) then
         Delim.Marker := M_LINEBREAK;
         Delim.Count := Count + 1;
         Buffers.Next (Pos);
         Text := Pos;
      end if;
   end Scan_Linebreak;

   procedure Scan_Autolink (Text   : in out Wiki.Buffers.Cursor;
                            Delim  : in out Delimiter_Type);

   --  Scan the markdown autolink and setup the delimiter if there is a valid one.
   --  Autolinks have the form:
   --    <scheme:link>
   --  where `scheme` must start with a letter and must be between 2..32 characters.
   --  The autolink cannot contain spaces.
   procedure Scan_Autolink (Text   : in out Wiki.Buffers.Cursor;
                            Delim  : in out Delimiter_Type) is
      Pos    : Wiki.Buffers.Cursor := Text;
      Count  : Natural;
      Length : Natural;
      C      : Wiki.Strings.WChar;
   begin
      Delim.Cursor := Text;
      Delim.Count := 0;
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         return;
      end if;
      C := Buffers.Char_At (Pos);
      if not (C in 'a' .. 'z' | 'A' .. 'Z') then
         return;
      end if;
      Buffers.Next (Pos);
      Length := 1;

      --  Parse the scheme part.
      Count := 1;
      loop
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
         C := Buffers.Char_At (Pos);
         Buffers.Next (Pos);
         Length := Length + 1;
         exit when C = ':' or C = '@';
         if not (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '.' | '-') then
            return;
         end if;
         if Count >= 32 then
            return;
         end if;
         Count := Count + 1;
      end loop;
      --  Scheme length must be 2..32
      if Count < 2 then
         return;
      end if;

      --  Parse until end of autolink for matching '>'.
      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         if C = '>' then
            Delim.Count := Length;
            Delim.Marker := M_AUTOLINK;
            Buffers.Next (Pos);
            Text := Pos;
            return;
         end if;

         --  Spaces are not allowed in autolink.
         exit when Helpers.Is_Space (C);
         Buffers.Next (Pos);
         Length := Length + 1;
      end loop;
   end Scan_Autolink;

   procedure Scan_Inline_Html (Parser : in out Parser_Type;
                               Text   : in out Wiki.Buffers.Cursor;
                               Delim  : in out Delimiter_Type) is
      Pos    : Wiki.Buffers.Cursor := Text;
      Count  : Natural;
      Length : Natural;
      C      : Wiki.Strings.WChar;
      Kind   : Wiki.Html_Parser.State_Type;
   begin
      Delim.Cursor := Text;
      Delim.Count := 0;
      Buffers.Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         return;
      end if;
      Wiki.Html_Parser.Parse_Element (Parser.Html,
                                      Pos.Block.Content,
                                      Pos.Pos,
                                      Kind, Pos.Pos);
      if not (Kind in Wiki.Html_Parser.HTML_NONE | Wiki.Html_Parser.HTML_ERROR) then
         Delim.Count := 1;
         Delim.Marker := M_INLINE_HTML;
         Buffers.Next (Pos);
         Text := Pos;
         return;
      end if;
   end Scan_Inline_Html;

   HREF_LOOSE  : constant Util.Encoders.URI.Encoding_Array
     := ('0' .. '9' => False,
         'a' .. 'z' => False,
         'A' .. 'Z' => False,
         '-' => False, '.' => False, '_' => False, '~' => False, '+' => False,
         ''' => False, '*' => False, '(' => False, '&' => False, '$' => False,
         ')' => False, ',' => False, '%' => False, '#' => False, '@' => False,
         '?' => False, '=' => False, ';' => False, ':' => False, '/' => False,
         others => True);

   procedure Parse_Inline_Text (Parser : in out Parser_Type;
                                Text   : in Wiki.Buffers.Cursor) is
      use Delimiter_Vectors;

      procedure Add_Image (Parser       : in out Parser_Type;
                           Text         : in out Wiki.Buffers.Cursor;
                           First        : in Delimiter_Index_Type;
                           To           : in Delimiter_Index_Type;
                           Is_Reference : in Boolean);
      procedure Parse_Link (Text   : in out Wiki.Buffers.Cursor;
                            Prev_C : in out WChar);
      procedure Process_Emphasis (Text  : in out Wiki.Buffers.Cursor;
                                  First : in Delimiter_Index_Type;
                                  To    : in Delimiter_Index_Type);
      procedure Process_Emphasis (From, To : in Delimiter_Index_Type);
      function Find_Closing (Starting : in Delimiter_Index_Type;
                             Last     : in Delimiter_Index_Type;
                             Opening  : in Delimiter_Type) return Delimiter_Index_Type;
      function Find_Previous_Bracket return Delimiter_Index_Type;
      procedure Clear_Brackets;

      C          : Wiki.Strings.WChar;
      Prev       : Wiki.Strings.WChar := ' ';
      Delimiters : Delimiter_Vector;

      procedure Add_Image (Parser       : in out Parser_Type;
                           Text         : in out Wiki.Buffers.Cursor;
                           First        : in Delimiter_Index_Type;
                           To           : in Delimiter_Index_Type;
                           Is_Reference : in Boolean) is
         procedure Add_Text (Limit : in Wiki.Buffers.Cursor);

         Pos    : Wiki.Buffers.Cursor := Text;
         Alt    : Wiki.Strings.BString (128);
         Ref    : Wiki.Strings.BString (128);
         Link   : Wiki.Strings.BString (128);
         Title  : Wiki.Strings.BString (128);
         C      : Wiki.Strings.WChar := ' ';
         I      : Delimiter_Index_Type := First;

         procedure Add_Text (Limit : in Wiki.Buffers.Cursor) is
            use type Wiki.Buffers.Cursor;
         begin
            while Buffers.Is_Valid (Pos) loop
               if Pos = Limit then
                  return;
               end if;
               C := Buffers.Char_At (Pos);
               Append (Alt, C);
               Append (Ref, C);
               Buffers.Next (Pos);
            end loop;
         end Add_Text;

      begin
         while I <= To loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type := Delimiters.Reference (I);
            begin
               if Delim.Marker /= M_TEXT
                 and then (Delim.Cursor.Block.Offset > Pos.Block.Offset
                           or else (Delim.Cursor.Block.Offset = Pos.Block.Offset
                                    and then Delim.Cursor.Pos >= Pos.Pos))
               then
                  Add_Text (Delim.Cursor);
                  if I /= To then
                     for J in 1 .. Delim.Count + Delim.Skip_Count loop
                        C := Buffers.Char_At (Pos);
                        Append (Ref, C);
                        Buffers.Next (Pos);
                     end loop;
                  end if;
               end if;
            end;
            I := I + 1;
         end loop;

         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ']' then
            Buffers.Next (Pos);
         end if;
         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '(' then
            Buffers.Next (Pos);
            Scan_Link_Title (Parser, Pos, ')', Link, Title);
            if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ')' then
               Buffers.Next (Pos);
            end if;
         elsif Is_Reference
           and then Buffers.Is_Valid (Pos)
           and then Buffers.Char_At (Pos) = '['
         then
            Buffers.Next (Pos);
            Parse_Link_Label (Parser, Pos, Link, True);
            if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ']' then
               Buffers.Next (Pos);
            end if;
            if Strings.Length (Link) = 0 then
               Strings.Append_String (Link, Strings.To_WString (Ref));
            end if;
         elsif Is_Reference then
            Strings.Append_String (Link, Strings.To_WString (Ref));
         end if;
         Flush_Text (Parser);
         if not Parser.Context.Is_Hidden then
            Wiki.Attributes.Clear (Parser.Attributes);
            Wiki.Attributes.Append (Parser.Attributes, "alt", Alt);
            Wiki.Attributes.Append (Parser.Attributes, "src", Link);
            Wiki.Attributes.Append (Parser.Attributes, "title", Title);
            if Is_Reference then
               Parser.Context.Filters.Add_Image (Parser.Document,
                                                 Strings.To_WString (Link),
                                                 Parser.Attributes, Is_Reference);
            else
               Parser.Context.Filters.Add_Image (Parser.Document,
                                                 Strings.To_WString (Ref),
                                                 Parser.Attributes, Is_Reference);
            end if;
         end if;
         Text := Pos;
      end Add_Image;

      function Find_Closing (Starting : in Delimiter_Index_Type;
                             Last     : in Delimiter_Index_Type;
                             Opening  : in Delimiter_Type) return Delimiter_Index_Type is
      begin
         for I in Starting + 1 .. Last loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type := Delimiters.Reference (I);
            begin
               if Delim.Can_Close and then Opening.Marker = Delim.Marker then
                  if not (Delim.Can_Open or Opening.Can_Close)
                    or else (Delim.Count mod 3) = 0
                    or else (((Opening.Count + Delim.Count) mod 3) /= 0)
                  then
                     return I;
                  end if;
               end if;
            end;
         end loop;
         return 0;
      end Find_Closing;

      procedure Process_Inline_Html (Text : in out Wiki.Buffers.Cursor) is
         use Wiki.Html_Parser;
         Start : Wiki.Buffers.Cursor := Text;
         Tag   : Wiki.Html_Tag;
         Kind  : Wiki.Html_Parser.State_Type;
      begin
         Buffers.Next (Text);
         Wiki.Html_Parser.Parse_Element
           (Parser.Html, Text.Block.Content (Text.Pos .. Text.Block.Last),
            Text.Pos, Kind, Text.Pos);

         if Kind = Wiki.Html_Parser.HTML_START then
            Flush_Text (Parser);
            if Parser.Previous_Tag /= UNKNOWN_TAG and then No_End_Tag (Parser.Previous_Tag) then
               if not Parser.Context.Is_Hidden then
                  Parser.Context.Filters.Pop_Node (Parser.Document, Parser.Previous_Tag);
               end if;
               Parser.Previous_Tag := UNKNOWN_TAG;
            end if;
            Tag := Wiki.Find_Tag (Wiki.Strings.To_WString (Parser.Html.Elt_Name));
            if Tag /= UNKNOWN_TAG then
               Parser.Context.Filters.Push_Node (Parser.Document, Tag, Parser.Html.Attributes);
            else
               Add_Text (Parser, Start, Text);
            end if;
         elsif Kind = Wiki.Html_Parser.HTML_END then
            Flush_Text (Parser);
            declare
               Previous_Tag : constant Wiki.Html_Tag := Parser.Previous_Tag;
            begin
               Tag := Wiki.Find_Tag (Wiki.Strings.To_WString (Parser.Html.Elt_Name));
               Parser.Previous_Tag := UNKNOWN_TAG;
               if Previous_Tag /= UNKNOWN_TAG
                 and then Previous_Tag /= Tag
                 and then No_End_Tag (Previous_Tag)
               then
                  if not Parser.Context.Is_Hidden then
                     Parser.Context.Filters.Pop_Node (Parser.Document, Previous_Tag);
                  end if;
               end if;
            end;
            if Tag /= UNKNOWN_TAG then
               Parser.Context.Filters.Pop_Node (Parser.Document, Tag);
            else
               Add_Text (Parser, Start, Text);
            end if;
         else
            Add_Text (Parser, Start, Text);
         end if;
      end Process_Inline_Html;

      procedure Process_Emphasis (Text  : in out Wiki.Buffers.Cursor;
                                  First : in Delimiter_Index_Type;
                                  To    : in Delimiter_Index_Type) is
         I : Delimiter_Index_Type := First;
      begin
         while I <= To loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type := Delimiters.Reference (I);
            begin
               if Delim.Cursor.Block.Offset > Text.Block.Offset
                 or else (Delim.Cursor.Block.Offset = Text.Block.Offset
                          and then Delim.Cursor.Pos >= Text.Pos)
               then
                  if Delim.Marker = M_ENTITY then
                     Add_Text (Parser, Text, Delim.Cursor);
                     Text := Delim.Cursor;
                     while Buffers.Is_Valid (Text) and then Buffers.Char_At (Text) /= ';' loop
                        Buffers.Next (Text);
                     end loop;
                     if Buffers.Is_Valid (Text) then
                        Buffers.Next (Text);
                     end if;
                  elsif Delim.Marker in M_LINK | M_LINK_REF then
                     Add_Text (Parser, Text, Delim.Cursor);
                     Flush_Text (Parser);
                     Text := Delim.Cursor;
                     Buffers.Next (Text);
                     declare
                        Bracket : constant Delimiter_Vectors.Reference_Type
                          := Delimiters.Reference (Delim.Associate);
                        Link  : Wiki.Strings.BString (128);
                        Title : Wiki.Strings.BString (128);
                        Tmp_Pos : Wiki.Buffers.Cursor := Bracket.Cursor;
                     begin
                        --  Skip ']' and then '('
                        if Delim.Marker = M_LINK then
                           Buffers.Next (Tmp_Pos);
                           C := Tmp_Pos.Block.Content (Tmp_Pos.Pos);
                           Buffers.Next (Tmp_Pos);
                           if C = '[' then
                              Parse_Link_Label (Parser, Tmp_Pos, Link, True);
                           elsif C = '(' then
                              Scan_Link_Title (Parser, Tmp_Pos, ')', Link, Title);
                           end if;
                        else
                           C := Helpers.NUL;
                        end if;
                        if not Parser.Context.Is_Hidden then
                           Wiki.Attributes.Clear (Parser.Attributes);
                           if C /= '(' then
                              if Strings.Length (Link) = 0 then
                                 Parse_Link_Label (Parser, Text, Link, True);
                                 Text := Delim.Cursor;
                                 Buffers.Next (Text);
                              end if;
                              Parser.Context.Filters.Add_Link (Parser.Document, To_WString (Link),
                                                               Parser.Attributes,
                                                               True, True);
                           else
                              Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR,
                                                      To_WString (Util.Encoders.URI.Encode
                                                        (Wiki.Strings.To_String
                                                           (Wiki.Strings.To_WString (Link)),
                                                           HREF_LOOSE)));
                              if Wiki.Strings.Length (Title) > 0 then
                                 Wiki.Attributes.Append (Parser.Attributes, TITLE_ATTR, Title);
                              end if;
                              Parser.Context.Filters.Add_Link (Parser.Document, To_WString (Title),
                                                               Parser.Attributes,
                                                               False, True);
                           end if;
                        end if;
                        Process_Emphasis (Text, I + 1, Delim.Associate);
                        Add_Text (Parser, Text, Bracket.Cursor);
                        Flush_Text (Parser, Trim => Wiki.Parsers.Right);
                        if not Parser.Context.Is_Hidden then
                           Parser.Context.Filters.Pop_Node (Parser.Document, A_TAG);
                        end if;
                        I := Delim.Associate;
                        Text := Tmp_Pos;
                        exit when not Buffers.Is_Valid (Text);
                        Buffers.Next (Text);
                     end;
                  elsif Delim.Marker in M_IMAGE | M_IMAGE_REF then
                     Add_Text (Parser, Text, Delim.Cursor);
                     Flush_Text (Parser);
                     Text := Delim.Cursor;
                     Buffers.Next (Text);
                     Buffers.Next (Text);
                     Add_Image (Parser, Text, I, Delim.Associate, Delim.Marker = M_IMAGE_REF);

                  elsif Delim.Marker = M_AUTOLINK then
                     Add_Text (Parser, Text, Delim.Cursor);
                     Flush_Text (Parser);
                     Text := Delim.Cursor;
                     Buffers.Next (Text);
                     Add_Autolink (Parser, Text, Delim.Count);

                  elsif Delim.Marker = M_TEXT then
                     null;

                  elsif Delim.Marker = M_LINEBREAK then
                     Add_Text (Parser, Text, Delim.Cursor);
                     Flush_Text (Parser);
                     Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                     Text := Delim.Cursor;
                     for I in 1 .. Delim.Count loop
                        Buffers.Next (Text);
                     end loop;

                  elsif Delim.Marker = M_INLINE_HTML then
                     Add_Text (Parser, Text, Delim.Cursor);
                     Text := Delim.Cursor;
                     Process_Inline_Html (Text);

                  elsif Delim.Count > 0 and then Delim.Can_Open
                    and then (Delim.Associate > I or else Delim.Marker = M_CODE)
                  then
                     Add_Text (Parser, Text, Delim.Cursor);
                     declare
                        Count : Natural;
                     begin
                        if Delim.Count > Delim.Close_Count then
                           Count := Delim.Close_Count;
                           for I in Count + 1 .. Delim.Count loop
                              Append (Parser.Text, Buffers.Char_At (Text));
                              Buffers.Next (Text);
                           end loop;
                        else
                           Count := Delim.Count;
                        end if;
                        Flush_Text (Parser);
                        if Delim.Marker in M_STAR | M_UNDERSCORE and then Count = 2 then
                           Parser.Format (STRONG) := True;

                        elsif Delim.Marker in M_TILDE and then Count = 2 then
                           Parser.Format (STRIKEOUT) := True;

                        elsif Delim.Marker in M_STAR | M_UNDERSCORE then
                           Parser.Format (EMPHASIS) := True;

                        elsif Delim.Marker = M_CODE then
                           Parser.Format (CODE) := not Parser.Format (CODE);

                        end if;
                        Text := Delim.Cursor;
                        for I in 1 .. Delim.Count loop
                           Buffers.Next (Text);
                        end loop;
                        if Delim.Marker = M_CODE and then Delim.Space_After
                          and then Delimiters.Element (Delim.Associate).Space_Before
                        then
                           Buffers.Next (Text);
                        end if;
                     end;
                  elsif Delim.Count > 0 and then Delim.Can_Close then
                     Add_Text (Parser, Text, Delim.Cursor);
                     if Delim.Marker in M_STAR | M_UNDERSCORE
                       and then Delim.Count = 2
                       and then Parser.Format (STRONG)
                     then
                        Flush_Text (Parser);
                        Parser.Format (STRONG) := False;
                     elsif Delim.Marker in M_STAR | M_UNDERSCORE
                       and then Parser.Format (EMPHASIS)
                     then
                        Flush_Text (Parser);
                        Parser.Format (EMPHASIS) := False;
                     elsif Delim.Marker = M_CODE and then Parser.Format (CODE) then
                        Flush_Text (Parser);
                        Parser.Format (CODE) := False;
                     elsif Delim.Marker = M_TILDE
                       and then Delim.Count = 2
                       and then Parser.Format (STRIKEOUT)
                     then
                        Flush_Text (Parser);
                        Parser.Format (STRIKEOUT) := False;
                     else
                        Text := Delim.Cursor;
                        for I in 1 .. Delim.Count loop
                           Append (Parser.Text, Buffers.Char_At (Text));
                           Buffers.Next (Text);
                        end loop;
                     end if;
                     Text := Delim.Cursor;
                     for I in 1 .. Delim.Count + Delim.Skip_Count loop
                        Buffers.Next (Text);
                     end loop;
                  end if;
               end if;
            end;
            I := I + 1;
         end loop;
      end Process_Emphasis;

      procedure Clear_Brackets is
      begin
         for Iter in Delimiters.Iterate loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type
                 := Delimiters.Reference (Iter);
            begin
               if Delim.Marker in M_BRACKET | M_BRACKET_IMAGE then
                  Delim.Marker := M_TEXT;
               end if;
            end;
         end loop;
      end Clear_Brackets;

      function Find_Previous_Bracket return Delimiter_Index_Type is
      begin
         for I in reverse 1 .. Delimiters.Last_Index loop
            declare
               Previous_Delim : constant Delimiter_Vectors.Reference_Type
                 := Delimiters.Reference (I);
            begin
               if Previous_Delim.Marker in M_BRACKET | M_BRACKET_IMAGE
               then
                  return I;
               end if;
            end;
         end loop;
         return 0;
      end Find_Previous_Bracket;

      procedure Process_Emphasis (From, To : in Delimiter_Index_Type) is
      begin
         for I in From .. To loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type
                 := Delimiters.Reference (I);
            begin
               if Delim.Count > 0
                 and then Delim.Associate = 0
                 and then Delim.Marker /= M_IMAGE
                 and then Delim.Marker /= M_IMAGE_REF
                 and then Delim.Marker /= M_LINK
                 and then Delim.Marker /= M_LINK_REF
                 and then Delim.Marker /= M_END
                 and then Delim.Marker /= M_ENTITY
                 and then Delim.Marker /= M_LINEBREAK
                 and then Delim.Marker /= M_AUTOLINK
                 and then Delim.Marker /= M_INLINE_HTML
                 and then Delim.Can_Open
               then
                  Delim.Associate := Find_Closing (I, To, Delim);
                  if Delim.Associate = 0 then
                     Delim.Marker := M_TEXT;
                  else
                     declare
                        Closing : constant Delimiter_Vectors.Reference_Type
                          := Delimiters.Reference (Delim.Associate);
                     begin
                        Closing.Associate := I;
                        Delim.Close_Count := Closing.Count;
                        if Closing.Count > Delim.Count then
                           Closing.Count := Delim.Count;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end loop;
         for I in reverse From .. To loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type
                 := Delimiters.Reference (I);
            begin
               if Delim.Marker /= M_IMAGE
                 and then Delim.Marker /= M_IMAGE_REF
                 and then Delim.Marker /= M_LINK
                 and then Delim.Marker /= M_LINK_REF
                 and then Delim.Marker /= M_END
                 and then Delim.Marker /= M_TEXT
                 and then Delim.Marker /= M_ENTITY
                 and then Delim.Marker /= M_LINEBREAK
                 and then Delim.Marker /= M_AUTOLINK
                 and then Delim.Marker /= M_INLINE_HTML
                 and then Delim.Associate = 0
               then
                  Delim.Marker := M_TEXT;
               end if;
            end;
         end loop;
      end Process_Emphasis;

      --  Parse a link at the ']' position:
      --
      --  Link (M_LINK):
      --    [link-label]
      --    [link](url)
      --    [link-label][link-ref]
      --    [link-ref]
      --  Link (M_IMAGE):
      --    ![label](url)
      --    ![label][image-ref]
      --    ![link-ref]
      procedure Parse_Link (Text   : in out Wiki.Buffers.Cursor;
                            Prev_C : in out WChar) is
         First  : Delimiter_Index_Type;
         Pos    : Wiki.Buffers.Cursor := Text;
         C      : Wiki.Strings.WChar;
      begin
         C := Buffers.Next (Pos);
         First := Find_Previous_Bracket;
         if First = 0 then
            Buffers.Next (Text);
            return;
         end if;

         if C = '(' then
            declare
               Link  : Wiki.Strings.BString (128);
               Title : Wiki.Strings.BString (128);
               Delim : Delimiter_Type;
               Kind  : Marker_Kind;
            begin
               Delim.Cursor := Text;
               Delim.Marker := M_END;
               Delim.Associate := First;
               Buffers.Next (Pos);
               Scan_Link_Title (Parser, Pos, ')', Link, Title);
               if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ')' then
                  Delimiters.Append (Delim);
                  declare
                     Bracket : constant Delimiter_Vectors.Reference_Type
                       := Delimiters.Reference (First);
                  begin
                     Kind := Bracket.Marker;
                     Bracket.Marker :=
                       (if Bracket.Marker = M_BRACKET_IMAGE then M_IMAGE else M_LINK);
                     Bracket.Associate := Delimiters.Last_Index;
                  end;
                  Buffers.Next (Pos);
                  Prev_C := ')';
                  Text := Pos;
                  if Kind = M_BRACKET then
                     Clear_Brackets;
                  end if;
                  Process_Emphasis (First + 1, Delimiters.Last_Index);
                  return;
               end if;
               Delimiters.Reference (First).Marker := M_TEXT;
               return;
            end;
         elsif C = '[' then
            declare
               Label : Wiki.Strings.BString (128);
               Delim : Delimiter_Type;
               Kind  : Marker_Kind;
            begin
               Delim.Cursor := Text;
               Delim.Marker := M_END;
               Delim.Associate := First;
               Buffers.Next (Pos);
               Parse_Link_Label (Parser, Pos, Label, False);
               if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ']' then
                  Delimiters.Append (Delim);
                  declare
                     Bracket : constant Delimiter_Vectors.Reference_Type
                       := Delimiters.Reference (First);
                  begin
                     Kind := Bracket.Marker;
                     Bracket.Marker :=
                       (if Bracket.Marker = M_BRACKET_IMAGE then M_IMAGE_REF else M_LINK);
                     Bracket.Associate := Delimiters.Last_Index;
                  end;
                  Buffers.Next (Pos);
                  Prev_C := ']';
                  Text := Pos;
                  if Kind = M_BRACKET then
                     Clear_Brackets;
                  end if;
                  Process_Emphasis (First + 1, Delimiters.Last_Index);
               end if;
            end;

         elsif Delimiters.Element (First).Marker in M_BRACKET | M_BRACKET_IMAGE then
            if Delimiters.Element (First).Cursor.Pos + 1 >= Text.Pos then
               Clear_Brackets;
               return;
            end if;
            declare
               Delim : Delimiter_Type;
            begin
               Delim.Cursor := Text;
               Delim.Marker := M_END;
               Delim.Associate := First;
               Delimiters.Append (Delim);
               Delimiters.Reference (First).Associate := Delimiters.Last_Index;
               Process_Emphasis (First + 1, Delimiters.Last_Index);
            end;
         end if;
         declare
            Bracket : constant Delimiter_Vectors.Reference_Type
              := Delimiters.Reference (First);
         begin
            if Buffers.Is_Valid (Pos) then
               if Bracket.Marker in M_BRACKET | M_BRACKET_IMAGE then
                  Text := Pos;
                  Bracket.Marker := (if Bracket.Marker = M_BRACKET
                                     then M_LINK_REF else M_IMAGE_REF);
                  return;
               elsif Bracket.Marker in M_IMAGE | M_LINK | M_IMAGE_REF then
                  return;
               end if;
            end if;
            Bracket.Marker := M_TEXT;
         end;
         Clear_Brackets;
      end Parse_Link;

      Pos          : Wiki.Buffers.Cursor := Text;
      Delim       : Delimiter_Type;
      First_Pos   : Wiki.Buffers.Cursor;
   begin
      if not Buffers.Is_Valid (Pos) then
         return;
      end if;

      if Parser.Current_Node in N_NONE | N_PARAGRAPH or else Parser.Need_Paragraph then
         Parse_Link_Definition (Parser, Pos);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
      end if;
      First_Pos := Pos;

      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         case C is
            when '\' =>
               Scan_Linebreak (Pos, Delim);
               if Delim.Count > 0 then
                  Delimiters.Append (Delim);
               else
                  Buffers.Next (Pos);
                  Buffers.Next (Pos);
                  Prev := C;
               end if;

            when '`' =>
               declare
                  End_Marker : Delimiter_Type;
               begin
                  Scan_Backtick (Parser, Pos, Delim, End_Marker);
                  if Delim.Count > 0 then
                     Delimiters.Append (Delim);
                     Delimiters.Append (End_Marker);
                  end if;
               end;
               Prev := C;

            when '*' =>
               Get_Delimiter (Pos, Prev, C, Delim);
               if Delim.Can_Open or else Delim.Can_Close then
                  Delim.Marker := M_STAR;
                  Delimiters.Append (Delim);
               end if;
               Prev := C;

            when '_' =>
               Get_Delimiter (Pos, Prev, C, Delim);
               if Delim.Can_Open or else Delim.Can_Close then
                  Delim.Marker := M_UNDERSCORE;
                  Delimiters.Append (Delim);
               end if;
               Prev := C;

            when '~' =>
               Get_Delimiter (Pos, Prev, C, Delim);
               if Delim.Can_Open or else Delim.Can_Close then
                  Delim.Marker := M_TILDE;
                  Delimiters.Append (Delim);
               end if;
               Prev := C;

            when '[' =>
               Delim.Marker := M_BRACKET;
               Delim.Cursor := Pos;
               Delim.Count := 1;
               Delim.Can_Open := True;
               Delim.Can_Close := False;
               Prev := C;
               Buffers.Next (Pos);
               exit when not Buffers.Is_Valid (Pos);
               C := Buffers.Char_At (Pos);
               Delim.Marker := M_BRACKET;
               Delimiters.Append (Delim);

            when ']' =>
               Prev := C;
               Parse_Link (Pos, Prev);

            when '!' =>
               Delim.Cursor := Pos;
               Buffers.Next (Pos);
               Prev := C;
               exit when not Buffers.Is_Valid (Pos);
               if Buffers.Char_At (Pos) = '[' then
                  Delim.Marker := M_BRACKET_IMAGE;
                  Delim.Count := 1;
                  Delim.Can_Close := False;
                  Delim.Can_Open := False;
                  Delimiters.Append (Delim);
                  Buffers.Next (Pos);
                  Prev := '[';
               end if;

            when '&' =>
               declare
                  Status : Wiki.Html_Parser.Entity_State_Type;
               begin
                  Delim.Cursor := Pos;
                  Common.Parse_Entity (Parser, Pos, Status, C);

                  if Status = Wiki.Html_Parser.ENTITY_VALID then
                     Delim.Cursor.Block.Content (Delim.Cursor.Pos) := C;
                     Buffers.Next (Delim.Cursor);
                     Delim.Marker := M_ENTITY;
                     Delim.Count := 1;
                     Delim.Can_Close := False;
                     Delim.Can_Open := False;
                     Delimiters.Append (Delim);
                  else
                     Buffers.Next (Pos);
                  end if;
               end;

            when ' ' =>
               Scan_Linebreak (Pos, Delim);
               if Delim.Count > 0 then
                  Delimiters.Append (Delim);
               else
                  Buffers.Next (Pos);
               end if;
               Prev := C;

            when '<' =>
               Scan_Autolink (Pos, Delim);
               if Delim.Count > 0 then
                  Delimiters.Append (Delim);
                  Prev := '>';
               else
                  Scan_Inline_Html (Parser, Pos, Delim);
                  if Delim.Count > 0 then
                     Delimiters.Append (Delim);
                     Prev := '>';
                  else
                     Buffers.Next (Pos);
                  end if;
               end if;
               Prev := C;

            when others =>
               Buffers.Next (Pos);
               Prev := C;

         end case;
      end loop;

      if Parser.Current_Node = N_NONE or else Parser.Need_Paragraph then
         Parser.Context.Filters.Add_Node (Parser.Document, N_PARAGRAPH);
         Parser.Need_Paragraph := False;
      end if;
      Pos := First_Pos;
      Process_Emphasis (1, Delimiters.Last_Index);
      Process_Emphasis (Pos, 1, Delimiters.Last_Index);

      Add_Text (Parser, Pos, (null, 1));
      Flush_Text (Parser, Trim => Wiki.Parsers.Right);
   end Parse_Inline_Text;

end Wiki.Parsers.Markdown;
