-----------------------------------------------------------------------
--  wiki-parsers-markdown -- Markdown parser operations
--  Copyright (C) 2016 - 2024 Stephane Carrez
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
                        M_ENTITY, M_TEXT, M_LINEBREAK);

   type Delimiter_Index_Type is new Natural;
   subtype Delimiter_Index is Delimiter_Index_Type range 1 .. Delimiter_Index_Type'Last;
   type Delimiter_Type;

   type Delimiter_Type is record
      Marker    : Marker_Kind := M_END;
      Pos       : Natural := 0;
      Block     : Wiki.Buffers.Buffer_Access;
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

   function Get_Header_Level (Text   : in out Wiki.Buffers.Buffer_Access;
                              From   : in out Positive) return Natural;
   function Is_Thematic_Break (Text      : in Wiki.Buffers.Buffer_Access;
                               From      : in Positive;
                               Token     : in Wiki.Strings.WChar) return Boolean;
   procedure Get_List_Level (Parser : in out Parser_Type;
                             Text   : in out Wiki.Buffers.Buffer_Access;
                             From   : in out Positive;
                             Level  : out Integer;
                             Indent : out Natural;
                             Marker : out Wiki.Strings.WChar);
   function Is_End_Preformat (Text   : in Wiki.Buffers.Buffer_Access;
                              From   : in Positive;
                              Expect : in WChar;
                              Length : in Positive) return Boolean;
   procedure Scan_Link_Title (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Buffer_Access;
                              From   : in out Positive;
                              Expect : in Wiki.Strings.WChar;
                              Link   : in out Wiki.Strings.BString;
                              Title  : in out Wiki.Strings.BString);
   procedure Add_Header (Parser : in out Parser_Type;
                         Level  : in Positive);
   procedure Add_Text (Parser   : in out Parser_Type;
                       Text     : in out Wiki.Buffers.Buffer_Access;
                       From     : in out Positive;
                       Limit    : in Wiki.Buffers.Buffer_Access;
                       Last_Pos : in Positive);
   procedure Add_Autolink (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Buffer_Access;
                           From   : in out Positive;
                           Length : in Natural);

   procedure Parse_Table (Parser : in out Parser_Type;
                          Text   : in out Wiki.Buffers.Buffer_Access;
                          From   : in out Positive;
                          Kind   : in Nodes.Row_Kind);
   procedure Get_Delimiter (Text        : in out Wiki.Buffers.Buffer_Access;
                            From        : in out Positive;
                            Before_Char : Strings.WChar;
                            C           : in Strings.WChar;
                            Delim       : in out Delimiter_Type);

   procedure Parse_Link_Label (Parser : in out Parser_Type;
                               Text   : in out Wiki.Buffers.Buffer_Access;
                               From   : in out Positive;
                               Label  : in out Wiki.Strings.BString;
                               Allow_Bracket : in Boolean);

   procedure Parse_Link_Definition (Parser : in out Parser_Type;
                                    Text   : in out Wiki.Buffers.Buffer_Access;
                                    From   : in out Positive);

   procedure Scan_Backtick (Parser   : in out Parser_Type;
                            Text     : in out Wiki.Buffers.Buffer_Access;
                            From     : in out Positive;
                            Start    : in out Delimiter_Type;
                            Stop     : in out Delimiter_Type);

   procedure Scan_Link (Text   : in out Wiki.Buffers.Buffer_Access;
                        From   : in out Positive;
                        Expect : in Wiki.Strings.WChar;
                        Link   : in out Wiki.Strings.BString;
                        Valid  : out Boolean);

   procedure Scan_Title (Text   : in out Wiki.Buffers.Buffer_Access;
                         From   : in out Positive;
                         Expect : in Wiki.Strings.WChar;
                         Title  : in out Wiki.Strings.BString;
                         Valid  : out Boolean);

   function Is_Escapable (C : in Wiki.Strings.WChar) return Boolean is
     (C in '!' .. '/' | ':' .. '@' | '{' .. '~' | '[' .. '`');

   function Get_Header_Level (Text   : in out Wiki.Buffers.Buffer_Access;
                              From   : in out Positive) return Natural is
      Count : constant Natural := Buffers.Count_Occurence (Text, From, '#');
      Space_Count : Natural;
   begin
      if Count > 6 then
         return 0;
      end if;
      if From + Count > Text.Last then
         return 0;
      end if;
      if not Is_Space_Or_Newline (Text.Content (From + Count)) then
         return 0;
      end if;
      From := From + Count;
      Wiki.Buffers.Skip_Spaces (Text, From, Space_Count);
      return Count;
   end Get_Header_Level;

   function Is_Thematic_Break (Text      : in Wiki.Buffers.Buffer_Access;
                               From      : in Positive;
                               Token     : in Wiki.Strings.WChar) return Boolean is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      Count : Natural := 1;
      C     : Wiki.Strings.WChar;
   begin
      loop
         C := Buffers.Next (Block, Pos);
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
   function Get_Setext_Heading (Parser    : in out Parser_Type;
                                Text      : in Wiki.Buffers.Buffer_Access;
                                From      : in Positive) return Natural is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
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
      C := Block.Content (Pos);
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
         C := Buffers.Next (Block, Pos);
      end loop;
   end Get_Setext_Heading;

   --  Parse a preformatted header block.
   --  Example:
   --    ```
   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Buffer_Access;
                                 From   : in out Positive;
                                 Marker : in Wiki.Strings.WChar;
                                 Keep_Block : in Boolean := False) is
      Count : constant Natural := Buffers.Count_Occurence (Text, From, Marker);
   begin
      if Count < 3 then
         return;
      end if;

      --  Extract the format either 'Ada' or '[Ada]'
      declare
         Pos   : Natural := From;
         Block : Wiki.Buffers.Buffer_Access := Text;
         Space_Count : Natural;
         C : Wiki.Strings.WChar;
         Has_Bracket : Boolean := False;
      begin
         for I in 1 .. Count loop
            Buffers.Next (Block, Pos);
         end loop;
         Wiki.Strings.Clear (Parser.Preformat_Format);
         Buffers.Skip_Spaces (Block, Pos, Space_Count);
         if Block /= null and then Pos <= Block.Last and then Block.Content (Pos) = '[' then
            Buffers.Next (Block, Pos);
            Has_Bracket := False;
         end if;
         while Block /= null and then Pos <= Block.Last loop
            C := Block.Content (Pos);
            exit when Helpers.Is_Space_Or_Tab (C) or else Helpers.Is_Newline (C);
            if C = '`' and then Marker = C then
               --  The backtick cannot occur in the string for a ``` code block.
               return;
            end if;
            exit when Has_Bracket and C = ']';
            if C = '\' then
               Buffers.Next (Block, Pos);
               if Block = null then
                  return;
               end if;
               C := Block.Content (Pos);
            end if;
            Wiki.Strings.Append_Char (Parser.Preformat_Format, C);
            Buffers.Next (Block, Pos);
         end loop;
         if Block /= null then
            Buffers.Skip_Spaces (Block, Pos, Space_Count);
         end if;
         while Block /= null and then Pos <= Block.Last loop
            C := Block.Content (Pos);
            if C = '`' and then Marker = C then
               --  The backtick cannot occur in the string for a ``` code block.
               return;
            end if;
            Buffers.Next (Block, Pos);
            exit when Helpers.Is_Newline (C);
         end loop;
         Text := Block;
         From := Pos;
      end;

      --  If we are in a list, it is loose due to the blank line.
      if Parser.Previous_Line_Empty > 0 then
         Documents.Set_Loose (Parser.Document);
      end if;
      Parser.Preformat_Indent := Parser.Indent;
      Parser.Preformat_Fence := Marker;
      Parser.Preformat_Fcount := Count;
      Flush_Block (Parser, Trim => Right);
      if not Keep_Block then
         Pop_Block (Parser);
      end if;
      Push_Block (Parser, N_PREFORMAT, Parser.Indent);
      Parser.Previous_Line_Empty := 0;
   end Parse_Preformatted;

   procedure Get_List_Level (Parser : in out Parser_Type;
                             Text   : in out Wiki.Buffers.Buffer_Access;
                             From   : in out Positive;
                             Level  : out Integer;
                             Indent : out Natural;
                             Marker : out WChar) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      C     : Strings.WChar;
      Limit : Natural := 9;
   begin
      Level := 0;
      Indent := 0;
      Marker := Helpers.NUL;
      Main :
      while Block /= null loop
         declare
            Last  : constant Natural := Block.Last;
            Count : Natural;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               if C in '0' .. '9' then
                  --  Must not exceed 9 digits
                  exit Main when Limit = 0;
                  Level := Level * 10;
                  Level := Level + WChar'Pos (C) - WChar'Pos ('0');
                  Indent := Indent + 1;
                  Limit := Limit - 1;
               elsif C in '.' | ')' then
                  Indent := Indent + 1;
                  Buffers.Next (Block, Pos);
                  exit Main when Block = null;
                  Buffers.Skip_Spaces (Block, Pos, Count);
                  exit Main when Count = 0;

                  --  A list that interrupts a paragraph must start with 1.
                  exit Main when Level /= 1
                    and then not (Parser.Current_Node in Nodes.N_LIST_ITEM)
                    and then Parser.Text_Buffer.Length > 0;

                  --  An empty list item cannot interrupt a paragraph.
                  exit Main when (Block = null and then Parser.Text_Buffer.Length > 0);
                  Indent := Indent + Count;
                  Marker := C;
                  Text := Block;
                  From := Pos;
                  return;
               else
                  Level := -1;
                  return;
               end if;
               Pos := Pos + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main;
      Level := -1;
   end Get_List_Level;

   procedure Add_Header (Parser : in out Parser_Type;
                         Level  : in Positive) is
      Top     : Block_Access;
   begin
      if not Parser.Context.Is_Hidden then
         Block_Stack.Push (Parser.Blocks);
         Top := Current (Parser);
         Top.Kind := N_HEADER;
         Top.Level := Level;
         Parser.Need_Paragraph := False;
         Parser.Current_Node := N_HEADER;
         Parser.Context.Filters.Start_Block (Parser.Document, Nodes.N_HEADER, Level);
         Pop_Block (Parser);
      end if;
      Parser.Empty_Line   := True;
      Parser.In_Paragraph := False;
      Buffers.Clear (Parser.Text_Buffer);
   end Add_Header;

   function Is_End_Preformat (Text   : in Wiki.Buffers.Buffer_Access;
                              From   : in Positive;
                              Expect : in WChar;
                              Length : in Positive) return Boolean is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      C     : WChar;
      Count : Natural := 0;
   begin
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               if Is_Newline (C) then
                  return Count >= Length;
               end if;
               if C /= Expect then
                  if Count >= Length and then Is_Space (C) then
                     Buffers.Skip_Spaces (Block, Pos, Count);
                     return Block = null or else Is_Newline (Block.Content (Pos));
                  end if;
                  return False;
               end if;
               Pos := Pos + 1;
               Count := Count + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
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

   procedure Parse_Columns (Parser  : in out Parser_Type;
                            Text    : in Wiki.Buffers.Buffer_Access;
                            From    : in Positive;
                            Columns : in out Nodes.Column_Array_Style;
                            Count   : out Natural) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
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
      while Pos <= Block.Last loop
         C := Block.Content (Pos);
         exit when C /= '|';
         Style := Nodes.ALIGN_DEFAULT;
         Bar_Count := Bar_Count + 1;
         Buffers.Next (Block, Pos);
         Buffers.Skip_Spaces (Block, Pos, Space_Count, Line_Count);
         exit when Line_Count > 0 or else Block = null or else Pos > Block.Last;
         C := Block.Content (Pos);
         if C = ':' then
            Buffers.Next (Block, Pos);
            Style := Nodes.ALIGN_LEFT;
         end if;
         Width := 0;
         while Block /= null and then Pos <= Block.Last loop
            C := Block.Content (Pos);
            exit when C /= '-';
            Buffers.Next (Block, Pos);
            Width := Width + 1;
         end loop;
         if Width = 0 then
            Count := 0;
            return;
         end if;
         if C = ':' then
            Buffers.Next (Block, Pos);
            Style := (if Style = Nodes.ALIGN_LEFT then Nodes.ALIGN_CENTER else Nodes.ALIGN_RIGHT);
         end if;
         Buffers.Skip_Spaces (Block, Pos, Space_Count, Line_Count);
         if Line_Count > 0 or else Block = null or else Pos > Block.Last then
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
                          Text   : in out Wiki.Buffers.Buffer_Access;
                          From   : in out Positive;
                          Kind   : in Nodes.Row_Kind) is
      Block       : Wiki.Buffers.Buffer_Access := Text;
      Pos         : Positive := From + 1;
      C           : Wiki.Strings.WChar;
      Skip_Spaces : Boolean := True;
   begin
      --  if Parser.Current_Node /= Nodes.N_TABLE then
      --   Flush_Block (Parser);
      --   Push_Block (Parser, Nodes.N_TABLE);
      --  end if;
      Parser.Context.Filters.Add_Row (Parser.Document, Kind);
      Wiki.Attributes.Clear (Parser.Attributes);

      Main :
      while Block /= null loop
         declare
            Last : Natural := Block.Last;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               if Skip_Spaces and then Is_Space_Or_Newline (C) then
                  Pos := Pos + 1;
               else
                  if Skip_Spaces then
                     Skip_Spaces := False;
                     Parser.Context.Filters.Add_Column (Parser.Document, Parser.Attributes);
                  end if;

                  if C = '\' then
                     Buffers.Next (Block, Pos);
                     exit Main when Block = null;
                     Last := Block.Last;
                     C := Block.Content (Pos);
                     Buffers.Append (Parser.Text_Buffer, C);
                  elsif C = '|' then
                     Skip_Spaces := True;
                     Flush_Block (Parser);
                  else
                     Buffers.Append (Parser.Text_Buffer, C);
                  end if;
                  Pos := Pos + 1;
               end if;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main;
      if not Skip_Spaces then
         Flush_Block (Parser);
      end if;
      Text := Block;
      From := Pos;
   end Parse_Table;

   --  Parse a markdown header and column specification.
   --  The column header was saved in the `Parser.Text_Buffer` and the
   --  column specification is passed in the `Text` buffer.
   --  Example:
   --    | col1 | col2 | ... | colN |
   --    | ---- | ---- | ... | ---- |
   procedure Parse_Table_Header (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Buffer_Access;
                                 From   : in out Positive;
                                 C      : in out Wiki.Strings.WChar) is
      Count   : Natural;
      Empty   : Nodes.Column_Array_Style (1 .. 0);
   begin
      Parse_Columns (Parser, Text, From, Empty, Count);
      if Count = 0 then
         return;
      end if;

      --  Save the text buffer because we must not write it yet.
      declare
         Block      : Wiki.Buffers.Buffer_Access := Text;
         Pos        : Positive := From + 1;
         Bar_Count  : Natural := 0;
         Char_Count : Natural := 0;
         Header     : Wiki.Buffers.Builder (512);
         Is_Newline : Boolean := True;
         Truncate_Block : Wiki.Buffers.Buffer_Access;
         Truncate_Pos   : Positive := 1;
      begin
         Block := Parser.Text_Buffer.First'Unchecked_Access;
         Pos := 1;
         while Block /= null and then Pos <= Block.Last loop
            C := Block.Content (Pos);
            exit when Is_Newline and then C = '|';
            Buffers.Next (Block, Pos);
            --  Buffers.Append (Header, C);
            if Helpers.Is_Newline (C) then
               Is_Newline := True;
            else
               Is_Newline := False;
            end if;
         end loop;
         Truncate_Pos := Pos;
         Truncate_Block := Block;

         while Block /= null and then Pos <= Block.Last loop
            C := Block.Content (Pos);
            Buffers.Next (Block, Pos);
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

         Buffers.Truncate (Parser.Text_Buffer, Truncate_Block, Truncate_Pos);
         --  Buffers.Clear (Parser.Text_Buffer);

         --  Get the column layout and create the table.
         declare
            Columns : Nodes.Column_Array_Style (1 .. Count);
         begin
            Parse_Columns (Parser, Text, From, Columns, Count);
            Push_Block (Parser, Nodes.N_TABLE);
            Parser.Context.Filters.Add_Table (Parser.Document, Columns);
         end;

         --  The header is not empty, emit the table row header.
         if Char_Count > 0 then
            Pos := 1;
            Block := Header.First'Unchecked_Access;
            Parse_Table (Parser, Block, Pos, Nodes.N_ROW_HEADER);
         end if;
         Text := null;
         From := Pos;
         C := Helpers.NUL;
      end;
   end Parse_Table_Header;

   --  Parse a markdown link definition.
   --  Example:
   --    [label]: url
   --    [label]: url "title"
   procedure Parse_Link_Label (Parser : in out Parser_Type;
                               Text   : in out Wiki.Buffers.Buffer_Access;
                               From   : in out Positive;
                               Label  : in out Wiki.Strings.BString;
                               Allow_Bracket : in Boolean) is
      pragma Unreferenced (Parser);

      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      C     : Wiki.Strings.WChar;
      Has_Space : Boolean := False;
      Count_Bracket : Natural := 0;
   begin
      Main :
      while Block /= null loop
         while Pos <= Block.Last loop
            C := Block.Content (Pos);
            if C = '\' then
               Buffers.Next (Block, Pos);
               exit Main when Block = null;
               C := Block.Content (Pos);
               if Is_Escapable (C) then
                  Buffers.Next (Block, Pos);
                  exit Main when Block = null;
                  Append (Label, '\');
                  Append (Label, C);
               else
                  Append (Label, '\');
                  Append (Label, C);
               end if;
               Has_Space := False;
            elsif C = '[' then
               exit Main when not Allow_Bracket;
               Count_Bracket := Count_Bracket + 1;
               Has_Space := False;
               Append (Label, C);
               Pos := Pos + 1;
            elsif C = ']' and then Count_Bracket = 0 then
               Text := Block;
               From := Pos;
               return;
            elsif Helpers.Is_Space_Or_Newline (C) then
               if not Has_Space then
                  Append (Label, ' ');
               end if;
               Has_Space := True;
               Pos := Pos + 1;
            else
               if C = '[' then
                  Count_Bracket := Count_Bracket + 1;
               elsif C = ']' then
                  exit Main when not Allow_Bracket;
                  Count_Bracket := Count_Bracket - 1;
               end if;
               Has_Space := False;
               Append (Label, C);
               Pos := Pos + 1;
            end if;
         end loop;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main;
      Text := null;
   end Parse_Link_Label;

   --  Parse a markdown link definition.
   --  Example:
   --    [label]: url
   --    [label]: url "title"
   --    [label]: <url>
   procedure Parse_Link_Definition (Parser : in out Parser_Type;
                                    Text   : in out Wiki.Buffers.Buffer_Access;
                                    From   : in out Positive) is
      Block       : Wiki.Buffers.Buffer_Access := Text;
      Pos         : Positive := From;
      Label       : Wiki.Strings.BString (128);
      Link        : Wiki.Strings.BString (128);
      Title       : Wiki.Strings.BString (128);
      Space_Count : Natural;
      Is_Valid    : Boolean;
   begin
      while Block /= null and then Block.Content (Pos) = '[' loop
         Buffers.Next (Block, Pos);
         Parse_Link_Label (Parser, Block, Pos, Label, False);
         if Wiki.Strings.Length (Label) = 0 then
            return;
         end if;
         if Block = null or else Block.Content (Pos) /= ']' then
            return;
         end if;
         Buffers.Next (Block, Pos);
         if Block = null then
            return;
         end if;
         if Block.Content (Pos) /= ':' then
            return;
         end if;
         Buffers.Next (Block, Pos);
         Buffers.Skip_Spaces (Block, Pos, Space_Count);
         Scan_Link (Block, Pos, ' ', Link, Is_Valid);
         if Block = null or else not Is_Valid then
            return;
         end if;
         Scan_Title (Block, Pos, ' ', Title, Is_Valid);
         exit when not Is_Valid;
         Parser.Document.Set_Link (Strings.To_WString (Label),
                                   Helpers.Encode_URI (Link),
                                   Strings.To_WString (Title));
         Text := Block;
         From := Pos;
         Strings.Clear (Label);
         Strings.Clear (Link);
         Strings.Clear (Title);
      end loop;
   end Parse_Link_Definition;

   function Is_List_Item (From   : in Wiki.Buffers.Buffer_Access;
                          Start  : in Natural;
                          Marker : in Wiki.Strings.WChar) return Boolean is
      Block : Wiki.Buffers.Buffer_Access := From;
      Pos   : Natural := Start;
      C     : Wiki.Strings.WChar;
   begin
      C := Block.Content (Pos);
      if C /= Marker then
         return False;
      end if;
      Buffers.Next (Block, Pos);
      if Block = null then
         return False;
      end if;
      return Helpers.Is_Space_Or_Tab (Block.Content (Pos));
   end Is_List_Item;

   procedure Parse_Block (Parser : in out Parser_Type;
                          Block  : in out Wiki.Buffers.Buffer_Access;
                          Pos    : in out Natural;
                          C      : out Wiki.Strings.WChar) is

      procedure Process (Nodes : in Block_Stack.Element_Type_Array) is
         Need_Char : Boolean := True;
         Matched   : Boolean;
         I         : Positive := Nodes'First;
         Indent    : Natural := 0;
      begin
         while I <= Nodes'Last loop
            if Need_Char then
               First_Nonspace (Parser, Block, Pos, C);
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
                             and then not Is_List_Item (Block, Pos, Nodes (I).Marker));
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
                          --  and then Parser.Indent > 0
                          and then Parser.Indent < Nodes (I + 1).Level
                          and then (Parser.Text_Buffer.Length = 0
                                    or else Parser.Previous_Line_Empty > 0)
                        then
                           Matched := False;
                           Indent := 0;
                           if Is_List_Item (Block, Pos, Nodes (I).Marker) then
                              I := I + 1;
                              Documents.Set_Loose (Parser.Document);
                              Parser.Need_Paragraph := False;
                           end if;
                           --if Parser.Need_Paragraph and then Parser.Previous_Line_Empty > 0
                           --then
                           --   Documents.Set_Loose (Parser.Document);
                              --  Parser.Need_Paragraph := Nodes (I).Line_Count > 1;
                           --end if;
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
                    and then Is_End_Preformat (Block, Pos, C, Parser.Preformat_Fcount)
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
                     Common.Append (Parser.Text, Block, Pos);
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
            C := Buffers.Next (Block, Pos);
         end if;
      end Process;

   begin
      if not Block_Stack.Is_Empty (Parser.Blocks) then
         Block_Stack.Read (Parser.Blocks, Process'Access);
      else
         First_Nonspace (Parser, Block, Pos, C);
      end if;
   end Parse_Block;

   procedure Add_Text (Parser : in out Parser_Type;
                       Block  : in out Wiki.Buffers.Buffer_Access;
                       Pos    : in out Positive;
                       C      : in out Wiki.Strings.WChar) is
   begin
      if Parser.Current_Node = N_PREFORMAT then
         Common.Append (Parser.Text, Block, Pos);
         return;
      end if;

      if C in '*' | '-' and then Is_Thematic_Break (Block, Pos, C)
        and then Parser.Indent = 0
      then
         Add_Horizontal_Rule (Parser);
         return;
      end if;

      if C = ' ' then
         First_Nonspace (Parser, Block, Pos, C);
      end if;

      if Parser.Current_Node in N_BLOCKQUOTE then
         if not Parser.Is_Blank and then Parser.Text_Buffer.Length > 0 then
            declare
               Level : Integer;
            begin
               Level := Get_Setext_Heading (Parser, Block, Pos);
               if Level > 0 then
                  Add_Header (Parser, Level);
                  return;
               end if;
            end;
         end if;
         if Parser.Column = 0 and then Parser.Is_Blank then
            while Parser.Current_Node = N_BLOCKQUOTE loop
               Pop_Block (Parser);
            end loop;
            --  Parser.Quote_Level := 0;
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
                        or else (Top /= null and then Top.Line_Count > 0)) then
               Parser.Need_Paragraph := True;
            end if;
            Parser.Line_Count := 0;
            Parser.Previous_Line_Empty := Parser.Previous_Line_Empty + 1;
            --  Documents.Set_Loose (Parser.Document);
            if Parser.Column = 0 then
               while Parser.In_Blockquote loop
                  Pop_Block (Parser);
               end loop;
            end if;
         else
            --if Parser.Previous_Line_Empty > 0 then
            --   Documents.Set_Loose (Parser.Document);
            --end if;
            if Parser.Previous_Line_Empty > 0
              and then Parser.Text_Buffer.Length > 0
            then
               Flush_Block (Parser, Trim => Right);
               Parser.Need_Paragraph := True;
               Documents.Set_Loose (Parser.Document);
            end if;

            --if Top /= null then
            --   Top.Line_Count := Top.Line_Count + 1;
            --end if;
            Parser.Previous_Line_Empty := 0;
            Buffers.Append (Parser.Text_Buffer, Block, Pos);
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

   function Check_Trailing_Header (Text : in Wiki.Buffers.Buffer_Access;
                                   From : in Positive) return Boolean is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      Space_Count : Natural;
      Line_Count  : Natural;
      Count       : Natural;
   begin
      Buffers.Skip_Spaces (Block, Pos, Space_Count, Line_Count);
      if Block = null or else Line_Count > 0 then
         return True;
      end if;
      Buffers.Count_Occurence (Block, Pos, '#', Count);
      if Count = 0 then
         return False;
      end if;
      Buffers.Skip_Spaces (Block, Pos, Space_Count, Line_Count);
      return Block = null or else Line_Count > 0;
   end Check_Trailing_Header;

   procedure Append_Header (Parser : in out Parser_Type;
                            Block  : in out Wiki.Buffers.Buffer_Access;
                            Pos    : in out Natural) is
      C : Wiki.Strings.WChar;
   begin
      if Check_Trailing_Header (Block, Pos) then
         return;
      end if;
      while Block /= null and then Pos <= Block.Last loop
         C := Block.Content (Pos);
         if Helpers.Is_Space (C) then
            exit when Check_Trailing_Header (Block, Pos);
         end if;
         Buffers.Append (Parser.Text_Buffer, C);
         Buffers.Next (Block, Pos);
      end loop;
   end Append_Header;

   procedure Parse_Html (Parser : in out Parser_Type;
                         Block  : in out Wiki.Buffers.Buffer_Access;
                         From   : in out Natural;
                         C      : in out Wiki.Strings.WChar) is
      use type Wiki.Html_Parser.State_Type;

      procedure Process (Kind : in Wiki.Html_Parser.State_Type;
                         Name : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List);

      Has_Error : Boolean := False;

      procedure Process (Kind : in Wiki.Html_Parser.State_Type;
                         Name : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
      begin
         if Kind /= Wiki.Html_Parser.HTML_ERROR then
            Process_Html (Parser, Kind, Name, Attributes);
         else
            Has_Error := True;
         end if;
      end Process;

      Buffer : Wiki.Buffers.Buffer_Access := Block;
      Pos    : Positive := From;
   begin
      Buffers.Next (Buffer, Pos);
      loop
         Wiki.Html_Parser.Parse_Element (Parser.Html, Buffer.Content (1 .. Buffer.Last),
                                         Pos, Process'Access, Pos);
         if Pos = Buffer.Last + 1 then
            Buffer := Buffer.Next_Block;
            exit when Buffer = null;
            Pos := 1;
         end if;
         exit when Wiki.Html_Parser.Is_Empty (Parser.Html);
      end loop;
      if Has_Error then
         Buffers.Append (Parser.Text_Buffer, Block, From);
         Buffer := null;
      end if;
      Block := Buffer;
      From := Pos;
   end Parse_Html;

   ---------------------
   -- Open_New_Blocks --
   ---------------------

   procedure Open_New_Blocks (Parser : in out Parser_Type;
                              Block  : in out Wiki.Buffers.Buffer_Access;
                              Pos    : in out Natural;
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
                  if Block /= null and then Pos + 1 <= Block.Last
                    and then Wiki.Helpers.Is_Space (Block.Content (Pos + 1))
                  then
                     Parser.Column := Parser.Column + 1;
                     Buffers.Next (Block, Pos);
                  end if;
                  Need_Char := True;

               when '#' =>
                  Level := Get_Header_Level (Block, Pos);
                  if Level > 0 then
                     Push_Block (Parser, Nodes.N_HEADER, Level);
                     Parser.Previous_Line_Empty := 0;
                     Append_Header (Parser, Block, Pos);
                     --  Buffers.Append (Parser.Text_Buffer, Block, Pos);
                     Parser.Line_Count := Parser.Line_Count + 1;
                     --  Trim_Header (Parser.Text_Buffer);
                     C := Helpers.NUL;
                     return;
                  end if;
                  return;

               when '_' =>
                  if Is_Thematic_Break (Block, Pos, C) then
                     Add_Horizontal_Rule (Parser);
                     C := Helpers.NUL;
                  end if;
                  return;

               when '=' =>
                  Level := Get_Setext_Heading (Parser, Block, Pos);
                  if Level > 0 then
                     Add_Header (Parser, Level);
                     C := Helpers.NUL;
                  end if;
                  return;

               when '*' | '-' | '+' =>
                  if C in '*' | '-' then
                     declare
                        Is_Break : constant Boolean := Is_Thematic_Break (Block, Pos, C);
                     begin
                        Level := Get_Setext_Heading (Parser, Block, Pos);
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
                              Add_Header (Parser, Level);
                              C := Helpers.NUL;
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

                  if (Pos + 1 <= Block.Last and then Helpers.Is_Space_Or_Tab (Block.Content (Pos + 1)))
                    or else (Pos + 1 <= Block.Last and then Helpers.Is_Newline (Block.Content (Pos + 1))
                             and then Parser.Text_Buffer.Length = 0)
                  then
                     declare
                        Top   : Block_Access := Current (Parser);
                        Count : Natural;
                     begin
                        Buffers.Next (Block, Pos);
                        if Block /= null then
                           Buffers.Skip_Spaces (Block, Pos, Count);
                           if Block = null or else Helpers.Is_Newline (Block.Content (Pos)) then
                              Count := 0;
                           end if;
                        else
                           Count := 0;
                        end if;
                        Level := Parser.Column + Count;
                        if Top /= null and then Top.Kind /= N_BLOCKQUOTE then
                           if Top.Kind = N_LIST_START then
                              if top.Level + 1 > Parser.Column
                                or else (Top.Marker /= C and then Top.Level + 1 >= Parser.Column)
                              then
                                 Pop_List (Parser, Parser.Column, C, 0);
                                 Top := Current (Parser);
                              end if;
                           elsif Top.Level + 1 <= Parser.Column and then Top.Kind /= N_LIST_START then
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
                        if Block /= null and then Pos <= Block.Last then
                           C := Block.Content (Pos);
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
                     Get_List_Level (Parser, Block, Pos, Number, Indent, Marker);
                     if Number >= 0 then
                        Parser.Previous_Line_Empty := 0;
                        if Block /= null then
                           Buffers.Skip_Spaces (Block, Pos, Count);
                        end if;
                        Level := Parser.Column;
                        if Top /= null then
                           if Top.Level > Indent or else (Top.Marker /= Marker and then Top.Level >= Level) then
                              Pop_List (Parser, Parser.Column, C, 0);
                              Top := Current (Parser);
                           elsif Top.Level < Indent and then Top.Kind /= N_NUM_LIST_START then
                              Push_Block (Parser, Nodes.N_NUM_LIST_START,
                                          Level => Parser.Column - 1, Marker => Marker, Number => Number);
                           else
                              if Top.Kind = N_LIST_ITEM then
                                 Pop_Block (Parser, Trim => Right);
                              end if;
                           end if;
                        end if;
                        if Top = null or else Top.Kind not in N_LIST_ITEM | N_NUM_LIST_START then
                           Push_Block (Parser, Nodes.N_NUM_LIST_START,
                                       Level => Parser.Column - 1, Marker => Marker, Number => Number);
                        end if;
                        Push_Block (Parser, Nodes.N_LIST_ITEM,
                                    Level => Indent, Marker => Marker, Number => Number);
                        Parser.Previous_Line_Empty := 0;
                        if Block /= null and then Pos <= Block.Last then
                           C := Block.Content (Pos);
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
                  Parse_Preformatted (Parser, Block, Pos, C, True);
                  if Parser.Current_Node = Nodes.N_PREFORMAT then
                     Common.Append (Parser.Text, Block, Pos);
                     C := Helpers.NUL;
                  end if;
                  return;

               when '|' =>
                  if Parser.Current_Node = N_TABLE then
                     Parse_Table (Parser, Block, Pos, Nodes.N_ROW);
                     if Block = null then
                        C := Helpers.NUL;
                     end if;
                     return;
                  end if;
                  if Parser.Text_Buffer.Length > 0 then
                     Parse_Table_Header (Parser, Block, Pos, C);
                  end if;
                  return;

               when '<' =>
                  declare
                     Text      : Wiki.Buffers.Buffer_Access := Block;
                     Text_Pos  : Natural := Pos;
                     Count     : Natural;
                  begin
                     loop
                        Parse_Html (Parser, Text, Text_Pos, C);
                        if Text = null then
                           C := Helpers.NUL;
                           return;
                        end if;
                        Block := Text;
                        Pos := Text_Pos;
                        if Parser.In_Html /= HTML_NONE then
                           Buffers.Skip_Spaces (Text, Text_Pos, Count);
                           if Text = null or else Text_Pos > Text.Last then
                              if Parser.In_Html = HTML_BLOCK then
                                 Append_Char (Parser.Text, ' ');
                              end if;
                              C := Helpers.NUL;
                              return;
                           end if;
                           Parser.In_Html := HTML_NONE;
                           C := Text.Content (Text_Pos);
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
            Common.Append (Parser.Text, Block, Pos);
            C := Helpers.NUL;
            return;
         else
            return;
         end if;
         if Need_Char then
            First_Nonspace (Parser, Block, Pos, C);
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
                         Text   : in Wiki.Buffers.Buffer_Access) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Natural := 0;
      C      : WChar;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Pos := 1;
         Common.Parse_Html_Element (Parser, Block, Pos, Start => False);
         if Block = null then
            return;
         end if;
         if Parser.In_Html /= HTML_NONE then
            Common.Parse_Html_Preformatted (Parser, Block, Pos);
            return;
         end if;
      elsif Parser.In_Html /= HTML_NONE then
         if Parser.In_Html = HTML_BLOCK and then Wiki.Helpers.Is_Newline (Block.Content (1)) then
            Parser.In_Html := HTML_NONE;
            Flush_Text (Parser, Trim => Both);
            Flush_Block (Parser);
            Parser.Need_Paragraph := True;
            Parser.Previous_Line_Empty := 1;
            return;
         end if;
         Pos := 1;
         Common.Parse_Html_Preformatted (Parser, Block, Pos);
         return;
      end if;

      Parser.Column := 0;
      Parse_Block (Parser, Block, Pos, C);
      Open_New_Blocks (Parser, Block, Pos, C);
      if C = Helpers.NUL then
         return;
      end if;
      Add_Text (Parser, Block, Pos, C);
      if Parser.Current_Node /= Nodes.N_NONE or else Parser.Current_Node = Nodes.N_NONE then
         return;
      end if;
   end Parse_Line;

   procedure Scan_Link (Text   : in out Wiki.Buffers.Buffer_Access;
                        From   : in out Positive;
                        Expect : in Wiki.Strings.WChar;
                        Link   : in out Wiki.Strings.BString;
                        Valid  : out Boolean) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      C      : Wiki.Strings.WChar := Helpers.NUL;
      Space_Count : Natural;
      Paren_Count : Integer := 0;
   begin
      Valid := False;
      Wiki.Strings.Clear (Link);
      if Block /= null and then Block.Content (Pos) = '<' then
         Buffers.Next (Block, Pos);
         Scan_Bracket_Link :
         while Block /= null loop
            declare
               Last : constant Natural := Block.Last;
            begin
               while Pos <= Last loop
                  C := Block.Content (Pos);
                  exit Scan_Bracket_Link when C = '>';
                  if C = LF or else C = '<' then
                     return;
                  end if;
                  Pos := Pos + 1;
                  Append (Link, C);
               end loop;
            end;
            Block := Block.Next_Block;
            Pos := 1;
         end loop Scan_Bracket_Link;
         if Block /= null and then C = '>' then
            Buffers.Next (Block, Pos);
            Valid := True;
         end if;
      else
         Buffers.Skip_Ascii_Spaces (Block, Pos, Space_Count);
         Scan_Link :
         while Block /= null loop
            while Pos <= Block.Last loop
               C := Block.Content (Pos);
               exit Scan_Link when C = Expect or else Wiki.Helpers.Is_Space_Or_Newline (C);
               if C = '\' then
                  Buffers.Next (Block, Pos);
                  exit Scan_Link when Block = null;
                  C := Block.Content (Pos);
                  if Is_Escapable (C) then
                     Buffers.Next (Block, Pos);
                     exit Scan_Link when Block = null;
                     Append (Link, C);
                  else
                     Append (Link, '\');
                     Append (Link, C);
                     Buffers.Next (Block, Pos);
                     exit Scan_Link when Block = null;
                  end if;
               else
                  Pos := Pos + 1;
                  Append (Link, C);
                  if C = '(' then
                     Paren_Count := Paren_Count + 1;
                  elsif C = ')' then
                     Paren_Count := Paren_Count - 1;
                  end if;
               end if;
            end loop;
            Block := Block.Next_Block;
            Pos := 1;
         end loop Scan_Link;
         Valid := Paren_Count = 0;
         if Paren_Count /= 0 then
            Block := null;
         end if;
      end if;
      Text := Block;
      From := Pos;
   end Scan_Link;

   procedure Scan_Title (Text   : in out Wiki.Buffers.Buffer_Access;
                         From   : in out Positive;
                         Expect : in Wiki.Strings.WChar;
                         Title  : in out Wiki.Strings.BString;
                         Valid  : out Boolean) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      C      : Wiki.Strings.WChar := Helpers.NUL;
      Sep    : Wiki.Strings.WChar;
      Space_Count : Natural;
      Line_Count  : Natural;
      Paren_Count : Integer := 0;
   begin
      Wiki.Strings.Clear (Title);
      Buffers.Skip_Spaces (Block, Pos, Space_Count, Line_Count);
      Valid := Line_Count > 0 or else Block = null;
      if Valid then
         Text := Block;
         From := Pos;
      end if;
      if Block /= null and then Block.Content (Pos) in '"' | ''' | '(' then
         if Space_Count = 0 and then Line_Count = 0 then
            return;
         end if;
         Sep := Block.Content (Pos);
         if Sep = '(' then
            Sep := ')';
         end if;
         Buffers.Next (Block, Pos);
         Scan_Title :
         while Block /= null loop
            while Pos <= Block.Last loop
               C := Block.Content (Pos);
               exit Scan_Title when C = Sep;
               if C = '\' then
                  Buffers.Next (Block, Pos);
                  exit Scan_Title when Block = null;
                  C := Block.Content (Pos);
                  if Is_Escapable (C) then
                     Append (Title, C);
                     Buffers.Next (Block, Pos);
                     exit Scan_Title when Block = null;
                  else
                     Append (Title, '\');
                     Append (Title, C);
                     Buffers.Next (Block, Pos);
                     exit Scan_Title when Block = null;
                  end if;
               else
                  Pos := Pos + 1;
                  Append (Title, C);
               end if;
            end loop;
            Block := Block.Next_Block;
            Pos := 1;
         end loop Scan_Title;
         if C /= Sep then
            Wiki.Strings.Clear (Title);
            return;
         end if;
         while Block /= null loop
            Buffers.Next (Block, Pos);
            exit when Block = null or else Pos > Block.Last;
            C := Block.Content (Pos);
            exit when not Helpers.Is_Space (C) or else Helpers.Is_Newline (C);
         end loop;
         if Block /= null and then Helpers.Is_Newline (C) then
            Buffers.Next (Block, Pos);
         end if;
         if Block = null or else Pos >= Block.Last or else Helpers.Is_Newline (C) then
            Valid := True;
            Text := Block;
            From := Pos;
         end if;
      end if;
   end Scan_Title;

   procedure Scan_Link_Title (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Buffer_Access;
                              From   : in out Positive;
                              Expect : in Wiki.Strings.WChar;
                              Link   : in out Wiki.Strings.BString;
                              Title  : in out Wiki.Strings.BString) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      C      : Wiki.Strings.WChar := Helpers.NUL;
      Sep    : Wiki.Strings.WChar;
      Space_Count : Natural;
      Paren_Count : Integer := 0;
   begin
      Wiki.Strings.Clear (Link);
      Wiki.Strings.Clear (Title);
      if Block /= null and then Block.Content (Pos) = '<' then
         Buffers.Next (Block, Pos);
         Scan_Bracket_Link :
         while Block /= null loop
            declare
               Last : constant Natural := Block.Last;
            begin
               while Pos <= Last loop
                  C := Block.Content (Pos);
                  exit Scan_Bracket_Link when C = '>';
                  if C = LF or else C = '<' or else C = '\' then
                     return;
                  end if;
                  Pos := Pos + 1;
                  Append (Link, C);
               end loop;
            end;
            Block := Block.Next_Block;
            Pos := 1;
         end loop Scan_Bracket_Link;
         if Block /= null and then C = '>' then
            Buffers.Next (Block, Pos);
         end if;
      else
         Buffers.Skip_Ascii_Spaces (Block, Pos, Space_Count);
         Scan_Link :
         while Block /= null loop
            while Pos <= Block.Last loop
               C := Block.Content (Pos);
               exit Scan_Link when (C = Expect and then Paren_Count = 0)
                 or else Wiki.Helpers.Is_Space_Or_Tab (C)
                 or else Wiki.Helpers.Is_Newline (C);
               if C = '\' then
                  Buffers.Next (Block, Pos);
                  exit Scan_Link when Block = null;
                  C := Block.Content (Pos);
                  if Is_Escapable (C) then
                     Buffers.Next (Block, Pos);
                     exit Scan_Link when Block = null;
                     Append (Link, C);
                  else
                     Append (Link, '\');
                     Append (Link, C);
                     Buffers.Next (Block, Pos);
                     exit Scan_Link when Block = null;
                  end if;
               elsif C = '&' then
                  declare
                     Status : Wiki.Html_Parser.Entity_State_Type;
                  begin
                     Common.Parse_Entity (Parser, Block, Pos, Status, C);
                     if Status = Wiki.Html_Parser.ENTITY_VALID then
                        Append (Link, C);
                     else
                        Append (Link, C);
                     end if;
                  end;
               else
                  Pos := Pos + 1;
                  Append (Link, C);
                  if C = '(' then
                     Paren_Count := Paren_Count + 1;
                  elsif C = ')' then
                     Paren_Count := Paren_Count - 1;
                  end if;
               end if;
            end loop;
            Block := Block.Next_Block;
            Pos := 1;
         end loop Scan_Link;
         if Paren_Count /= 0 then
            Block := null;
         end if;
      end if;
      Buffers.Skip_Spaces (Block, Pos, Space_Count);
      if Block /= null and then Block.Content (Pos) in '"' | ''' | '(' then
         Sep := Block.Content (Pos);
         if Sep = '(' then
            Sep := ')';
         end if;
         Buffers.Next (Block, Pos);
         Scan_Title :
         while Block /= null loop
            while Pos <= Block.Last loop
               C := Block.Content (Pos);
               exit Scan_Title when C = Sep;
               if C = '\' then
                  Buffers.Next (Block, Pos);
                  exit Scan_Title when Block = null;
                  C := Block.Content (Pos);
                  if Is_Escapable (C) then
                     Append (Title, C);
                     Buffers.Next (Block, Pos);
                     exit Scan_Title when Block = null;
                  else
                     Append (Title, '\');
                     Append (Title, C);
                     Buffers.Next (Block, Pos);
                     exit Scan_Title when Block = null;
                  end if;
               else
                  Pos := Pos + 1;
                  Append (Title, C);
               end if;
            end loop;
            Block := Block.Next_Block;
            Pos := 1;
         end loop Scan_Title;
         if Block /= null then
            Buffers.Next (Block, Pos);
         end if;
         Buffers.Skip_Spaces (Block, Pos, Space_Count);
      end if;
      Text := Block;
      From := Pos;
   end Scan_Link_Title;

   procedure Get_Delimiter (Text        : in out Wiki.Buffers.Buffer_Access;
                            From        : in out Positive;
                            Before_Char : Strings.WChar;
                            C           : in Strings.WChar;
                            Delim       : in out Delimiter_Type) is
      Block         : Wiki.Buffers.Buffer_Access := Text;
      Pos           : Positive := From;
      After_Char    : Strings.WChar;
      Numdelims     : Natural := 1;
   begin
      Buffers.Next (Block, Pos);
      if C not in ''' | '"' then
         Count_Delimiters :
         while Block /= null loop
            while Pos <= Block.Last loop
               exit Count_Delimiters when Block.Content (Pos) /= C;
               Pos := Pos + 1;
               Numdelims := Numdelims + 1;
            end loop;
            Block := Block.Next_Block;
            Pos := Pos + 1;
         end loop Count_Delimiters;
      end if;
      if Block = null then
         After_Char := LF;
      else
         After_Char := Block.Content (Pos);
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
         Delim.Pos := From;
         Delim.Block := Text;
         Text := Block;
         From := Pos;
      end;
   end Get_Delimiter;

   procedure Add_Autolink (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Buffer_Access;
                           From   : in out Positive;
                           Length : in Natural) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      Link   : Wiki.Strings.BString (128);
      C      : Wiki.Strings.WChar;
   begin
      for I in 1 .. Length loop
         C := Block.Content (Pos);
         Buffers.Next (Block, Pos);
         Append_Char (Link, C);
      end loop;
      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR,
                                 Helpers.Encode_URI (Link));
         Parser.Context.Filters.Add_Link (Parser.Document,
                                          Strings.To_WString (Link),
                                          Parser.Attributes, False);
      end if;
      Buffers.Next (Block, Pos);
      Text := Block;
      From := Pos;
   end Add_Autolink;

   procedure Add_Text (Parser   : in out Parser_Type;
                       Text     : in out Wiki.Buffers.Buffer_Access;
                       From     : in out Positive;
                       Limit    : in Wiki.Buffers.Buffer_Access;
                       Last_Pos : in Positive) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
   begin
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
            C    : Wiki.Strings.WChar;
         begin
            while Pos <= Last loop
               if Block = Limit and then Pos = Last_Pos then
                  return;
               end if;
               C := Block.Content (Pos);
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
                  Buffers.Next (Block, Pos);
                  if Block = Limit and then Pos = Last_Pos then
                     Append (Parser.Text, '\');
                     return;
                  end if;
                  C := Block.Content (Pos);
                  if Wiki.Helpers.Is_Newline (C)
                    and then not Parser.Format (CODE)
                    and then (Limit /= null or else Pos < Last)
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
               Pos := Pos + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop;
      Text := Block;
      From := Pos;
   end Add_Text;

   procedure Scan_Backtick (Parser   : in out Parser_Type;
                            Text     : in out Wiki.Buffers.Buffer_Access;
                            From     : in out Positive;
                            Start    : in out Delimiter_Type;
                            Stop     : in out Delimiter_Type) is
      pragma Unreferenced (Parser);

      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      Count  : Natural;
      C      : Wiki.Strings.WChar;
      Prev   : Wiki.Strings.WChar := Helpers.NUL;
      Total  : Natural := 0;
   begin
      Start.Pos := Pos;
      Start.Block := Block;
      Buffers.Count_Occurence (Block, Pos, '`', Count);
      Text := Block;
      From := Pos;
      Start.Count := Count;
      while Block /= null loop
         while Pos <= Block.Last loop
            C := Block.Content (Pos);
            if C = '`' then
               Stop.Block := Block;
               Stop.Pos := Pos;
               Buffers.Count_Occurence (Block, Pos, '`', Count);

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
                     Stop.Pos := Stop.Pos - 1;
                     Stop.Skip_Count := 1;
                  else
                     Stop.Space_Before := False;
                     Start.Space_After := False;
                  end if;
                  Stop.Marker := M_CODE;
                  Text := Block;
                  From := Pos;
                  return;
               end if;
               Total := Total + 1;
            elsif (C = ' ' or else Helpers.Is_Newline (C)) and then Prev = Helpers.NUL then
               Start.Space_After := True;
               Pos := Pos + 1;
            else
               Pos := Pos + 1;
               Total := Total + 1;
            end if;
            Prev := C;
         end loop;
         Block := Block.Next_Block;
         Pos := 1;
      end loop;

      --  End of buffer reached: we have not found the end marker.
      Start.Count := 0;
   end Scan_Backtick;

   procedure Scan_Linebreak (Parser : in out Parser_Type;
                             Text   : in out Wiki.Buffers.Buffer_Access;
                             From   : in out Positive;
                             Delim  : in out Delimiter_Type) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      C      : Wiki.Strings.WChar;
      Count  : Natural := 0;
   begin
      C := Block.Content (Pos);
      Delim.Pos := Pos;
      Delim.Block := Block;
      Delim.Count := 0;
      if C = '\' then
         C := Buffers.Next (Block, Pos);
         if C /= Helpers.LF or else Block = null or else Pos + 1 > Block.Last then
            Delim.Count := 0;
            return;
         end if;
         Delim.Marker := M_LINEBREAK;
         Delim.Count := 2;
         Text := Block;
         From := Pos;
         return;
      end if;

      Main :
      while Block /= null loop
         while Pos <= Block.Last loop
            C := Block.Content (Pos);
            exit Main when C = Helpers.LF;
            if not Helpers.Is_Space (C) then
               return;
            end if;
            Count := Count + 1;
            Pos := Pos + 1;
         end loop;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main;
      if Count >= 2 and then Block /= null and then Pos + 1 <= Block.Last then
         Delim.Marker := M_LINEBREAK;
         Delim.Count := Count + 1;
         Text := Block;
         From := Pos;
         Buffers.Next (Text, From);
      end if;
   end Scan_Linebreak;

   procedure Scan_Autolink (Parser : in out Parser_Type;
                            Text   : in out Wiki.Buffers.Buffer_Access;
                            From   : in out Positive;
                            Delim  : in out Delimiter_Type);

   --  Scan the markdown autolink and setup the delimiter if there is a valid one.
   --  Autolinks have the form:
   --    <scheme:link>
   --  where `scheme` must start with a letter and must be between 2..32 characters.
   --  The autolink cannot contain spaces.
   procedure Scan_Autolink (Parser : in out Parser_Type;
                            Text   : in out Wiki.Buffers.Buffer_Access;
                            From   : in out Positive;
                            Delim  : in out Delimiter_Type) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      Count  : Natural;
      Length : Natural;
      C      : Wiki.Strings.WChar;
   begin
      Delim.Pos := From;
      Delim.Block := Text;
      Delim.Count := 0;
      Buffers.Next (Block, Pos);
      if Block = null then
         return;
      end if;
      C := Block.Content (Pos);
      if not (C in 'a' .. 'z' | 'A' .. 'Z') then
         return;
      end if;
      Buffers.Next (Block, Pos);
      Length := 1;

      --  Parse the scheme part.
      Count := 1;
      loop
         if Block = null or else Pos > Block.Last then
            return;
         end if;
         C := Block.Content (Pos);
         Buffers.Next (Block, Pos);
         Length := Length + 1;
         exit when C = ':';
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
      while Block /= null and then Pos <= Block.Last loop
         C := Block.Content (Pos);
         if C = '>' then
            Delim.Count := Length;
            Delim.Marker := M_AUTOLINK;
            Buffers.Next (Block, Pos);
            Text := Block;
            From := Pos;
            return;
         end if;

         --  Spaces are not allowed in autolink.
         exit when Helpers.Is_Space (C);
         Buffers.Next (Block, Pos);
         Length := Length + 1;
      end loop;
   end Scan_Autolink;

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
                                Text   : in Wiki.Buffers.Buffer_Access) is
      use Delimiter_Vectors;

      C          : Wiki.Strings.WChar;
      Prev       : Wiki.Strings.WChar := ' ';
      Delimiters : Delimiter_Vector;

      procedure Add_Image (Parser       : in out Parser_Type;
                           Text         : in out Wiki.Buffers.Buffer_Access;
                           From         : in out Positive;
                           First        : in Delimiter_Index_Type;
                           To           : in Delimiter_Index_Type;
                           Is_Reference : in Boolean) is
         Block  : Wiki.Buffers.Buffer_Access := Text;
         Pos    : Positive := From;
         Alt    : Wiki.Strings.BString (128);
         Ref    : Wiki.Strings.BString (128);
         Link   : Wiki.Strings.BString (128);
         Title  : Wiki.Strings.BString (128);
         C      : Wiki.Strings.WChar := ' ';
         I      : Delimiter_Index_Type := First;

         procedure Add_Text (Limit    : in Wiki.Buffers.Buffer_Access;
                             Last_Pos : in Positive) is
         begin
            while Block /= null loop
               declare
                  Last : constant Natural := Block.Last;
               begin
                  while Pos <= Last loop
                     if Block = Limit and then Pos >= Last_Pos then
                        return;
                     end if;
                     C := Block.Content (Pos);
                     --  exit Scan_Alt when C = ']';
                     Append (Alt, C);
                     Append (Ref, C);
                     Pos := Pos + 1;
                  end loop;
               end;
               Block := Block.Next_Block;
               Pos := 1;
            end loop;
         end Add_Text;

      begin
         while I <= To loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type := Delimiters.Reference (I);
            begin
               if Delim.Marker /= M_TEXT
                 and then (Delim.Block.Offset > Block.Offset
                           or else (Delim.Block.Offset = Block.Offset
                                    and then Delim.Pos >= Pos))
               then
                  Add_Text (Delim.Block, Delim.Pos);
                  if I /= To then
                     for J in 1 .. Delim.Count + Delim.Skip_Count loop
                        C := Block.Content (Pos);
                        Append (Ref, C);
                        Buffers.Next (Block, Pos);
                     end loop;
                  end if;
               end if;
            end;
            I := I + 1;
         end loop;

         if Block /= null and then Block.Content (Pos) = ']' then
            Buffers.Next (Block, Pos);
         end if;
         if Block /= null and then Block.Content (Pos) = '(' then
            Buffers.Next (Block, Pos);
            Scan_Link_Title (Parser, Block, Pos, ')', Link, Title);
            if Block /= null and then Block.Content (Pos) = ')' then
               Buffers.Next (Block, Pos);
            end if;
         elsif Is_Reference and then Block /= null and then Block.Content (Pos) = '[' then
            Buffers.Next (Block, Pos);
            Parse_Link_Label (Parser, Block, Pos, Link, True);
            if Block /= null and then Block.Content (Pos) = ']' then
               Buffers.Next (Block, Pos);
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
         Text := Block;
         From := Pos;
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

      procedure Process_Emphasis (Block : in out Wiki.Buffers.Buffer_Access;
                                  Pos   : in out Positive;
                                  First : in Delimiter_Index_Type;
                                  To    : in Delimiter_Index_Type) is
         I : Delimiter_Index_Type := First;
      begin
         while I <= To loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type := Delimiters.Reference (I);
            begin
               if Delim.Block.Offset > Block.Offset
                 or else (Delim.Block.Offset = Block.Offset and then Delim.Pos >= Pos)
               then
                  if Delim.Marker = M_ENTITY then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     Pos := Delim.Pos;
                     Block := Delim.Block;
                     while Block /= null and then Block.Content (Pos) /= ';' loop
                        Buffers.Next (Block, Pos);
                     end loop;
                     if Block /= null then
                        Buffers.Next (Block, Pos);
                     end if;
                  elsif Delim.Marker in M_LINK | M_LINK_REF then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     Flush_Text (Parser);
                     Pos := Delim.Pos;
                     Block := Delim.Block;
                     Buffers.Next (Block, Pos);
                     declare
                        Bracket : constant Delimiter_Vectors.Reference_Type
                          := Delimiters.Reference (Delim.Associate);
                        Link  : Wiki.Strings.BString (128);
                        Title : Wiki.Strings.BString (128);
                        Tmp_Block : Wiki.Buffers.Buffer_Access := Bracket.Block;
                        Tmp_Pos : Positive := Bracket.Pos;
                     begin
                        --  Skip ']' and then '('
                        if Delim.Marker = M_LINK then
                           Buffers.Next (Tmp_Block, Tmp_Pos);
                           C := Tmp_Block.Content (Tmp_Pos);
                           Buffers.Next (Tmp_Block, Tmp_Pos);
                           if C = '[' then
                              Parse_Link_Label (Parser, Tmp_Block, Tmp_Pos, Link, True);
                           elsif C = '(' then
                              Scan_Link_Title (Parser, Tmp_Block, Tmp_Pos, ')', Link, Title);
                           end if;
                        else
                           C := Helpers.NUL;
                        end if;
                        if not Parser.Context.Is_Hidden then
                           Wiki.Attributes.Clear (Parser.Attributes);
                           if C /= '(' then
                              if Strings.Length (Link) = 0 then
                                 Parse_Link_Label (Parser, Block, Pos, Link, True);
                                 Block := Delim.Block;
                                 Pos := Delim.Pos;
                                 Buffers.Next (Block, Pos);
                              end if;
                              Parser.Context.Filters.Add_Link (Parser.Document, To_WString (Link),
                                                               Parser.Attributes,
                                                               True, True);
                           else
                              Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR,
                                                      To_WString (Util.Encoders.URI.Encode
                                                        (Wiki.Strings.To_String (Wiki.Strings.To_WString (Link)), HREF_LOOSE)));
                              if Wiki.Strings.Length (Title) > 0 then
                                 Wiki.Attributes.Append (Parser.Attributes, TITLE_ATTR, Title);
                              end if;
                              Parser.Context.Filters.Add_Link (Parser.Document, To_WString (Title),
                                                               Parser.Attributes,
                                                               False, True);
                           end if;
                        end if;
                        Process_Emphasis (Block, Pos, I + 1, Delim.Associate);
                        Add_Text (Parser, Block, Pos, Bracket.Block, Bracket.Pos);
                        Flush_Text (Parser, Trim => Wiki.Parsers.Right);
                        if not Parser.Context.Is_Hidden then
                           Parser.Context.Filters.Pop_Node (Parser.Document, A_TAG);
                        end if;
                        I := Delim.Associate;
                        Block := Tmp_Block;
                        Pos := Tmp_Pos;
                        exit when Tmp_Block = null;
                        Buffers.Next (Block, Pos);
                     end;
                  elsif Delim.Marker in M_IMAGE | M_IMAGE_REF then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     Flush_Text (Parser);
                     Block := Delim.Block;
                     Pos := Delim.Pos;
                     Buffers.Next (Block, Pos);
                     Buffers.Next (Block, Pos);
                     Add_Image (Parser, Block, Pos, I, Delim.Associate, Delim.Marker = M_IMAGE_REF);

                  elsif Delim.Marker = M_AUTOLINK then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     Flush_Text (Parser);
                     Block := Delim.Block;
                     Pos := Delim.Pos;
                     Buffers.Next (Block, Pos);
                     Add_Autolink (Parser, Block, Pos, Delim.Count);

                  elsif Delim.Marker = M_TEXT then
                     null;

                  elsif Delim.Marker = M_LINEBREAK then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     Flush_Text (Parser);
                     Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                     Block := Delim.Block;
                     Pos := Delim.Pos;
                     for I in 1 .. Delim.Count loop
                        Buffers.Next (Block, Pos);
                     end loop;

                  elsif Delim.Count > 0 and then Delim.Can_Open
                    and then (Delim.Associate > I or else Delim.Marker = M_CODE)
                  then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     declare
                        Count : Natural;
                     begin
                        if Delim.Count > Delim.Close_Count then
                           Count := Delim.Close_Count;
                           for I in Count + 1 .. Delim.Count loop
                              Append (Parser.Text, Block.Content (Pos));
                              Buffers.Next (Block, Pos);
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
                        Block := Delim.Block;
                        Pos := Delim.Pos;
                        for I in 1 .. Delim.Count loop
                           Buffers.Next (Block, Pos);
                        end loop;
                        if Delim.Marker = M_CODE and then Delim.Space_After
                          and then Delimiters.Element (Delim.Associate).Space_Before
                        then
                           Buffers.Next (Block, Pos);
                        end if;
                     end;
                  elsif Delim.Count > 0 and then Delim.Can_Close then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
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
                        Block := Delim.Block;
                        Pos := Delim.Pos;
                        for I in 1 .. Delim.Count loop
                           Append (Parser.Text, Block.Content (Pos));
                           Buffers.Next (Block, Pos);
                        end loop;
                     end if;
                     Block := Delim.Block;
                     Pos := Delim.Pos;
                     for I in 1 .. Delim.Count + Delim.Skip_Count loop
                        Buffers.Next (Block, Pos);
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

      function Find_Previous_Bracket (Next_Char : Strings.WChar) return Delimiter_Index_Type is
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
      procedure Parse_Link (Text   : in out Wiki.Buffers.Buffer_Access;
                            From   : in out Positive;
                            Prev_C : in out WChar) is
         First  : Delimiter_Index_Type;
         Block  : Wiki.Buffers.Buffer_Access := Text;
         Pos    : Positive := From;
         C      : Wiki.Strings.WChar;
      begin
         C := Buffers.Next (Block, Pos);
         First := Find_Previous_Bracket (C);
         if First = 0 then
            From := From + 1;
            return;
         end if;

         if C = '(' then
            declare
               Link  : Wiki.Strings.BString (128);
               Title : Wiki.Strings.BString (128);
               Delim : Delimiter_Type;
               Kind  : Marker_Kind;
            begin
               Delim.Block := Text;
               Delim.Pos := From;
               Delim.Marker := M_END;
               Delim.Associate := First;
               Buffers.Next (Block, Pos);
               Scan_Link_Title (Parser, Block, Pos, ')', Link, Title);
               if Block /= null and then Block.Content (Pos) = ')' then
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
                  Buffers.Next (Block, Pos);
                  Prev_C := ')';
                  Text := Block;
                  From := Pos;
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
               Delim.Block := Text;
               Delim.Pos := From;
               Delim.Marker := M_END;
               Delim.Associate := First;
               Buffers.Next (Block, Pos);
               Parse_Link_Label (Parser, Block, Pos, Label, False);
               if Block /= null and then Block.Content (Pos) = ']' then
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
                  Buffers.Next (Block, Pos);
                  Prev_C := ']';
                  Text := Block;
                  From := Pos;
                  if Kind = M_BRACKET then
                     Clear_Brackets;
                  end if;
                  Process_Emphasis (First + 1, Delimiters.Last_Index);
               end if;
            end;

         elsif Delimiters.Element (First).Marker in M_BRACKET | M_BRACKET_IMAGE then
            if Delimiters.Element (First).Pos + 1 >= From then
               Clear_Brackets;
               return;
            end if;
            declare
               Delim : Delimiter_Type;
            begin
               Delim.Block := Text;
               Delim.Pos := From;
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
            if Block /= null and then Bracket.Marker in M_BRACKET | M_BRACKET_IMAGE then
               Text := Block;
               From := Pos;
               Bracket.Marker := (if Bracket.Marker = M_BRACKET then M_LINK_REF else M_IMAGE_REF);
               return;
            elsif Block /= null and then Bracket.Marker in M_IMAGE | M_LINK | M_IMAGE_REF then
               return;
            else
               Bracket.Marker := M_TEXT;
            end if;
         end;
         Clear_Brackets;
      end Parse_Link;

      Block       : Wiki.Buffers.Buffer_Access := Text;
      Pos         : Positive := 1;
      Delim       : Delimiter_Type;
      First_Pos   : Positive := 1;
   begin
      if Block.Last < Pos then
         return;
      end if;

      if Parser.Current_Node in N_NONE | N_PARAGRAPH or else Parser.Need_Paragraph then
         Parse_Link_Definition (Parser, Block, First_Pos);
         if Block = null then
            return;
         end if;
      end if;
      Pos := First_Pos;

      Main :
      while Block /= null loop
         while Pos <= Block.Last loop
            C := Block.Content (Pos);
            case C is
               when '\' =>
                  Scan_Linebreak (Parser, Block, Pos, Delim);
                  if Delim.Count > 0 then
                     Delimiters.Append (Delim);
                  else
                     Pos := Pos + 1;
                     Buffers.Next (Block, Pos);
                     exit Main when Block = null;
                     Prev := C;
                  end if;

               when '`' =>
                  declare
                     End_Marker : Delimiter_Type;
                  begin
                     Scan_Backtick (Parser, Block, Pos, Delim, End_Marker);
                     if Delim.Count > 0 then
                        Delimiters.Append (Delim);
                        Delimiters.Append (End_Marker);
                     end if;
                     exit Main when Block = null;
                  end;
                  Prev := C;

               when '*' =>
                  Get_Delimiter (Block, Pos, Prev, C, Delim);
                  if Delim.Can_Open or else Delim.Can_Close then
                     Delim.Marker := M_STAR;
                     Delimiters.Append (Delim);
                  end if;
                  Prev := C;
                  exit Main when Block = null;

               when '_' =>
                  Get_Delimiter (Block, Pos, Prev, C, Delim);
                  if Delim.Can_Open or else Delim.Can_Close then
                     Delim.Marker := M_UNDERSCORE;
                     Delimiters.Append (Delim);
                  end if;
                  Prev := C;
                  exit Main when Block = null;

               when '~' =>
                  Get_Delimiter (Block, Pos, Prev, C, Delim);
                  if Delim.Can_Open or else Delim.Can_Close then
                     Delim.Marker := M_TILDE;
                     Delimiters.Append (Delim);
                  end if;
                  Prev := C;
                  exit Main when Block = null;

               when '[' =>
                  Delim.Marker := M_BRACKET;
                  Delim.Block := Block;
                  Delim.Pos := Pos;
                  Delim.Count := 1;
                  Delim.Can_Open := True;
                  Delim.Can_Close := False;
                  Prev := C;
                  Buffers.Next (Block, Pos);
                  exit Main when Block = null;
                  C := Block.Content (Pos);
                  Delim.Marker := M_BRACKET;
                  Delimiters.Append (Delim);

               when ']' =>
                  Prev := C;
                  Parse_Link (Block, Pos, Prev);
                  exit Main when Block = null;

               when '!' =>
                  Delim.Block := Block;
                  Delim.Pos := Pos;
                  Buffers.Next (Block, Pos);
                  Prev := C;
                  exit Main when Block = null;
                  if Block.Content (Pos) = '[' then
                     Delim.Marker := M_BRACKET_IMAGE;
                     Delim.Count := 1;
                     Delim.Can_Close := False;
                     Delim.Can_Open := False;
                     Delimiters.Append (Delim);
                     Pos := Pos + 1;
                     Prev := '[';
                  end if;

               when '&' =>
                  declare
                     Status : Wiki.Html_Parser.Entity_State_Type;
                  begin
                     Delim.Block := Block;
                     Delim.Pos := Pos;
                     Common.Parse_Entity (Parser, Block, Pos, Status, C);

                     if Status = Wiki.Html_Parser.ENTITY_VALID then
                        Delim.Block.Content (Delim.Pos) := C;
                        Buffers.Next (Delim.Block, Delim.Pos);
                        Delim.Marker := M_ENTITY;
                        Delim.Count := 1;
                        Delim.Can_Close := False;
                        Delim.Can_Open := False;
                        Delimiters.Append (Delim);
                     else
                        Pos := Pos + 1;
                     end if;
                     exit Main when Block = null;
                  end;

               when ' ' =>
                  Scan_Linebreak (Parser, Block, Pos, Delim);
                  if Delim.Count > 0 then
                     Delimiters.Append (Delim);
                  else
                     Pos := Pos + 1;
                  end if;
                  Prev := C;

               when '<' =>
                  Scan_Autolink (Parser, Block, Pos, Delim);
                  if Delim.Count > 0 then
                     Delimiters.Append (Delim);
                     Prev := '>';
                  else
                     Pos := Pos + 1;
                  end if;
                  Prev := C;

               when others =>
                  Pos := Pos + 1;
                  Prev := C;

            end case;
         end loop;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main;

      if Parser.Current_Node = N_NONE or else Parser.Need_Paragraph then
         Parser.Context.Filters.Add_Node (Parser.Document, N_PARAGRAPH);
         Parser.Need_Paragraph := False;
      end if;
      Block := Text;
      Pos := First_Pos;
      Process_Emphasis (1, Delimiters.Last_Index);
      Process_Emphasis (Block, Pos, 1, Delimiters.Last_Index);

      Add_Text (Parser, Block, Pos, null, 1);
      Flush_Text (Parser, Trim => Wiki.Parsers.Right);
   end Parse_Inline_Text;

end Wiki.Parsers.Markdown;
