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

   type Marker_Kind is (M_CODE, M_STAR, M_UNDERSCORE, M_LINK, M_TILDE,
                        M_LINK_REF, M_IMAGE, M_END, M_BRACKET, M_BRACKET_IMAGE,
                        M_ENTITY, M_TEXT);

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
      Link_Pos  : Natural := 0;
      Link_End  : Natural := 0;
      Associate : Delimiter_Index_Type := 0;
   end record;

   package Delimiter_Vectors is
      new Ada.Containers.Vectors (Delimiter_Index, Delimiter_Type);

   subtype Delimiter_Vector is Delimiter_Vectors.Vector;

   function Get_Header_Level (Text   : in Wiki.Buffers.Buffer_Access;
                              From   : in Positive) return Natural;
   function Is_Thematic_Break (Text      : in Wiki.Buffers.Buffer_Access;
                               From      : in Positive;
                               Token     : in Wiki.Strings.WChar) return Boolean;
   procedure Get_List_Level (Parser : in out Parser_Type;
                             Text   : in out Wiki.Buffers.Buffer_Access;
                             From   : in out Positive;
                             Level  : out Integer;
                             Indent : out Natural);
   function Is_End_Preformat (Text   : in Wiki.Buffers.Buffer_Access;
                              From   : in Positive;
                              Expect : in WChar;
                              Length : in Positive) return Boolean;
   procedure Scan_Link_Title (Text   : in out Wiki.Buffers.Buffer_Access;
                              From   : in out Positive;
                              Expect : in Wiki.Strings.WChar;
                              Link   : in out Wiki.Strings.BString;
                              Title  : in out Wiki.Strings.BString);
   procedure Add_Header (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access;
                         From   : in Positive;
                         Level  : in Positive);
   procedure Add_Text (Parser   : in out Parser_Type;
                       Text     : in out Wiki.Buffers.Buffer_Access;
                       From     : in out Positive;
                       Limit    : in Wiki.Buffers.Buffer_Access;
                       Last_Pos : in Positive);
   procedure Add_Link (Parser : in out Parser_Type;
                       Text   : in out Wiki.Buffers.Buffer_Access;
                       From   : in out Positive);
   procedure Add_Link_Ref (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Buffer_Access;
                           From   : in out Positive);
   procedure Add_Image (Parser   : in out Parser_Type;
                        Text     : in out Wiki.Buffers.Buffer_Access;
                        From     : in out Positive);
   procedure Parse_Table (Parser : in out Parser_Type;
                          Text   : in out Wiki.Buffers.Buffer_Access;
                          From   : in out Positive);
   procedure Get_Delimiter (Text        : in out Wiki.Buffers.Buffer_Access;
                            From        : in out Positive;
                            Before_Char : Strings.WChar;
                            C           : in Strings.WChar;
                            Delim       : in out Delimiter_Type);

   procedure Parse_Link_Label (Parser : in out Parser_Type;
                               Text   : in out Wiki.Buffers.Buffer_Access;
                               From   : in out Positive;
                               Label  : in out Wiki.Strings.BString);

   procedure Parse_Link_Definition (Parser : in out Parser_Type;
                                    Text   : in out Wiki.Buffers.Buffer_Access;
                                    From   : in out Positive);

   procedure Scan_Backtick (Parser   : in out Parser_Type;
                            Text     : in out Wiki.Buffers.Buffer_Access;
                            From     : in out Positive;
                            Start    : in out Delimiter_Type;
                            Stop     : in out Delimiter_Type);

   function Is_Escapable (C : in Wiki.Strings.WChar) return Boolean is
     (C in '!' .. '/' | ':' .. '@' | '{' .. '~' | '[' .. '`');

   function Get_Header_Level (Text   : in Wiki.Buffers.Buffer_Access;
                              From   : in Positive) return Natural is
      Count : constant Natural := Buffers.Count_Occurence (Text, From, '#');
   begin
      if Count > 6 then
         return 0;
      end if;
      if From + Count > Text.Last then
         return 0;
      end if;
      if not Is_Space (Text.Content (From + Count)) then
         return 0;
      end if;
      return Count;
   end Get_Header_Level;

   function Is_Thematic_Break (Text      : in Wiki.Buffers.Buffer_Access;
                               From      : in Positive;
                               Token     : in Wiki.Strings.WChar) return Boolean is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      Count : Natural := 0;
   begin
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
            C    : Wiki.Strings.WChar;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               if C = Token then
                  Count := Count + 1;
               elsif not Is_Space_Or_Newline (C) then
                  return False;
               end if;
               Pos := Pos + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop;
      return Count >= 3;
   end Is_Thematic_Break;

   procedure Get_List_Level (Parser : in out Parser_Type;
                             Text   : in out Wiki.Buffers.Buffer_Access;
                             From   : in out Positive;
                             Level  : out Integer;
                             Indent : out Natural) is
      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      C     : Strings.WChar;
      Limit : Natural := 9;
   begin
      Level := 0;
      Indent := 0;
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
                  Indent := Indent + Count;
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
                         Text   : in Wiki.Buffers.Buffer_Access;
                         From   : in Positive;
                         Level  : in Positive) is
      procedure Add_Header (Content : in Wiki.Strings.WString);

      procedure Add_Header (Content : in Wiki.Strings.WString) is
         Last : Natural := Content'Last;
      begin
         --  Remove trailing spaces.
         while Last > Content'First and then Is_Space (Content (Last)) loop
            Last := Last - 1;
         end loop;

         --  If there are ending '#', remove all of them and remove trailing spaces again.
         if Last > Content'First and then Content (Last) = '#' then
            while Last > Content'First and then Content (Last) = '#' loop
               Last := Last - 1;
            end loop;
            while Last > Content'First and then Is_Space (Content (Last)) loop
               Last := Last - 1;
            end loop;
         end if;
         Parser.Context.Filters.Start_Block (Parser.Document, Nodes.N_HEADER, Level);
         Strings.Clear (Parser.Text);
         Buffers.Append (Parser.Text_Buffer, Content (Content'First .. Last));

         Parse_Inline_Text (Parser, Parser.Text_Buffer.First'Unchecked_Access);
         Buffers.Clear (Parser.Text_Buffer);
         Parser.Document.Pop_Node (Wiki.H1_TAG);
      end Add_Header;

      procedure Add_Header is
        new Wiki.Strings.Wide_Wide_Builders.Get (Add_Header);

      Block   : Wiki.Buffers.Buffer_Access := Text;
      Pos     : Positive := From;
      C       : Wiki.Strings.WChar;
      Space_Count : Natural;
   begin
      Flush_Text (Parser);
      Flush_List (Parser);
      if not Parser.Context.Is_Hidden then
         Buffers.Skip_Spaces (Block, Pos, Space_Count);
         while Block /= null loop
            declare
               Last : constant Natural := Block.Last;
            begin
               while Pos <= Last loop
                  C := Block.Content (Pos);
                  exit when Is_Newline (C);
                  Append (Parser.Text, C);
                  Pos := Pos + 1;
               end loop;
            end;
            Block := Block.Next_Block;
         end loop;
         Add_Header (Parser.Text);
         Wiki.Strings.Clear (Parser.Text);
         Parser.Format := (others => False);
      end if;
      Parser.Empty_Line   := True;
      Parser.In_Paragraph := False;
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

   --  Parse a markdown table/column.
   --  Example:
   --    | col1 | col2 | ... | colN |
   procedure Parse_Table (Parser : in out Parser_Type;
                          Text   : in out Wiki.Buffers.Buffer_Access;
                          From   : in out Positive) is
      Block       : Wiki.Buffers.Buffer_Access := Text;
      Pos         : Positive := From + 1;
      C           : Wiki.Strings.WChar;
      Skip_Spaces : Boolean := True;
   begin
      if Parser.Current_Node /= Nodes.N_TABLE then
         Flush_List (Parser);
         Push_Block (Parser, Nodes.N_TABLE);
      end if;
      Parser.Context.Filters.Add_Row (Parser.Document);
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

   --  Parse a markdown link definition.
   --  Example:
   --    [label]: url
   --    [label]: url "title"
   procedure Parse_Link_Label (Parser : in out Parser_Type;
                               Text   : in out Wiki.Buffers.Buffer_Access;
                               From   : in out Positive;
                               Label  : in out Wiki.Strings.BString) is
      pragma Unreferenced (Parser);

      Block : Wiki.Buffers.Buffer_Access := Text;
      Pos   : Positive := From;
      C     : Wiki.Strings.WChar;
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
                  Append (Label, C);
               else
                  Append (Label, '\');
                  Append (Label, C);
               end if;
            elsif C = ']' then
               Text := Block;
               From := Pos;
               return;
            else
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
      Pos         : Positive := From + 1;
      Label       : Wiki.Strings.BString (128);
      Link        : Wiki.Strings.BString (128);
      Title       : Wiki.Strings.BString (128);
      Space_Count : Natural;
   begin
      Parse_Link_Label (Parser, Block, Pos, Label);
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
      Scan_Link_Title (Block, Pos, ' ', Link, Title);
      if Block = null then
         if Wiki.Strings.Length (Link) = 0 then
            return;
         end if;
         Parser.Document.Set_Link (Strings.To_WString (Label),
                                   Strings.To_WString (Link),
                                   Strings.To_WString (Title));
         Text := null;
         From := 1;
         return;
      end if;
   end Parse_Link_Definition;

   --  Current paragraph
   --  N_BLOCKQUOTE
   --  N_LIST
   --  N_LIST_ITEM
   --  N_PARAGRAPH    *Never nested*
   --  N_PREFORMAT, Fence, Indent level *Never nested*
   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := 1;
      C      : WChar;
      Count  : Natural := 0;
      Level  : Integer;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Common.Parse_Html_Element (Parser, Block, Pos, Start => False);
         if Block = null then
            return;
         end if;
      end if;
      if Parser.In_Html and then not Wiki.Helpers.Is_Newline (Block.Content (1)) then
         Common.Parse_Html_Preformatted (Parser, Block, Pos);
         return;
      end if;

      Spaces :
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               if C = ' ' then
                  Count := Count + 1;
               elsif C = Wiki.Helpers.HT then
                  Count := Count + 4;
               else
                  exit Spaces;
               end if;
               Pos := Pos + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Spaces;

      --  Handle blockquote first because it can contain its specific
      --  formatting (lists, headers, pre-formatted, ...).
      if Parser.In_Blockquote and then Count <= 3 then
         Level := Buffers.Count_Occurence (Block, Pos, '>');
         if Level = 0 then
            loop
               Pop_Block (Parser);
               exit when Parser.Current_Node = Nodes.N_NONE;
            end loop;
         else
            Buffers.Next (Block, Pos, Level);
            Buffers.Skip_Spaces (Block, Pos, Count);
            if Block = null then
               return;
            end if;
            Push_Block (Parser, Nodes.N_BLOCKQUOTE, Level);
            C := Block.Content (Pos);
         end if;
      end if;

      --  Continue a pre-formatted block.
      if Parser.Current_Node = N_PREFORMAT then
         if Parser.Preformat_Fence = ' '
           and then Count = 0
           and then C in Wiki.Helpers.LF | Wiki.Helpers.CR
           and then not Parser.Previous_Line_Empty
         then
            Parser.Previous_Line_Empty := True;
            return;
         end if;
         if Parser.Preformat_Fence = ' ' and then Count < Parser.Preformat_Indent - 3 then
            Pop_Block (Parser);
         else
            if C = Parser.Preformat_Fence then
               if Is_End_Preformat (Block, Pos, C, Parser.Preformat_Fcount) then
                  Parser.Previous_Line_Empty := False;
                  Pop_Block (Parser);
                  return;
               end if;
            end if;
            if Count > Parser.Preformat_Indent then
               Block := Text;
               Pos := 1;
               Buffers.Next (Block, Pos, Parser.Preformat_Indent);
            end if;
            if Parser.Previous_Line_Empty then
               Strings.Append_Char (Parser.Text, Wiki.Helpers.LF);
            end if;
            Common.Append (Parser.Text, Block, Pos);
            Parser.Previous_Line_Empty := False;
            return;
         end if;
      end if;

      if Parser.Current_Node = Nodes.N_LIST_ITEM then
         declare
            Level : constant Natural := Get_Current_Level (Parser);
         begin
            if Count >= Level + 3 then
               Parser.Preformat_Indent := Count;
               Parser.Preformat_Fence := ' ';
               Parser.Preformat_Fcount := 0;
               Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_PARAGRAPH);
               Push_Block (Parser, N_PREFORMAT);
               Common.Append (Parser.Text, Block, Pos);
               return;
            end if;
            if Count = Level and then Parser.Previous_Line_Empty then
               --  Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_PARAGRAPH);
               Flush_Block (Parser);
               Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_PARAGRAPH);

            elsif Count < Level and then Parser.Previous_Line_Empty
              and then C not in Wiki.Helpers.LF | Wiki.Helpers.CR
            then
               Pop_Block (Parser);
            end if;
         end;
      end if;

      if C in Wiki.Helpers.LF | Wiki.Helpers.CR then
         Parser.In_Html := False;
         if Parser.Current_Node = Nodes.N_PARAGRAPH then
            Parser.Previous_Line_Empty := True;
            Pop_Block (Parser);
            return;
         end if;
         if Count = 0 and then Parser.Current_Node = N_LIST_ITEM then
            Parser.Previous_Line_Empty := True;
            return;
         end if;
         while not (Parser.Current_Node in N_PARAGRAPH | N_NONE | N_LIST_ITEM | N_BLOCKQUOTE) loop
            Pop_Block (Parser);
         end loop;
         if not (Parser.Current_Node in N_PARAGRAPH | N_LIST_ITEM) then
            Push_Block (Parser, N_PARAGRAPH);
         end if;
         Parser.Previous_Line_Empty := False;
         return;
      end if;

      if Parser.Previous_Line_Empty and then Parser.Current_Node = Nodes.N_LIST_ITEM then
         Flush_Block (Parser);
         Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_PARAGRAPH);
         Parser.Is_Empty_Paragraph := True;

      elsif Parser.Previous_Line_Empty and then Parser.Current_Node = Nodes.N_PARAGRAPH then
         Flush_Block (Parser);
         Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_PARAGRAPH);
         Parser.Is_Empty_Paragraph := True;
      end if;

      --
      if C = '>' and then Count <= 3 then
         Count := Buffers.Count_Occurence (Block, Pos, '>');
         Push_Block (Parser, Nodes.N_BLOCKQUOTE, Count);
         Buffers.Next (Block, Pos, Count);
         Buffers.Skip_Spaces (Block, Pos, Count);
         if Block = null then
            return;
         end if;
         if Count > 0 then
            Count := Count - 1;
         end if;
         C := Block.Content (Pos);
      end if;

      if Count > 3 and then Parser.Previous_Line_Empty then
         Parser.Previous_Line_Empty := False;
         if Parser.Current_Node in Nodes.N_LIST_START | Nodes.N_NUM_LIST_START then
            Pop_Block (Parser);
         end if;
         Parser.Preformat_Indent := Count;
         Parser.Preformat_Fence := ' ';
         Parser.Preformat_Fcount := 0;
         Push_Block (Parser, N_PREFORMAT);
         Common.Append (Parser.Text, Block, Pos);
         return;
      end if;
      Parser.Previous_Line_Empty := False;

      case C is
         when '#' =>
            if Count < 4 then
               Level := Get_Header_Level (Block, Pos);
               if Level > 0 then
                  if Parser.Current_Node /= N_BLOCKQUOTE then
                     Pop_Block (Parser);
                  end if;
                  Pos := Pos + Level + 1;
                  Add_Header (Parser, Block, Pos, Level);
                  Parser.Previous_Line_Empty := False;
                  return;
               end if;
            end if;

         when '_' =>
            if Count <= 3 and then Is_Thematic_Break (Block, Pos, C) then
               Add_Horizontal_Rule (Parser);
               return;
            end if;

         when '*' | '-' | '+' =>
            if Count <= 3 and then C in '*' | '-' and then Is_Thematic_Break (Block, Pos, C) then
               Add_Horizontal_Rule (Parser);
               return;
            end if;

            if (Pos + 1 <= Block.Last and then Helpers.Is_Space (Block.Content (Pos + 1)))
              or else (Parser.Current_Node = Nodes.N_LIST_ITEM)
            then
               declare
                  Cur_Level : constant Natural := Get_Current_Level (Parser);
               begin
                  Level := Count + 1;
                  Buffers.Next (Block, Pos);
                  Buffers.Skip_Spaces (Block, Pos, Count);
                  Level := Level + Count;
                  if Level <= Cur_Level or else Level = Cur_Level + 1 then
                     Pop_List (Parser, Level, C, 0);
                  end if;
                  if not Is_List_Item (Parser, Level) then
                     if not (Parser.Current_Node in Nodes.N_BLOCKQUOTE | Nodes.N_LIST_ITEM) then
                        Pop_Block (Parser);
                     end if;
                     Push_Block (Parser, Nodes.N_LIST_START, Level, C);
                  end if;
                  Push_Block (Parser, Nodes.N_LIST_ITEM, Level, C);
                  Buffers.Append (Parser.Text_Buffer, Block, Pos);
                  Parser.Previous_Line_Empty := False;
                  return;
               end;
            end if;

         when '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            Parser.Previous_Line_Empty := False;
            declare
               Indent : Natural;
            begin
               Get_List_Level (Parser, Block, Pos, Level, Indent);
               if Level >= 0 then
                  Indent := Indent + Count;
                  Pop_List (Parser, Indent, '0', Level);
                  if not Is_List_Item (Parser, Indent) then
                     Push_Block (Parser, Nodes.N_NUM_LIST_START,
                                 Level => Indent, Marker => '0', Number => Level);
                  end if;
                  Push_Block (Parser, Nodes.N_LIST_ITEM,
                              Level => Indent, Marker => '0', Number => Level);
                  Parser.List_Level := Level;
                  Buffers.Append (Parser.Text_Buffer, Block, Pos);
                  return;
               end if;
            end;

         when '~' | '`' =>
            if Count > 0 and then Parser.Current_Node in Nodes.N_LIST_ITEM then
               Common.Parse_Preformatted (Parser, Block, Pos, C, True);
            else
               Common.Parse_Preformatted (Parser, Block, Pos, C, False);
            end if;
            if Parser.Current_Node = Nodes.N_PREFORMAT then
               Parser.Preformat_Indent := Count;
               --  Parser.Preformat_Fence := C;
               --  Parser.Preformat_Fcount := Level;
               --  Push_Block (Parser, N_PREFORMAT);
               return;
            end if;

         when '>' =>
            if Count < 4 then
               --  Enter block quote
               return;
            end if;
            --  Parse_Blockquote (Parser, C);

         when '|' =>
            if Count = 0 then
               Parse_Table (Parser, Block, Pos);
               if Block = null then
                  return;
               end if;
            end if;

         when '<' =>
            if Count <= 3 then
               declare
                  Html_Block : Wiki.Buffers.Buffer_Access := Block;
                  Html_Pos   : Positive := Pos;
               begin
                  Common.Parse_Html_Element (Parser, Html_Block, Html_Pos, True);
                  if Html_Block /= null and then Parser.In_Html then
                     Common.Parse_Html_Preformatted (Parser, Html_Block, Html_Pos);
                     return;
                  end if;
               end;
            end if;

         when '[' =>
            Parse_Link_Definition (Parser, Block, Pos);
            if Block = null then
               return;
            end if;
            if Parser.Previous_Line_Empty and then Parser.Current_Node /= N_PARAGRAPH then
               Pop_List (Parser);
               Push_Block (Parser, N_PARAGRAPH);
            end if;

         when others =>
            if Parser.Previous_Line_Empty and then Parser.Current_Node /= N_PARAGRAPH then
               Pop_List (Parser);
               Push_Block (Parser, N_PARAGRAPH);
            end if;

      end case;

      while not (Parser.Current_Node in N_PARAGRAPH | N_NONE | N_LIST_ITEM | N_BLOCKQUOTE) loop
         Pop_Block (Parser);
      end loop;
      if not (Parser.Current_Node in N_PARAGRAPH | N_LIST_ITEM) then
         Push_Block (Parser, N_PARAGRAPH);
      end if;
      Parser.Previous_Line_Empty := False;
      Buffers.Append (Parser.Text_Buffer, Block, Pos);
   end Parse_Line;

   procedure Scan_Link_Title (Text   : in out Wiki.Buffers.Buffer_Access;
                              From   : in out Positive;
                              Expect : in Wiki.Strings.WChar;
                              Link   : in out Wiki.Strings.BString;
                              Title  : in out Wiki.Strings.BString) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      C      : Wiki.Strings.WChar;
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
         end if;
      else
         Buffers.Skip_Spaces (Block, Pos, Space_Count);
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
         Before_Space  : constant Boolean := Helpers.Is_Space (Before_Char);
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
         Delim.Count := Numdelims;
         Delim.Pos := From;
         Delim.Block := Text;
         Text := Block;
         From := Pos;
      end;
   end Get_Delimiter;

   procedure Add_Image (Parser   : in out Parser_Type;
                        Text     : in out Wiki.Buffers.Buffer_Access;
                        From     : in out Positive) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      Alt    : Wiki.Strings.BString (128);
      Link   : Wiki.Strings.BString (128);
      Title  : Wiki.Strings.BString (128);
      C      : Wiki.Strings.WChar := ' ';
   begin
      Scan_Alt :
      while Block /= null loop
         declare
            Last : constant Natural := Block.Last;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               exit Scan_Alt when C = ']';
               Append (Alt, C);
               Pos := Pos + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Scan_Alt;
      if Block /= null and then C = ']' then
         Buffers.Next (Block, Pos);
      end if;
      if Block /= null and then Block.Content (Pos) = '(' then
         Buffers.Next (Block, Pos);
         Scan_Link_Title (Block, Pos, ')', Link, Title);
         if Block /= null and then Block.Content (Pos) = ')' then
            Buffers.Next (Block, Pos);
         end if;
      end if;
      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         Wiki.Attributes.Append (Parser.Attributes, "src", Link);
         Wiki.Attributes.Append (Parser.Attributes, "title", Title);
         Parser.Context.Filters.Add_Image (Parser.Document,
                                           Strings.To_WString (Alt),
                                           Parser.Attributes);
      end if;
      Text := Block;
      From := Pos;
   end Add_Image;

   procedure Add_Link (Parser : in out Parser_Type;
                       Text   : in out Wiki.Buffers.Buffer_Access;
                       From   : in out Positive) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      Alt    : Wiki.Strings.BString (128);
      Title  : Wiki.Strings.BString (128);
      Link   : Wiki.Strings.BString (128);
      C      : Wiki.Strings.WChar;
   begin
      Scan_Alt :
      while Block /= null loop
         declare
            Last : Natural := Block.Last;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               exit Scan_Alt when C = ']';
               if C = '\' then
                  Pos := Pos + 1;
                  if Pos > Last then
                     Block := Block.Next_Block;
                     exit Scan_Alt when Block = null;
                     Last := Block.Last;
                     Pos := 1;
                  end if;
                  C := Block.Content (Pos);
                  if not Is_Escapable (C) then
                     Append (Alt, '\');
                  end if;
               elsif Is_Newline (C) then
                  C := ' ';
               end if;
               Append (Alt, C);
               Pos := Pos + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Scan_Alt;
      if Block /= null and then C = ']' then
         Buffers.Next (Block, Pos);
      end if;
      if Block /= null and then Block.Content (Pos) = '(' then
         Buffers.Next (Block, Pos);
         Scan_Link_Title (Block, Pos, ')', Link, Title);
         if Block /= null and then Block.Content (Pos) = ')' then
            Buffers.Next (Block, Pos);
         end if;
      end if;
      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR, Link);
         Wiki.Attributes.Append (Parser.Attributes, "title", Title);
         Parser.Context.Filters.Add_Link (Parser.Document,
                                          Strings.To_WString (Alt),
                                          Parser.Attributes);
      end if;
      Text := Block;
      From := Pos;
   end Add_Link;

   procedure Add_Link_Ref (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Buffer_Access;
                           From   : in out Positive) is
      Block  : Wiki.Buffers.Buffer_Access := Text;
      Pos    : Positive := From;
      Label  : Wiki.Strings.BString (128);
      C      : Wiki.Strings.WChar;
   begin
      Scan_Label :
      while Block /= null loop
         declare
            Last : Natural := Block.Last;
         begin
            while Pos <= Last loop
               C := Block.Content (Pos);
               exit Scan_Label when C = ']';
               if C = '\' then
                  Pos := Pos + 1;
                  if Pos > Last then
                     Block := Block.Next_Block;
                     exit Scan_Label when Block = null;
                     Last := Block.Last;
                     Pos := 1;
                  end if;
                  C := Block.Content (Pos);
                  if not Is_Escapable (C) then
                     Append (Label, '\');
                  end if;
               end if;
               Append (Label, C);
               Pos := Pos + 1;
            end loop;
         end;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Scan_Label;
      if Block /= null and then C = ']' then
         Buffers.Next (Block, Pos);
      end if;
      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Parser.Context.Filters.Add_Link_Ref (Parser.Document,
                                              Strings.To_WString (Label));
      end if;
      Text := Block;
      From := Pos;
   end Add_Link_Ref;

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
                  if Parser.Format (STRONG) or else Parser.Format (EMPHASIS) then
                     Flush_Text (Parser);
                     Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                  else
                     Append (Parser.Text, ' ');
                  end if;
               elsif C = '\' then
                  Buffers.Next (Block, Pos);
                  if Block = Limit and then Pos = Last_Pos then
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
   begin
      Start.Pos := Pos;
      Start.Block := Block;
      Buffers.Count_Occurence (Block, Pos, '`', Count);
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
                  Stop.Count := Count;
                  Stop.Marker := M_CODE;
                  Text := Block;
                  From := Pos;
                  return;
               end if;
            elsif C = '\' then
               Buffers.Next (Block, Pos);
            end if;
            Pos := Pos + 1;
         end loop;
         Block := Block.Next_Block;
         Pos := 1;
      end loop;

      --  End of buffer reached: we have not found the end marker.
      Start.Count := 0;
      From := From + 1;
   end Scan_Backtick;

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

      function Has_Closing (Starting : in Delimiter_Index_Type;
                            Last     : in Delimiter_Index_Type;
                            Opening  : in Delimiter_Type) return Boolean is
      begin
         for I in Starting + 1 .. Last loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type := Delimiters.Reference (I);
            begin
               if Delim.Can_Close and then Opening.Marker = Delim.Marker
                 and then Opening.Count = Delim.Count
               then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Has_Closing;

      function Find_Closing (Starting : in Delimiter_Index_Type;
                             Last     : in Delimiter_Index_Type;
                             Opening  : in Delimiter_Type) return Delimiter_Index_Type is
      begin
         for I in Starting + 1 .. Last loop
            declare
               Delim : constant Delimiter_Vectors.Reference_Type := Delimiters.Reference (I);
            begin
               if Delim.Can_Close and then Opening.Marker = Delim.Marker
                 and then Opening.Count = Delim.Count
               then
                  return I;
               end if;
            end;
         end loop;
         return 0;
      end Find_Closing;

      procedure Process_Emphasis (Block : in out Wiki.Buffers.Buffer_Access;
                                  Pos   : in out Positive;
                                  First : in Delimiter_Index_Type;
                                  To    : in Delimiter_Index_Type) is
      begin
         for I in First .. To loop
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
                  elsif Delim.Marker in M_LINK then
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
                        Buffers.Next (Tmp_Block, Tmp_Pos);
                        Buffers.Next (Tmp_Block, Tmp_Pos);
                        Scan_Link_Title (Tmp_Block, Tmp_Pos, ')', Link, Title);
                        if Tmp_Block /= null and then Tmp_Block.Content (Tmp_Pos) = ')' then
                           Wiki.Attributes.Clear (Parser.Attributes);
                           Wiki.Attributes.Append (Parser.Attributes, HREF_ATTR,
                                                   To_WString (Util.Encoders.URI.Encode
                                                     (Wiki.Strings.To_String (Wiki.Strings.To_WString (Link)), HREF_LOOSE)));
                           if Wiki.Strings.Length (Title) > 0 then
                              Wiki.Attributes.Append (Parser.Attributes, TITLE_ATTR, Title);
                           end if;
                           Parser.Context.Filters.Push_Node (Parser.Document, A_TAG, Parser.Attributes);
                           Process_Emphasis (Block, Pos, I + 1, Delim.Associate);
                           Add_Text (Parser, Block, Pos, Bracket.Block, Bracket.Pos);
                           Flush_Text (Parser, Trim => Wiki.Parsers.Right);
                           Parser.Context.Filters.Pop_Node (Parser.Document, A_TAG);
                           Block := Tmp_Block;
                           Pos := Tmp_Pos;
                           Buffers.Next (Block, Pos);
                        end if;
                     end;
                  elsif Delim.Marker in M_IMAGE then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     Flush_Text (Parser);
                     Block := Delim.Block;
                     Pos := Delim.Pos;
                     Buffers.Next (Block, Pos);
                     Buffers.Next (Block, Pos);
                     Add_Image (Parser, Block, Pos);

                  elsif Delim.Marker = M_TEXT then
                     null;

                  elsif Delim.Count > 0 and then Delim.Can_Open
                    and then (Has_Closing (I, To, Delim) or else Delim.Marker = M_CODE)
                  then
                     Add_Text (Parser, Block, Pos, Delim.Block, Delim.Pos);
                     Flush_Text (Parser);
                     if Delim.Marker in M_STAR | M_UNDERSCORE and then Delim.Count = 2 then
                        Parser.Format (STRONG) := True;

                     elsif Delim.Marker in M_TILDE and then Delim.Count = 2 then
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
                     for I in 1 .. Delim.Count loop
                        Buffers.Next (Block, Pos);
                     end loop;
                  end if;
               end if;
            end;
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
                 and then Previous_Delim.Link_Pos = 0
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
                 and then Delim.Marker /= M_LINK
                 and then Delim.Marker /= M_LINK_REF
                 and then Delim.Marker /= M_END
                 and then Delim.Marker /= M_ENTITY
                 and then Delim.Can_Open
               then
                  Delim.Associate := Find_Closing (I, To, Delim);
                  if Delim.Associate = 0 then
                     Delim.Marker := M_TEXT;
                  else
                     Delimiters.Reference (Delim.Associate).Associate := I;
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
                 and then Delim.Marker /= M_LINK
                 and then Delim.Marker /= M_LINK_REF
                 and then Delim.Marker /= M_END
                 and then Delim.Marker /= M_TEXT
                 and then Delim.Marker /= M_ENTITY
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
      --  Link (M_IMAGE):
      --    ![label](url)
      --    ![label][image-ref]
      procedure Parse_Link (Text   : in Wiki.Buffers.Buffer_Access;
                            From   : in Positive) is
         First  : constant Delimiter_Index_Type := Find_Previous_Bracket;
         Block  : Wiki.Buffers.Buffer_Access := Text;
         Pos    : Positive := From;
         C      : Wiki.Strings.WChar;
      begin
         if First = 0 then
            return;
         end if;

         C := Buffers.Next (Block, Pos);
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
               Scan_Link_Title (Block, Pos, ')', Link, Title);
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
                  if Kind = M_BRACKET then
                     Clear_Brackets;
                  end if;
                  Process_Emphasis (First + 1, Delimiters.Last_Index);
                  return;
               end if;
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
               Parse_Link_Label (Parser, Block, Pos, Label);
               if Block /= null and then Block.Content (Pos) = ']' then
                  Delimiters.Append (Delim);
                  declare
                     Bracket : constant Delimiter_Vectors.Reference_Type
                       := Delimiters.Reference (First);
                  begin
                     Kind := Bracket.Marker;
                     Bracket.Marker :=
                       (if Bracket.Marker = M_BRACKET_IMAGE then M_IMAGE else M_LINK_REF);
                     Bracket.Associate := Delimiters.Last_Index;
                  end;
                  Buffers.Next (Block, Pos);
                  if Kind = M_BRACKET then
                     Clear_Brackets;
                  end if;
                  Process_Emphasis (First + 1, Delimiters.Last_Index);
               end if;
            end;
         end if;
         declare
            Bracket : constant Delimiter_Vectors.Reference_Type
              := Delimiters.Reference (First);
         begin
            if Block /= null and then Bracket.Marker = M_BRACKET then
               Bracket.Marker := M_LINK_REF;
               return;
            else
               Bracket.Marker := M_TEXT;
            end if;
         end;
         Clear_Brackets;
      end Parse_Link;

      Block      : Wiki.Buffers.Buffer_Access := Text;
      Pos        : Positive := 1;
      Delim : Delimiter_Type;
   begin
      Main :
      while Block /= null loop
         while Pos <= Block.Last loop
            C := Block.Content (Pos);
            case C is
               when '\' =>
                  Pos := Pos + 1;
                  Buffers.Next (Block, Pos);
                  exit Main when Block = null;
                  Prev := C;

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
                  Get_Delimiter (Block, Pos, Prev, C, Delim);
                  if Delim.Can_Open or else Delim.Can_Close then
                     Delim.Marker := M_BRACKET;
                     Delimiters.Append (Delim);
                  end if;
                  Prev := C;
                  exit Main when Block = null;

               when ']' =>
                  Parse_Link (Block, Pos);
                  Pos := Pos + 1;
                  Prev := C;

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

               when others =>
                  Pos := Pos + 1;
                  Prev := C;

            end case;
         end loop;
         Block := Block.Next_Block;
         Pos := 1;
      end loop Main;

      Block := Text;
      Pos := 1;
      Process_Emphasis (1, Delimiters.Last_Index);
      Process_Emphasis (Block, Pos, 1, Delimiters.Last_Index);

      Add_Text (Parser, Block, Pos, null, 1);
      Flush_Text (Parser, Trim => Wiki.Parsers.Right);
   end Parse_Inline_Text;

end Wiki.Parsers.Markdown;
