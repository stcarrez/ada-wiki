-----------------------------------------------------------------------
--  wiki-parsers-dotclear -- Dotclear parser operations
--  Copyright (C) 2011 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Common;
package body Wiki.Parsers.Dotclear is

   use Wiki.Helpers;
   use Wiki.Nodes;
   use Wiki.Strings;
   use Wiki.Buffers;

   --  Parse an image.
   --  Example:
   --    ((url|alt text))
   --    ((url|alt text|position))
   --    ((url|alt text|position||description))
   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Cursor);

   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Cursor);

   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Cursor) is
      Count : constant Natural := Count_Occurence (Text.Block, Text.Pos, '/');
   begin
      if Count /= 3 then
         return;
      end if;

      --  Extract the format either 'Ada' or '[Ada]'
      declare
         Pos  : Wiki.Buffers.Cursor := Text;
         Space_Count : Natural;
      begin
         Buffers.Next (Pos, Count);
         Wiki.Strings.Clear (Parser.Preformat_Format);
         Buffers.Skip_Spaces (Pos, Space_Count);
         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '[' then
            Buffers.Next (Pos);
            Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, ']', ']',
                                Parser.Preformat_Format);
            Buffers.Next (Pos);
         else
            Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, CR, LF,
                                Parser.Preformat_Format);
         end if;
         if Buffers.Is_Valid (Pos) then
            Buffers.Skip_Spaces (Pos, Space_Count);
         end if;
         Text := Pos;
      end;

      Parser.Preformat_Indent := 0;
      Parser.Preformat_Fence := ' ';
      Parser.Preformat_Fcount := 0;
      Flush_Text (Parser, Trim => Right);
      Pop_Block (Parser);
      Push_Block (Parser, N_PREFORMAT);
   end Parse_Preformatted;

   --  ------------------------------
   --  Parse an image.
   --  Example:
   --    ((url|alt text))
   --    ((url|alt text|position))
   --    ((url|alt text|position||description))
   --  ------------------------------
   procedure Parse_Image (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Cursor) is
      procedure Append_Position (Position : in Wiki.Strings.WString);

      procedure Append_Position (Position : in Wiki.Strings.WString) is
      begin
         if Position in "L" | "G" then
            Wiki.Attributes.Append (Parser.Attributes, String '("align"), "left");
         elsif Position in "R" | "D" then
            Wiki.Attributes.Append (Parser.Attributes, String '("align"), "right");
         elsif Position = "C" then
            Wiki.Attributes.Append (Parser.Attributes, String '("align"), "center");
         end if;
      end Append_Position;

      procedure Append_Position is
         new Wiki.Strings.Wide_Wide_Builders.Get (Append_Position);

      Link     : Wiki.Strings.BString (128);
      Alt      : Wiki.Strings.BString (128);
      Position : Wiki.Strings.BString (128);
      Desc     : Wiki.Strings.BString (128);
      Pos      : Wiki.Buffers.Cursor := Text;
   begin
      Next (Pos);
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= '(' then
         Common.Parse_Text (Parser, Text);
         return;
      end if;

      Next (Pos);
      if not Buffers.Is_Valid (Pos) then
         Common.Parse_Text (Parser, Pos, Count => 2);
         return;
      end if;

      Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '|', ')', Link);
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '|' then
         Next (Pos);
         if Buffers.Is_Valid (Pos) then
            Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '|', ')', Alt);
         end if;
         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '|' then
            Next (Pos);
            if Buffers.Is_Valid (Pos) then
               Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '|', ')', Position);
            end if;
            if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = '|' then
               Next (Pos);
               if Buffers.Is_Valid (Pos) then
                  Common.Parse_Token (Pos.Block, Pos.Pos, Parser.Escape_Char, '|', ')', Desc);
               end if;
            end if;
         end if;
      end if;

      --  Check for the first ')'.
      if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ')' then
         Next (Pos);
      end if;

      --  Check for the second ')', abort the image and emit the '((' if the '))' is missing.
      if not Buffers.Is_Valid (Pos) or else Buffers.Char_At (Pos) /= ')' then
         Common.Parse_Text (Parser, Text, Count => 2);
         return;
      end if;
      Next (Pos);
      Text := Pos;

      Flush_Text (Parser);
      if not Parser.Context.Is_Hidden then
         Wiki.Attributes.Clear (Parser.Attributes);
         Wiki.Attributes.Append (Parser.Attributes, "src", Link);
         Append_Position (Position);
         Wiki.Attributes.Append (Parser.Attributes, "longdesc", Desc);
         Parser.Context.Filters.Add_Image (Parser.Document,
                                           Strings.To_WString (Alt),
                                           Parser.Attributes, False);
      end if;
   end Parse_Image;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Count  : Natural;
   begin
      if Parser.In_Blockquote then
         Count := Count_Occurence (Pos.Block, Pos.Pos, '>');
         if Count = 0 then
            Parser.Quote_Level := 0;
            loop
               Pop_Block (Parser);
               exit when Parser.Current_Node = Nodes.N_NONE;
            end loop;
         else
            Buffers.Next (Pos, Count);
            Buffers.Skip_Optional_Space (Pos.Block, Pos.Pos);
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;
            Push_Block (Parser, Nodes.N_BLOCKQUOTE, Count);
         end if;
      end if;

      if Parser.Current_Node = N_PREFORMAT then
         if Parser.Preformat_Fcount = 0 then
            Count := Count_Occurence (Pos.Block, Pos.Pos, '/');
            if Count /= 3 then
               Common.Append (Parser.Text, Pos);
               return;
            end if;
            Pop_Block (Parser);
            return;
         end if;
         if Buffers.Is_Valid (Pos) and then Buffers.Char_At (Pos) = ' ' then
            Buffers.Next (Pos);
            Common.Append (Parser.Text, Pos);
            return;
         end if;
         Pop_Block (Parser);
      end if;

      if Parser.Current_Node = N_HEADER then
         Pop_Block (Parser);
      end if;
      if not Buffers.Is_Valid (Pos) then
         return;
      end if;

      C := Buffers.Char_At (Pos);
      if C = '>' then
         Count := Count_Occurence (Pos.Block, Pos.Pos, '>');
         Parser.Quote_Level := Count;
         Push_Block (Parser, Nodes.N_BLOCKQUOTE, Count);
         Buffers.Next (Pos, Count);
         Buffers.Skip_Optional_Space (Pos.Block, Pos.Pos);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
         C := Buffers.Char_At (Pos);
      end if;

      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Pos);
            return;

         when '!' =>
            Common.Parse_Header (Parser, Pos, '!');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when '/' =>
            Parse_Preformatted (Parser, Pos);
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when ' ' =>
            Parser.Preformat_Indent := 1;
            Parser.Preformat_Fence := ' ';
            Parser.Preformat_Fcount := 1;
            Flush_Text (Parser, Trim => Right);
            if Parser.Current_Node /= N_BLOCKQUOTE then
               Pop_Block (Parser);
            end if;
            Push_Block (Parser, N_PREFORMAT);
            Buffers.Next (Pos);
            Common.Append (Parser.Text, Pos);
            return;

         when '-' =>
            Common.Parse_Horizontal_Rule (Parser, Pos, '-');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when '*' | '#' =>
            Common.Parse_List (Parser, Pos);

         when others =>
            if Parser.Current_Node not in N_PARAGRAPH | N_BLOCKQUOTE then
               Pop_List (Parser);
               Push_Block (Parser, N_PARAGRAPH);
            end if;

      end case;

      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         case C is
            when '_' =>
               Parse_Format_Double (Parser, Pos, '_', Wiki.BOLD);

            when ''' =>
               Parse_Format_Double (Parser, Pos, ''', Wiki.ITALIC);

            when '-' =>
               Parse_Format_Double (Parser, Pos, '-', Wiki.STRIKEOUT);

            when '+' =>
               Parse_Format_Double (Parser, Pos, '+', Wiki.INS);

            when ',' =>
               Parse_Format_Double (Parser, Pos, ',', Wiki.SUBSCRIPT);

            when '@' =>
               Parse_Format_Double (Parser, Pos, '@', Wiki.CODE);

            when '^' =>
               Parse_Format (Parser, Pos, '^', Wiki.SUPERSCRIPT);

            when '{' =>
               Common.Parse_Quote (Parser, Pos, '{');

            when '(' =>
               Parse_Image (Parser, Pos);

            when '[' =>
               Common.Parse_Link (Parser, Pos);

            when '<' =>
               Common.Parse_Template (Parser, Pos, '<');

            when '%' =>
               Count := Count_Occurence (Pos.Block, Pos.Pos, '%');
               if Count >= 3 then
                  Parser.Empty_Line := True;
                  Flush_Text (Parser, Trim => Right);
                  if not Parser.Context.Is_Hidden then
                     Parser.Context.Filters.Add_Node (Parser.Document, Nodes.N_LINE_BREAK);
                  end if;

                  --  Skip 3 '%' characters.
                  for I in 1 .. 3 loop
                     Next (Pos);
                  end loop;
                  if Buffers.Is_Valid (Pos)
                    and then Helpers.Is_Newline (Buffers.Char_At (Pos))
                  then
                     Next (Pos);
                  end if;
               else
                  Append (Parser.Text, C);
                  Buffers.Next (Pos);
               end if;

            when CR | LF =>
               Append (Parser.Text, ' ');
               Buffers.Next (Pos);

            when '\' =>
               Next (Pos);
               if not Buffers.Is_Valid (Pos) then
                  Append (Parser.Text, C);
               else
                  Append (Parser.Text, Buffers.Char_At (Pos));
                  Buffers.Next (Pos);
               end if;

            when others =>
               Append (Parser.Text, C);
               Buffers.Next (Pos);

         end case;
      end loop;
   end Parse_Line;

end Wiki.Parsers.Dotclear;
