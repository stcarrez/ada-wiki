-----------------------------------------------------------------------
--  wiki-parsers-mediawiki -- Media Wiki parser operations
--  Copyright (C) 2011- 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Common;
package body Wiki.Parsers.MediaWiki is

   use Wiki.Helpers;
   use Wiki.Buffers;
   use type Wiki.Nodes.Node_Kind;

   procedure Parse_Bold_Italic (Parser  : in out Parser_Type;
                                Text    : in out Wiki.Buffers.Cursor);

   --  ------------------------------
   --  Parse an italic, bold or bold + italic sequence.
   --  Example:
   --    ''name''         (italic)
   --    '''name'''       (bold)
   --    '''''name'''''   (bold+italic)
   --  ------------------------------
   procedure Parse_Bold_Italic (Parser  : in out Parser_Type;
                                Text    : in out Wiki.Buffers.Cursor) is
      Count : Natural := Count_Occurence (Text.Block, Text.Pos, ''');
   begin
      case Count is
         when 1 =>
            Common.Parse_Text (Parser, Text);
            return;

         when 2 =>
            Toggle_Format (Parser, ITALIC);

         when 3 =>
            Toggle_Format (Parser, BOLD);

         when 4 =>
            Toggle_Format (Parser, BOLD);
            Common.Parse_Text (Parser, Text);
            Count := 3;

         when 5 =>
            Toggle_Format (Parser, BOLD);
            Toggle_Format (Parser, ITALIC);

         when others =>
            Common.Parse_Text (Parser, Text);
            return;

      end case;
      Buffers.Next (Text, Count);
   end Parse_Bold_Italic;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Common.Parse_Html_Element (Parser, Pos, Start => False);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
      end if;

      if Parser.Pre_Tag_Counter > 0 then
         Common.Parse_Html_Preformatted (Parser, Pos);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
      end if;

      if Parser.Current_Node = Nodes.N_PREFORMAT then
         if Buffers.Char_At (Pos) = ' ' then
            Buffers.Next (Pos);
            Common.Append (Parser.Text, Pos);
            return;
         end if;
         Pop_Block (Parser);
      end if;

      if Parser.Current_Node = Nodes.N_HEADER then
         Pop_Block (Parser);
      end if;

      C := Buffers.Char_At (Pos);
      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Pos);
            return;

         when '=' =>
            Common.Parse_Header (Parser, Pos, '=');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when '-' =>
            Common.Parse_Horizontal_Rule (Parser, Pos, '-');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when '*' | '#' =>
            if Common.Is_List (Pos) then
               Common.Parse_List (Parser, Pos);
            end if;

         when ' ' =>
            declare
               Next_Pos : Wiki.Buffers.Cursor := Pos;
            begin
               Buffers.Next (Next_Pos);
               if Common.Is_List (Next_Pos) then
                  Pos := Next_Pos;
                  Common.Parse_List (Parser, Pos);
               end if;
            end;
            if Parser.In_Html = HTML_NONE then
               Parser.Preformat_Indent := 1;
               Parser.Preformat_Fence := ' ';
               Parser.Preformat_Fcount := 1;
               Flush_Text (Parser, Trim => Right);
               Pop_Block (Parser);
               Push_Block (Parser, Nodes.N_PREFORMAT);
               Buffers.Next (Pos);
               Common.Append (Parser.Text, Pos);
               return;
            end if;

         when ';' =>
            Common.Parse_Definition (Parser, Pos, True);

         when ':' =>
            if Parser.Current_Node = Nodes.N_DEFINITION_TERM then
               Common.Parse_Definition (Parser, Pos, False);
            end if;

         when others =>
            if Parser.Current_Node /= Nodes.N_PARAGRAPH then
               Pop_List (Parser);
               Push_Block (Parser, Nodes.N_PARAGRAPH);
            end if;

      end case;

      while Buffers.Is_Valid (Pos) loop
         C := Buffers.Char_At (Pos);
         case C is
            when ''' =>
               Parse_Bold_Italic (Parser, Pos);

            when '/' =>
               Parse_Format_Double (Parser, Pos, '/', Wiki.EMPHASIS);

            when '[' =>
               Common.Parse_Link (Parser, Pos);

            when '{' =>
               Common.Parse_Template (Parser, Pos, '{');

            when CR | LF =>
               Append (Parser.Text, ' ');
               Buffers.Next (Pos);

            when '<' =>
               Common.Parse_Html_Element (Parser, Pos, Start => True);

            when '&' =>
               Common.Parse_Entity (Parser, Pos);

            when others =>
               if C = ':' and then Parser.Current_Node = Nodes.N_DEFINITION_TERM then
                  Common.Parse_Definition (Parser, Pos, False);
               else
                  Append (Parser.Text, C);
                  Buffers.Next (Pos);
               end if;

         end case;
      end loop;
   end Parse_Line;

end Wiki.Parsers.MediaWiki;
