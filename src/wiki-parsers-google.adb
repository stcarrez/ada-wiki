-----------------------------------------------------------------------
--  wiki-parsers-google -- Google Code parser operations
--  Copyright (C) 2011 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Common;
package body Wiki.Parsers.Google is

   use Wiki.Helpers;
   use type Wiki.Nodes.Node_Kind;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor) is
      Pos    : Wiki.Buffers.Cursor := Text;
      C      : Wiki.Strings.WChar;
      Count  : Natural;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Common.Parse_Html_Element (Parser, Pos.Block, Pos.Pos, Start => False);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
      end if;

      if Parser.Pre_Tag_Counter > 0 then
         Common.Parse_Html_Preformatted (Parser, Pos.Block, Pos.Pos);
         if not Buffers.Is_Valid (Pos) then
            return;
         end if;
      end if;

      if Parser.Current_Node = Nodes.N_PREFORMAT then
         Count := Buffers.Count_Occurence (Pos.Block, Pos.Pos, '}');
         if Count /= 3 then
            Common.Append (Parser.Text, Pos.Block, Pos.Pos);
            return;
         end if;
         Pop_Block (Parser);
         return;
      end if;

      if Parser.Current_Node = Nodes.N_HEADER then
         Pop_Block (Parser);
      end if;

      if not Buffers.Is_Valid (Pos) then
         return;
      end if;

      C := Buffers.Char_At (Pos);
      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Pos.Block, Pos.Pos);
            return;

         when '=' =>
            Common.Parse_Header (Parser, Pos.Block, Pos.Pos, '=');
            if not Buffers.Is_Valid (Pos) then
               return;
            end if;

         when '*' | '#' =>
            if Common.Is_List (Pos.Block, Pos.Pos) then
               Common.Parse_List (Parser, Pos.Block, Pos.Pos);
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
            when '_' =>
               Parse_Format (Parser, Pos.Block, Pos.Pos, '_', Wiki.ITALIC);

            when '*' =>
               Parse_Format (Parser, Pos.Block, Pos.Pos, '*', Wiki.BOLD);

            when '^' =>
               Parse_Format (Parser, Pos.Block, Pos.Pos, '^', Wiki.SUPERSCRIPT);

            when '`' =>
               Parse_Format (Parser, Pos.Block, Pos.Pos, '`', Wiki.CODE);

            when ',' =>
               Parse_Format_Double (Parser, Pos.Block, Pos.Pos, ',', Wiki.SUBSCRIPT);

            when '~' =>
               Parse_Format_Double (Parser, Pos.Block, Pos.Pos, '~', Wiki.STRIKEOUT);

            when '[' =>
               Common.Parse_Link (Parser, Pos.Block, Pos.Pos);

            when '{' =>
               Common.Parse_Preformatted (Parser, Pos.Block, Pos.Pos, '{');

            when CR | LF =>
               Append (Parser.Text, ' ');
               Buffers.Next (Pos);

            when '<' =>
               Common.Parse_Html_Element (Parser, Pos.Block, Pos.Pos, Start => True);

            when '&' =>
               Common.Parse_Entity (Parser, Pos.Block, Pos.Pos);

            when others =>
               Append (Parser.Text, C);
               Buffers.Next (Pos);

         end case;
      end loop;
   end Parse_Line;

end Wiki.Parsers.Google;
