-----------------------------------------------------------------------
--  wiki-parsers-google -- Google Code parser operations
--  Copyright (C) 2011 - 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Wiki.Nodes;
with Wiki.Helpers;
with Wiki.Parsers.Common;
package body Wiki.Parsers.Google is

   use Wiki.Helpers;
   use Wiki.Buffers;
   use type Wiki.Nodes.Node_Kind;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access) is
      Pos    : Natural := 1;
      C      : Wiki.Strings.WChar;
      Buffer : Wiki.Buffers.Buffer_Access := Text;
   begin
      --  Feed the HTML parser if there are some pending state.
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Common.Parse_Html_Element (Parser, Buffer, Pos, Start => False);
         if Buffer = null then
            return;
         end if;
      end if;

      if Parser.Pre_Tag_Counter > 0 then
         Common.Parse_Html_Preformatted (Parser, Buffer, Pos);
         if Buffer = null then
            return;
         end if;
      end if;

      if Parser.Current_Node = Nodes.N_PREFORMAT then
         if Buffer.Content (Pos) = ' ' then
            Common.Append (Parser.Text, Buffer, Pos + 1);
            return;
         end if;
         Pop_Block (Parser);
      end if;

      if Parser.Current_Node = Nodes.N_HEADER then
         Pop_Block (Parser);
      end if;

      C := Buffer.Content (Pos);
      case C is
         when CR | LF =>
            Common.Parse_Paragraph (Parser, Buffer, Pos);
            return;

         when '=' =>
            Common.Parse_Header (Parser, Buffer, Pos, '=');
            if Buffer = null then
               return;
            end if;

         when '*' | '#' =>
            if Common.Is_List (Buffer, Pos) then
               Common.Parse_List (Parser, Buffer, Pos);
            end if;

         when others =>
            if Parser.Current_Node /= Nodes.N_PARAGRAPH then
               Pop_List (Parser);
               Push_Block (Parser, Nodes.N_PARAGRAPH);
            end if;

      end case;

      Main :
      while Buffer /= null loop
         declare
            Last : constant Natural := Buffer.Last;
         begin
            while Pos <= Last loop
               C := Buffer.Content (Pos);
               case C is
                  when '_' =>
                     Parse_Format (Parser, Buffer, Pos, '_', Wiki.ITALIC);
                     exit Main when Buffer = null;

                  when '*' =>
                     Parse_Format (Parser, Buffer, Pos, '*', Wiki.BOLD);
                     exit Main when Buffer = null;

                  when '^' =>
                     Parse_Format (Parser, Buffer, Pos, '^', Wiki.SUPERSCRIPT);
                     exit Main when Buffer = null;

                  when '`' =>
                     Parse_Format (Parser, Buffer, Pos, '`', Wiki.CODE);
                     exit Main when Buffer = null;

                  when ',' =>
                     Parse_Format_Double (Parser, Buffer, Pos, ',', Wiki.SUBSCRIPT);
                     exit Main when Buffer = null;

                  when '~' =>
                     Parse_Format_Double (Parser, Buffer, Pos, '~', Wiki.STRIKEOUT);
                     exit Main when Buffer = null;

                  when '[' =>
                     Common.Parse_Link (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when '{' =>
                     Common.Parse_Template (Parser, Buffer, Pos, '{');
                     exit Main when Buffer = null;

                  when CR | LF =>
                     Append (Parser.Text, ' ');
                     Pos := Pos + 1;

                  when '<' =>
                     Common.Parse_Html_Element (Parser, Buffer, Pos, Start => True);
                     exit Main when Buffer = null;

                  when '&' =>
                     Common.Parse_Entity (Parser, Buffer, Pos);
                     exit Main when Buffer = null;

                  when others =>
                     Append (Parser.Text, C);
                     Pos := Pos + 1;

               end case;
            end loop;
         end;
         Buffer := Buffer.Next_Block;
         Pos := 1;
      end loop Main;
   end Parse_Line;

end Wiki.Parsers.Google;
