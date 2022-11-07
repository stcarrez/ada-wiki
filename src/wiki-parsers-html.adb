-----------------------------------------------------------------------
--  wiki-parsers-html -- Wiki HTML parser
--  Copyright (C) 2015, 2016, 2018, 2020, 2021, 2022 Stephane Carrez
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

with Wiki.Helpers;
with Wiki.Html_Parser;
package body Wiki.Parsers.Html is

   use type Wiki.Html_Parser.State_Type;
   use type Wiki.Html_Parser.Entity_State_Type;
   use type Wiki.Buffers.Buffer_Access;

   procedure Parse_Line_Fragment (Parser : in out Parser_Type;
                                  Text   : in out Wiki.Strings.WString);

   procedure Parse_Line_Fragment (Parser : in out Parser_Type;
                                  Text   : in out Wiki.Strings.WString) is
      procedure Process (Kind : in Wiki.Html_Parser.State_Type;
                         Name : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List);

      procedure Process (Kind : in Wiki.Html_Parser.State_Type;
                         Name : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
         Tag  : constant Wiki.Html_Tag := Wiki.Find_Tag (Name);
      begin
         if Kind = Wiki.Html_Parser.HTML_START then
            Start_Element (Parser, Tag, Attributes);
         elsif Kind = Wiki.Html_Parser.HTML_END then
            End_Element (Parser, Tag);
         elsif Kind = Wiki.Html_Parser.HTML_START_END then
            Start_Element (Parser, Tag, Attributes);
            End_Element (Parser, Tag);
         end if;
      end Process;

      Pos   : Natural := Text'First;
      Last  : constant Natural := Text'Last;
      First : Natural := Text'First;
      C     : Wiki.Strings.WChar;
   begin
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Wiki.Html_Parser.Parse_Element (Parser.Html, Text, Pos, Process'Access, Pos);
      end if;
      First := Pos;
      while Pos <= Last loop
         C := Text (Pos);
         case C is
            when '<' =>
               if First < Pos then
                  Append (Parser.Text, Text (First .. Pos - 1));
               end if;
               Wiki.Html_Parser.Parse_Element (Parser.Html, Text, Pos + 1, Process'Access, Pos);
               First := Pos;

            when '&' =>
               declare
                  Status : Wiki.Html_Parser.Entity_State_Type := Wiki.Html_Parser.ENTITY_NONE;
                  Next   : Positive;
               begin
                  Wiki.Html_Parser.Parse_Entity (Parser.Html, Text, Pos + 1, Status, C, Next);
                  if Status = Wiki.Html_Parser.ENTITY_VALID then
                     if First < Pos then
                        Append (Parser.Text, Text (First .. Pos - 1));
                     end if;
                     Append (Parser.Text, C);
                     First := Next;
                     Pos := Next;
                  else
                     Pos := Pos + 1;
                  end if;
               end;

            when Wiki.Helpers.LF | Wiki.Helpers.CR =>
               if Parser.Pre_Tag_Counter = 0 then
                  Text (Pos) := ' ';
               end if;
               Pos := Pos + 1;

            when others =>
               Pos := Pos + 1;

         end case;
      end loop;
      if First < Pos then
         Append (Parser.Text, Text (First .. Last));
      end if;
   end Parse_Line_Fragment;

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access) is
      Buffer : Wiki.Buffers.Buffer_Access := Text;
   begin
      while Buffer /= null loop
         Parse_Line_Fragment (Parser, Buffer.Content (1 .. Buffer.Last));
         Buffer := Buffer.Next_Block;
      end loop;
   end Parse_Line;

end Wiki.Parsers.Html;
