-----------------------------------------------------------------------
--  wiki-parsers-html -- Wiki HTML parser
--  Copyright (C) 2015, 2016, 2018, 2020, 2021, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
                         Name : in Wiki.Strings.BString;
                         Attributes : in out Wiki.Attributes.Attribute_List);

      procedure Process (Kind : in Wiki.Html_Parser.State_Type;
                         Name : in Wiki.Strings.BString;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
         WN   : constant Wiki.Strings.WString := Wiki.Strings.To_WString (Name);
         Tag  : constant Wiki.Html_Tag := Wiki.Find_Tag (WN);
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
      Kind  : Wiki.Html_Parser.State_Type;
   begin
      if not Wiki.Html_Parser.Is_Empty (Parser.Html) then
         Wiki.Html_Parser.Parse_Element (Parser.Html, Text, Pos, Kind, Pos);
         if Kind /= Wiki.Html_Parser.HTML_NONE then
            Process (Kind, Parser.Html.Elt_Name, Parser.Html.Attributes);
         end if;
      end if;
      First := Pos;
      while Pos <= Last loop
         C := Text (Pos);
         case C is
            when '<' =>
               if First < Pos then
                  Append (Parser.Text, Text (First .. Pos - 1));
               end if;
               Wiki.Html_Parser.Parse_Element (Parser.Html, Text, Pos + 1, Kind, Pos);
               if Kind /= Wiki.Html_Parser.HTML_NONE then
                  Process (Kind, Parser.Html.Elt_Name, Parser.Html.Attributes);
               end if;
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
               else
                  if First < Pos then
                     Append (Parser.Text, Text (First .. Pos - 1));
                  end if;
                  Append (Parser.Text, Wiki.Helpers.LF);
                  First := Pos + 1;
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
                         Text   : in Wiki.Buffers.Cursor) is
      Buffer : Wiki.Buffers.Buffer_Access := Text.Block;
   begin
      while Buffer /= null loop
         Parse_Line_Fragment (Parser, Buffer.Content (1 .. Buffer.Last));
         Buffer := Buffer.Next_Block;
      end loop;
   end Parse_Line;

end Wiki.Parsers.Html;
