-----------------------------------------------------------------------
--  words -- Extract words and links from a Wiki or HTML document
--  Copyright (C) 2016 Stephane Carrez
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
with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.IO_Exceptions;

with GNAT.Command_Line;

with Wiki.Documents;
with Wiki.Parsers;
with Wiki.Streams.Text_IO;
with Wiki.Filters.Html;
with Wiki.Filters.Autolink;
with Wiki.Filters.Collectors;

procedure Words is

   use GNAT.Command_Line;

   procedure Usage;
   procedure Print (Pos : in Wiki.Filters.Collectors.Cursor);

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Report list of words or links used in a wiki text file or HTML file");
      Ada.Text_IO.Put_Line ("Usage: words [-l] [-i] [-m] [-H] [-M] [-d] [-c] {wiki-file}");
      Ada.Text_IO.Put_Line ("  -l        Report links instead of words");
      Ada.Text_IO.Put_Line ("  -i        Report images instead of words");
      Ada.Text_IO.Put_Line ("  -m        Parse a Markdown wiki content");
      Ada.Text_IO.Put_Line ("  -M        Parse a Mediawiki wiki content");
      Ada.Text_IO.Put_Line ("  -d        Parse a Dotclear wiki content");
      Ada.Text_IO.Put_Line ("  -g        Parse a Google wiki content");
      Ada.Text_IO.Put_Line ("  -c        Parse a Creole wiki content");
      Ada.Text_IO.Put_Line ("  -H        Parse an HTML content");
   end Usage;

   procedure Print (Pos : in Wiki.Filters.Collectors.Cursor) is
      Count : constant Natural := Wiki.Filters.Collectors.WString_Maps.Element (Pos);
   begin
      Ada.Wide_Wide_Text_IO.Put (Wiki.Filters.Collectors.WString_Maps.Key (Pos));
      Ada.Wide_Wide_Text_IO.Put (" ");
      Ada.Wide_Wide_Text_IO.Put_Line (Natural'Wide_Wide_Image (Count));
   end Print;

   Count      : Natural := 0;
   Syntax     : Wiki.Wiki_Syntax := Wiki.SYNTAX_MARKDOWN;
   Link_Mode  : Boolean := False;
   Image_Mode : Boolean := False;
   Words      : aliased Wiki.Filters.Collectors.Word_Collector_Type;
   Links      : aliased Wiki.Filters.Collectors.Link_Collector_Type;
   Images     : aliased Wiki.Filters.Collectors.Image_Collector_Type;
begin
   loop
      case Getopt ("m M H d c g l i") is
         when 'm' =>
            Syntax := Wiki.SYNTAX_MARKDOWN;

         when 'M' =>
            Syntax := Wiki.SYNTAX_MEDIA_WIKI;

         when 'H' =>
            Syntax := Wiki.SYNTAX_HTML;

         when 'c' =>
            Syntax := Wiki.SYNTAX_CREOLE;

         when 'd' =>
            Syntax := Wiki.SYNTAX_DOTCLEAR;

         when 'g' =>
            Syntax := Wiki.SYNTAX_GOOGLE;

         when 'l' =>
            Link_Mode := True;

         when 'i' =>
            Image_Mode := True;

         when others =>
            exit;
      end case;
   end loop;

   loop
      declare
         Name     : constant String := GNAT.Command_Line.Get_Argument;
         Input    : aliased Wiki.Streams.Text_IO.File_Input_Stream;
         Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
         Autolink : aliased Wiki.Filters.Autolink.Autolink_Filter;
         Doc      : Wiki.Documents.Document;
         Engine   : Wiki.Parsers.Parser;
      begin
         if Name = "" then
            if Count = 0 then
               Usage;
            end if;
            exit;
         end if;
         Count := Count + 1;

         --  Open the file and parse it (assume UTF-8).
         Input.Open (Name, "WCEM=8");
         Engine.Add_Filter (Words'Unchecked_Access);
         Engine.Add_Filter (Links'Unchecked_Access);
         Engine.Add_Filter (Images'Unchecked_Access);
         Engine.Add_Filter (Filter'Unchecked_Access);
         Engine.Add_Filter (Autolink'Unchecked_Access);
         Engine.Set_Syntax (Syntax);
         Engine.Parse (Input'Unchecked_Access, Doc);

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line ("Cannot read file '" & Name & "'");

      end;
   end loop;

   if Image_Mode then
      Images.Iterate (Print'Access);
   elsif Link_Mode then
      Links.Iterate (Print'Access);
   else
      Words.Iterate (Print'Access);
   end if;

exception
   when Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option.");
      Usage;
end Words;
