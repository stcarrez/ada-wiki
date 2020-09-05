-----------------------------------------------------------------------
--  render -- Wiki rendering example
--  Copyright (C) 2015, 2016, 2020 Stephane Carrez
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
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Directories;

with GNAT.Command_Line;

with Wiki.Documents;
with Wiki.Parsers;
with Wiki.Filters.TOC;
with Wiki.Filters.Html;
with Wiki.Filters.Autolink;
with Wiki.Plugins.Templates;
with Wiki.Render.Html;
with Wiki.Render.Text;
with Wiki.Streams.Text_IO;
with Wiki.Streams.Html.Text_IO;

procedure Render is

   use GNAT.Command_Line;
   use Ada.Strings.Unbounded;

   procedure Usage;
   procedure Render_Html (Doc   : in out Wiki.Documents.Document;
                          Style : in Unbounded_String);
   procedure Render_Text (Doc : in out Wiki.Documents.Document);

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Render a wiki text file into HTML (default) or text");
      Ada.Text_IO.Put_Line ("Usage: render [-t] [-m] [-M] [-d] [-c] [-s style] {wiki-file}");
      Ada.Text_IO.Put_Line ("  -t        Render to text only");
      Ada.Text_IO.Put_Line ("  -m        Render a Markdown wiki content");
      Ada.Text_IO.Put_Line ("  -M        Render a Mediawiki wiki content");
      Ada.Text_IO.Put_Line ("  -d        Render a Dotclear wiki content");
      Ada.Text_IO.Put_Line ("  -g        Render a Google wiki content");
      Ada.Text_IO.Put_Line ("  -c        Render a Creole wiki content");
      Ada.Text_IO.Put_Line ("  -s style  Use the CSS style file");
   end Usage;

   procedure Render_Html (Doc   : in out Wiki.Documents.Document;
                          Style : in Unbounded_String) is
      Output   : aliased Wiki.Streams.Html.Text_IO.Html_Output_Stream;
      Renderer : aliased Wiki.Render.Html.Html_Renderer;
   begin
      if Length (Style) > 0 then
         Output.Start_Element ("html");
         Output.Start_Element ("head");
         Output.Start_Element ("link");
         Output.Write_Attribute ("type", "text/css");
         Output.Write_Attribute ("rel", "stylesheet");
         Output.Write_Attribute ("href", To_String (Style));
         Output.End_Element ("link");
         Output.End_Element ("head");
         Output.Start_Element ("body");
      end if;
      Renderer.Set_Output_Stream (Output'Unchecked_Access);
      Renderer.Set_Render_TOC (True);
      Renderer.Render (Doc);
      if Length (Style) > 0 then
         Output.End_Element ("body");
         Output.End_Element ("html");
      end if;
   end Render_Html;

   procedure Render_Text (Doc : in out Wiki.Documents.Document) is
      Output   : aliased Wiki.Streams.Text_IO.File_Output_Stream;
      Renderer : aliased Wiki.Render.Text.Text_Renderer;
   begin
      Renderer.Set_Output_Stream (Output'Unchecked_Access);
      Renderer.Render (Doc);
   end Render_Text;

   Count     : Natural := 0;
   Html_Mode : Boolean := True;
   Syntax    : Wiki.Wiki_Syntax := Wiki.SYNTAX_MARKDOWN;
   Style     : Unbounded_String;
begin
   loop
      case Getopt ("m M d c g t s:") is
         when 'm' =>
            Syntax := Wiki.SYNTAX_MARKDOWN;

         when 'M' =>
            Syntax := Wiki.SYNTAX_MEDIA_WIKI;

         when 'c' =>
            Syntax := Wiki.SYNTAX_CREOLE;

         when 'd' =>
            Syntax := Wiki.SYNTAX_DOTCLEAR;

         when 'g' =>
            Syntax := Wiki.SYNTAX_GOOGLE;

         when 't' =>
            Html_Mode := False;

         when 's' =>
            Style := To_Unbounded_String (Parameter);

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
         Template : aliased Wiki.Plugins.Templates.File_Template_Plugin;
         TOC      : aliased Wiki.Filters.TOC.TOC_Filter;
         Doc      : Wiki.Documents.Document;
         Engine   : Wiki.Parsers.Parser;
      begin
         if Name = "" then
            if Count = 0 then
               Usage;
            end if;
            return;
         end if;
         Count := Count + 1;

         Template.Set_Template_Path (Ada.Directories.Containing_Directory (Name));

         --  Open the file and parse it (assume UTF-8).
         Input.Open (Name, "WCEM=8");
         Engine.Set_Plugin_Factory (Template'Unchecked_Access);
         Engine.Add_Filter (TOC'Unchecked_Access);
         Engine.Add_Filter (Autolink'Unchecked_Access);
         Engine.Add_Filter (Filter'Unchecked_Access);
         Engine.Set_Syntax (Syntax);
         Engine.Parse (Input'Unchecked_Access, Doc);

         --  Render the document in text or HTML.
         if Html_Mode then
            Render_Html (Doc, Style);
         else
            Render_Text (Doc);
         end if;

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line ("Cannot read file '" & Name & "'");

      end;
   end loop;

exception
   when Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option.");
      Usage;
end Render;
