-----------------------------------------------------------------------
--  render -- Wiki rendering example
--  Copyright (C) 2015, 2016, 2020, 2021, 2022 Stephane Carrez
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
with Ada.Command_Line;

with Wiki.Documents;
with Wiki.Parsers;
with Wiki.Strings;
with Wiki.Filters.TOC;
with Wiki.Filters.Html;
with Wiki.Filters.Autolink;
with Wiki.Plugins.Templates;
with Wiki.Render.Html;
with Wiki.Render.Text;
with Wiki.Streams.Text_IO;
with Wiki.Streams.Html.Text_IO;
with Wiki.Nodes.Dump;

procedure Render is

   use Ada.Strings.Unbounded;

   procedure Usage;
   procedure Render_Html (Doc   : in out Wiki.Documents.Document;
                          Style : in Unbounded_String);
   procedure Render_Text (Doc : in out Wiki.Documents.Document);
   procedure Dump (Doc : in Wiki.Documents.Document);
   procedure Render_File (Name : in String);

   Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   Count     : Natural := 0;
   Html_Mode : Boolean := True;
   Html_Toc  : Boolean := False;
   Syntax    : Wiki.Wiki_Syntax := Wiki.SYNTAX_MARKDOWN;
   Style     : Unbounded_String;
   Indent    : Natural := 3;
   Dump_Tree : Boolean := False;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Render a wiki text file into HTML (default) or text");
      Ada.Text_IO.Put_Line ("Usage: render [options] {wiki-file}");
      Ada.Text_IO.Put_Line ("  -0, -1, -2  Controls the indentation level");
      Ada.Text_IO.Put_Line ("  -t          Render to text only");
      Ada.Text_IO.Put_Line ("  -m          Render a Markdown wiki content");
      Ada.Text_IO.Put_Line ("  -M          Render a Mediawiki wiki content");
      Ada.Text_IO.Put_Line ("  -T          Render a Textile wiki content");
      Ada.Text_IO.Put_Line ("  -H          Render a HTML wiki content");
      Ada.Text_IO.Put_Line ("  -d          Render a Dotclear wiki content");
      Ada.Text_IO.Put_Line ("  -g          Render a Google wiki content");
      Ada.Text_IO.Put_Line ("  -c          Render a Creole wiki content");
      Ada.Text_IO.Put_Line ("  -s style    Use the CSS style file");
      Ada.Text_IO.Put_Line ("  -dump       Dump the document tree");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Usage;

   procedure Render_Html (Doc   : in out Wiki.Documents.Document;
                          Style : in Unbounded_String) is
      Output   : aliased Wiki.Streams.Html.Text_IO.Html_Output_Stream;
      Renderer : aliased Wiki.Render.Html.Html_Renderer;
   begin
      Output.Set_Indent_Level (Indent);
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
      Renderer.Set_Render_TOC (Html_Toc);
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

   procedure Dump (Doc : in Wiki.Documents.Document) is
      procedure Write (Indent : in Positive;
                       Line   : in Wiki.Strings.WString);

      procedure Write (Indent : in Positive;
                       Line   : in Wiki.Strings.WString) is
         S : constant String := Wiki.Strings.To_String (Line);
      begin
         Ada.Text_IO.Set_Col (Ada.Text_IO.Count (Indent));
         Ada.Text_IO.Put_Line (S);
      end Write;

      procedure Dump is
         new Wiki.Nodes.Dump (Write);
   begin
      Doc.Iterate (Dump'Access);
   end Dump;

   procedure Render_File (Name : in String) is
      Input    : aliased Wiki.Streams.Text_IO.File_Input_Stream;
      Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
      Autolink : aliased Wiki.Filters.Autolink.Autolink_Filter;
      Template : aliased Wiki.Plugins.Templates.File_Template_Plugin;
      TOC      : aliased Wiki.Filters.TOC.TOC_Filter;
      Doc      : Wiki.Documents.Document;
      Engine   : Wiki.Parsers.Parser;
   begin
      Count := Count + 1;

      Template.Set_Template_Path (Ada.Directories.Containing_Directory (Name));

      --  Open the file and parse it (assume UTF-8).
      if Name /= "--" then
         Input.Open (Name, "WCEM=8");
      end if;
      Engine.Set_Plugin_Factory (Template'Unchecked_Access);
      Engine.Add_Filter (TOC'Unchecked_Access);
      Engine.Add_Filter (Autolink'Unchecked_Access);
      Engine.Add_Filter (Filter'Unchecked_Access);
      Engine.Set_Syntax (Syntax);
      Engine.Parse (Input'Unchecked_Access, Doc);

      --  Render the document in text or HTML.
      if Dump_Tree then
         Dump (Doc);
      elsif Html_Mode then
         Render_Html (Doc, Style);
      else
         Render_Text (Doc);
      end if;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Cannot read file '" & Name & "'");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   end Render_File;

begin
   for I in 1 .. Arg_Count loop
      declare
         Param : constant String := Ada.Command_Line.Argument (I);
      begin
         if Param = "-m" then
            Syntax := Wiki.SYNTAX_MARKDOWN;
         elsif Param = "-M" then
            Syntax := Wiki.SYNTAX_MEDIA_WIKI;
         elsif Param = "-c" then
            Syntax := Wiki.SYNTAX_CREOLE;
         elsif Param = "-d" then
            Syntax := Wiki.SYNTAX_DOTCLEAR;
         elsif Param = "-H" then
            Syntax := Wiki.SYNTAX_HTML;
         elsif Param = "-T" then
            Syntax := Wiki.SYNTAX_TEXTILE;
         elsif Param = "-g" then
            Syntax := Wiki.SYNTAX_GOOGLE;
         elsif Param = "-t" then
            Html_Mode := False;
         elsif Param = "-z" then
            Html_Toc := True;
         elsif Param = "-s" then
            Style := To_Unbounded_String (Param);
         elsif Param = "--" then
            Render_File (Param);
         elsif Param = "-0" then
            Indent := 0;
         elsif Param = "-1" then
            Indent := 1;
         elsif Param = "-2" then
            Indent := 2;
         elsif Param = "-dump" then
            Dump_Tree := True;
         elsif Param (Param'First) = '-' then
            Usage;
            return;
         else
            Render_File (Param);
         end if;
      end;
   end loop;

   if Count = 0 then
      Usage;
   end if;

end Render;
