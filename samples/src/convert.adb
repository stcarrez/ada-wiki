-----------------------------------------------------------------------
--  convert -- Convert a wiki format into another
--  Copyright (C) 2022 Stephane Carrez
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
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.Command_Line;

with Util.Files;
with Util.Strings.Transforms;

with Wiki.Strings;
with Wiki.Filters.Html;
with Wiki.Filters.TOC;
with Wiki.Streams.Builders;
with Wiki.Render.Wiki;
with Wiki.Documents;
with Wiki.Parsers;

procedure Convert is

   use GNAT.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Strings.UTF_Encoding;

   procedure Usage;
   procedure Parse (Content : in String);
   procedure Print (Item : in Wiki.Strings.WString);
   function To_Syntax (Name : in String) return Wiki.Wiki_Syntax;

   Html_Filter : aliased Wiki.Filters.Html.Html_Filter_Type;
   TOC         : aliased Wiki.Filters.TOC.TOC_Filter;
   Count       : Natural := 0;
   Src_Syntax  : Wiki.Wiki_Syntax := Wiki.SYNTAX_MARKDOWN;
   Dst_Syntax  : Wiki.Wiki_Syntax := Wiki.SYNTAX_MARKDOWN;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Convert a wiki file from one format to another");
      Ada.Text_IO.Put_Line ("Usage: convert [-s format] [-d format] file...");
      Ada.Text_IO.Put_Line ("  -s format  Define the source file format");
      Ada.Text_IO.Put_Line ("  -d format  Define the destination file format");
   end Usage;

   procedure Print (Item : in Wiki.Strings.WString) is
   begin
      Ada.Wide_Wide_Text_IO.Put (Item);
   end Print;

   procedure Parse (Content : in String) is
      Doc      : Wiki.Documents.Document;
      Engine   : Wiki.Parsers.Parser;
      Stream   : aliased Wiki.Streams.Builders.Output_Builder_Stream;
      Renderer : aliased Wiki.Render.Wiki.Wiki_Renderer;
   begin
      Engine.Add_Filter (TOC'Unchecked_Access);
      Engine.Add_Filter (Html_Filter'Unchecked_Access);
      Engine.Set_Syntax (Src_Syntax);
      Engine.Parse (Wide_Wide_Strings.Decode (Content), Doc);

      Renderer.Set_Output_Stream (Stream'Unchecked_Access, Dst_Syntax);
      Renderer.Render (Doc);
      Stream.Iterate (Print'Access);
      Ada.Wide_Wide_Text_IO.New_Line;
   end Parse;

   function To_Syntax (Name : in String) return Wiki.Wiki_Syntax is
   begin
      if Name = "markdown" then
         return Wiki.SYNTAX_MARKDOWN;
      end if;

      if Name = "dotclear" then
         return Wiki.SYNTAX_DOTCLEAR;
      end if;

      if Name = "creole" then
         return Wiki.SYNTAX_CREOLE;
      end if;

      if Name = "textile" then
         return Wiki.SYNTAX_TEXTILE;
      end if;

      if Name = "mediawiki" then
         return Wiki.SYNTAX_MEDIA_WIKI;
      end if;

      if Name = "html" then
         return Wiki.SYNTAX_HTML;
      end if;

      return Wiki.SYNTAX_MARKDOWN;
   end To_Syntax;

begin
   loop
      case Getopt ("s: d:") is

         when 's' =>
            declare
               Value : constant String := Util.Strings.Transforms.To_Lower_Case (Parameter);
            begin
               Src_Syntax := To_Syntax (Value);

            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line ("Invalid source format " & Value);
            end;

         when 'd' =>
            declare
               Value : constant String := Util.Strings.Transforms.To_Lower_Case (Parameter);
            begin
               Dst_Syntax := To_Syntax (Value);

            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line ("Invalid source format " & Value);
            end;

         when others =>
            exit;
      end case;
   end loop;

   loop
      declare
         Name : constant String := GNAT.Command_Line.Get_Argument;
         Data : Unbounded_String;
      begin
         if Name = "" then
            if Count = 0 then
               Usage;
            end if;
            return;
         end if;
         Count := Count + 1;
         Util.Files.Read_File (Name, Data);
         Parse (To_String (Data));

      exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line ("Cannot read file '" & Name & "'");

      end;
   end loop;

exception
   when Invalid_Switch =>
      Ada.Text_IO.Put_Line ("Invalid option.");
      Usage;

end Convert;
