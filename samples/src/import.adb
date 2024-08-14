-----------------------------------------------------------------------
--  import -- Import some HTML content and generate Wiki text
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
with Ada.Wide_Wide_Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNAT.Command_Line;

with Util.Files;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Strings.Transforms;

with Wiki.Strings;
with Wiki.Filters.Html;
with Wiki.Filters.TOC;
with Wiki.Streams.Builders;
with Wiki.Streams.Html.Builders;
with Wiki.Render.Html;
with Wiki.Render.Text;
with Wiki.Render.Wiki;
with Wiki.Documents;
with Wiki.Parsers;

procedure Import is

   use GNAT.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Strings.UTF_Encoding;

   procedure Usage;
   function Is_Url (Name : in String) return Boolean;
   procedure Parse_Url (Url : in String);
   procedure Parse (Content : in String);
   procedure Print (Item : in Wiki.Strings.WString);

   Html_Filter : aliased Wiki.Filters.Html.Html_Filter_Type;
   TOC         : aliased Wiki.Filters.TOC.TOC_Filter;
   Count       : Natural := 0;
   Html_Mode   : Boolean := True;
   Wiki_Mode   : Boolean := False;
   Syntax      : Wiki.Wiki_Syntax := Wiki.SYNTAX_MARKDOWN;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Import HTML into a target Wiki format");
      Ada.Text_IO.Put_Line ("Usage: import [-t] [-m] [-M] [-H] [-d] [-c] {URL | file}");
      Ada.Text_IO.Put_Line ("  -t      Convert to text only");
      Ada.Text_IO.Put_Line ("  -m      Convert to Markdown");
      Ada.Text_IO.Put_Line ("  -M      Convert to Mediawiki");
      Ada.Text_IO.Put_Line ("  -H      Convert to HTML");
      Ada.Text_IO.Put_Line ("  -d      Convert to Dotclear");
      Ada.Text_IO.Put_Line ("  -c      Convert to Creole");
   end Usage;

   procedure Print (Item : in Wiki.Strings.WString) is
   begin
      Ada.Wide_Wide_Text_IO.Put (Item);
   end Print;

   procedure Parse (Content : in String) is
      Doc    : Wiki.Documents.Document;
      Engine : Wiki.Parsers.Parser;
   begin
      Engine.Add_Filter (TOC'Unchecked_Access);
      Engine.Add_Filter (Html_Filter'Unchecked_Access);
      if Wiki_Mode then
         declare
            Stream   : aliased Wiki.Streams.Builders.Output_Builder_Stream;
            Renderer : aliased Wiki.Render.Wiki.Wiki_Renderer;
         begin
            Engine.Set_Syntax (Wiki.SYNTAX_HTML);
            Engine.Parse (Wide_Wide_Strings.Decode (Content), Doc);
            Renderer.Set_Output_Stream (Stream'Unchecked_Access, Syntax);
            Renderer.Render (Doc);
            Stream.Iterate (Print'Access);
         end;
      elsif Html_Mode then
         declare
            Stream   : aliased Wiki.Streams.Html.Builders.Html_Output_Stream;
            Renderer : aliased Wiki.Render.Html.Html_Renderer;
         begin
            Engine.Set_Syntax (Syntax);
            Engine.Parse (Wide_Wide_Strings.Decode (Content), Doc);
            Renderer.Set_Output_Stream (Stream'Unchecked_Access);
            Renderer.Render (Doc);
            Stream.Iterate (Print'Access);
         end;
      else
         declare
            Stream   : aliased Wiki.Streams.Builders.Output_Builder_Stream;
            Renderer : aliased Wiki.Render.Text.Text_Renderer;
         begin
            Engine.Set_Syntax (Syntax);
            Engine.Parse (Wide_Wide_Strings.Decode (Content), Doc);
            Renderer.Set_Output_Stream (Stream'Unchecked_Access);
            Renderer.Render (Doc);
            Stream.Iterate (Print'Access);
         end;
      end if;
      Ada.Wide_Wide_Text_IO.New_Line;
   end Parse;

   procedure Parse_Url (Url : in String) is
      Command : constant String := "wget -q -O - " & Url;
      Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
      Buffer  : Util.Streams.Buffered.Input_Buffer_Stream;
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Pipe.Open (Command);
      Buffer.Initialize (Pipe'Unchecked_Access, 1024 * 1024);
      Buffer.Read (Content);
      Pipe.Close;
      if Pipe.Get_Exit_Status /= 0 then
         Ada.Text_IO.Put_Line (Command & " exited with status "
                               & Integer'Image (Pipe.Get_Exit_Status));
      else
         Parse (To_String (Content));
      end if;
   end Parse_Url;

   function Is_Url (Name : in String) return Boolean is
   begin
      if Name'Length <= 9 then
         return False;
      else
         return Name (Name'First .. Name'First + 6) = "http://"
           or else Name (Name'First .. Name'First + 7) = "https://";
      end if;
   end Is_Url;

begin
   loop
      case Getopt ("m M H d c t f:") is
         when 'm' =>
            Syntax := Wiki.SYNTAX_MARKDOWN;
            Wiki_Mode := True;

         when 'M' =>
            Syntax := Wiki.SYNTAX_MEDIA_WIKI;
            Wiki_Mode := True;

         when 'H' =>
            Syntax := Wiki.SYNTAX_HTML;

         when 'c' =>
            Syntax := Wiki.SYNTAX_CREOLE;
            Wiki_Mode := True;

         when 'd' =>
            Syntax := Wiki.SYNTAX_DOTCLEAR;
            Wiki_Mode := True;

         when 't' =>
            Html_Mode := False;

         when 'f' =>
            declare
               Value : constant String := Util.Strings.Transforms.To_Upper_Case (Parameter);
            begin
               Html_Filter.Hide (Wiki.Html_Tag'Value (Value & "_TAG"));
            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line ("Invalid tag " & Value);
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
         if Is_Url (Name) then
            Parse_Url (Name);
         else
            Util.Files.Read_File (Name, Data);
            Parse (To_String (Data));
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

end Import;
