-----------------------------------------------------------------------
--  import -- Import some HTML content and generate Wiki text
--  Copyright (C) 2015, 2016 Stephane Carrez
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
with Ada.Characters.Conversions;

with GNAT.Command_Line;

with Util.Files;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Strings.Transforms;

with Wiki.Parsers;
with Wiki.Filters.Html;
with Wiki.Writers.Builders;
with Wiki.Render.Html;
with Wiki.Render.Text;
with Wiki.Render.Wiki;

procedure Import is

   use GNAT.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Characters.Conversions;

   procedure Usage;
   function Is_Url (Name : in String) return Boolean;
   procedure Parse_Url (Url : in String);
   procedure Parse (Content : in String);

   Html_Filter : aliased Wiki.Filters.Html.Html_Filter_Type;
   Count       : Natural := 0;
   Html_Mode   : Boolean := True;
   Wiki_Mode   : Boolean := False;
   Syntax      : Wiki.Parsers.Wiki_Syntax_Type := Wiki.Parsers.SYNTAX_MARKDOWN;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Usage: import [-t] [-m] [-M] [-d] [-c] {URL | file}");
      Ada.Text_IO.Put_Line ("  -t      Convert to text only");
      Ada.Text_IO.Put_Line ("  -m      Convert to Markdown");
      Ada.Text_IO.Put_Line ("  -M      Convert to Mediawiki");
      Ada.Text_IO.Put_Line ("  -d      Convert to Dotclear");
      Ada.Text_IO.Put_Line ("  -c      Convert to Creole");
   end Usage;

   procedure Parse (Content : in String) is
   begin
      if Wiki_Mode then
         declare
            Writer   : aliased Wiki.Writers.Builders.Writer_Builder_Type;
            Renderer : aliased Wiki.Render.Wiki.Wiki_Renderer;
         begin
            Renderer.Set_Writer (Writer'Unchecked_Access, Syntax);
            Html_Filter.Set_Document (Renderer'Unchecked_Access);
            Wiki.Parsers.Parse (Html_Filter'Unchecked_Access,
                                To_Wide_Wide_String (Content), Wiki.Parsers.SYNTAX_HTML);
            Ada.Text_IO.Put_Line (Writer.To_String);
         end;
      elsif Html_Mode then
         declare
            Writer   : aliased Wiki.Writers.Builders.Html_Writer_Type;
            Renderer : aliased Wiki.Render.Html.Html_Renderer;
         begin
            Renderer.Set_Writer (Writer'Unchecked_Access);
            Html_Filter.Set_Document (Renderer'Unchecked_Access);
            Wiki.Parsers.Parse (Html_Filter'Unchecked_Access,
                                To_Wide_Wide_String (Content), Syntax);
            Ada.Text_IO.Put_Line (Writer.To_String);
         end;
      else
         declare
            Writer   : aliased Wiki.Writers.Builders.Writer_Builder_Type;
            Renderer : aliased Wiki.Render.Text.Text_Renderer;
         begin
            Renderer.Set_Writer (Writer'Unchecked_Access);
            Html_Filter.Set_Document (Renderer'Unchecked_Access);
            Wiki.Parsers.Parse (Html_Filter'Unchecked_Access,
                                To_Wide_Wide_String (Content), Syntax);
            Ada.Text_IO.Put_Line (Writer.To_String);
         end;
      end if;
   end Parse;

   procedure Parse_Url (Url : in String) is
      Command : constant String := "wget -q -O - " & Url;
      Pipe    : aliased Util.Streams.Pipes.Pipe_Stream;
      Buffer  : Util.Streams.Buffered.Buffered_Stream;
      Content : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Pipe.Open (Command);
      Buffer.Initialize (null, Pipe'Unchecked_Access, 1024 * 1024);
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
           or Name (Name'First .. Name'First + 7) = "https://";
      end if;
   end Is_Url;

begin
   loop
      case Getopt ("m M d c t f:") is
         when 'm' =>
            Syntax := Wiki.Parsers.SYNTAX_MARKDOWN;
            Wiki_Mode := True;

         when 'M' =>
            Syntax := Wiki.Parsers.SYNTAX_MEDIA_WIKI;
            Wiki_Mode := True;

         when 'c' =>
            Syntax := Wiki.Parsers.SYNTAX_CREOLE;
            Wiki_Mode := True;

         when 'd' =>
            Syntax := Wiki.Parsers.SYNTAX_DOTCLEAR;
            Wiki_Mode := True;

         when 't' =>
            Html_Mode := False;

         when 'f' =>
            declare
               Value : constant String := Util.Strings.Transforms.To_Upper_Case (Parameter);
            begin
               Html_Filter.Hide (Wiki.Filters.Html.Html_Tag_Type'Value (Value & "_TAG"));
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
