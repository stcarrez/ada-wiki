-----------------------------------------------------------------------
--  render -- Wiki rendering example
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

with Wiki.Utils;

procedure Render is

   use GNAT.Command_Line;
   use Ada.Strings.Unbounded;
   use Ada.Characters.Conversions;

   procedure Usage;

   procedure Usage is
   begin
      Ada.Text_IO.Put_Line ("Render a wiki text file into HTML (default) or text");
      Ada.Text_IO.Put_Line ("Usage: render [-t] [-m] [-M] [-d] [-c] {wiki-file}");
      Ada.Text_IO.Put_Line ("  -t      Render to text only");
      Ada.Text_IO.Put_Line ("  -m      Render a Markdown wiki content");
      Ada.Text_IO.Put_Line ("  -M      Render a Mediawiki wiki content");
      Ada.Text_IO.Put_Line ("  -d      Render a Dotclear wiki content");
      Ada.Text_IO.Put_Line ("  -c      Render a Creole wiki content");
   end Usage;

   Count     : Natural := 0;
   Html_Mode : Boolean := True;
   Syntax    : Wiki.Wiki_Syntax := Wiki.SYNTAX_MARKDOWN;
begin
   loop
      case Getopt ("m M d c t") is
         when 'm' =>
            Syntax := Wiki.SYNTAX_MARKDOWN;

         when 'M' =>
            Syntax := Wiki.SYNTAX_MEDIA_WIKI;

         when 'c' =>
            Syntax := Wiki.SYNTAX_CREOLE;

         when 'd' =>
            Syntax := Wiki.SYNTAX_DOTCLEAR;

         when 't' =>
            Html_Mode := False;

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
         if Html_Mode then
            Ada.Text_IO.Put_Line
              (Wiki.Utils.To_Html (To_Wide_Wide_String (To_String (Data)), Syntax));
         else
            Ada.Text_IO.Put_Line
              (Wiki.Utils.To_Text (To_Wide_Wide_String (To_String (Data)), Syntax));
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
