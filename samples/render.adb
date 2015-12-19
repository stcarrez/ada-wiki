-----------------------------------------------------------------------
--  render -- XHTML Rendering example
--  Copyright (C) 2010 Stephane Carrez
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
with Ada.Strings.Fixed;
with Ada.Characters.Conversions;

with GNAT.Command_Line;

with Util.Files;

with Wiki.Utils;
with Wiki.Parsers;

--  This example reads an XHTML file and renders the result.
--  render -html -dotclear -markdown infile
--  render -text
procedure Render is

   use GNAT.Command_Line;
   use Ada.Strings.Fixed;
   use Ada.Strings.Unbounded;
   use Ada.Characters.Conversions;

   Count     : Natural := 0;
   Html_Mode : Boolean := True;
   Syntax    : Wiki.Parsers.Wiki_Syntax_Type := Wiki.Parsers.SYNTAX_MARKDOWN;
begin
   loop
      case Getopt ("t f:") is
         when 't' =>
            Html_Mode := False;

         when 'f' =>
            declare
               Value : constant String := Parameter;
               Pos   : constant Natural := Index (Value, "=");
            begin
               Ada.Text_IO.Put_Line (Value);
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
               Ada.Text_IO.Put_Line ("Usage: render [-DNAME=VALUE ] file");
               Ada.Text_IO.Put_Line ("Example: render -DcontextPath=/test samples/web/ajax.xhtml");
            end if;
            return;
         end if;
         Count := Count + 1;
         Util.Files.Read_File (Name, Data);
         if Html_Mode then
            Ada.Text_IO.Put_Line (Wiki.Utils.To_Html (To_Wide_Wide_String (To_String (Data)), Syntax));
         else
            Ada.Text_IO.Put_Line (Wiki.Utils.To_Text (To_Wide_Wide_String (To_String (Data)), Syntax));
         end if;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line ("Cannot read file '" & Name & "'");

      end;
   end loop;
end Render;
