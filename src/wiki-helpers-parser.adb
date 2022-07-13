-----------------------------------------------------------------------
--  wiki-helpers-parser -- Generic procedure for the wiki parser
--  Copyright (C) 2016, 2018, 2022 Stephane Carrez
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
with Wiki.Streams;
procedure Wiki.Helpers.Parser (Engine  : in out Engine_Type;
                               Content : in Element_Type;
                               Doc     : in out Wiki.Documents.Document) is

   type Wide_Input is new Wiki.Streams.Input_Stream with record
      Pos : Positive;
      Len : Natural;
   end record;

   --  Read the input stream and fill the `Into` buffer until either it is full or
   --  we reach the end of line.  Returns in `Last` the last valid position in the
   --  `Into` buffer.  When there is no character to read, return True in
   --  the `Eof` indicator.
   overriding
   procedure Read (Input : in out Wide_Input;
                   Into  : in out Wiki.Strings.WString;
                   Last  : out Natural;
                   Eof   : out Boolean);

   overriding
   procedure Read (Input : in out Wide_Input;
                   Into  : in out Wiki.Strings.WString;
                   Last  : out Natural;
                   Eof   : out Boolean) is
      Pos  : Natural := Into'First;
      Char : Wiki.Strings.WChar;
   begin
      Eof := False;
      while Pos <= Into'Last loop
         if Input.Pos <= Input.Len then
            Element (Content, Input.Pos, Char);
         else
            Eof := True;
            exit;
         end if;
         Into (Pos) := Char;
         Pos := Pos + 1;
         exit when Char = Helpers.LF;
         if Char = Helpers.CR then
            exit when Input.Pos > Input.Len;

            --  Look for a possible LF and drop it.
            declare
               Read_Pos : constant Natural := Input.Pos;
            begin
               Element (Content, Input.Pos, Char);
               if Char /= Helpers.LF then
                  Input.Pos := Read_Pos;
               end if;
               exit;
            end;
         end if;
      end loop;
      Last := Pos - 1;
   end Read;

   Buffer : aliased Wide_Input;
begin
   Buffer.Pos   := 1;
   Buffer.Len   := Length (Content);
   Parse (Engine, Buffer'Unchecked_Access, Doc);
end Wiki.Helpers.Parser;
