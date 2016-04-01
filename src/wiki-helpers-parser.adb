-----------------------------------------------------------------------
--  wiki-helpers-parser -- Generic procedure for the wiki parser
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
with Wiki.Helpers;
with Wiki.Streams;
procedure Wiki.Helpers.Parser (Engine  : in out Engine_Type;
                               Content : in Element_Type;
                               Doc     : in out Wiki.Documents.Document) is

   use type Wiki.Streams.Input_Stream_Access;

   type Wide_Input is new Wiki.Streams.Input_Stream with record
      Pos : Positive;
      Len : Natural;
   end record;

   overriding
   procedure Read (Buf    : in out Wide_Input;
                   Token  : out Wiki.Strings.WChar;
                   Is_Eof : out Boolean);

   procedure Read (Buf    : in out Wide_Input;
                   Token  : out Wiki.Strings.WChar;
                   Is_Eof : out Boolean) is
   begin
      if Buf.Pos > Buf.Len then
         Is_Eof := True;
         Token := Wiki.Helpers.CR;
      else
         Token := Element (Content, Buf.Pos);
         Buf.Pos := Buf.Pos + 1;
         Is_Eof := False;
      end if;
   end Read;

   Buffer : aliased Wide_Input;
begin
   Buffer.Pos   := 1;
   Buffer.Len   := Length (Content);
   Parse (Engine, Buffer'Unchecked_Access, Doc);
end Wiki.Helpers.Parser;


