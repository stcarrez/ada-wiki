-----------------------------------------------------------------------
--  wiki-strings -- Wiki string types and operations
--  Copyright (C) 2016, 2017, 2020, 2022 Stephane Carrez
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
package body Wiki.Strings is

   --  ------------------------------
   --  Search for the first occurrence of the character in the builder and
   --  starting after the from index.  Returns the index of the first occurence or 0.
   --  ------------------------------
   function Index (Source : in BString;
                   Char   : in WChar;
                   From   : in Positive := 1) return Natural is
      function Find (Content : in WString) return Natural;

      function Find (Content : in WString) return Natural is
      begin
         for I in Content'Range loop
            if Content (I) = Char then
               return I;
            end if;
         end loop;
         return 0;
      end Find;

      function Find_Builder is
         new Wide_Wide_Builders.Find (Find);

   begin
      return Find_Builder (Source, From);
   end Index;

   --  ------------------------------
   --  Find the last position of the character in the string and starting
   --  at the given position.  Stop at the first character different than `Char`.
   --  ------------------------------
   function Last_Position (Source : in BString;
                           Char   : in WChar;
                           From   : in Positive := 1) return Natural is

      function Find (Content : in WString) return Natural;

      function Find (Content : in WString) return Natural is
      begin
         for I in Content'Range loop
            if Content (I) /= Char then
               return I;
            end if;
         end loop;
         return 0;
      end Find;

      function Find_Builder is
         new Wide_Wide_Builders.Find (Find);

   begin
      return Find_Builder (Source, From);
   end Last_Position;

   --  ------------------------------
   --  Count the the number of consecutive occurence of the given character
   --  and starting at the given position.
   --  ------------------------------
   function Count_Occurence (Source : in BString;
                             Char   : in WChar;
                             From   : in Positive := 1) return Natural is
      Pos : constant Natural := Last_Position (Source, Char, From);
   begin
      if Pos > From then
         return Pos - From;
      else
         return 0;
      end if;
   end Count_Occurence;

   function Count_Occurence (Source : in WString;
                             Char   : in WChar;
                             From   : in Positive := 1) return Natural is
      Pos : Positive := From;
   begin
      while Pos <= Source'Last and then Source (Pos) = Char loop
         Pos := Pos + 1;
      end loop;
      return Pos - From;
   end Count_Occurence;

   function Skip_Spaces (Source : in BString;
                         From   : in Positive;
                         Last   : in Positive) return Positive is
      Pos : Positive := From;
   begin
      while Pos <= Last and then Helpers.Is_Space_Or_Newline (Element (Source, Pos)) loop
         Pos := Pos + 1;
      end loop;
      return Pos;
   end Skip_Spaces;

   procedure Scan_Line_Fragment (Source : in BString;
                                 Process : not null
                                    access procedure (Text   : in WString;
                                                      Offset : in Natural)) is
      procedure Parse_Line_Fragment (Content : in Wiki.Strings.WString);

      Offset : Natural := 0;

      procedure Parse_Line_Fragment (Content : in Wiki.Strings.WString) is
      begin
         Process (Content, Offset);
         Offset := Offset + Content'Length;
      end Parse_Line_Fragment;

      procedure Parse_Line_Fragment is
         new Wiki.Strings.Wide_Wide_Builders.Get (Parse_Line_Fragment);
      pragma Inline (Parse_Line_Fragment);

   begin
      Parse_Line_Fragment (Source);
   end Scan_Line_Fragment;

end Wiki.Strings;
