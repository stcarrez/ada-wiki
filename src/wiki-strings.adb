-----------------------------------------------------------------------
--  wiki-strings -- Wiki string types and operations
--  Copyright (C) 2016, 2017, 2020 Stephane Carrez
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

package body Wiki.Strings is

   --  Search for the first occurrence of the character in the builder and
   --  starting after the from index.  Returns the index of the first occurence or 0.
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

end Wiki.Strings;
