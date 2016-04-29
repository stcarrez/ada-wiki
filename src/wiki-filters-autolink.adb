-----------------------------------------------------------------------
--  wiki-filters-autolink -- Autolink filter to identify links in wiki
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

package body Wiki.Filters.Autolink is

   --  ------------------------------
   --  Find the position of the end of the link.
   --  Returns 0 if the content is not a link.
   --  ------------------------------
   function Find_End_Link (Filter : in Autolink_Filter;
                           Content : in Wiki.Strings.WString) return Natural is
      pragma Unreferenced (Filter);
   begin
      if Content'Length < 7 then
         return 0;
      elsif not Wiki.Helpers.Is_Url (Content) then
         return 0;
      else
         for Pos in Content'First + 4 .. Content'Last loop
            if Wiki.Helpers.Is_Space_Or_Newline (Content (Pos)) then
               return Pos - 1;
            end if;
         end loop;
         return Content'Last;
      end if;
   end Find_End_Link;

   --  ------------------------------
   --  Add a text content with the given format to the document.  Identify URLs in the text
   --  and transform them into links.  For each link, call the Add_Link operation.  The operation
   --  recognizes http:// https:// ftp:// ftps://
   --  ------------------------------
   overriding
   procedure Add_Text (Filter    : in out Autolink_Filter;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map) is
      Pos   : Natural := Text'First;
      Start : Natural := Text'First;
      Last  : Natural;
      C     : Wiki.Strings.WChar;
      Check : Boolean := True;
   begin
      while Pos <= Text'Last loop
         C := Text (Pos);
         if Wiki.Helpers.Is_Space (C) then
            Check := True;
            Pos := Pos + 1;
         elsif Check then
            Last := Autolink_Filter'Class (Filter).Find_End_Link (Text (Pos .. Text'Last));
            if Last > 0 then
               if Start /= Pos then
                  Filter_Type (Filter).Add_Text (Document, Text (Start .. Pos - 1), Format);
               end if;
               declare
                  Attr : Wiki.Attributes.Attribute_List;
               begin
                  Wiki.Attributes.Append (Attr, String '("href"), Text (Pos .. Last));
                  Autolink_Filter'Class (Filter).Add_Link (Document, Text (Pos .. Last),
                                                           Attr);
               end;
               Start := Last + 1;
               Pos := Start;
            else
               Check := False;
               Pos := Pos + 1;
            end if;
         else
            Pos := Pos + 1;
         end if;
      end loop;
      if Start <= Text'Last then
         Filter_Type (Filter).Add_Text (Document, Text (Start .. Text'Last), Format);
      end if;
   end Add_Text;

end Wiki.Filters.Autolink;
