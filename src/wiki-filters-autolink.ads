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

--  === Autolink Filters ===
--  The <tt>Wiki.Filters.Autolink</tt> package defines a filter the transforms URLs
--  in the Wiki text into links.
package Wiki.Filters.Autolink is

   pragma Preelaborate;

   type Autolink_Filter is new Filter_Type with null record;
   type Autolink_Filter_Access is access all Autolink_Filter'Class;

   --  Find the position of the end of the link.
   --  Returns 0 if the content is not a link.
   function Find_End_Link (Filter  : in Autolink_Filter;
                           Content : in Wiki.Strings.WString) return Natural;

   --  Add a text content with the given format to the document.  Identify URLs in the text
   --  and transform them into links.  For each link, call the Add_Link operation.  The operation
   --  recognizes http:// https:// ftp:// ftps://
   overriding
   procedure Add_Text (Filter    : in out Autolink_Filter;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map);


end Wiki.Filters.Autolink;
