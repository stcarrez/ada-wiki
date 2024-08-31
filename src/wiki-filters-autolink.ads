-----------------------------------------------------------------------
--  wiki-filters-autolink -- Autolink filter to identify links in wiki
--  Copyright (C) 2016, 2020, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

--  === Autolink Filters ===
--  The `Wiki.Filters.Autolink` package defines a filter that transforms URLs
--  in the Wiki text into links.  The filter should be inserted in the filter chain
--  after the HTML and after the collector filters.  The filter looks for the
--  text and transforms `http://`, `https://`, `ftp://` and `ftps://` links into real links.
--  When such links are found, the text is split so that next filters see only the text without
--  links and the `Add_Link` filter operations are called with the link.
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
