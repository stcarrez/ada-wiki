-----------------------------------------------------------------------
--  wiki-filters -- Wiki filters
--  Copyright (C) 2015, 2016, 2020, 2022 Stephane Carrez
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
with Ada.Finalization;

with Wiki.Attributes;
with Wiki.Documents;
with Wiki.Nodes;
with Wiki.Strings;

--  == Filters ==
--  The `Wiki.Filters` package provides a simple filter framework that allows to plug
--  specific filters when a wiki document is parsed and processed.  The `Filter_Type`
--  implements the operations that the `Wiki.Parsers` will use to populate the document.
--  A filter can do some operations while calls are made so that it can:
--
--  * Get the text content and filter it by looking at forbidden words in some dictionary,
--  * Ignore some formatting construct (for example to forbid the use of links),
--  * Verify and do some corrections on HTML content embedded in wiki text,
--  * Expand some plugins, specific links to complex content.
--
--  To implement a new filter, the `Filter_Type` type must be used as a base type
--  and some of the operations have to be overridden.  The default `Filter_Type` operations
--  just propagate the call to the attached wiki document instance (ie, a kind of pass
--  through filter).
--
--  @include wiki-filters-toc.ads
--  @include wiki-filters-html.ads
--  @include wiki-filters-collectors.ads
--  @include wiki-filters-autolink.ads
--  @include wiki-filters-variables.ads
package Wiki.Filters is

   pragma Preelaborate;

   --  ------------------------------
   --  Filter type
   --  ------------------------------
   type Filter_Type is limited new Ada.Finalization.Limited_Controlled with private;
   type Filter_Type_Access is access all Filter_Type'Class;

   --  Add a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH to the document.
   procedure Add_Node (Filter    : in out Filter_Type;
                       Document  : in out Wiki.Documents.Document;
                       Kind      : in Wiki.Nodes.Simple_Node_Kind);

   --  Add a text content with the given format to the document.
   procedure Add_Text (Filter    : in out Filter_Type;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map);

   --  Add a definition item at end of the document.
   procedure Add_Definition (Filter     : in out Filter_Type;
                             Document   : in out Wiki.Documents.Document;
                             Definition : in Wiki.Strings.WString);

   procedure Start_Block (Filter   : in out Filter_Type;
                          Document : in out Wiki.Documents.Document;
                          Kind     : in Wiki.Nodes.Node_Kind;
                          Level    : in Natural);

   procedure End_Block (Filter   : in out Filter_Type;
                        Document : in out Wiki.Documents.Document;
                        Kind     : in Wiki.Nodes.Node_Kind);

   --  Push a HTML node with the given tag to the document.
   procedure Push_Node (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Pop a HTML node with the given tag.
   procedure Pop_Node (Filter   : in out Filter_Type;
                       Document : in out Wiki.Documents.Document;
                       Tag      : in Wiki.Html_Tag);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Add_Blockquote (Filter   : in out Filter_Type;
                             Document : in out Wiki.Documents.Document;
                             Level    : in Natural);

   --  Add a list (<ul> or <ol>) starting at the given number.
   procedure Add_List (Filter   : in out Filter_Type;
                       Document : in out Wiki.Documents.Document;
                       Level    : in Natural;
                       Ordered  : in Boolean);

   --  Add a link.
   procedure Add_Link (Filter     : in out Filter_Type;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a link reference with the given label.
   procedure Add_Link_Ref (Filter     : in out Filter_Type;
                           Document   : in out Wiki.Documents.Document;
                           Label      : in Wiki.Strings.WString);

   --  Add an image.
   procedure Add_Image (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a quote.
   procedure Add_Quote (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Filter   : in out Filter_Type;
                               Document : in out Wiki.Documents.Document;
                               Text     : in Wiki.Strings.WString;
                               Format   : in Wiki.Strings.WString);

   --  Add a new row to the current table.
   procedure Add_Row (Filter   : in out Filter_Type;
                      Document : in out Wiki.Documents.Document);

   --  Add a column to the current table row.  The column is configured with the
   --  given attributes.  The column content is provided through calls to Append.
   procedure Add_Column (Filter     : in out Filter_Type;
                         Document   : in out Wiki.Documents.Document;
                         Attributes : in out Wiki.Attributes.Attribute_List);

   --  Finish the creation of the table.
   procedure Finish_Table (Filter   : in out Filter_Type;
                           Document : in out Wiki.Documents.Document);

   --  Finish the document after complete wiki text has been parsed.
   procedure Finish (Filter   : in out Filter_Type;
                     Document : in out Wiki.Documents.Document);

   type Filter_Chain is new Filter_Type with private;

   --  Add the filter at beginning of the filter chain.
   procedure Add_Filter (Chain  : in out Filter_Chain;
                         Filter : in Filter_Type_Access);

   --  Internal operation to copy the filter chain.
   procedure Set_Chain (Chain : in out Filter_Chain;
                        From  : in Filter_Chain'Class);

private

   type Filter_Type is limited new Ada.Finalization.Limited_Controlled with record
      Next     : Filter_Type_Access;
   end record;

   type Filter_Chain is new Filter_Type with null record;

end Wiki.Filters;
