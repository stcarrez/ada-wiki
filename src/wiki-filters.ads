-----------------------------------------------------------------------
--  wiki-filters -- Wiki filters
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
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Finalization;

with Wiki.Attributes;
with Wiki.Documents;
with Wiki.Nodes;
with Wiki.Strings;

--  == Filters ==
--  The <b>Wiki.Filters</b> package provides a simple filter framework that allows to plug
--  specific filters when a wiki document is parsed and processed.  The <tt>Filter_Type</tt>
--  implements the <tt>Document_Reader</tt> interface to catch all the wiki document operations
--  and it forwards the different calls to a next wiki document instance.  A filter can do some
--  operations while calls are made so that it can:
--
--  * Get the text content and filter it by looking at forbidden words in some dictionary,
--  * Ignore some formatting construct (for example to forbid the use of links),
--  * Verify and do some corrections on HTML content embedded in wiki text,
--  * Expand some plugins, specific links to complex content.
--
--  To implement a new filter, the <tt>Filter_Type</tt> type must be used as a base type
--  and some of the operations have to be overriden.  The default <tt>Filter_Type</tt> operations
--  just propagate the call to the attached wiki document instance (ie, a kind of pass
--  through filter).
--
package Wiki.Filters is

   pragma Preelaborate;

   use Ada.Strings.Wide_Wide_Unbounded;

   --  ------------------------------
   --  Filter type
   --  ------------------------------
   type Filter_Type is new Ada.Finalization.Limited_Controlled with private;
   type Filter_Type_Access is access all Filter_Type'Class;

   --  Add a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH to the document.
   procedure Add_Node (Filter    : in out Filter_Type;
                       Document  : in out Wiki.Nodes.Document;
                       Kind      : in Wiki.Nodes.Simple_Node_Kind);

   --  Add a text content with the given format to the document.
   procedure Add_Text (Filter    : in out Filter_Type;
                       Document  : in out Wiki.Nodes.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Nodes.Format_Map);

   --  Add a section header with the given level in the document.
   procedure Add_Header (Filter    : in out Filter_Type;
                         Document  : in out Wiki.Nodes.Document;
                         Header    : in Wiki.Strings.WString;
                         Level     : in Natural);

   --  Push a HTML node with the given tag to the document.
   procedure Push_Node (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Nodes.Document;
                        Tag        : in Wiki.Nodes.Html_Tag_Type;
                        Attributes : in out Wiki.Attributes.Attribute_List_Type);

   --  Pop a HTML node with the given tag.
   procedure Pop_Node (Filter   : in out Filter_Type;
                       Document : in out Wiki.Nodes.Document;
                       Tag      : in Wiki.Nodes.Html_Tag_Type);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Add_Blockquote (Filter   : in out Filter_Type;
                             Document : in out Wiki.Nodes.Document;
                             Level    : in Natural);

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   procedure Add_List_Item (Filter   : in out Filter_Type;
                            Document : in out Wiki.Nodes.Document;
                            Level    : in Positive;
                            Ordered  : in Boolean);

   --  Add a link.
   procedure Add_Link (Filter     : in out Filter_Type;
                       Document   : in out Wiki.Nodes.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List_Type);

   --  Add an image.
   procedure Add_Image (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Nodes.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List_Type);

   --  Add a quote.
   procedure Add_Quote (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Nodes.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List_Type);

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Filter   : in out Filter_Type;
                               Document : in out Wiki.Nodes.Document;
                               Text     : in Wiki.Strings.WString;
                               Format   : in Unbounded_Wide_Wide_String);

   --  Finish the document after complete wiki text has been parsed.
   procedure Finish (Filter : in out Filter_Type);

private

   type Filter_Type is new Ada.Finalization.Limited_Controlled with record
      Next     : Filter_Type_Access;
   end record;

end Wiki.Filters;
