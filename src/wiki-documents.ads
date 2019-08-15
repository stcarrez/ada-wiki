-----------------------------------------------------------------------
--  wiki-documents -- Wiki document
--  Copyright (C) 2011, 2015, 2016, 2018, 2019 Stephane Carrez
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

with Wiki.Strings;
with Wiki.Attributes;
with Wiki.Nodes.Lists;

--  === Documents ===
--  The <tt>Document</tt> type is used to hold a Wiki document that was parsed by the parser
--  with one of the supported syntax.  The <tt>Document</tt> holds two distinct parts:
--
--  * A main document body that represents the Wiki content that was parsed.
--  * A table of contents part that was built while Wiki sections are collected.
--
--  Most of the operations provided by the <tt>Wiki.Documents</tt> package are intended to
--  be used by the wiki parser and filters to build the document.  The document is made of
--  nodes whose knowledge is required by the renderer.
--
--  A document instance must be declared before parsing a text:
--
--     Doc    : Wiki.Documents.Document;
package Wiki.Documents is

   pragma Preelaborate;

   --  ------------------------------
   --  A Wiki Document
   --  ------------------------------
   type Document is tagged private;

   --  Append a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH.
   procedure Append (Into : in out Document;
                     Kind : in Wiki.Nodes.Simple_Node_Kind);

   --  Append a HTML tag start node to the document.
   procedure Push_Node (Into       : in out Document;
                        Tag        : in Html_Tag;
                        Attributes : in Wiki.Attributes.Attribute_List);

   --  Pop the HTML tag.
   procedure Pop_Node (From : in out Document;
                       Tag  : in Html_Tag);

   --  Returns True if the current node is the root document node.
   function Is_Root_Node (Doc : in Document) return Boolean;

   --  Append the text with the given format at end of the document.
   procedure Append (Into   : in out Document;
                     Text   : in Wiki.Strings.WString;
                     Format : in Format_Map);

   --  Append a section header at end of the document.
   procedure Append (Into   : in out Document;
                     Header : in Wiki.Strings.WString;
                     Level  : in Positive);

   --  Add a link.
   procedure Add_Link (Into       : in out Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add an image.
   procedure Add_Image (Into       : in out Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a quote.
   procedure Add_Quote (Into       : in out Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   procedure Add_List_Item (Into     : in out Document;
                            Level    : in Positive;
                            Ordered  : in Boolean);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Add_Blockquote (Into     : in out Document;
                             Level    : in Natural);

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Into     : in out Document;
                               Text     : in Wiki.Strings.WString;
                               Format   : in Wiki.Strings.WString);

   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   procedure Iterate (Doc     : in Document;
                      Process : not null access procedure (Node : in Wiki.Nodes.Node_Type));

   --  Returns True if the document is empty.
   function Is_Empty (Doc : in Document) return Boolean;

   --  Returns True if the document displays the table of contents by itself.
   function Is_Using_TOC (Doc : in Document) return Boolean;

   --  Returns True if the table of contents is visible and must be rendered.
   function Is_Visible_TOC (Doc : in Document) return Boolean;

   --  Hide the table of contents.
   procedure Hide_TOC (Doc : in out Document);

   --  Get the table of content node associated with the document.
   procedure Get_TOC (Doc : in out Document;
                      TOC : out Wiki.Nodes.Lists.Node_List_Ref);

   --  Get the table of content node associated with the document.
   function Get_TOC (Doc : in Document) return Wiki.Nodes.Lists.Node_List_Ref;

private

   --  Append a node to the document.
   procedure Append (Into : in out Document;
                     Node : in Wiki.Nodes.Node_Type_Access);

   type Document is tagged record
      Nodes       : Wiki.Nodes.Lists.Node_List_Ref;
      TOC         : Wiki.Nodes.Lists.Node_List_Ref;
      Current     : Wiki.Nodes.Node_Type_Access;
      Using_TOC   : Boolean := False;
      Visible_TOC : Boolean := True;
   end record;

end Wiki.Documents;
