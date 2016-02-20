-----------------------------------------------------------------------
--  wiki -- Ada Wiki Engine
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
with Wiki.Attributes;
with Wiki.Strings;
with Ada.Finalization;
with Util.Refs;
package Wiki.Nodes is

   pragma Preelaborate;

   subtype WString is Wide_Wide_String;

   type Node_Kind is (N_LINE_BREAK,
                      N_HORIZONTAL_RULE,
                      N_PARAGRAPH,
                      N_HEADER,
                      N_BLOCKQUOTE,
                      N_QUOTE,
                      N_TAG_START,
                      N_PREFORMAT,
                      N_LIST,
                      N_NUM_LIST,
                      N_INDENT,
                      N_TEXT,
                      N_LINK,
                      N_IMAGE);

   --  Node kinds which are simple markers in the document.
   subtype Simple_Node_Kind is Node_Kind range N_LINE_BREAK .. N_PARAGRAPH;

   type Node_List_Ref is private;

   type Node_List is limited private;
   type Node_List_Access is access all Node_List;

   type Node_Type;
   type Node_Type_Access is access all Node_Type;

   type Node_Type (Kind : Node_Kind; Len : Natural) is limited record
      case Kind is
         when N_HEADER | N_BLOCKQUOTE | N_INDENT | N_LIST | N_NUM_LIST =>
            Level  : Natural := 0;
            Header : WString (1 .. Len);

         when N_TEXT =>
            Format : Format_Map;
            Text   : WString (1 .. Len);

         when N_LINK | N_IMAGE | N_QUOTE =>
            Link_Attr  : Wiki.Attributes.Attribute_List;
            Title      : WString (1 .. Len);

         when N_TAG_START =>
            Tag_Start  : Html_Tag;
            Attributes : Wiki.Attributes.Attribute_List;
            Children   : Node_List_Access;
            Parent     : Node_Type_Access;

         when N_PREFORMAT =>
            Preformatted : WString (1 .. Len);

         when others =>
            null;

      end case;
   end record;

   type Document is tagged private;

   --  Append a node to the document.
   procedure Append (Into : in out Node_List;
                     Node : in Node_Type_Access);

   --  Append a node to the node list.
   procedure Append (Into : in out Node_List_Ref;
                     Node : in Node_Type_Access);

   --  Append a node to the document.
   procedure Append (Into : in out Document;
                     Node : in Node_Type_Access);

   --  Append a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH.
   procedure Append (Into : in out Document;
                     Kind : in Simple_Node_Kind);

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
   procedure Iterate (List    : in Node_List_Access;
                      Process : not null access procedure (Node : in Node_Type));

   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   procedure Iterate (List    : in Node_List_Ref;
                      Process : not null access procedure (Node : in Node_Type));

   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   procedure Iterate (Doc     : in Document;
                      Process : not null access procedure (Node : in Node_Type));

private

   NODE_LIST_BLOCK_SIZE : constant Positive := 20;

   type Node_Array is array (Positive range <>) of Node_Type_Access;

   type Node_List_Block;
   type Node_List_Block_Access is access all Node_List_Block;

   type Node_List_Block (Max : Positive) is limited record
      Next  : Node_List_Block_Access;
      Last  : Natural := 0;
      List  : Node_Array (1 .. Max);
   end record;

   type Node_List is limited new Util.Refs.Ref_Entity with record
      Current : Node_List_Block_Access;
      Length  : Natural := 0;
      First   : aliased Node_List_Block (NODE_LIST_BLOCK_SIZE);
   end record;

   --  Finalize the node list to release the allocated memory.
   overriding
   procedure Finalize (List : in out Node_List);

   --  Append a node to the node list.
--   procedure Append (Into : in out Node_List;
--                     Node : in Node_Type_Access);

   package Node_List_Refs is new Util.Refs.References (Node_List, Node_List_Access);

   type Node_List_Ref is new Node_List_Refs.Ref with null record;

   type Document is new Ada.Finalization.Controlled with record
      Nodes   : Node_List_Ref;
      Current : Node_Type_Access;
   end record;

   overriding
   procedure Initialize (Doc : in out Document);

end Wiki.Nodes;
