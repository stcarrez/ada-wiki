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
with Util.Refs;
package Wiki.Nodes is

   pragma Preelaborate;

   type Node_Kind is (N_LINE_BREAK,
                      N_HORIZONTAL_RULE,
                      N_TOC_DISPLAY,
                      N_PARAGRAPH,
                      N_HEADER,
                      N_TOC,
                      N_TOC_ENTRY,
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
         when N_HEADER | N_BLOCKQUOTE | N_INDENT | N_LIST | N_NUM_LIST | N_TOC_ENTRY =>
            Level  : Natural := 0;
            Header : Wiki.Strings.WString (1 .. Len);

         when N_TEXT =>
            Format : Format_Map;
            Text   : Wiki.Strings.WString (1 .. Len);

         when N_LINK | N_IMAGE | N_QUOTE =>
            Link_Attr  : Wiki.Attributes.Attribute_List;
            Title      : Wiki.Strings.WString (1 .. Len);

         when N_TAG_START =>
            Tag_Start  : Html_Tag;
            Attributes : Wiki.Attributes.Attribute_List;
            Children   : Node_List_Access;
            Parent     : Node_Type_Access;

         when N_PREFORMAT =>
            Preformatted : Wiki.Strings.WString (1 .. Len);

         when N_TOC =>
            Entries    : Node_List_Access;

         when others =>
            null;

      end case;
   end record;

   --  Append a node to the tag node.
   procedure Append (Into : in Node_Type_Access;
                     Node : in Node_Type_Access);

   --  Append a node to the document.
   procedure Append (Into : in out Node_List;
                     Node : in Node_Type_Access);

   --  Append a node to the node list.
   procedure Append (Into : in out Node_List_Ref;
                     Node : in Node_Type_Access);

   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   procedure Iterate (List    : in Node_List_Access;
                      Process : not null access procedure (Node : in Node_Type));

   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   procedure Iterate (List    : in Node_List_Ref;
                      Process : not null access procedure (Node : in Node_Type));

   --  Returns True if the list reference is empty.
   function Is_Empty (List : in Node_List_Ref) return Boolean;

   --  Get the number of nodes in the list.
   function Length (List : in Node_List_Ref) return Natural;

private

   NODE_LIST_BLOCK_SIZE : constant Positive := 16;

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

end Wiki.Nodes;
