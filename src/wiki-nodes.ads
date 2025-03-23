-----------------------------------------------------------------------
--  wiki-nodes -- Wiki Document Internal representation
--  Copyright (C) 2016 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Attributes;
with Wiki.Strings;
package Wiki.Nodes is

   pragma Preelaborate;

   type Node_Kind is (N_NONE,
                      N_LINE_BREAK,
                      N_HORIZONTAL_RULE,
                      N_TOC_DISPLAY,
                      N_PARAGRAPH,
                      N_END_DEFINITION,

                      N_TOC_ENTRY,
                      N_LIST_ITEM,

                      --  Nodes with level and content.
                      N_HEADER,
                      N_BLOCKQUOTE,
                      N_INDENT,
                      N_NUM_LIST_START,
                      N_LIST_START,
                      N_DEFINITION_TERM,
                      N_DEFINITION,

                      --  Nodes with children and attributes.
                      N_TAG_START,
                      N_TABLE,
                      N_ROW_HEADER,
                      N_ROW,
                      N_ROW_FOOTER,
                      N_COLUMN,

                      N_TOC,
                      N_QUOTE,
                      N_PREFORMAT,
                      N_TEXT,
                      N_LINK,
                      N_LINK_REF,
                      N_IMAGE_REF,
                      N_IMAGE);

   --  Node kinds which are simple markers in the document.
   subtype Simple_Node_Kind is Node_Kind range N_NONE .. N_END_DEFINITION;

   subtype Row_Kind is Node_Kind range N_ROW_HEADER .. N_ROW_FOOTER;

   type Align_Style is (ALIGN_DEFAULT, ALIGN_LEFT, ALIGN_RIGHT, ALIGN_CENTER);

   type Column_Style is record
      Format : Align_Style := ALIGN_DEFAULT;
      Width  : Natural := 0;
   end record;

   type Column_Array_Style is array (Positive range <>) of Column_Style;

   type Node_List is limited private;
   type Node_List_Access is access all Node_List;

   type Node_Type;
   type Node_Type_Access is access all Node_Type;

   type Node_Type (Kind : Node_Kind; Len : Natural) is limited record
      Parent     : Node_Type_Access;
      Children   : Node_List_Access;
      case Kind is
         when N_HEADER | N_BLOCKQUOTE | N_INDENT
            | N_NUM_LIST_START | N_LIST_START | N_DEFINITION | N_DEFINITION_TERM =>
            Level  : Natural := 0;
            Loose  : Boolean := False;

         when N_TEXT =>
            Format : Format_Map;
            Text   : Wiki.Strings.WString (1 .. Len);

         when N_LINK | N_IMAGE | N_QUOTE | N_LINK_REF | N_IMAGE_REF =>
            Link_Attr  : Wiki.Attributes.Attribute_List;
            Title      : Wiki.Strings.WString (1 .. Len);

         when N_TAG_START | N_ROW_HEADER | N_ROW | N_ROW_FOOTER | N_COLUMN =>
            Tag_Start  : Html_Tag;
            Attributes : Wiki.Attributes.Attribute_List;

         when N_PREFORMAT =>
            Language     : Wiki.Strings.UString;
            Preformatted : Wiki.Strings.WString (1 .. Len);

         when N_TOC_ENTRY =>
            Header    : Wiki.Strings.WString (1 .. Len);
            Toc_Level : Natural := 0;

         when N_TABLE =>
            Columns   : Column_Array_Style (1 .. Len);

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

   --  Finalize the node list to release the allocated memory.
   procedure Finalize (List : in out Node_List);

   --  Find the closest row or header rom from a leaf node.
   function Find_Row (From : in Node_Type) return Node_Type_Access;

   --  Find the closest table node from a leaf node.
   function Find_Table (From : in Node_Type) return Node_Type_Access;

   --  Find the closest list node from a leaf node.
   function Find_List (From : in Node_Type) return Node_Type_Access;

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

   type Node_List is limited record
      Current : Node_List_Block_Access;
      Length  : Natural := 0;
      First   : aliased Node_List_Block (NODE_LIST_BLOCK_SIZE);
   end record;

end Wiki.Nodes;
