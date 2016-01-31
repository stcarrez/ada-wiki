-----------------------------------------------------------------------
--  wiki-nodes -- Wiki Document Internal representation
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

package body Wiki.Nodes is

   --  ------------------------------
   --  Create a text node.
   --  ------------------------------
   function Create_Text (Text : in WString) return Node_Type_Access is
   begin
      return new Node_Type '(Kind => N_TEXT,
                             Len  => Text'Length,
                             Text => Text,
                             others => <>);
   end Create_Text;

   --  ------------------------------
   --  Append a HTML tag start node to the document.
   --  ------------------------------
   procedure Add_Tag (Document   : in out Wiki.Nodes.Document;
                      Tag        : in Html_Tag_Type;
                      Attributes : in Wiki.Attributes.Attribute_List_Type) is
      Node : constant Node_Type_Access := new Node_Type '(Kind       => N_TAG_START,
                                                          Len        => 0,
                                                          Tag_Start  => Tag,
                                                          Attributes => Attributes,
                                                          Children   => null,
                                                          Parent     => Document.Current);
   begin
      Append (Document.Nodes, Node);
      Document.Current := Node;
   end Add_Tag;

   --  ------------------------------
   --  Append a node to the document.
   --  ------------------------------
   procedure Append (Into : in out Document;
                     Node : in Node_Type_Access) is
   begin
      if Into.Current = null then
         Append (Into.Nodes, Node);
      else
         if Into.Current.Children = null then
            Into.Current.Children := new Node_List;
         end if;
         Append (Into.Current.Children.all, Node);
      end if;
   end Append;

   --  ------------------------------
   --  Append a node to the node list.
   --  ------------------------------
   procedure Append (Into : in out Node_List;
                     Node : in Node_Type_Access) is
      Block : Node_List_Block_Access := Into.Current;
   begin
      if Block.Last = Block.Max then
         Block.Next := new Node_List_Block (Into.Length);
         Block := Block.Next;
         Into.Current := Block;
      end if;
      Block.Last := Block.Last + 1;
      Block.List (Block.Last) := Node;
      Into.Length := Into.Length + 1;
   end Append;

end Wiki.Nodes;
