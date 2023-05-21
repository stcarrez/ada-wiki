-----------------------------------------------------------------------
--  wiki-nodes -- Wiki Document Internal representation
--  Copyright (C) 2016, 2019, 2023 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
package body Wiki.Nodes is

   --  ------------------------------
   --  Append a node to the tag node.
   --  ------------------------------
   procedure Append (Into : in Node_Type_Access;
                     Node : in Node_Type_Access) is
   begin
      if Into.Kind in N_HEADER .. N_DEFINITION then
         if Into.Content = null then
            Into.Content := new Node_List;
            Into.Content.Current := Into.Content.First'Access;
         end if;
         Append (Into.Content.all, Node);
      else
         if Into.Children = null then
            Into.Children := new Node_List;
            Into.Children.Current := Into.Children.First'Access;
         end if;
         Append (Into.Children.all, Node);
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
         Block.Next := new Node_List_Block (Block.Max * 2);
         Block := Block.Next;
         Into.Current := Block;
      end if;
      Block.Last := Block.Last + 1;
      Block.List (Block.Last) := Node;
      Into.Length := Into.Length + 1;
   end Append;

   --  ------------------------------
   --  Finalize the node list to release the allocated memory.
   --  ------------------------------
   procedure Finalize (List : in out Node_List) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Node_List_Block, Node_List_Block_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation (Node_Type, Node_Type_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation (Node_List, Node_List_Access);

      procedure Release (List : in Node_List_Block_Access);
      procedure Free_Block (Block : in out Node_List_Block);

      procedure Free_Block (Block : in out Node_List_Block) is
      begin
         for I in 1 .. Block.Last loop
            case Block.List (I).Kind is
               when N_TAG_START | N_TABLE | N_ROW | N_COLUMN =>
                  if Block.List (I).Children /= null then
                     Finalize (Block.List (I).Children.all);
                     Free (Block.List (I).Children);
                  end if;

               when N_HEADER | N_BLOCKQUOTE | N_INDENT
                  | N_NUM_LIST_START | N_LIST_START | N_DEFINITION =>
                  if Block.List (I).Content /= null then
                     Finalize (Block.List (I).Content.all);
                     Free (Block.List (I).Content);
                  end if;

               when others =>
                  null;
            end case;
            Free (Block.List (I));
         end loop;
      end Free_Block;

      procedure Release (List : in Node_List_Block_Access) is
         Next  : Node_List_Block_Access := List;
         Block : Node_List_Block_Access;
      begin
         while Next /= null loop
            Block := Next;
            Free_Block (Block.all);
            Next := Next.Next;
            Free (Block);
         end loop;
      end Release;

   begin
      Release (List.First.Next);
      Free_Block (List.First);
   end Finalize;

end Wiki.Nodes;
