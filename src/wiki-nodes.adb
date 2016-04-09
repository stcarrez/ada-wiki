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
with Ada.Unchecked_Deallocation;
package body Wiki.Nodes is

   --  ------------------------------
   --  Append a node to the tag node.
   --  ------------------------------
   procedure Append (Into : in Node_Type_Access;
                     Node : in Node_Type_Access) is
   begin
      if Into.Children = null then
         Into.Children := new Node_List;
         Into.Children.Current := Into.Children.First'Access;
      end if;
      Append (Into.Children.all, Node);
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
   overriding
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
            if Block.List (I).Kind = N_TAG_START and then Block.List (I).Children /= null then
               Finalize (Block.List (I).Children.all);
               Free (Block.List (I).Children);
            end if;
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

   --  ------------------------------
   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   --  ------------------------------
   procedure Iterate (List    : in Node_List_Access;
                      Process : not null access procedure (Node : in Node_Type)) is
      Block : Node_List_Block_Access := List.First'Access;
   begin
      loop
         for I in 1 .. Block.Last loop
            Process (Block.List (I).all);
         end loop;
         Block := Block.Next;
         exit when Block = null;
      end loop;
   end Iterate;

   --  ------------------------------
   --  Append a node to the node list.
   --  ------------------------------
   procedure Append (Into : in out Node_List_Ref;
                     Node : in Node_Type_Access) is
   begin
      if Into.Is_Null then
         Node_List_Refs.Ref (Into) := Node_List_Refs.Create;
         Into.Value.Current := Into.Value.First'Access;
      end if;
      Append (Into.Value.all, Node);
   end Append;

   --  ------------------------------
   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   --  ------------------------------
   procedure Iterate (List    : in Node_List_Ref;
                      Process : not null access procedure (Node : in Node_Type)) is
   begin
      if not List.Is_Null then
         Iterate (List.Value, Process);
      end if;
   end Iterate;

   --  ------------------------------
   --  Returns True if the list reference is empty.
   --  ------------------------------
   function Is_Empty (List : in Node_List_Ref) return Boolean is
   begin
      return List.Is_Null;
   end Is_Empty;

   --  ------------------------------
   --  Get the number of nodes in the list.
   --  ------------------------------
   function Length (List : in Node_List_Ref) return Natural is
   begin
      if List.Is_Null then
         return 0;
      else
         return List.Value.Length;
      end if;
   end Length;

end Wiki.Nodes;
