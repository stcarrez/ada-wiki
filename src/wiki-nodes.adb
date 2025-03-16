-----------------------------------------------------------------------
--  wiki-nodes -- Wiki Document Internal representation
--  Copyright (C) 2016, 2019, 2023, 2024, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
   --  Find the closest row or header rom from a leaf node.
   --  ------------------------------
   function Find_Row (From : in Node_Type) return Node_Type_Access is
      Row : Node_Type_Access := From.Parent;
   begin
      while Row /= null and then not (Row.Kind in N_ROW | N_ROW_HEADER) loop
         Row := Row.Parent;
      end loop;
      return Row;
   end Find_Row;

   --  ------------------------------
   --  Find the closest table node from a leaf node.
   --  ------------------------------
   function Find_Table (From : in Node_Type) return Node_Type_Access is
      Table : Node_Type_Access := From.Parent;
   begin
      while Table /= null and then Table.Kind /= N_TABLE loop
         Table := Table.Parent;
      end loop;
      return Table;
   end Find_Table;

   --  ------------------------------
   --  Find the closest list node from a leaf node.
   --  ------------------------------
   function Find_List (From : in Node_Type) return Node_Type_Access is
      List : Node_Type_Access := From.Parent;
   begin
      while List /= null and then not (List.Kind in N_LIST_START | N_NUM_LIST_START) loop
         List := List.Parent;
      end loop;
      return List;
   end Find_List;

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
            if Block.List (I).Children /= null then
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

end Wiki.Nodes;
