-----------------------------------------------------------------------
--  wiki-nodes-lists -- Wiki Document Internal representation
--  Copyright (C) 2016, 2019, 2021 Stephane Carrez
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
package body Wiki.Nodes.Lists is

   --  ------------------------------
   --  Append a node to the node list.
   --  ------------------------------
   procedure Append (Into : in out Node_List_Ref;
                     Node : in Node_Type_Access) is
   begin
      if Into.Is_Null then
         Node_List_Refs.Ref (Into) := Node_List_Refs.Create;
         Into.Value.Current := Into.Value.First'Unchecked_Access;
      end if;
      Append (Into.Value, Node);
   end Append;

   --  ------------------------------
   --  Iterate over the nodes of the list and call the <tt>Process</tt> procedure with
   --  each node instance.
   --  ------------------------------
   procedure Iterate (List    : in Node_List_Accessor;
                      Process : not null access procedure (Node : in Node_Type)) is
      Block : access Node_List_Block := List.First'Access;
   begin
      loop
         for I in 1 .. Block.Last loop
            Process (Block.List (I).all);
         end loop;
         Block := Block.Next;
         exit when Block = null;
      end loop;
   end Iterate;

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

end Wiki.Nodes.Lists;
