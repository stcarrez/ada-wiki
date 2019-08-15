-----------------------------------------------------------------------
--  wiki-nodes-lists -- Wiki Document Internal representation
--  Copyright (C) 2016, 2019 Stephane Carrez
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
with Util.Refs;

package Wiki.Nodes.Lists is

   pragma Preelaborate;

   package Node_List_Refs is new Util.Refs.General_References (Node_List, Finalize);

   type Node_List_Ref is new Node_List_Refs.Ref with null record;

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

   subtype Node_List_Accessor is Node_List_Refs.Element_Accessor;

   procedure Iterate (List    : in Node_List_Accessor;
                      Process : not null access procedure (Node : in Node_Type));

end Wiki.Nodes.Lists;
