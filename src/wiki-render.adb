-----------------------------------------------------------------------
--  wiki-render -- Wiki renderer
--  Copyright (C) 2015, 2016, 2019 Stephane Carrez
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
with Wiki.Nodes.Lists;
package body Wiki.Render is

   --  ------------------------------
   --  Render the list of nodes from the document.
   --  ------------------------------
   procedure Render (Engine : in out Renderer'Class;
                     Doc    : in Wiki.Documents.Document;
                     List   : in Wiki.Nodes.Node_List_Access) is
      use type Wiki.Nodes.Node_List_Access;

      procedure Process (Node : in Wiki.Nodes.Node_Type);

      procedure Process (Node : in Wiki.Nodes.Node_Type) is
      begin
         Engine.Render (Doc, Node);
      end Process;

   begin
      if List /= null then
         Wiki.Nodes.Lists.Iterate (List, Process'Access);
      end if;
   end Render;

   --  ------------------------------
   --  Render the document.
   --  ------------------------------
   procedure Render (Engine : in out Renderer'Class;
                     Doc    : in Wiki.Documents.Document) is
      procedure Process (Node : in Wiki.Nodes.Node_Type);

      procedure Process (Node : in Wiki.Nodes.Node_Type) is
      begin
         Engine.Render (Doc, Node);
      end Process;
   begin
      Doc.Iterate (Process'Access);
      Engine.Finish (Doc);
   end Render;

end Wiki.Render;
