-----------------------------------------------------------------------
--  wiki-render -- Wiki renderer
--  Copyright (C) 2015, 2016, 2019 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
