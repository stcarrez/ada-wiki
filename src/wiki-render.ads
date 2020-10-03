-----------------------------------------------------------------------
--  wiki-render -- Wiki renderer
--  Copyright (C) 2015, 2016, 2020 Stephane Carrez
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
with Wiki.Nodes;
with Wiki.Documents;

--  == Wiki Renderer {#wiki-render} ==
--  The `Wiki.Render` package represents the renderer that takes a wiki document
--  and render the result either in text, HTML or another format.
--
--  @include wiki-render-html.ads
--  @include wiki-render-links.ads
--  @include wiki-render-text.ads
--  @include wiki-render-wiki.ads
package Wiki.Render is

   pragma Preelaborate;

   --  ------------------------------
   --  Document renderer
   --  ------------------------------
   type Renderer is limited interface;
   type Renderer_Access is access all Renderer'Class;

   --  Render the node instance from the document.
   procedure Render (Engine : in out Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type) is abstract;

   --  Finish the rendering pass after all the wiki document nodes are rendered.
   procedure Finish (Engine : in out Renderer;
                     Doc    : in Wiki.Documents.Document) is null;

   --  Render the list of nodes from the document.
   procedure Render (Engine : in out Renderer'Class;
                     Doc    : in Wiki.Documents.Document;
                     List   : in Wiki.Nodes.Node_List_Access);

   --  Render the document.
   procedure Render (Engine : in out Renderer'Class;
                     Doc    : in Wiki.Documents.Document);

end Wiki.Render;
