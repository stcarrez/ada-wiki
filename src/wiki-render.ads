-----------------------------------------------------------------------
--  wiki-render -- Wiki renderer
--  Copyright (C) 2015 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Unbounded;
with Wiki.Documents;
with Wiki.Attributes;
with Wiki.Nodes;
package Wiki.Render is

   pragma Preelaborate;

   use Ada.Strings.Wide_Wide_Unbounded;

   type Link_Renderer is limited interface;
   type Link_Renderer_Access is access all Link_Renderer'Class;

   --  Get the image link that must be rendered from the wiki image link.
   procedure Make_Image_Link (Renderer : in Link_Renderer;
                              Link     : in Unbounded_Wide_Wide_String;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : out Natural;
                              Height   : out Natural) is abstract;

   --  Get the page link that must be rendered from the wiki page link.
   procedure Make_Page_Link (Renderer : in Link_Renderer;
                             Link     : in Unbounded_Wide_Wide_String;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean) is abstract;

   type Default_Link_Renderer is new Link_Renderer with null record;

   --  Get the image link that must be rendered from the wiki image link.
   overriding
   procedure Make_Image_Link (Renderer : in Default_Link_Renderer;
                              Link     : in Unbounded_Wide_Wide_String;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : out Natural;
                              Height   : out Natural);

   --  Get the page link that must be rendered from the wiki page link.
   overriding
   procedure Make_Page_Link (Renderer : in Default_Link_Renderer;
                             Link     : in Unbounded_Wide_Wide_String;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean);

   --  ------------------------------
   --  Document renderer
   --  ------------------------------
   type Renderer is limited interface;
   type Renderer_Access is access all Renderer'Class;

   --  Render the node instance from the document.
   procedure Render (Engine : in out Renderer;
                     Doc    : in Wiki.Nodes.Document;
                     Node   : in Wiki.Nodes.Node_Type) is abstract;

   --  Finish the rendering after complete wiki document nodes are rendered.
   procedure Finish (Document : in out Renderer) is abstract;

end Wiki.Render;
