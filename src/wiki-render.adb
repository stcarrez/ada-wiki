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
package body Wiki.Render is

   --  ------------------------------
   --  Get the image link that must be rendered from the wiki image link.
   --  ------------------------------
   overriding
   procedure Make_Image_Link (Renderer : in Default_Link_Renderer;
                              Link     : in Unbounded_Wide_Wide_String;
                              URI      : out Unbounded_Wide_Wide_String;
                              Width    : out Natural;
                              Height   : out Natural) is
      pragma Unreferenced (Renderer);
   begin
      URI    := Link;
      Width  := 0;
      Height := 0;
   end Make_Image_Link;

   --  ------------------------------
   --  Get the page link that must be rendered from the wiki page link.
   --  ------------------------------
   overriding
   procedure Make_Page_Link (Renderer : in Default_Link_Renderer;
                             Link     : in Unbounded_Wide_Wide_String;
                             URI      : out Unbounded_Wide_Wide_String;
                             Exists   : out Boolean) is
      pragma Unreferenced (Renderer);
   begin
      URI    := Link;
      Exists := True;
   end Make_Page_Link;

end Wiki.Render;
