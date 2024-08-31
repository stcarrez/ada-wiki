-----------------------------------------------------------------------
--  wiki-render-links -- Wiki links renderering
--  Copyright (C) 2015, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
package body Wiki.Render.Links is

   --  ------------------------------
   --  Get the image link that must be rendered from the wiki image link.
   --  ------------------------------
   overriding
   procedure Make_Image_Link (Renderer : in out Default_Link_Renderer;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Wiki.Strings.UString;
                              Width    : in out Natural;
                              Height   : in out Natural) is
      pragma Unreferenced (Renderer);
   begin
      URI    := Wiki.Strings.To_UString (Link);
      Width  := 0;
      Height := 0;
   end Make_Image_Link;

   --  ------------------------------
   --  Get the page link that must be rendered from the wiki page link.
   --  ------------------------------
   overriding
   procedure Make_Page_Link (Renderer : in out Default_Link_Renderer;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Wiki.Strings.UString;
                             Exists   : out Boolean) is
      pragma Unreferenced (Renderer);
   begin
      URI    := Wiki.Strings.To_UString (Link);
      Exists := True;
   end Make_Page_Link;

end Wiki.Render.Links;
