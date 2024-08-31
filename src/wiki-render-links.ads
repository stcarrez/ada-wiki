-----------------------------------------------------------------------
--  wiki-render-links -- Wiki links renderering
--  Copyright (C) 2015, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Strings;

--  === Link Renderer ===
--  The <tt>Wiki.Render.Links</tt> package defines the <tt>Link_Renderer</tt> interface used
--  for the rendering of links and images.  The interface allows to customize the generated
--  links and image source for the final HTML.
--
package Wiki.Render.Links is

   pragma Preelaborate;

   type Link_Renderer is limited interface;
   type Link_Renderer_Access is access all Link_Renderer'Class;

   --  Get the image link that must be rendered from the wiki image link.
   procedure Make_Image_Link (Renderer : in out Link_Renderer;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Wiki.Strings.UString;
                              Width    : in out Natural;
                              Height   : in out Natural) is abstract;

   --  Get the page link that must be rendered from the wiki page link.
   procedure Make_Page_Link (Renderer : in out Link_Renderer;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Wiki.Strings.UString;
                             Exists   : out Boolean) is abstract;

   type Default_Link_Renderer is new Link_Renderer with null record;

   --  Get the image link that must be rendered from the wiki image link.
   overriding
   procedure Make_Image_Link (Renderer : in out Default_Link_Renderer;
                              Link     : in Wiki.Strings.WString;
                              URI      : out Wiki.Strings.UString;
                              Width    : in out Natural;
                              Height   : in out Natural);

   --  Get the page link that must be rendered from the wiki page link.
   overriding
   procedure Make_Page_Link (Renderer : in out Default_Link_Renderer;
                             Link     : in Wiki.Strings.WString;
                             URI      : out Wiki.Strings.UString;
                             Exists   : out Boolean);

end Wiki.Render.Links;
