-----------------------------------------------------------------------
--  wiki-render -- Wiki renderer
--  Copyright (C) 2015, 2016, 2020, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Nodes;
with Wiki.Documents;
with Wiki.Strings;

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

   MAX_LIST_LEVEL : constant := 32;
   MAX_TOC_LEVEL  : constant := 6;

   type List_Index_Type is new Integer range 0 .. MAX_LIST_LEVEL;

   type List_Level_Array is array (List_Index_Type range <>) of Natural;

   subtype Toc_Index_Type is List_Index_Type range 0 .. MAX_TOC_LEVEL;

   subtype Toc_Number_Array is List_Level_Array;

   --  Format the section or list number with the optional prefix and separator.
   function Format_Section_Number (List      : in List_Level_Array;
                                   Prefix    : in Wiki.Strings.WString;
                                   Separator : in Wiki.Strings.WChar) return Wiki.Strings.WString;

end Wiki.Render;
