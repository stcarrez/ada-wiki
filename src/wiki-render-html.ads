-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
--  Copyright (C) 2011, 2012, 2013, 2015, 2016 Stephane Carrez
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
with Wiki.Attributes;
with Wiki.Streams.Html;
with Wiki.Strings;

--  == HTML Renderer ==
--  The <tt>Text_Renderer</tt> allows to render a wiki document into a text content.
--  The formatting rules are ignored except for the paragraphs and sections.
package Wiki.Render.Html is

   --  ------------------------------
   --  Wiki to HTML renderer
   --  ------------------------------
   type Html_Renderer is new Renderer with private;

   --  Set the output stream.
   procedure Set_Output_Stream (Engine : in out Html_Renderer;
                                Stream : in Wiki.Streams.Html.Html_Output_Stream_Access);

   --  Set the link renderer.
   procedure Set_Link_Renderer (Document : in out Html_Renderer;
                                Links    : in Link_Renderer_Access);

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Nodes.Document;
                     Node   : in Wiki.Nodes.Node_Type);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Add_Blockquote (Document : in out Html_Renderer;
                             Level    : in Natural);

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   procedure Add_List_Item (Document : in out Html_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean);

   --  Add a text block with the given format.
   procedure Add_Text (Engine   : in out Html_Renderer;
                       Text     : in Wiki.Strings.WString;
                       Format   : in Wiki.Format_Map);

   --  Render a text block that is pre-formatted.
   procedure Render_Preformatted (Engine : in out Html_Renderer;
                                  Text   : in Wiki.Strings.WString;
                                  Format : in Unbounded_Wide_Wide_String);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Document : in out Html_Renderer);

private

   procedure Close_Paragraph (Document : in out Html_Renderer);
   procedure Open_Paragraph (Document : in out Html_Renderer);

   type List_Style_Array is array (1 .. 32) of Boolean;

   Default_Links : aliased Default_Link_Renderer;

   type Html_Renderer is new Renderer with record
      Output         : Wiki.Streams.Html.Html_Output_Stream_Access := null;
      Format         : Wiki.Format_Map := (others => False);
      Links          : Link_Renderer_Access := Default_Links'Access;
      Has_Paragraph  : Boolean := False;
      Need_Paragraph : Boolean := False;
      Has_Item       : Boolean := False;
      Current_Level  : Natural := 0;
      List_Styles    : List_Style_Array := (others => False);
      Quote_Level    : Natural := 0;
      Html_Level     : Natural := 0;
   end record;

   procedure Render_Tag (Engine : in out Html_Renderer;
                         Doc    : in Wiki.Nodes.Document;
                         Node   : in Wiki.Nodes.Node_Type);

   --  Render a section header in the document.
   procedure Render_Header (Engine : in out Html_Renderer;
                            Header : in Wiki.Strings.WString;
                            Level  : in Positive);

   --  Render a link.
   procedure Render_Link (Engine : in out Html_Renderer;
                          Doc    : in Wiki.Nodes.Document;
                          Title  : in Wiki.Strings.WString;
                          Attr   : in Wiki.Attributes.Attribute_List_Type);

   --  Render an image.
   procedure Render_Image (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Nodes.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List_Type);

   --  Render a quote.
   procedure Render_Quote (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Nodes.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List_Type);

end Wiki.Render.Html;
