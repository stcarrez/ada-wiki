-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
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
with Wiki.Streams;
with Wiki.Strings;

--  == Text Renderer ==
--  The <tt>Text_Renderer</tt> allows to render a wiki document into a text content.
--  The formatting rules are ignored except for the paragraphs and sections.
package Wiki.Render.Text is

   --  ------------------------------
   --  Wiki to Text renderer
   --  ------------------------------
   type Text_Renderer is new Wiki.Render.Renderer with private;

   --  Set the output stream.
   procedure Set_Output_Stream (Engine : in out Text_Renderer;
                                Stream : in Streams.Output_Stream_Access);

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type);

   --  Add a line break (<br>).
   procedure Add_Line_Break (Document : in out Text_Renderer);

   --  Render a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Render_Blockquote (Engine : in out Text_Renderer;
                                Level  : in Natural);

   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   procedure Render_List_Item (Engine   : in out Text_Renderer;
                               Level    : in Positive;
                               Ordered  : in Boolean);

   --  Render a link.
   procedure Render_Link (Engine   : in out Text_Renderer;
                          Title    : in Wiki.Strings.WString;
                          Attr     : in Wiki.Attributes.Attribute_List);

   --  Render an image.
   procedure Render_Image (Engine   : in out Text_Renderer;
                           Title    : in Wiki.Strings.WString;
                           Attr     : in Wiki.Attributes.Attribute_List);

   --  Render a text block that is pre-formatted.
   procedure Render_Preformatted (Engine   : in out Text_Renderer;
                                  Text     : in Wiki.Strings.WString;
                                  Format   : in Wiki.Strings.WString);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document);

private

   --  Emit a new line.
   procedure New_Line (Document : in out Text_Renderer);

   procedure Close_Paragraph (Document : in out Text_Renderer);
   procedure Open_Paragraph (Document : in out Text_Renderer);

   type Text_Renderer is new Wiki.Render.Renderer with record
      Output         : Streams.Output_Stream_Access := null;
      Format         : Wiki.Format_Map := (others => False);
      Has_Paragraph  : Boolean := False;
      Need_Paragraph : Boolean := False;
      Empty_Line     : Boolean := True;
      Indent_Level   : Natural := 0;
   end record;

end Wiki.Render.Text;
