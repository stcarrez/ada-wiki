-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2019, 2020, 2022 Stephane Carrez
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

--  === Text Renderer ===
--  The `Text_Renderer` allows to render a wiki document into a text content.
--  The formatting rules are ignored except for the paragraphs and sections.
package Wiki.Render.Text is

   --  ------------------------------
   --  Wiki to Text renderer
   --  ------------------------------
   type Text_Renderer is new Wiki.Render.Renderer with private;

   --  Set the output stream.
   procedure Set_Output_Stream (Engine : in out Text_Renderer;
                                Stream : in Streams.Output_Stream_Access);

   --  Set the no-newline mode to produce a single line text (disabled by default).
   procedure Set_No_Newline (Engine : in out Text_Renderer;
                             Enable : in Boolean);

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

   procedure Render_List_Start (Engine   : in out Text_Renderer;
                                Tag      : in String;
                                Level    : in Natural);

   procedure Render_List_End (Engine   : in out Text_Renderer;
                              Tag      : in String);
   procedure Render_List_Item_Start (Engine   : in out Text_Renderer);
   procedure Render_List_Item_End (Engine   : in out Text_Renderer);

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

   type List_Index_Type is new Integer range 0 .. 32;

   type List_Level_Array is array (List_Index_Type range 1 .. 32) of Natural;

   --  Emit a new line.
   procedure New_Line (Document : in out Text_Renderer);

   procedure Close_Paragraph (Document : in out Text_Renderer);
   procedure Open_Paragraph (Document : in out Text_Renderer);

   --  Render a text block indenting the text if necessary.
   procedure Render_Paragraph (Engine : in out Text_Renderer;
                               Text   : in Wiki.Strings.WString);

   type Text_Renderer is new Wiki.Render.Renderer with record
      Output         : Streams.Output_Stream_Access := null;
      Format         : Wiki.Format_Map := (others => False);
      Has_Paragraph  : Boolean := False;
      Need_Paragraph : Boolean := False;
      Empty_Line     : Boolean := True;
      No_Newline     : Boolean := False;
      Current_Indent : Natural := 0;
      Indent_Level   : Natural := 0;
      List_Index     : List_Index_Type := 0;
      List_Levels    : List_Level_Array;
   end record;

end Wiki.Render.Text;
