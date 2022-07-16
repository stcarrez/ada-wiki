-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
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
with Wiki.Streams.Html;
with Wiki.Strings;
with Wiki.Render.Links;

--  === HTML Renderer ===
--  The `Html_Renderer` allows to render a wiki document into an HTML content.
--
package Wiki.Render.Html is

   --  ------------------------------
   --  Wiki to HTML renderer
   --  ------------------------------
   type Html_Renderer is new Renderer with private;

   --  Set the output stream.
   procedure Set_Output_Stream (Engine : in out Html_Renderer;
                                Stream : in Wiki.Streams.Html.Html_Output_Stream_Access);

   --  Set the link renderer.
   procedure Set_Link_Renderer (Engine : in out Html_Renderer;
                                Links  : in Wiki.Render.Links.Link_Renderer_Access);

   --  Set the render TOC flag that controls the TOC rendering.
   procedure Set_Render_TOC (Engine : in out Html_Renderer;
                             State  : in Boolean);

   --  Set the no-newline mode to avoid emitting newlines (disabled by default).
   procedure Set_No_Newline (Engine : in out Html_Renderer;
                             Enable : in Boolean);

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type);

   --  Get the current section number.
   function Get_Section_Number (Engine    : in Html_Renderer;
                                Prefix    : in Wiki.Strings.WString;
                                Separator : in Wiki.Strings.WChar) return Wiki.Strings.WString;

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Add_Blockquote (Engine : in out Html_Renderer;
                             Level    : in Natural);

   procedure Render_List_Start (Engine   : in out Html_Renderer;
                                Tag      : in String;
                                Level    : in Natural);

   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   procedure Render_List_Item (Engine   : in out Html_Renderer);

   --  Render a list item end (</ul> or </ol>).
   --  Close the previous paragraph and list item if any.
   procedure Render_List_End (Engine : in out Html_Renderer;
                              Tag    : in String);

   --  Add a text block with the given format.
   procedure Add_Text (Engine   : in out Html_Renderer;
                       Text     : in Wiki.Strings.WString;
                       Format   : in Wiki.Format_Map);

   --  Render a text block that is pre-formatted.
   procedure Render_Preformatted (Engine : in out Html_Renderer;
                                  Text   : in Wiki.Strings.WString;
                                  Format : in Wiki.Strings.WString);

   --  Render a table component such as N_TABLE, N_ROW or N_COLUMN.
   procedure Render_Table (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Node   : in Wiki.Nodes.Node_Type;
                           Tag    : in String);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Documents.Document);

private

   type Toc_Number_Array is array (1 .. 6) of Natural;

   type List_Style_Array is array (1 .. 32) of Boolean;

   Default_Links : aliased Wiki.Render.Links.Default_Link_Renderer;

   type Html_Renderer is new Renderer with record
      Output            : Wiki.Streams.Html.Html_Output_Stream_Access := null;
      Format            : Wiki.Format_Map := (others => False);
      Links             : Wiki.Render.Links.Link_Renderer_Access := Default_Links'Access;
      Has_Paragraph     : Boolean := False;
      Need_Paragraph    : Boolean := False;
      Has_Item          : Boolean := False;
      Enable_Render_TOC : Boolean := False;
      TOC_Rendered      : Boolean := False;
      No_Newline        : Boolean := False;
      Current_Level     : Natural := 0;
      Html_Tag          : Wiki.Html_Tag := BODY_TAG;
      List_Styles       : List_Style_Array := (others => False);
      Quote_Level       : Natural := 0;
      Html_Level        : Natural := 0;
      Current_Section   : Toc_Number_Array := (others => 0);
      Section_Level     : Natural := 0;
      Column            : Natural := 0;
      In_Definition     : Boolean := False;
   end record;

end Wiki.Render.Html;
