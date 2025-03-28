-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
--  Copyright (C) 2011 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Streams.Html;
with Wiki.Strings;
with Wiki.Render.Links;
private with Wiki.Stacks;

--  === HTML Renderer ===
--  The `Html_Renderer` allows to render a wiki document into an HTML content.
--
package Wiki.Render.Html is

   --  ------------------------------
   --  Wiki to HTML renderer
   --  ------------------------------
   type Html_Renderer is limited new Renderer with private;

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
   procedure Render_List_Item (Engine : in out Html_Renderer;
                               Doc    : in Wiki.Documents.Document;
                               Node   : in Wiki.Nodes.Node_Type);

   --  Render a list item end (</ul> or </ol>).
   --  Close the previous paragraph and list item if any.
   procedure Render_List_End (Engine : in out Html_Renderer;
                              Tag    : in String);

   --  Add a text block with the given format.
   procedure Add_Text (Engine   : in out Html_Renderer;
                       Text     : in Wiki.Strings.WString;
                       Format   : in Wiki.Format_Map);

   --  Apply the given format before writing some text or after closing some paragraph.
   procedure Set_Format (Engine : in out Html_Renderer;
                         Format : in Wiki.Format_Map);

   --  Render a text block that is pre-formatted.
   procedure Render_Preformatted (Engine : in out Html_Renderer;
                                  Text   : in Wiki.Strings.WString;
                                  Format : in Wiki.Strings.WString);

   --  Render a table component such as N_TABLE, N_ROW or N_COLUMN.
   procedure Render_Table (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Node   : in Wiki.Nodes.Node_Type;
                           Tag    : in String;
                           Class  : in String);

   procedure Render_Definition (Engine  : in out Html_Renderer;
                                Doc     : in Wiki.Documents.Document;
                                Node    : in Wiki.Nodes.Node_Type);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Documents.Document);

private

   type Html_Tag_Access is access all Html_Tag;

   type List_Style_Array is array (1 .. 32) of Boolean;

   type Format_Stack is array (Positive range 1 .. Format_Map'Length) of Format_Type;

   package Tag_Stacks is
     new Wiki.Stacks (Element_Type        => Html_Tag,
                      Element_Type_Access => Html_Tag_Access);

   Default_Links : aliased Wiki.Render.Links.Default_Link_Renderer;

   type Html_Renderer is limited new Renderer with record
      Output            : Wiki.Streams.Html.Html_Output_Stream_Access := null;
      Format            : Wiki.Format_Map := (others => False);
      Current_Format    : Format_Map := (others => False);
      Fmt_Stack         : Format_Stack;
      Fmt_Stack_Size    : Natural := 0;
      Links             : Wiki.Render.Links.Link_Renderer_Access := Default_Links'Access;
      Has_Paragraph     : Boolean := False;
      Need_Paragraph    : Boolean := False;
      Has_Item          : Boolean := False;
      Enable_Render_TOC : Boolean := False;
      TOC_Rendered      : Boolean := False;
      No_Newline        : Boolean := False;
      Loose_List        : Boolean := False;
      Current_Level     : Natural := 0;
      Html_Tag          : Wiki.Html_Tag := BODY_TAG;
      Html_Stack        : Tag_Stacks.Stack;
      List_Styles       : List_Style_Array := (others => False);
      Quote_Level       : Natural := 0;
      Html_Level        : Natural := 0;
      Current_Section   : Toc_Number_Array (1 .. MAX_TOC_LEVEL) := (others => 0);
      Section_Level     : List_Index_Type := 0;
      Column            : Natural := 0;
      In_Definition     : Boolean := False;
      Row_Kind          : Nodes.Row_Kind;
   end record;

end Wiki.Render.Html;
