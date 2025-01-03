-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
--  Copyright (C) 2011 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
   type Text_Renderer is limited new Wiki.Render.Renderer with private;

   --  Set the output stream.
   procedure Set_Output_Stream (Engine : in out Text_Renderer;
                                Stream : in Streams.Output_Stream_Access);

   --  Set the no-newline mode to produce a single line text (disabled by default).
   procedure Set_No_Newline (Engine : in out Text_Renderer;
                             Enable : in Boolean);

   --  Set the length of output line.  A positive value will try to insert
   --  new lines to break long lines if possible.  0 means the line is not limited.
   procedure Set_Line_Length (Engine : in out Text_Renderer;
                              Length : in Natural);

   --  Set the display of links in the output (default enabled).
   procedure Set_Display_Links (Engine : in out Text_Renderer;
                                Enable : in Boolean);

   --  Set the indentation of pre-formatted text (default is 0).
   procedure Set_Preformatted_Indentation (Engine : in out Text_Renderer;
                                           Level  : in Natural);

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type);

   --  Add a text block with the given format.
   procedure Write_Text (Engine : in out Text_Renderer;
                         Kind   : in Wiki.Nodes.Node_Kind;
                         Text   : in Strings.WString;
                         Format : in Format_Map);

   procedure Write_Newline (Engine : in out Text_Renderer);

   --  Add a line break (<br>).
   procedure Add_Line_Break (Document : in out Text_Renderer);

   --  Render a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Render_Blockquote (Engine : in out Text_Renderer;
                                Level  : in Natural);

   procedure Render_List_Start (Engine   : in out Text_Renderer;
                                Tag      : in String;
                                Level    : in Natural);

   procedure Render_List_End (Engine   : in out Text_Renderer);
   procedure Render_List_Item_Start (Engine   : in out Text_Renderer);
   procedure Render_List_Item_End (Engine   : in out Text_Renderer);

   --  Render a link.
   procedure Render_Link (Engine   : in out Text_Renderer;
                          Title    : in Wiki.Strings.WString;
                          Attr     : in Wiki.Attributes.Attribute_List);

   --  Render a link reference.
   procedure Render_Link_Ref (Engine : in out Text_Renderer;
                              Doc    : in Wiki.Documents.Document;
                              Label  : in Wiki.Strings.WString);

   --  Render an image.
   procedure Render_Image (Engine   : in out Text_Renderer;
                           Title    : in Wiki.Strings.WString;
                           Attr     : in Wiki.Attributes.Attribute_List);

   --  Render a text block that is pre-formatted.
   procedure Render_Preformatted (Engine   : in out Text_Renderer;
                                  Text     : in Wiki.Strings.WString;
                                  Format   : in Wiki.Strings.WString);

   --  Render a table component such as N_TABLE, N_ROW or N_COLUMN.
   procedure Render_Table (Engine : in out Text_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Node   : in Wiki.Nodes.Node_Type);

   --  Render a section header in the document.
   procedure Render_Header (Engine : in out Text_Renderer;
                            Doc    : in Documents.Document;
                            Node   : in Nodes.Node_Type;
                            Level  : in Natural;
                            List   : in Nodes.Node_List_Access);

   --  Render the section number before the header title.
   procedure Render_Section_Number (Engine  : in out Text_Renderer;
                                    Numbers : in List_Level_Array);

   procedure Render_Tag (Engine : in out Text_Renderer;
                         Doc    : in Documents.Document;
                         Node   : in Nodes.Node_Type);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document);

   --  Emit a new line.
   procedure New_Line (Document : in out Text_Renderer);

   procedure Close_Paragraph (Document : in out Text_Renderer);
   procedure Open_Paragraph (Document : in out Text_Renderer);

   --  Render a text block indenting the text if necessary.
   procedure Render_Paragraph (Engine : in out Text_Renderer;
                               Kind   : in Wiki.Nodes.Node_Kind;
                               Text   : in Wiki.Strings.WString;
                               Format : in Format_Map := (others => False));

   --  Render a quote.
   procedure Render_Quote (Engine : in out Text_Renderer;
                           Title  : in Strings.WString;
                           Attrs  : in Attributes.Attribute_List);

   procedure Render_Definition (Engine : in out Text_Renderer;
                                Doc    : in Documents.Document;
                                Node   : in Nodes.Node_Type);

   --  Set the text style format.
   procedure Set_Format (Engine : in out Text_Renderer;
                         Format   : in Format_Map);

private

   type Text_Renderer is limited new Wiki.Render.Renderer with record
      Output         : Streams.Output_Stream_Access := null;
      Format         : Wiki.Format_Map := (others => False);
      Has_Paragraph  : Boolean := False;
      Need_Paragraph : Boolean := False;
      Empty_Line     : Boolean := True;
      No_Newline     : Boolean := False;
      Display_Links  : Boolean := True;
      Has_Space      : Boolean := False;
      Current_Indent : Natural := 0;
      Indent_Level   : Natural := 0;
      Line_Length    : Natural := 0;
      Indent_Preformatted : Natural := 0;
      List_Index     : List_Index_Type := 0;
      List_Levels    : List_Level_Array (1 .. MAX_LIST_LEVEL);
      Header_Index   : List_Index_Type := 0;
      Header_Levels  : List_Level_Array (1 .. 30);
      Current_Section     : Toc_Number_Array (1 .. MAX_TOC_LEVEL) := (others => 0);
      Need_Space          : Boolean := False;
      Quote_Level         : Natural := 0;
      UL_List_Level       : Natural := 0;
      OL_List_Level       : Natural := 0;
      In_List             : Boolean := False;
      In_Definition       : Boolean := False;
      Current_Mode        : Nodes.Node_Kind := Nodes.N_NONE;
   end record;

end Wiki.Render.Text;
