-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
--  Copyright (C) 2011 - 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Wiki.Attributes;
with Wiki.Streams;
with Wiki.Strings;
private with Ada.Finalization;

--  === Text Renderer ===
--  The `Text_Renderer` allows to render a wiki document into a text content.
--  The formatting rules are ignored except for the paragraphs and sections.
package Wiki.Render.Text is

   type Text_Diverter is limited interface;
   type Text_Diverter_Access is access all Text_Diverter'Class;

   procedure Clear (Diverter : in out Text_Diverter) is abstract;

   procedure Write_Text (Diverter : in out Text_Diverter;
                         Kind     : in Wiki.Nodes.Node_Kind;
                         Text     : in Strings.WString;
                         Format   : in Format_Map) is abstract;

   procedure Write_Newline (Diverter : in out Text_Diverter) is abstract;

   function Get_Line_Count (Diverter : in Text_Diverter) return Natural is abstract;

   procedure Flush_Line (Diverter : in out Text_Diverter;
                         Stream   : in out Streams.Output_Stream'Class;
                         Line     : in Natural;
                         Align    : in Wiki.Nodes.Align_Style;
                         Width    : in Natural;
                         Last     : out Boolean) is abstract;

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

   procedure Render_List (Engine   : in out Text_Renderer;
                          Doc      : in Wiki.Documents.Document;
                          Node     : in Nodes.Node_Type;
                          Tag      : in String;
                          Level    : in Natural);

   procedure Render_List_Item_Start (Engine   : in out Text_Renderer);
   procedure Render_List_Item_End (Engine   : in out Text_Renderer);

   --  Render a link.
   procedure Render_Link (Engine   : in out Text_Renderer;
                          Doc      : in Wiki.Documents.Document;
                          Node     : in Nodes.Node_Type;
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

   --  Render a table component such as N_TABLE, N_ROW or N_COLUMN.
   procedure Render_Table (Engine : in out Text_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Node   : in Wiki.Nodes.Node_Type);
   procedure Render_Table (Engine  : in out Text_Renderer;
                           Doc     : in Wiki.Documents.Document;
                           Node    : in Wiki.Nodes.Node_Type;
                           Column_Styles : in Wiki.Nodes.Column_Array_Style);
   procedure Render_Html_Table (Engine : in out Text_Renderer;
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

   function Create_Text_Diverter (Engine     : in Text_Renderer;
                                  Max_Length : in Natural) return Text_Diverter_Access;

private

   type Diverter_Array is array (Positive range <>) of Text_Diverter_Access;

   type Text_Renderer is limited new Wiki.Render.Renderer with record
      Output         : Streams.Output_Stream_Access := null;
      Format         : Wiki.Format_Map := (others => False);
      Has_Paragraph  : Boolean := False;
      Need_Paragraph : Boolean := False;
      Need_Space     : Boolean := False;
      Empty_Line     : Boolean := True;
      No_Newline     : Boolean := False;
      Display_Links  : Boolean := True;
      Separate_Items : Boolean := False;
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
      Quote_Level         : Natural := 0;
      UL_List_Level       : Natural := 0;
      OL_List_Level       : Natural := 0;
      In_List             : Boolean := False;
      In_Definition       : Boolean := False;
      Current_Mode        : Nodes.Node_Kind := Nodes.N_NONE;
      Diverter            : Wiki.Render.Text.Text_Diverter_Access;
   end record;

   type Text_Line;
   type Text_Line_Access is access all Text_Line;
   type Text_Line (Len : Natural) is record
      Next    : Text_Line_Access;
      Last    : Natural := 0;
      Content : Wiki.Strings.WString (1 .. Len);
   end record;

   type Default_Text_Diverter (Len : Natural) is limited
   new Ada.Finalization.Limited_Controlled and Text_Diverter with record
      Current : Text_Line_Access;
      Line    : aliased Text_Line (Len);
   end record;
   type Default_Text_Diverter_Access is access all Default_Text_Diverter;

   overriding
   procedure Write_Text (Diverter : in out Default_Text_Diverter;
                         Kind     : in Wiki.Nodes.Node_Kind;
                         Text     : in Strings.WString;
                         Format   : in Format_Map);

   overriding
   procedure Clear (Diverter : in out Default_Text_Diverter);

   overriding
   procedure Write_Newline (Diverter : in out Default_Text_Diverter);

   overriding
   function Get_Line_Count (Diverter : in Default_Text_Diverter) return Natural;

   overriding
   procedure Flush_Line (Diverter : in out Default_Text_Diverter;
                         Stream   : in out Streams.Output_Stream'Class;
                         Line     : in Natural;
                         Align    : in Wiki.Nodes.Align_Style;
                         Width    : in Natural;
                         Last     : out Boolean);

   overriding
   procedure Initialize (Diverter : in out Default_Text_Diverter);

   overriding
   procedure Finalize (Diverter : in out Default_Text_Diverter);

   procedure Release (Columns : in out Diverter_Array);

end Wiki.Render.Text;
