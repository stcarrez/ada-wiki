-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
--  Copyright (C) 2011 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Helpers;
with Util.Strings;
package body Wiki.Render.Text is

   use type Wiki.Nodes.Node_Kind;

   Empty_Formats : constant Format_Map := (others => False);

   --  ------------------------------
   --  Set the output writer.
   --  ------------------------------
   procedure Set_Output_Stream (Engine : in out Text_Renderer;
                                Stream : in Streams.Output_Stream_Access) is
   begin
      Engine.Output := Stream;
   end Set_Output_Stream;

   --  ------------------------------
   --  Set the no-newline mode to produce a single line text (disabled by default).
   --  ------------------------------
   procedure Set_No_Newline (Engine : in out Text_Renderer;
                             Enable : in Boolean) is
   begin
      Engine.No_Newline := Enable;
   end Set_No_Newline;

   procedure Set_Line_Length (Engine : in out Text_Renderer;
                              Length : in Natural) is
   begin
      Engine.Line_Length := Length;
   end Set_Line_Length;

   --  ------------------------------
   --  Set the display of links in the output (default enabled).
   --  ------------------------------
   procedure Set_Display_Links (Engine : in out Text_Renderer;
                                Enable : in Boolean) is
   begin
      Engine.Display_Links := Enable;
   end Set_Display_Links;

   --  ------------------------------
   --  Set the indentation of pre-formatted text (default is 0).
   --  ------------------------------
   procedure Set_Preformatted_Indentation (Engine : in out Text_Renderer;
                                           Level  : in Natural) is
   begin
      Engine.Indent_Preformatted := Level;
   end Set_Preformatted_Indentation;

   --  ------------------------------
   --  Set the text style format.
   --  ------------------------------
   procedure Set_Format (Engine : in out Text_Renderer;
                         Format   : in Format_Map) is
   begin
      Engine.Format := Format;
   end Set_Format;

   --  ------------------------------
   --  Emit a new line.
   --  ------------------------------
   procedure New_Line (Document : in out Text_Renderer) is
   begin
      if not Document.No_Newline then
         if Document.Current_Indent > 0 then
            Text_Renderer'Class (Document).Write_Newline;
         end if;
         if not Document.Empty_Line then
            Text_Renderer'Class (Document).Write_Newline;
         end if;
      end if;
      Document.Empty_Line := True;
      Document.Current_Indent := 0;
      Document.Has_Space := False;
   end New_Line;

   procedure Close_Paragraph (Document : in out Text_Renderer) is
   begin
      if Document.Current_Indent > 0 then
         Document.Add_Line_Break;
      end if;
      Document.Has_Paragraph := False;
      Document.Has_Space := False;
   end Close_Paragraph;

   procedure Open_Paragraph (Document : in out Text_Renderer) is
   begin
      if Document.Need_Paragraph then
         Document.Has_Paragraph  := True;
         Document.Need_Paragraph := False;
      end if;
   end Open_Paragraph;

   --  ------------------------------
   --  Add a line break (<br>).
   --  ------------------------------
   procedure Add_Line_Break (Document : in out Text_Renderer) is
   begin
      if Document.Current_Indent > 0 then
         if not Document.No_Newline then
            Text_Renderer'Class (Document).Write_Newline;
         end if;
      end if;
      Document.Current_Indent := 0;
      Document.Has_Space := False;
   end Add_Line_Break;

   --  ------------------------------
   --  Render a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Render_Blockquote (Engine : in out Text_Renderer;
                                Level  : in Natural) is
   begin
      Engine.Close_Paragraph;
      for I in 1 .. Level loop
         Engine.Output.Write ("  ");
      end loop;
   end Render_Blockquote;

   procedure Render_List_Start (Engine   : in out Text_Renderer;
                                Tag      : in String;
                                Level    : in Natural) is
      pragma Unreferenced (Tag);
   begin
      Engine.New_Line;
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;
      Engine.List_Index := Engine.List_Index + 1;
      Engine.List_Levels (Engine.List_Index) := Level;
      Engine.Indent_Level := Engine.Indent_Level + 2;
      Engine.Current_Indent := 0;
   end Render_List_Start;

   procedure Render_List_End (Engine   : in out Text_Renderer) is
   begin
      Engine.Need_Paragraph := False;
      Engine.Close_Paragraph;
      Engine.List_Index := Engine.List_Index - 1;
      Engine.Indent_Level := Engine.Indent_Level - 2;
   end Render_List_End;

   --  ------------------------------
   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Render_List_Item_Start (Engine   : in out Text_Renderer) is
   begin
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;

      if Engine.List_Levels (Engine.List_Index) > 0 then
         Engine.Render_Paragraph
           (Nodes.N_LIST_ITEM, Strings.To_WString (Util.Strings.Image (Engine.List_Levels (Engine.List_Index))));
         Engine.List_Levels (Engine.List_Index) := Engine.List_Levels (Engine.List_Index) + 1;
         Engine.Render_Paragraph (Nodes.N_LIST_ITEM, ") ");
         Engine.Indent_Level := Engine.Indent_Level + 4;
      else
         Engine.Render_Paragraph (Nodes.N_LIST_ITEM, "- ");
         Engine.Indent_Level := Engine.Indent_Level + 2;
      end if;
   end Render_List_Item_Start;

   procedure Render_List_Item_End (Engine   : in out Text_Renderer) is
   begin
      if Engine.Current_Indent > 0 then
         Engine.Add_Line_Break;
      end if;
      if Engine.List_Levels (Engine.List_Index) > 0 then
         Engine.Indent_Level := Engine.Indent_Level - 4;
      else
         Engine.Indent_Level := Engine.Indent_Level - 2;
      end if;
   end Render_List_Item_End;

   --  ------------------------------
   --  Render a link.
   --  ------------------------------
   procedure Render_Link (Engine : in out Text_Renderer;
                          Title  : in Wiki.Strings.WString;
                          Attr   : in Wiki.Attributes.Attribute_List) is
      Href : constant Wiki.Strings.WString := Wiki.Attributes.Get_Attribute (Attr, "href");
   begin
      if Title'Length /= 0 then
         Engine.Render_Paragraph (Nodes.N_LINK, Title, (others => False));
      end if;
      if Title /= Href and then Href'Length /= 0 and then Engine.Display_Links then
         if Title'Length /= 0 then
            Engine.Render_Paragraph (Nodes.N_LINK, " (", (others => False));
         end if;
         Engine.Output.Write (Href);
         if Title'Length /= 0 then
            Engine.Render_Paragraph (Nodes.N_LINK, ")", (others => False));
         end if;
      end if;
   end Render_Link;

   --  ------------------------------
   --  Render a link reference.
   --  ------------------------------
   procedure Render_Link_Ref (Engine : in out Text_Renderer;
                              Doc    : in Wiki.Documents.Document;
                              Label  : in Wiki.Strings.WString) is
      Link : constant Wiki.Strings.WString := Doc.Get_Link (Label);
      Title  : constant Wiki.Strings.WString := Doc.Get_Link_Title (Label);
   begin
      if Title'Length = 0 then
         Engine.Render_Paragraph (Nodes.N_TEXT, "[", (others => False));
         Engine.Render_Paragraph (Nodes.N_TEXT, Label, (others => False));
         Engine.Render_Paragraph (Nodes.N_TEXT, "]", (others => False));
      else
         Engine.Render_Paragraph (Nodes.N_LINK, Title, (others => False));
      end if;
   end Render_Link_Ref;

   --  ------------------------------
   --  Render an image.
   --  ------------------------------
   procedure Render_Image (Engine   : in out Text_Renderer;
                           Title    : in Wiki.Strings.WString;
                           Attr     : in Wiki.Attributes.Attribute_List) is
      Desc : constant Wiki.Strings.WString := Wiki.Attributes.Get_Attribute (Attr, "longdesc");
   begin
      if Title'Length > 0 then
         Engine.Render_Paragraph (Nodes.N_IMAGE, Title);
      end if;
      if Title'Length > 0 and then Desc'Length > 0 then
         Engine.Render_Paragraph (Nodes.N_IMAGE, " ");
      end if;
      if Desc'Length > 0 then
         Engine.Render_Paragraph (Nodes.N_IMAGE, Desc);
      end if;
   end Render_Image;

   --  ------------------------------
   --  Render a text block that is pre-formatted.
   --  ------------------------------
   procedure Render_Preformatted (Engine : in out Text_Renderer;
                                  Text   : in Wiki.Strings.WString;
                                  Format : in Wiki.Strings.WString) is
      pragma Unreferenced (Format);
   begin
      Engine.Close_Paragraph;
      Text_Renderer'Class (Engine).New_Line;
      if Engine.Indent_Preformatted = 0 then
         Text_Renderer'Class (Engine).Write_Text (Nodes.N_PREFORMAT, Text, (others => False));
      elsif Text'Length > 0 then
         declare
            First : Natural := Text'First;
            Pos   : Natural;
            Last  : Natural;
         begin
            loop
               for I in 1 .. Engine.Indent_Preformatted loop
                  Engine.Output.Write (' ');
               end loop;
               Pos := First;
               while Pos < Text'Last and then not Helpers.Is_Newline (Text (Pos)) loop
                  Pos := Pos + 1;
               end loop;
               Last := (if Helpers.Is_Newline (Text (Pos)) then Pos - 1 else Pos);
               Text_Renderer'Class (Engine).Write_Text (Nodes.N_PREFORMAT,
                                                        Text (First .. Last),
                                                        (others => False));
               Engine.Current_Indent := 1;
               Engine.Add_Line_Break;
               exit when Pos >= Text'Last;
               First := Pos + 1;
            end loop;
         end;
         Engine.Empty_Line := False;
         Engine.New_Line;
      end if;
   end Render_Preformatted;

   --  ------------------------------
   --  Render a table component such as N_TABLE, N_ROW or N_COLUMN.
   --  ------------------------------
   procedure Render_Table (Engine : in out Text_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Node   : in Wiki.Nodes.Node_Type) is
   begin
      Engine.Close_Paragraph;
   end Render_Table;

   --  ------------------------------
   --  Render a text block indenting the text if necessary.
   --  ------------------------------
   procedure Render_Paragraph (Engine : in out Text_Renderer;
                               Kind   : in Wiki.Nodes.Node_Kind;
                               Text   : in Wiki.Strings.WString;
                               Format : in Format_Map := (others => False)) is
      Max : constant Natural := Engine.Line_Length;
      C : Wiki.Strings.WChar;
   begin
      Engine.Open_Paragraph;
      for I in Text'Range loop
         C := Text (I);
         if C = Helpers.LF then
            Engine.Current_Indent := 0;
            Text_Renderer'Class (Engine).Write_Newline;
         elsif C = ' ' and then Engine.Has_Space then
            null;
         elsif Max > 0
           and then Engine.Current_Indent > Max
           and then Wiki.Helpers.Is_Space (C)
         then
            Engine.Current_Indent := 0;
            Text_Renderer'Class (Engine).Write_Newline;
         elsif Engine.Current_Indent /= 0
           or else not Wiki.Helpers.Is_Space (C)
         then
            while Engine.Current_Indent < Engine.Indent_Level loop
               Engine.Output.Write (' ');
               Engine.Current_Indent := Engine.Current_Indent + 1;
            end loop;

            Text_Renderer'Class (Engine).Write_Text (Kind, C & "", Format);

            Engine.Has_Space := C = ' ';
            Engine.Empty_Line := False;
            Engine.Current_Indent := Engine.Current_Indent + 1;
            Engine.Has_Paragraph := True;
         end if;
      end loop;
   end Render_Paragraph;

   --  Add a text block with the given format.
   procedure Write_Text (Engine : in out Text_Renderer;
                         Kind   : in Wiki.Nodes.Node_Kind;
                         Text   : in Strings.WString;
                         Format : in Format_Map) is
   begin
      Engine.Output.Write (Text);
   end Write_Text;

   procedure Write_Newline (Engine : in out Text_Renderer) is
   begin
      Engine.Output.Write (Helpers.LF);
   end Write_Newline;

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type) is
      use type Wiki.Nodes.Node_List_Access;
   begin
      case Node.Kind is
         when Wiki.Nodes.N_HEADER =>
            Engine.Render_Header (Doc, Node, Node.Level, Node.Content);

         when Wiki.Nodes.N_LINE_BREAK =>
            Engine.Add_Line_Break;

         when Wiki.Nodes.N_HORIZONTAL_RULE =>
            Engine.Close_Paragraph;
            Engine.Output.Write ("---------------------------------------------------------");
            Engine.Add_Line_Break;

         when Wiki.Nodes.N_PARAGRAPH =>
            Engine.New_Line;

         when Wiki.Nodes.N_NEWLINE =>
            if not Engine.No_Newline then
               Text_Renderer'Class (Engine).Write_Newline;
            else
               Engine.Output.Write (' ');
            end if;

         when Wiki.Nodes.N_INDENT =>
            Engine.Indent_Level := Node.Level;

         when Wiki.Nodes.N_BLOCKQUOTE =>
            Engine.Render_Blockquote (Node.Level);

         when Wiki.Nodes.N_LIST_START =>
            Engine.Render_List_Start ("o", 0);

         when Wiki.Nodes.N_NUM_LIST_START =>
            Engine.Render_List_Start (".", Node.Level);

         when Wiki.Nodes.N_LIST_END | Wiki.Nodes.N_NUM_LIST_END =>
            Engine.Render_List_End;

         when Wiki.Nodes.N_LIST_ITEM =>
            Engine.Render_List_Item_Start;

         when Wiki.Nodes.N_LIST_ITEM_END =>
            Engine.Render_List_Item_End;

         when Wiki.Nodes.N_TEXT =>
            if Engine.Current_Mode = Nodes.N_NONE then
               Engine.Render_Paragraph (Nodes.N_TEXT, Node.Text, Node.Format or Engine.Format);
            elsif Engine.Current_Mode = Nodes.N_PREFORMAT then
               Engine.Render_Preformatted (Node.Text, "");
            else
               Engine.Render_Paragraph (Engine.Current_Mode, Node.Text, Node.Format or Engine.Format);
            end if;

         when Wiki.Nodes.N_QUOTE =>
            Engine.Render_Paragraph (Nodes.N_QUOTE, Node.Title, Engine.Format);

         when Wiki.Nodes.N_LINK =>
            Text_Renderer'Class (Engine).Render_Link (Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_LINK_REF =>
            Engine.Render_Link_Ref (Doc, Node.Title);

         when Wiki.Nodes.N_IMAGE =>
            Engine.Render_Image (Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_PREFORMAT =>
            Engine.Render_Preformatted (Node.Preformatted, "");

         when Wiki.Nodes.N_TAG_START =>
            Engine.Render_Tag (Doc, Node);

         when Wiki.Nodes.N_TABLE =>
            Engine.Render_Table (Doc, Node);

         when others =>
            null;

      end case;
   end Render;

   procedure Render_Tag (Engine : in out Text_Renderer;
                         Doc    : in Documents.Document;
                         Node   : in Nodes.Node_Type) is
   begin
      case Node.Tag_Start is
         when H1_TAG =>
            Engine.Render_Header (Doc, Node, 1, Node.Children);
            return;

         when H2_TAG =>
            Engine.Render_Header (Doc, Node, 2, Node.Children);
            return;

         when H3_TAG =>
            Engine.Render_Header (Doc, Node, 3, Node.Children);
            return;

         when H4_TAG =>
            Engine.Render_Header (Doc, Node, 4, Node.Children);
            return;

         when H5_TAG =>
            Engine.Render_Header (Doc, Node, 5, Node.Children);
            return;

         when H6_TAG =>
            Engine.Render_Header (Doc, Node, 6, Node.Children);
            return;

         when BR_TAG =>
            Engine.Add_Line_Break;
            return;

         when HR_TAG =>
            Engine.Close_Paragraph;
            --  Engine.Output.Write (Engine.Tags (Horizontal_Rule).all);
            return;

         when IMG_TAG =>
            Engine.Render_Image (Title => Attributes.Get_Attribute (Node.Attributes, "alt"),
                                 Attr  => Node.Attributes);

         when A_TAG | Q_TAG =>
            declare
               Previous_Mode : constant Nodes.Node_Kind := Engine.Current_Mode;
            begin
               Engine.Current_Mode := Nodes.N_LINK;
               Engine.Render (Doc, Node.Children);
               engine.Current_Mode := Previous_Mode;
               return;
            end;

         when B_TAG | EM_TAG | STRONG_TAG =>
            Engine.Format (BOLD) := True;

         when I_TAG =>
            Engine.Format (ITALIC) := True;

         when U_TAG | TT_TAG | CODE_TAG | KBD_TAG =>
            Engine.Format (CODE) := True;

         when SUP_TAG =>
            Engine.Format (SUPERSCRIPT) := True;

         when SUB_TAG =>
            Engine.Format (SUBSCRIPT) := True;

         when P_TAG | DL_TAG =>
            Engine.Set_Format (Empty_Formats);
            Engine.Close_Paragraph;
            Engine.Need_Paragraph := True;
            Engine.New_Line;

         when PRE_TAG =>
            declare
               Current : constant Nodes.Node_Kind := Engine.Current_Mode;
            begin
               Engine.Current_Mode := Nodes.N_PREFORMAT;
               Engine.Render (Doc, Node.Children);
               Engine.New_Line;
               Engine.Current_Mode := Current;
               return;
            end;

         when DD_TAG =>
            Engine.Close_Paragraph;
            Engine.Indent_Level := Engine.Indent_Level + 4;
            Engine.Render (Doc, Node.Children);
            Engine.Close_Paragraph;
            Engine.Indent_Level := Engine.Indent_Level - 4;
            return;

         when DT_TAG =>
            Engine.Close_Paragraph;
            Engine.Render (Doc, Node.Children);
            return;

         when UL_TAG =>
            Engine.Render_List_Start ("", 0);

         when OL_TAG =>
            Engine.Render_List_Start ("", 1);

         when LI_TAG =>
            Engine.Render_List_Item_Start;

         when BLOCKQUOTE_TAG =>
            Engine.Set_Format (Empty_Formats);
            --  Engine.Need_Separator_Line;
            Engine.Close_Paragraph;
            Engine.Quote_Level := Engine.Quote_Level + 1;
            --if Engine.Html_Blockquote then
               --  Make sure there is en empty line before the HTML <blockquote>.
            --   Engine.Output.Write (LF & "<blockquote>" & LF);
            --end if;

         when others =>
            null;

      end case;
      Engine.Render (Doc, Node.Children);

      case Node.Tag_Start is

         when A_TAG | Q_TAG =>
            null;

         when P_TAG =>
            Engine.Set_Format (Empty_Formats);
            Engine.New_Line;

         when B_TAG | EM_TAG | STRONG_TAG =>
            Engine.Format (BOLD) := False;

         when I_TAG =>
            Engine.Format (ITALIC) := False;

         when U_TAG | TT_TAG | CODE_TAG | KBD_TAG =>
            Engine.Format (CODE) := False;

         when SUP_TAG =>
            Engine.Format (SUPERSCRIPT) := False;

         when SUB_TAG =>
            Engine.Format (SUBSCRIPT) := False;

         when UL_TAG | OL_TAG =>
            Engine.Render_List_End;

         when LI_TAG =>
            Engine.Render_List_Item_End;

         when BLOCKQUOTE_TAG =>
            Engine.Set_Format (Empty_Formats);
            --  Engine.Need_Separator_Line;
            Engine.Quote_Level := Engine.Quote_Level - 1;
            Engine.Need_Space := False;
            --if Engine.Html_Blockquote then
               --  Make sure there is an empty line after the HTML </blockquote>.
            --   Engine.Output.Write ("</blockquote>" & LF & LF);
            --elsif Engine.Quote_Level = 0 then
            --   Engine.New_Line;
            --   Engine.Need_Newline := True;
            --end if;

         when others =>
            Engine.Need_Space := True;

      end case;
   end Render_Tag;

   --  Render a quote.
   procedure Render_Quote (Engine : in out Text_Renderer;
                           Title  : in Strings.WString;
                           Attrs  : in Attributes.Attribute_List) is
   begin
      Engine.Render_Paragraph (Nodes.N_QUOTE, Title, Engine.Format);
   end Render_Quote;

   --  ------------------------------
   --  Add a section header in the document.
   --  ------------------------------
   procedure Render_Header (Engine : in out Text_Renderer;
                            Doc    : in Documents.Document;
                            Node   : in Nodes.Node_Type;
                            Level  : in Natural;
                            List   : in Nodes.Node_List_Access) is
      New_Level : constant List_Index_Type :=
        (if List_Index_Type (Level) < List_Index_Type'Last
         then List_Index_Type (Level) else List_Index_Type'Last);
   begin
      Engine.Close_Paragraph;
      Engine.New_Line;
      if Engine.Header_Index < New_Level then
         for I in Engine.Header_Index + 1 .. New_Level loop
            Engine.Header_Levels (I) := 1;
         end loop;
      else
         Engine.Header_Levels (New_Level) := Engine.Header_Levels (New_Level) + 1;
      end if;
      Engine.Header_Index := New_Level;
      Engine.Render_Section_Number (Engine.Header_Levels (1 .. New_Level));
      if Node.Kind = Nodes.N_HEADER then
         Engine.Render (Doc, Node.Content);
      else
         Engine.Render (Doc, Node.Children);
      end if;
      Engine.Add_Line_Break;
      Engine.Has_Paragraph := False;
   end Render_Header;

   --  ------------------------------
   --  Render the section number before the header title.
   --  ------------------------------
   procedure Render_Section_Number (Engine  : in out Text_Renderer;
                                    Numbers : in List_Level_Array) is
   begin
      for Number of Numbers loop
         Engine.Output.Write (Strings.To_WString (Util.Strings.Image (Number)));
         Engine.Output.Write (".");
      end loop;
      Engine.Output.Write (" ");
   end Render_Section_Number;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Engine : in out Text_Renderer;
                     Doc    : in Wiki.Documents.Document) is
      pragma Unreferenced (Doc);
   begin
      Engine.Close_Paragraph;
   end Finish;

end Wiki.Render.Text;
