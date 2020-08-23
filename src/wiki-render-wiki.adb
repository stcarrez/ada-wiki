-----------------------------------------------------------------------
--  wiki-render-wiki -- Wiki to Wiki renderer
--  Copyright (C) 2015, 2016, 2018, 2020 Stephane Carrez
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

with Wiki.Helpers;
package body Wiki.Render.Wiki is

   use Helpers;

   HEADER_CREOLE             : aliased constant Strings.WString := "=";
   BOLD_CREOLE               : aliased constant Strings.WString := "**";
   ITALIC_CREOLE             : aliased constant Strings.WString := "//";
   SUPERSCRIPT_CREOLE        : aliased constant Strings.WString := "^^";
   SUBSCRIPT_CREOLE          : aliased constant Strings.WString := ",,";
   UNDERLINE_CREOLE          : aliased constant Strings.WString := "__";
   LINE_BREAK_CREOLE         : aliased constant Strings.WString := "%%%";
   IMG_START_CREOLE          : aliased constant Strings.WString := "{{";
   IMG_END_CREOLE            : aliased constant Strings.WString := "}}";
   LINK_START_CREOLE         : aliased constant Strings.WString := "[[";
   LINK_END_CREOLE           : aliased constant Strings.WString := "]]";
   PREFORMAT_START_CREOLE    : aliased constant Strings.WString := "{{{";
   PREFORMAT_END_CREOLE      : aliased constant Strings.WString := "}}}" & LF;
   HORIZONTAL_RULE_CREOLE    : aliased constant Strings.WString := "----" & LF;
   LINK_SEPARATOR_CREOLE     : aliased constant Strings.WString := "|";
   LIST_ITEM_CREOLE          : aliased constant Strings.WString := "*";
   LIST_ORDERED_ITEM_CREOLE  : aliased constant Strings.WString := "#";
   ESCAPE_CREOLE             : aliased constant Strings.WString := "~";

   HEADER_DOTCLEAR           : aliased constant Strings.WString := "!";
   BOLD_DOTCLEAR             : aliased constant Strings.WString := "__";
   ITALIC_DOTCLEAR           : aliased constant Strings.WString := "''";
   INSERT_DOTCLEAR           : aliased constant Strings.WString := "++";
   DELETE_DOTCLEAR           : aliased constant Strings.WString := "--";
   CODE_DOTCLEAR             : aliased constant Strings.WString := "@@";
   IMG_START_DOTCLEAR        : aliased constant Strings.WString := "((";
   IMG_END_DOTCLEAR          : aliased constant Strings.WString := "))";
   LINK_START_DOTCLEAR       : aliased constant Strings.WString := "[";
   LINK_END_DOTCLEAR         : aliased constant Strings.WString := "]";
   QUOTE_START_DOTCLEAR      : aliased constant Strings.WString := "{{";
   QUOTE_END_DOTCLEAR        : aliased constant Strings.WString := "}}";
   QUOTE_SEPARATOR_DOTCLEAR  : aliased constant Strings.WString := "|";
   PREFORMAT_START_DOTCLEAR  : aliased constant Strings.WString := "///";
   PREFORMAT_END_DOTCLEAR    : aliased constant Strings.WString := "///" & LF;
   ESCAPE_DOTCLEAR           : aliased constant Strings.WString := "\";
   QUOTE_DOTCLEAR            : aliased constant Strings.WString := ">";

   LINE_BREAK_MEDIAWIKI      : aliased constant Strings.WString := "<br />";
   BOLD_MEDIAWIKI            : aliased constant Strings.WString := "'''";
   ITALIC_MEDIAWIKI          : aliased constant Strings.WString := "''";
   PREFORMAT_START_MEDIAWIKI : aliased constant Strings.WString := "<pre>";
   PREFORMAT_END_MEDIAWIKI   : aliased constant Strings.WString := "</pre>";
   IMG_START_MEDIAWIKI       : aliased constant Strings.WString := "[[File:";
   IMG_END_MEDIAWIKI         : aliased constant Strings.WString := "]]";

   Empty_Formats : constant Format_Map := (others => False);

   --  Set the output writer.
   procedure Set_Output_Stream (Engine : in out Wiki_Renderer;
                                Stream : in Streams.Output_Stream_Access;
                                Format : in Wiki_Syntax) is
   begin
      Engine.Output := Stream;
      Engine.Syntax := Format;
      case Format is
         when SYNTAX_DOTCLEAR =>
            Engine.Style_Start_Tags (BOLD)   := BOLD_DOTCLEAR'Access;
            Engine.Style_End_Tags (BOLD)     := BOLD_DOTCLEAR'Access;
            Engine.Style_Start_Tags (ITALIC) := ITALIC_DOTCLEAR'Access;
            Engine.Style_End_Tags (ITALIC)   := ITALIC_DOTCLEAR'Access;
            Engine.Style_Start_Tags (STRIKEOUT) := DELETE_DOTCLEAR'Access;
            Engine.Style_End_Tags (STRIKEOUT)   := DELETE_DOTCLEAR'Access;
            Engine.Style_Start_Tags (CODE) := CODE_DOTCLEAR'Access;
            Engine.Style_End_Tags (CODE)   := CODE_DOTCLEAR'Access;
            Engine.Tags (Header_Start) := HEADER_DOTCLEAR'Access;
            Engine.Tags (Line_Break)   := LINE_BREAK_CREOLE'Access;
            Engine.Tags (Img_Start)    := IMG_START_DOTCLEAR'Access;
            Engine.Tags (Img_End)      := IMG_END_DOTCLEAR'Access;
            Engine.Tags (Link_Start)   := LINK_START_DOTCLEAR'Access;
            Engine.Tags (Link_End)     := LINK_END_DOTCLEAR'Access;
            Engine.Tags (Link_Separator)  := LINK_SEPARATOR_CREOLE'Access;
            Engine.Tags (Quote_Start)     := QUOTE_START_DOTCLEAR'Access;
            Engine.Tags (Quote_End)       := QUOTE_END_DOTCLEAR'Access;
            Engine.Tags (Quote_Separator)  := QUOTE_SEPARATOR_DOTCLEAR'Access;
            Engine.Tags (Preformat_Start) := PREFORMAT_START_DOTCLEAR'Access;
            Engine.Tags (Preformat_End)   := PREFORMAT_END_DOTCLEAR'Access;
            Engine.Tags (Horizontal_Rule) := HORIZONTAL_RULE_CREOLE'Access;
            Engine.Tags (List_Item)       := LIST_ITEM_CREOLE'Access;
            Engine.Tags (List_Ordered_Item) := LIST_ORDERED_ITEM_CREOLE'Access;
            Engine.Tags (Escape_Rule)       := ESCAPE_DOTCLEAR'Access;
            Engine.Tags (Blockquote_Start)  := QUOTE_DOTCLEAR'Access;
            Engine.Invert_Header_Level := True;
            Engine.Allow_Link_Language := True;
            Engine.Escape_Set := Ada.Strings.Wide_Wide_Maps.To_Set ("-+_*{}][/=\");

         when SYNTAX_MEDIA_WIKI =>
            Engine.Style_Start_Tags (BOLD)   := BOLD_MEDIAWIKI'Access;
            Engine.Style_End_Tags (BOLD)     := BOLD_MEDIAWIKI'Access;
            Engine.Style_Start_Tags (ITALIC) := ITALIC_MEDIAWIKI'Access;
            Engine.Style_End_Tags (ITALIC)   := ITALIC_MEDIAWIKI'Access;
            Engine.Tags (Header_Start) := HEADER_CREOLE'Access;
            Engine.Tags (Header_End)   := HEADER_CREOLE'Access;
            Engine.Tags (Line_Break)   := LINE_BREAK_MEDIAWIKI'Access;
            Engine.Tags (Img_Start)    := IMG_START_MEDIAWIKI'Access;
            Engine.Tags (Img_End)      := IMG_END_MEDIAWIKI'Access;
            Engine.Tags (Link_Start)   := LINK_START_CREOLE'Access;
            Engine.Tags (Link_End)     := LINK_END_CREOLE'Access;
            Engine.Tags (Link_Separator)  := LINK_SEPARATOR_CREOLE'Access;
            Engine.Tags (List_Item)       := LIST_ITEM_CREOLE'Access;
            Engine.Tags (List_Ordered_Item) := LIST_ORDERED_ITEM_CREOLE'Access;
            Engine.Tags (Preformat_Start) := PREFORMAT_START_MEDIAWIKI'Access;
            Engine.Tags (Preformat_End)   := PREFORMAT_END_MEDIAWIKI'Access;
            Engine.Tags (Horizontal_Rule) := HORIZONTAL_RULE_CREOLE'Access;
            Engine.Tags (Escape_Rule)     := ESCAPE_CREOLE'Access;
            Engine.Escape_Set := Ada.Strings.Wide_Wide_Maps.To_Set ("'+_-*(){}][!");

         when others =>
            Engine.Style_Start_Tags (BOLD)   := BOLD_CREOLE'Access;
            Engine.Style_End_Tags (BOLD)     := BOLD_CREOLE'Access;
            Engine.Style_Start_Tags (ITALIC) := ITALIC_CREOLE'Access;
            Engine.Style_End_Tags (ITALIC)   := ITALIC_CREOLE'Access;
            Engine.Style_Start_Tags (SUPERSCRIPT) := SUPERSCRIPT_CREOLE'Access;
            Engine.Style_End_Tags (SUPERSCRIPT)   := SUPERSCRIPT_CREOLE'Access;
            Engine.Style_Start_Tags (SUBSCRIPT) := SUBSCRIPT_CREOLE'Access;
            Engine.Style_End_Tags (SUBSCRIPT)   := SUBSCRIPT_CREOLE'Access;
            Engine.Style_Start_Tags (CODE) := UNDERLINE_CREOLE'Access;
            Engine.Style_End_Tags (CODE)   := UNDERLINE_CREOLE'Access;
            Engine.Tags (Header_Start) := HEADER_CREOLE'Access;
            Engine.Tags (Header_End)   := HEADER_CREOLE'Access;
            Engine.Tags (Line_Break)   := LINE_BREAK_CREOLE'Access;
            Engine.Tags (Img_Start)    := IMG_START_CREOLE'Access;
            Engine.Tags (Img_End)      := IMG_END_CREOLE'Access;
            Engine.Tags (Link_Start)   := LINK_START_CREOLE'Access;
            Engine.Tags (Link_End)     := LINK_END_CREOLE'Access;
            Engine.Tags (Link_Separator)  := LINK_SEPARATOR_CREOLE'Access;
            Engine.Tags (Quote_Start)     := LINK_START_CREOLE'Access;
            Engine.Tags (Quote_End)       := LINK_END_CREOLE'Access;
            Engine.Tags (Quote_Separator)  := LINK_SEPARATOR_CREOLE'Access;
            Engine.Tags (List_Item)       := LIST_ITEM_CREOLE'Access;
            Engine.Tags (List_Ordered_Item) := LIST_ORDERED_ITEM_CREOLE'Access;
            Engine.Tags (Preformat_Start) := PREFORMAT_START_CREOLE'Access;
            Engine.Tags (Preformat_End)   := PREFORMAT_END_CREOLE'Access;
            Engine.Tags (Horizontal_Rule) := HORIZONTAL_RULE_CREOLE'Access;
            Engine.Tags (Escape_Rule)     := ESCAPE_CREOLE'Access;
            Engine.Escape_Set := Ada.Strings.Wide_Wide_Maps.To_Set ("'+_-*(){}][!");

      end case;
   end Set_Output_Stream;

   --  ------------------------------
   --  Emit a new line.
   --  ------------------------------
   procedure New_Line (Engine : in out Wiki_Renderer) is
   begin
      Engine.Output.Write (LF);
      Engine.Empty_Line := True;
   end New_Line;

   --  ------------------------------
   --  Render the node instance from the document.
   --  ------------------------------
   overriding
   procedure Render (Engine : in out Wiki_Renderer;
                     Doc    : in Documents.Document;
                     Node   : in Nodes.Node_Type) is
   begin
      case Node.Kind is
         when Nodes.N_HEADER =>
            Engine.Render_Header (Node.Header, Node.Level);

         when Nodes.N_LINE_BREAK =>
            Engine.Output.Write (Engine.Tags (Line_Break).all);
            Engine.Empty_Line := False;

         when Nodes.N_HORIZONTAL_RULE =>
            Engine.Close_Paragraph;
            Engine.Output.Write (Engine.Tags (Horizontal_Rule).all);

         when Nodes.N_PARAGRAPH =>
            Engine.Close_Paragraph;

         when Nodes.N_TEXT =>
            Engine.Render_Text (Node.Text, Node.Format);

         when Nodes.N_LINK =>
            Engine.Render_Link (Node.Title, Node.Link_Attr);

         when Nodes.N_IMAGE =>
            Engine.Render_Image (Node.Title, Node.Link_Attr);

         when Nodes.N_QUOTE =>
            Engine.Render_Quote (Node.Title, Node.Link_Attr);

         when Nodes.N_TAG_START =>
            Engine.Render_Tag (Doc, Node);

         when others =>
            null;

      end case;
   end Render;

   --  ------------------------------
   --  Add a section header in the document.
   --  ------------------------------
   procedure Render_Header (Engine : in out Wiki_Renderer;
                            Header : in Strings.WString;
                            Level  : in Positive) is
      Count : Natural := Level;
   begin
      Engine.Set_Format (Empty_Formats);
      if not Engine.Empty_Line then
         Engine.Output.Write (LF);
      end if;
      Engine.Close_Paragraph;
      if Engine.Invert_Header_Level then
         if Count > 5 then
            Count := 5;
         end if;
         Count := 6 - Count;
      elsif Count > 6 then
         Count := 6;
      end if;
      for I in 1 .. Count loop
         Engine.Output.Write (Engine.Tags (Header_Start).all);
      end loop;
      Engine.Output.Write (' ');
      Engine.Output.Write (Header);
      if Engine.Tags (Header_End)'Length > 0 then
         Engine.Output.Write (' ');
         for I in 1 .. Level loop
            Engine.Output.Write (Engine.Tags (Header_End).all);
         end loop;
      end if;
      Engine.New_Line;
   end Render_Header;

   --  ------------------------------
   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   --  ------------------------------
   procedure Add_Paragraph (Engine : in out Wiki_Renderer) is
   begin
      Engine.Close_Paragraph;
      if not Engine.Empty_Line then
         Engine.New_Line;
      end if;
      Engine.New_Line;
   end Add_Paragraph;

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Add_Blockquote (Engine : in out Wiki_Renderer;
                             Level    : in Natural) is
   begin
      null;
   end Add_Blockquote;

   --  ------------------------------
   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Add_List_Item (Engine : in out Wiki_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean) is
   begin
      Engine.Close_Paragraph;
      Engine.Output.Write (Engine.Tags (List_Start).all);
      for I in 1 .. Level loop
         if Ordered then
            Engine.Output.Write (Engine.Tags (List_Ordered_Item).all);
         else
            Engine.Output.Write (Engine.Tags (List_Item).all);
         end if;
      end loop;
      Engine.Output.Write (' ');
   end Add_List_Item;

   --  ------------------------------
   --  Render a link.
   --  ------------------------------
   procedure Render_Link (Engine   : in out Wiki_Renderer;
                          Name     : in Strings.WString;
                          Attrs    : in Attributes.Attribute_List) is
      Link : constant Strings.WString := Attributes.Get_Attribute (Attrs, "href");
      Lang : constant Strings.WString := Attributes.Get_Attribute (Attrs, "lang");
   begin
      Engine.Output.Write (Engine.Tags (Link_Start).all);
      Engine.Output.Write (Link);
      if Name'Length > 0 then
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
         Engine.Output.Write (Name);
      end if;
      if Engine.Allow_Link_Language and Lang'Length > 0 then
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
         Engine.Output.Write (Lang);
      end if;
      Engine.Output.Write (Engine.Tags (Link_End).all);
      Engine.Empty_Line := False;
   end Render_Link;

   --  Render an image.
   procedure Render_Image (Engine : in out Wiki_Renderer;
                           Link   : in Strings.WString;
                           Attrs  : in Attributes.Attribute_List) is
      Alt : constant Strings.WString := Attributes.Get_Attribute (Attrs, "alt");
   begin
      Engine.Output.Write (Engine.Tags (Img_Start).all);
      Engine.Output.Write (Link);
      if Alt'Length > 0 then
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
         Engine.Output.Write (Alt);
      end if;
      Engine.Output.Write (Engine.Tags (Img_End).all);
      Engine.Empty_Line := False;
   end Render_Image;

   --  Render a quote.
   procedure Render_Quote (Engine : in out Wiki_Renderer;
                           Title  : in Strings.WString;
                           Attrs  : in Attributes.Attribute_List) is
      Link : constant Strings.WString := Attributes.Get_Attribute (Attrs, "cite");
      Lang : constant Strings.WString := Attributes.Get_Attribute (Attrs, "lang");
   begin
      Engine.Output.Write (Engine.Tags (Quote_Start).all);
      Engine.Output.Write (Title);
      if Engine.Allow_Link_Language and Lang'Length > 0 then
         Engine.Output.Write (Engine.Tags (Quote_Separator).all);
         Engine.Output.Write (Lang);
      end if;
      if Link'Length > 0 then
         if Lang'Length = 0 then
            Engine.Output.Write (Engine.Tags (Quote_Separator).all);
         end if;
         Engine.Output.Write (Engine.Tags (Quote_Separator).all);
         Engine.Output.Write (Link);
      end if;
      Engine.Output.Write (Engine.Tags (Quote_End).all);
      Engine.Empty_Line := False;
   end Render_Quote;

   --  Set the text style format.
   procedure Set_Format (Engine : in out Wiki_Renderer;
                         Format : in Format_Map) is
      F : Boolean;
   begin
      for I in Format'Range loop
         F := Format (I) or Engine.Current_Style (I);
         if Engine.Format (I) /= F then
            if F then
               Engine.Output.Write (Engine.Style_Start_Tags (I).all);
               Engine.Format (I) := True;
            else
               Engine.Output.Write (Engine.Style_End_Tags (I).all);
               Engine.Format (I) := False;
            end if;
         end if;
      end loop;
   end Set_Format;

   --  Add a text block with the given format.
   procedure Render_Text (Engine : in out Wiki_Renderer;
                          Text     : in Strings.WString;
                          Format   : in Format_Map) is
      Start        : Natural := Text'First;
      Last         : Natural := Text'Last;
      Apply_Format : Boolean := True;
      Check_Escape : Boolean := True;
   begin
      if Engine.Keep_Content or Engine.Empty_Line then
         while Start <= Text'Last and then Helpers.Is_Space_Or_Newline (Text (Start)) loop
            Start := Start + 1;
         end loop;
      end if;
      if Engine.Keep_Content then
         while Last >= Start and then Helpers.Is_Space_Or_Newline (Text (Last)) loop
            Last := Last - 1;
         end loop;
         Append (Engine.Content, Text (Start .. Last));
      else
         for I in Start .. Last loop
            if Helpers.Is_Newline (Text (I)) then
               if Engine.Empty_Line = False then
                  if Apply_Format and then Engine.Format /= Empty_Formats then
                     Engine.Set_Format (Empty_Formats);
                  end if;
                  Engine.Output.Write (LF);
                  Engine.Empty_Line := True;
               end if;
            elsif not Engine.Empty_Line or else not Helpers.Is_Space (Text (I)) then
               if Engine.Empty_Line and Engine.Quote_Level > 0 then
                  for Level in 1 .. Engine.Quote_Level loop
                     Engine.Output.Write (Engine.Tags (Blockquote_Start).all);
                  end loop;
               end if;
               if Engine.In_List then
                  if Engine.UL_List_Level > Engine.OL_List_Level then
                     Engine.Add_List_Item (Engine.UL_List_Level, False);
                  else
                     Engine.Add_List_Item (Engine.OL_List_Level, True);
                  end if;
                  Engine.In_List := False;
               end if;
               if Apply_Format then
                  Engine.Set_Format (Format);
                  Apply_Format := False;
               end if;
               if Check_Escape then
                  if Ada.Strings.Wide_Wide_Maps.Is_In (Text (I), Engine.Escape_Set) then
                     Engine.Output.Write (Engine.Tags (Escape_Rule).all);
                     Check_Escape := False;
                  end if;
               else
                  Check_Escape := True;
               end if;
               Engine.Output.Write (Text (I));
               Engine.Empty_Line := False;
            end if;
         end loop;
      end if;
   end Render_Text;

   --  ------------------------------
   --  Render a text block that is pre-formatted.
   --  ------------------------------
   procedure Render_Preformatted (Engine : in out Wiki_Renderer;
                                  Text   : in Strings.WString;
                                  Format : in Strings.WString) is
      pragma Unreferenced (Format);

      Col           : Natural := 2;
   begin
      Engine.New_Line;
      Engine.Output.Write (Engine.Tags (Preformat_Start).all);
      for I in Text'Range loop
         if Helpers.Is_Newline (Text (I)) then
            Col := 0;
         else
            Col := Col + 1;
         end if;
         if I = Text'First and then Col > 0 then
            Engine.Output.Write (LF);
            Col := 0;
         end if;
         Engine.Output.Write (Text (I));
      end loop;
      if Col /= 0 then
         Engine.New_Line;
      end if;
      Engine.Output.Write (Engine.Tags (Preformat_End).all);
      Engine.New_Line;
      Engine.Empty_Line := True;
   end Render_Preformatted;

   procedure Start_Keep_Content (Engine : in out Wiki_Renderer) is
   begin
      Engine.Keep_Content := True;
      Engine.Content := Strings.To_UString ("");
   end Start_Keep_Content;

   procedure Render_Tag (Engine : in out Wiki_Renderer;
                         Doc    : in Documents.Document;
                         Node   : in Nodes.Node_Type) is
   begin
      case Node.Tag_Start is
         when BR_TAG =>
            Engine.Output.Write (Engine.Tags (Line_Break).all);
            Engine.Empty_Line := False;
            return;

         when HR_TAG =>
            Engine.Close_Paragraph;
            Engine.Output.Write (Engine.Tags (Horizontal_Rule).all);
            return;

         when H1_TAG | H2_TAG
            | H3_TAG | H4_TAG
            | H5_TAG | H6_TAG =>
            Engine.Start_Keep_Content;

         when IMG_TAG =>
            Engine.Render_Image (Link  => Get_Attribute (Node.Attributes, "src"),
                                 Attrs => Node.Attributes);

         when A_TAG | Q_TAG =>
            Engine.Start_Keep_Content;

         when B_TAG | EM_TAG | STRONG_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (BOLD) := True;
            end if;

         when I_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (ITALIC) := True;
            end if;

         when U_TAG | TT_TAG | CODE_TAG | KBD_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (CODE) := True;
            end if;

         when SUP_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUPERSCRIPT) := True;
            end if;

         when SUB_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUBSCRIPT) := True;
            end if;

         when P_TAG =>
            Engine.Set_Format (Empty_Formats);
            if not Engine.Empty_Line then
               Engine.New_Line;
            end if;

         when PRE_TAG =>
            Engine.Start_Keep_Content;

         when UL_TAG =>
            Engine.UL_List_Level := Engine.UL_List_Level + 1;

         when OL_TAG =>
            Engine.OL_List_Level := Engine.OL_List_Level + 1;

         when LI_TAG =>
            Engine.In_List := True;

         when BLOCKQUOTE_TAG =>
            Engine.Set_Format (Empty_Formats);
            if not Engine.Empty_Line then
               Engine.New_Line;
            end if;
            Engine.Quote_Level := Engine.Quote_Level + 1;

         when others =>
            null;

      end case;
      Engine.Render (Doc, Node.Children);

      case Node.Tag_Start is
         when H1_TAG =>
            Engine.Render_Header (Strings.To_WString (Engine.Content), 1);
            Engine.Keep_Content := False;

         when H2_TAG =>
            Engine.Render_Header (Strings.To_WString (Engine.Content), 2);
            Engine.Keep_Content := False;

         when H3_TAG =>
            Engine.Render_Header (Strings.To_WString (Engine.Content), 3);
            Engine.Keep_Content := False;

         when H4_TAG =>
            Engine.Render_Header (Strings.To_WString (Engine.Content), 4);
            Engine.Keep_Content := False;

         when H5_TAG =>
            Engine.Render_Header (Strings.To_WString (Engine.Content), 5);
            Engine.Keep_Content := False;

         when H6_TAG =>
            Engine.Render_Header (Strings.To_WString (Engine.Content), 6);
            Engine.Keep_Content := False;

         when A_TAG =>
            Engine.Render_Link (Name     => Strings.To_WString (Engine.Content),
                                Attrs    => Node.Attributes);
            Engine.Keep_Content := False;

         when Q_TAG =>
            Engine.Render_Quote (Title    => Strings.To_WString (Engine.Content),
                                 Attrs    => Node.Attributes);
            Engine.Keep_Content := False;

         when P_TAG =>
            Engine.Set_Format (Empty_Formats);
            Engine.New_Line;

         when B_TAG | EM_TAG | STRONG_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (BOLD) := False;
            end if;

         when I_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (ITALIC) := False;
            end if;

         when U_TAG | TT_TAG | CODE_TAG | KBD_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (CODE) := False;
            end if;

         when SUP_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUPERSCRIPT) := False;
            end if;

         when SUB_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUBSCRIPT) := False;
            end if;

         when PRE_TAG =>
            Engine.Render_Preformatted (Strings.To_WString (Engine.Content), "");
            Engine.Keep_Content := False;

         when UL_TAG =>
            Engine.UL_List_Level := Engine.UL_List_Level - 1;

         when OL_TAG =>
            Engine.OL_List_Level := Engine.OL_List_Level - 1;

         when LI_TAG =>
            Engine.In_List := False;

         when BLOCKQUOTE_TAG =>
            Engine.Set_Format (Empty_Formats);
            if not Engine.Empty_Line then
               Engine.New_Line;
            end if;
            Engine.Quote_Level := Engine.Quote_Level - 1;

         when others =>
            null;

      end case;
   end Render_Tag;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Engine : in out Wiki_Renderer;
                     Doc    : in Documents.Document) is
      pragma Unreferenced (Doc);
   begin
      Engine.Set_Format (Empty_Formats);
   end Finish;

   procedure Close_Paragraph (Engine : in out Wiki_Renderer) is
   begin
      if not Engine.Empty_Line then
         Engine.New_Line;
      end if;
   end Close_Paragraph;

   procedure Open_Paragraph (Engine : in out Wiki_Renderer) is
   begin
      null;
   end Open_Paragraph;

end Wiki.Render.Wiki;
