-----------------------------------------------------------------------
--  wiki-render-wiki -- Wiki to Wiki renderer
--  Copyright (C) 2015, 2016, 2018, 2020, 2022 Stephane Carrez
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
with Util.Strings;
package body Wiki.Render.Wiki is

   pragma Wide_Character_Encoding (UTF8);

   use Helpers;
   use type Nodes.Node_List_Access;

   Regular_Map : constant Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping
     := Ada.Strings.Wide_Wide_Maps.To_Mapping
       (From => " ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöùúûüýÿ",
        To   => "-SZszYAAAAAACEEEEIIIIDNOOOOOUUUUYaaaaaaceeeeiiiidnooooouuuuyy");

   HEADER_CREOLE             : aliased constant Strings.WString := "=";
   BOLD_CREOLE               : aliased constant Strings.WString := "**";
   ITALIC_CREOLE             : aliased constant Strings.WString := "//";
   SUPERSCRIPT_CREOLE        : aliased constant Strings.WString := "^^";
   SUBSCRIPT_CREOLE          : aliased constant Strings.WString := ",,";
   UNDERLINE_CREOLE          : aliased constant Strings.WString := "__";
   LINE_BREAK_CREOLE         : aliased constant Strings.WString := "%%%" & LF;
   IMG_START_CREOLE          : aliased constant Strings.WString := "{{";
   IMG_END_CREOLE            : aliased constant Strings.WString := "}}";
   LINK_START_CREOLE         : aliased constant Strings.WString := "[[";
   LINK_END_CREOLE           : aliased constant Strings.WString := "]]";
   PREFORMAT_START_CREOLE    : aliased constant Strings.WString := "{{{";
   PREFORMAT_END_CREOLE      : aliased constant Strings.WString := "}}}" & LF;
   HORIZONTAL_RULE_CREOLE    : aliased constant Strings.WString := "----" & LF & LF;
   LINK_SEPARATOR_CREOLE     : aliased constant Strings.WString := "|";
   LIST_ITEM_CREOLE          : aliased constant Strings.WString := "*";
   LIST_ORDERED_ITEM_CREOLE  : aliased constant Strings.WString := "#";
   ESCAPE_CREOLE             : aliased constant Strings.WString := "~";

   HEADER_DOTCLEAR           : aliased constant Strings.WString := "!";
   BOLD_DOTCLEAR             : aliased constant Strings.WString := "__";
   ITALIC_DOTCLEAR           : aliased constant Strings.WString := "''";
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

   BOLD_MARKDOWN             : aliased constant Strings.WString := "**";
   ITALIC_MARKDOWN           : aliased constant Strings.WString := "__";
   CODE_MARKDOWN             : aliased constant Strings.WString := "`";
   HEADER_MARKDOWN           : aliased constant Strings.WString := "#";
   ESCAPE_MARKDOWN           : aliased constant Strings.WString := "\";
   QUOTE_MARKDOWN            : aliased constant Strings.WString := ">";
   PREFORMAT_START_MARKDOWN  : aliased constant Strings.WString := "```";
   PREFORMAT_END_MARKDOWN    : aliased constant Strings.WString := "```" & LF;
   HORIZONTAL_RULE_MARKDOWN  : aliased constant Strings.WString := "----" & LF & LF;
   IMG_START_MARKDOWN        : aliased constant Strings.WString := "![";
   IMG_END_MARKDOWN          : aliased constant Strings.WString := ")";
   LINK_START_MARKDOWN       : aliased constant Strings.WString := "[";
   LINK_SEPARATOR_MARKDOWN   : aliased constant Strings.WString := "](";
   LINK_END_MARKDOWN         : aliased constant Strings.WString := ")";
   LIST_ITEM_MARKDOWN        : aliased constant Strings.WString := "*";
   LIST_ORDERED_ITEM_MARKDOWN : aliased constant Strings.WString := "*";
   LINE_BREAK_MARKDOWN       : aliased constant Strings.WString := "\" & LF;

   BOLD_TEXTILE              : aliased constant Strings.WString := "*";
   ITALIC_TEXTILE            : aliased constant Strings.WString := "_";
   CODE_TEXTILE              : aliased constant Strings.WString := "@";
   SUPERSCRIPT_TEXTILE       : aliased constant Strings.WString := "^";
   SUBSCRIPT_TEXTILE         : aliased constant Strings.WString := "~";
   HEADER_START_TEXTILE      : aliased constant Strings.WString := "h";
   HEADER_END_TEXTILE        : aliased constant Strings.WString := ".";
   --  ESCAPE_TEXTILE            : aliased constant Strings.WString := "\";
   QUOTE_TEXTILE             : aliased constant Strings.WString := "bq.";
   PREFORMAT_START_TEXTILE   : aliased constant Strings.WString := "<pre>";
   PREFORMAT_END_TEXTILE     : aliased constant Strings.WString := "</pre>" & LF;
   HORIZONTAL_RULE_TEXTILE   : aliased constant Strings.WString := "----" & LF & LF;
   IMG_START_TEXTILE         : aliased constant Strings.WString := "!";
   IMG_END_TEXTILE           : aliased constant Strings.WString := "!";
   --  IMG_SEPARATOR_TEXTILE     : aliased constant Strings.WString := "(";
   LINK_START_TEXTILE        : aliased constant Strings.WString := "[[";
   LINK_SEPARATOR_TEXTILE    : aliased constant Strings.WString := "|";
   LINK_END_TEXTILE          : aliased constant Strings.WString := "]]";
   LIST_ITEM_TEXTILE         : aliased constant Strings.WString := "*";
   LIST_ORDERED_ITEM_TEXTILE : aliased constant Strings.WString := "#";
   --  LINE_BREAK_TEXTILE        : aliased constant Strings.WString := "\" & LF;

   LINE_BREAK_MEDIAWIKI      : aliased constant Strings.WString := "<br />" & LF;
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
            Engine.Link_First := True;
            Engine.Html_Table := False;
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
            Engine.Link_First := True;
            Engine.Html_Blockquote := True;
            Engine.Html_Table := True;
            Engine.Escape_Set := Ada.Strings.Wide_Wide_Maps.To_Set ("'+_-*(){}][!");

         when SYNTAX_MARKDOWN =>
            Engine.Style_Start_Tags (BOLD)   := BOLD_MARKDOWN'Access;
            Engine.Style_End_Tags (BOLD)     := BOLD_MARKDOWN'Access;
            Engine.Style_Start_Tags (ITALIC) := ITALIC_MARKDOWN'Access;
            Engine.Style_End_Tags (ITALIC)   := ITALIC_MARKDOWN'Access;
            Engine.Style_Start_Tags (CODE)   := CODE_MARKDOWN'Access;
            Engine.Style_End_Tags (CODE)     := CODE_MARKDOWN'Access;
            Engine.Tags (Header_Start)       := HEADER_MARKDOWN'Access;
            Engine.Tags (Line_Break)         := LINE_BREAK_MARKDOWN'Access;
            Engine.Tags (Img_Start)          := IMG_START_MARKDOWN'Access;
            Engine.Tags (Img_End)            := IMG_END_MARKDOWN'Access;
            --  Engine.Tags (Img_Separator)      := LINK_SEPARATOR_MARKDOWN'Access;
            Engine.Tags (Link_Start)         := LINK_START_MARKDOWN'Access;
            Engine.Tags (Link_End)           := LINK_END_MARKDOWN'Access;
            Engine.Tags (Link_Separator)     := LINK_SEPARATOR_MARKDOWN'Access;
            Engine.Tags (List_Item)          := LIST_ITEM_MARKDOWN'Access;
            Engine.Tags (List_Ordered_Item)  := LIST_ORDERED_ITEM_MARKDOWN'Access;
            Engine.Tags (Preformat_Start)    := PREFORMAT_START_MARKDOWN'Access;
            Engine.Tags (Preformat_End)      := PREFORMAT_END_MARKDOWN'Access;
            Engine.Tags (Horizontal_Rule)    := HORIZONTAL_RULE_MARKDOWN'Access;
            Engine.Tags (Escape_Rule)        := ESCAPE_MARKDOWN'Access;
            Engine.Tags (Blockquote_Start)   := QUOTE_MARKDOWN'Access;
            Engine.Link_First := False;
            Engine.Html_Blockquote := False;
            Engine.Html_Table := False;
            Engine.Escape_Set := Ada.Strings.Wide_Wide_Maps.To_Set ("\`*_{}[]()#+-!");

         when SYNTAX_TEXTILE =>
            Engine.Style_Start_Tags (BOLD)   := BOLD_TEXTILE'Access;
            Engine.Style_End_Tags (BOLD)     := BOLD_TEXTILE'Access;
            Engine.Style_Start_Tags (ITALIC) := ITALIC_TEXTILE'Access;
            Engine.Style_End_Tags (ITALIC)   := ITALIC_TEXTILE'Access;
            Engine.Style_Start_Tags (CODE)   := CODE_TEXTILE'Access;
            Engine.Style_End_Tags (CODE)     := CODE_TEXTILE'Access;
            Engine.Style_Start_Tags (SUPERSCRIPT) := SUPERSCRIPT_TEXTILE'Access;
            Engine.Style_End_Tags (SUPERSCRIPT)   := SUPERSCRIPT_TEXTILE'Access;
            Engine.Style_Start_Tags (SUBSCRIPT) := SUBSCRIPT_TEXTILE'Access;
            Engine.Style_End_Tags (SUBSCRIPT)   := SUBSCRIPT_TEXTILE'Access;
            Engine.Tags (Header_Start)       := HEADER_START_TEXTILE'Access;
            Engine.Tags (Header_End)         := HEADER_END_TEXTILE'Access;
            Engine.Tags (Line_Break)         := LINE_BREAK_MARKDOWN'Access;
            Engine.Tags (Img_Start)          := IMG_START_TEXTILE'Access;
            Engine.Tags (Img_End)            := IMG_END_TEXTILE'Access;
            Engine.Tags (Link_Start)         := LINK_START_TEXTILE'Access;
            Engine.Tags (Link_End)           := LINK_END_TEXTILE'Access;
            Engine.Tags (Link_Separator)     := LINK_SEPARATOR_TEXTILE'Access;
            Engine.Tags (List_Item)          := LIST_ITEM_TEXTILE'Access;
            Engine.Tags (List_Ordered_Item)  := LIST_ORDERED_ITEM_TEXTILE'Access;
            Engine.Tags (Preformat_Start)    := PREFORMAT_START_TEXTILE'Access;
            Engine.Tags (Preformat_End)      := PREFORMAT_END_TEXTILE'Access;
            Engine.Tags (Horizontal_Rule)    := HORIZONTAL_RULE_TEXTILE'Access;
            Engine.Tags (Escape_Rule)        := ESCAPE_MARKDOWN'Access;
            Engine.Tags (Blockquote_Start)   := QUOTE_TEXTILE'Access;
            Engine.Link_First := True;
            Engine.Html_Blockquote := False;
            Engine.Html_Table := False;
            Engine.Escape_Set := Ada.Strings.Wide_Wide_Maps.To_Set ("\`*_{}[]()#+-!");

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
            Engine.Link_First := True;
            Engine.Html_Table := False;

      end case;
   end Set_Output_Stream;

   --  ------------------------------
   --  Emit a new line.
   --  ------------------------------
   procedure New_Line (Engine   : in out Wiki_Renderer;
                       Optional : in Boolean := False) is
   begin
      if Optional and then (Engine.Line_Count = 0 or else Engine.Empty_Line) then
         return;
      end if;
      Engine.Set_Format (Empty_Formats);
      Engine.Output.Write (LF);
      if Engine.Quote_Level > 0 then
         for Level in 1 .. Engine.Quote_Level loop
            Engine.Output.Write (Engine.Tags (Blockquote_Start).all);
         end loop;
         Engine.Output.Write (' ');
      end if;
      Engine.Empty_Previous_Line := Engine.Empty_Line;
      Engine.Empty_Line := True;
      Engine.Need_Newline := False;
      Engine.Need_Space := False;
      Engine.Line_Count := Engine.Line_Count + 1;
   end New_Line;

   procedure Write_Optional_Space (Engine : in out Wiki_Renderer) is
   begin
      if Engine.Need_Space then
         Engine.Need_Space := False;
         Engine.Output.Write (' ');
      end if;
   end Write_Optional_Space;

   procedure Need_Separator_Line (Engine   : in out Wiki_Renderer) is
   begin
      if not Engine.Empty_Line then
         Engine.Set_Format (Empty_Formats);
         Engine.Empty_Previous_Line := False;
         Engine.Output.Write (LF);
         Engine.Empty_Line := True;
         Engine.Line_Count := Engine.Line_Count + 1;
      end if;
      Engine.Need_Newline := True;
   end Need_Separator_Line;

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
            Engine.Render_Header (Doc, Node, Node.Level, Node.Content);

         when Nodes.N_LINE_BREAK =>
            Engine.Output.Write (Engine.Tags (Line_Break).all);
            Engine.Empty_Line := False;
            Engine.Need_Space := False;

         when Nodes.N_HORIZONTAL_RULE =>
            Engine.Close_Paragraph;
            Engine.Output.Write (Engine.Tags (Horizontal_Rule).all);

         when Nodes.N_PARAGRAPH =>
            Engine.Add_Paragraph;

         when Nodes.N_PREFORMAT =>
            Engine.Render_Preformatted (Node.Preformatted,
                                        Strings.To_WString (Node.Language));

         when Nodes.N_LIST_START =>
            Engine.Render_List_Start (False, 0);

         when Nodes.N_NUM_LIST_START =>
            Engine.Render_List_Start (True, Node.Level);

         when Nodes.N_LIST_END =>
            Engine.Render_List_End ("");

         when Nodes.N_NUM_LIST_END =>
            Engine.Render_List_End ("");

         when Nodes.N_LIST_ITEM =>
            Engine.Render_List_Item_Start;

         when Nodes.N_LIST_ITEM_END =>
            Engine.Render_List_Item_End;

         when Nodes.N_TEXT =>
            declare
               F : Format_Map := Node.Format;
            begin
               for I in F'Range loop
                  F (I) := F (I) or else Engine.Current_Style (I);
               end loop;
               Engine.Render_Text (Node.Text, F);
            end;

         when Nodes.N_LINK =>
            Engine.Render_Link (Node.Title, Node.Link_Attr);

         when Nodes.N_IMAGE =>
            Engine.Render_Image (Node.Title, Node.Link_Attr);

         when Nodes.N_QUOTE =>
            Engine.Render_Quote (Node.Title, Node.Link_Attr);

         when Nodes.N_BLOCKQUOTE =>
            Engine.Add_Blockquote (Node.Level);

         when Nodes.N_TAG_START =>
            Engine.Render_Tag (Doc, Node);

         when Nodes.N_TOC =>
            Engine.Render_TOC (Doc, Node.Level);

         when Nodes.N_TOC_ENTRY =>
            null;

         when Nodes.N_TOC_DISPLAY =>
            Engine.Render_TOC (Doc, 3);

         when Nodes.N_TABLE =>
            Engine.Render_Table (Doc, Node);

         when Nodes.N_ROW =>
            Engine.Render_Row (Doc, Node);

         when Nodes.N_COLUMN =>
            Engine.Render_Column (Doc, Node);

         when others =>
            null;

      end case;
   end Render;

   --  ------------------------------
   --  Add a section header in the document.
   --  ------------------------------
   procedure Render_Header (Engine : in out Wiki_Renderer;
                            Doc    : in Documents.Document;
                            Node   : in Nodes.Node_Type;
                            Level  : in Natural;
                            List   : in Nodes.Node_List_Access) is
      Count : Natural := Level;
   begin
      Engine.Set_Format (Empty_Formats);
      if not Engine.Empty_Line then
         Engine.Output.Write (LF);
      end if;
      Engine.Close_Paragraph;
      Engine.Output.Write (LF);
      if Engine.Invert_Header_Level then
         if Count > 5 then
            Count := 5;
         end if;
         Count := 6 - Count;
      elsif Count > 6 then
         Count := 6;
      end if;

      --  Textile is using a special form: h<N>.
      if Engine.Syntax = SYNTAX_TEXTILE then
         Engine.Output.Write (Engine.Tags (Header_Start).all);
         Engine.Output.Write (Strings.To_WString (Util.Strings.Image (Count)));
         Engine.Output.Write (Engine.Tags (Header_End).all);
         Engine.Output.Write (' ');
         Engine.Render (Doc, Node.Content);
         Engine.New_Line;
         return;
      end if;

      for I in 1 .. Count loop
         Engine.Output.Write (Engine.Tags (Header_Start).all);
      end loop;
      Engine.Output.Write (' ');
      if List /= null then
         Engine.In_Header := True;
         Engine.Render (Doc, List);
         Engine.In_Header := False;
      end if;
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
      if Engine.Line_Count > 0 then
         if not Engine.Empty_Line then
            Engine.New_Line;
         end if;
         Engine.New_Line;
      end if;
   end Add_Paragraph;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Add_Blockquote (Engine : in out Wiki_Renderer;
                             Level    : in Natural) is
   begin
      Engine.Quote_Level := Level;
      Engine.New_Line;
   end Add_Blockquote;

   --  ------------------------------
   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Add_List_Item (Engine : in out Wiki_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean) is
   begin
      Engine.Need_Space := False;
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

   procedure Render_List_Start (Engine   : in out Wiki_Renderer;
                                Numbered : in Boolean;
                                Level    : in Natural) is
      pragma Unreferenced (Numbered);
   begin
      Engine.Need_Paragraph := False;
      Engine.Close_Paragraph;
      Engine.New_Line (False);
      Engine.List_Index := Engine.List_Index + 1;
      Engine.List_Levels (Engine.List_Index) := Level;
      Engine.Indent_Level := Engine.Indent_Level + 2;
   end Render_List_Start;

   procedure Render_List_End (Engine   : in out Wiki_Renderer;
                              Tag      : in String) is
      pragma Unreferenced (Tag);
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
   procedure Render_List_Item_Start (Engine   : in out Wiki_Renderer) is
   begin
      Engine.Need_Paragraph := False;
      Engine.Close_Paragraph;

      if Engine.List_Levels (Engine.List_Index) > 0 then
         Engine.Output.Write
           (Strings.To_WString (Util.Strings.Image (Engine.List_Levels (Engine.List_Index))));
         Engine.List_Levels (Engine.List_Index) := Engine.List_Levels (Engine.List_Index) + 1;
         Engine.Output.Write (") ");
         Engine.Indent_Level := Engine.Indent_Level + 4;
      else
         Engine.Output.Write (Engine.Tags (List_Item).all);
         Engine.Output.Write (" ");
         Engine.Indent_Level := Engine.Indent_Level + 2;
      end if;
   end Render_List_Item_Start;

   procedure Render_List_Item_End (Engine   : in out Wiki_Renderer) is
   begin
      Engine.Need_Space := False;
      Engine.Close_Paragraph;
      Engine.Need_Paragraph := False;
      if Engine.List_Levels (Engine.List_Index) > 0 then
         Engine.Indent_Level := Engine.Indent_Level - 4;
      else
         Engine.Indent_Level := Engine.Indent_Level - 2;
      end if;
   end Render_List_Item_End;

   procedure Write_Link (Engine : in out Wiki_Renderer;
                         Link   : in Strings.WString) is
   begin
      for C of Link loop
         Engine.Output.Write (Ada.Strings.Wide_Wide_Maps.Value (Regular_Map, C));
      end loop;
   end Write_Link;

   --  ------------------------------
   --  Render a link.
   --  ------------------------------
   procedure Render_Link (Engine   : in out Wiki_Renderer;
                          Name     : in Strings.WString;
                          Attrs    : in Attributes.Attribute_List) is

      Link : constant Strings.WString := Attributes.Get_Attribute (Attrs, "href");
      Lang : constant Strings.WString := Attributes.Get_Attribute (Attrs, "lang");

   begin
      if Engine.Empty_Line and then Engine.In_List then
         if Engine.UL_List_Level + Engine.OL_List_Level > 0 then
            if Engine.UL_List_Level > Engine.OL_List_Level then
               Engine.Add_List_Item (Engine.UL_List_Level, False);
            else
               Engine.Add_List_Item (Engine.OL_List_Level, True);
            end if;
         end if;
         Engine.In_List := False;
      end if;
      Engine.Write_Optional_Space;
      if Engine.Syntax = SYNTAX_DOTCLEAR and then Engine.In_Header then
         Engine.Output.Write (Name);
         return;
      end if;
      Engine.Output.Write (Engine.Tags (Link_Start).all);
      if Engine.Link_First then
         Engine.Write_Link (Link);
         if Name'Length > 0 then
            Engine.Output.Write (Engine.Tags (Link_Separator).all);
            Engine.Output.Write (Name);
         end if;
         if Engine.Allow_Link_Language and then Lang'Length > 0 then
            Engine.Output.Write (Engine.Tags (Link_Separator).all);
            Engine.Output.Write (Lang);
         end if;
      else
         Engine.Output.Write (Name);
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
         Engine.Write_Link (Link);
      end if;
      Engine.Output.Write (Engine.Tags (Link_End).all);
      Engine.Empty_Line := False;
   end Render_Link;

   --  Render an image.
   procedure Render_Image (Engine : in out Wiki_Renderer;
                           Title  : in Strings.WString;
                           Attrs  : in Attributes.Attribute_List) is
      Src  : constant Strings.WString := Attributes.Get_Attribute (Attrs, "src");
   begin
      Engine.Write_Optional_Space;
      Engine.Output.Write (Engine.Tags (Img_Start).all);
      if Engine.Link_First then
         Engine.Output.Write (Src);
         if Title'Length > 0 then
            Engine.Output.Write (Engine.Tags (Link_Separator).all);
            Engine.Output.Write (Title);
         end if;
      else
         Engine.Output.Write (Title);
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
         Engine.Output.Write (Src);
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
      if Engine.Allow_Link_Language and then Lang'Length > 0 then
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
   begin
      if Engine.Format /= Format then
         for I in Format'Range loop
            if Format (I) xor Engine.Format (I) then
               if Format (I) then
                  Engine.Output.Write (Engine.Style_Start_Tags (I).all);
                  Engine.Format (I) := True;
               else
                  Engine.Output.Write (Engine.Style_End_Tags (I).all);
                  Engine.Format (I) := False;
               end if;
            end if;
         end loop;
         Engine.Need_Space := False;
      end if;
   end Set_Format;

   --  Add a text block with the given format.
   procedure Render_Text (Engine : in out Wiki_Renderer;
                          Text     : in Strings.WString;
                          Format   : in Format_Map) is
      Start        : Natural := Text'First;
      Last         : Natural := Text'Last;
      Apply_Format : Boolean := True;
      Last_Char    : Strings.WChar;
   begin
      if Engine.Keep_Content > 0 or else Engine.Empty_Line then
         while Start <= Text'Last and then Helpers.Is_Space_Or_Newline (Text (Start)) loop
            Start := Start + 1;
         end loop;
      end if;
      if Engine.Keep_Content > 0 then
         while Last >= Start and then Helpers.Is_Space_Or_Newline (Text (Last)) loop
            Last := Last - 1;
         end loop;
         if Engine.Need_Space then
            Append (Engine.Content, ' ');
            Engine.Need_Space := False;
         end if;
         Append (Engine.Content, Text (Start .. Last));
         Engine.Need_Space := True;
      else
         --  Some rules:
         --  o avoid several consecutive LF
         --  o drop spaces at beginning of a text (because it can be interpreted)
         --  o emit the blockquote if we are at beginning of a new line
         --  o emit the list item if we are at beginning of a new line
         Last_Char := ' ';
         for I in Start .. Last loop
            Last_Char := Text (I);
            if Helpers.Is_Newline (Last_Char) then
               if Engine.Empty_Line = False then
                  if Apply_Format and then Engine.Format /= Empty_Formats then
                     Engine.Set_Format (Empty_Formats);
                  end if;
                  Engine.Write_Optional_Space;
                  Engine.New_Line;
               end if;
            elsif not Engine.Empty_Line or else not Helpers.Is_Space (Last_Char) then
               --  if Engine.Empty_Line and Engine.Quote_Level > 0 then
               --   for Level in 1 .. Engine.Quote_Level loop
               --      Engine.Output.Write (Engine.Tags (Blockquote_Start).all);
               --   end loop;
               --  end if;
               if Engine.In_List and then Engine.UL_List_Level + Engine.OL_List_Level > 0 then
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
                  if Is_Space (Last_Char) then
                     Engine.Need_Space := False;
                  else
                     Engine.Write_Optional_Space;
                  end if;
               end if;
               if Ada.Strings.Wide_Wide_Maps.Is_In (Last_Char, Engine.Escape_Set) then
                  Engine.Output.Write (Engine.Tags (Escape_Rule).all);
               end if;
               if Last_Char = NBSP then
                  Last_Char := ' ';
               end if;
               Engine.Output.Write (Last_Char);
               Engine.Empty_Line := False;
            end if;
         end loop;
         if not Helpers.Is_Space_Or_Newline (Last_Char) and then not Engine.Empty_Line then
            Engine.Need_Space := True;
         end if;
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
            Engine.New_Line (False);
            Col := 0;
         end if;
         Engine.Output.Write (Text (I));
      end loop;
      if Col /= 0 then
         Engine.New_Line;
      end if;
      Engine.Output.Write (Engine.Tags (Preformat_End).all);
      Engine.New_Line;
   end Render_Preformatted;

   procedure Start_Keep_Content (Engine : in out Wiki_Renderer) is
   begin
      if Engine.Need_Space then
         Engine.Write_Optional_Space;
      end if;
      Engine.Keep_Content := Engine.Keep_Content + 1;
      if Engine.Keep_Content = 1 then
         Engine.Content := Strings.To_UString ("");
         Engine.Need_Space := False;
      end if;
   end Start_Keep_Content;

   procedure Render_Tag (Engine : in out Wiki_Renderer;
                         Doc    : in Documents.Document;
                         Node   : in Nodes.Node_Type) is
   begin
      case Node.Tag_Start is
         when H1_TAG =>
            Engine.Need_Space := False;
            Engine.Render_Header (Doc, Node, 1, Node.Children);
            return;

         when H2_TAG =>
            Engine.Need_Space := False;
            Engine.Render_Header (Doc, Node, 2, Node.Children);
            return;

         when H3_TAG =>
            Engine.Need_Space := False;
            Engine.Render_Header (Doc, Node, 3, Node.Children);
            return;

         when H4_TAG =>
            Engine.Need_Space := False;
            Engine.Render_Header (Doc, Node, 4, Node.Children);
            return;

         when H5_TAG =>
            Engine.Need_Space := False;
            Engine.Render_Header (Doc, Node, 5, Node.Children);
            return;

         when H6_TAG =>
            Engine.Need_Space := False;
            Engine.Render_Header (Doc, Node, 6, Node.Children);
            return;

         when BR_TAG =>
            Engine.Output.Write (Engine.Tags (Line_Break).all);
            Engine.Empty_Line := False;
            Engine.Need_Space := False;
            return;

         when HR_TAG =>
            Engine.Close_Paragraph;
            Engine.Output.Write (Engine.Tags (Horizontal_Rule).all);
            return;

         when IMG_TAG =>
            Engine.Render_Image (Title => Get_Attribute (Node.Attributes, "alt"),
                                 Attrs => Node.Attributes);

         when A_TAG | Q_TAG =>
            Engine.Start_Keep_Content;

         when B_TAG | EM_TAG | STRONG_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (BOLD) := True;
            end if;

         when I_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (ITALIC) := True;
            end if;

         when U_TAG | TT_TAG | CODE_TAG | KBD_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (CODE) := True;
            end if;

         when SUP_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (SUPERSCRIPT) := True;
            end if;

         when SUB_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (SUBSCRIPT) := True;
            end if;

         when P_TAG =>
            Engine.Set_Format (Empty_Formats);
            Engine.Close_Paragraph;
            Engine.Need_Separator_Line;
            if Engine.Line_Count > 0 then
               Engine.New_Line;
            end if;

         when PRE_TAG =>
            Engine.Start_Keep_Content;

         when UL_TAG =>
            if Engine.UL_List_Level = 0 then
               Engine.Need_Separator_Line;
            end if;
            Engine.Close_Paragraph;
            Engine.UL_List_Level := Engine.UL_List_Level + 1;

         when OL_TAG =>
            if Engine.OL_List_Level = 0 then
               Engine.Need_Separator_Line;
            end if;
            Engine.Close_Paragraph;
            Engine.OL_List_Level := Engine.OL_List_Level + 1;

         when LI_TAG =>
            Engine.In_List := True;
            if Engine.UL_List_Level + Engine.OL_List_Level = 0 then
               Engine.Need_Separator_Line;
               Engine.Close_Paragraph;
            end if;

         when BLOCKQUOTE_TAG =>
            Engine.Set_Format (Empty_Formats);
            --  Engine.Need_Separator_Line;
            Engine.Close_Paragraph;
            Engine.Quote_Level := Engine.Quote_Level + 1;
            if Engine.Html_Blockquote then
               --  Make sure there is en empty line before the HTML <blockquote>.
               Engine.Output.Write (LF & "<blockquote>" & LF);
            end if;

         when others =>
            null;

      end case;
      Engine.Render (Doc, Node.Children);

      case Node.Tag_Start is

         when A_TAG =>
            if Engine.Keep_Content = 1 then
               Engine.Need_Space := False;
               Engine.Render_Link (Name     => Strings.To_WString (Engine.Content),
                                   Attrs    => Node.Attributes);
            end if;
            Engine.Keep_Content := Engine.Keep_Content - 1;

         when Q_TAG =>
            if Engine.Keep_Content = 1 then
               Engine.Need_Space := False;
               Engine.Render_Quote (Title    => Strings.To_WString (Engine.Content),
                                    Attrs    => Node.Attributes);
            end if;
            Engine.Keep_Content := Engine.Keep_Content - 1;

         when P_TAG =>
            Engine.Set_Format (Empty_Formats);
            Engine.New_Line;

         when B_TAG | EM_TAG | STRONG_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (BOLD) := False;
            end if;

         when I_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (ITALIC) := False;
            end if;

         when U_TAG | TT_TAG | CODE_TAG | KBD_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (CODE) := False;
            end if;

         when SUP_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (SUPERSCRIPT) := False;
            end if;

         when SUB_TAG =>
            if Engine.Keep_Content = 0 then
               Engine.Current_Style (SUBSCRIPT) := False;
            end if;

         when PRE_TAG =>
            if Engine.Keep_Content = 1 then
               Engine.Need_Space := False;
               Engine.Render_Preformatted (Strings.To_WString (Engine.Content), "");
            end if;
            Engine.Keep_Content := Engine.Keep_Content - 1;

         when UL_TAG =>
            Engine.UL_List_Level := Engine.UL_List_Level - 1;
            if Engine.UL_List_Level = 0 then
               Engine.Need_Separator_Line;
            end if;

         when OL_TAG =>
            Engine.OL_List_Level := Engine.OL_List_Level - 1;
            if Engine.UL_List_Level = 0 then
               Engine.Need_Separator_Line;
            end if;

         when LI_TAG =>
            Engine.In_List := False;
            if not Engine.Empty_Line then
               Engine.New_Line;
            end if;

         when BLOCKQUOTE_TAG =>
            Engine.Set_Format (Empty_Formats);
            Engine.Need_Separator_Line;
            Engine.Quote_Level := Engine.Quote_Level - 1;
            Engine.Need_Space := False;
            if Engine.Html_Blockquote then
               --  Make sure there is an empty line after the HTML </blockquote>.
               Engine.Output.Write ("</blockquote>" & LF & LF);
            elsif Engine.Quote_Level = 0 then
               Engine.New_Line;
               Engine.Need_Newline := True;
            end if;

         when others =>
            Engine.Need_Space := True;

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
      Need_Newline : constant Boolean := Engine.Need_Newline;
   begin
      if not Engine.Empty_Line then
         Engine.New_Line;
      end if;
      Engine.Need_Space := False;
      Engine.Has_Item := False;
      if Need_Newline and then not Engine.Empty_Previous_Line then
         Engine.New_Line;
      end if;
   end Close_Paragraph;

   --  ------------------------------
   --  Render the table of content.
   --  ------------------------------
   procedure Render_TOC (Engine : in out Wiki_Renderer;
                         Doc    : in Documents.Document;
                         Level  : in Natural) is
      pragma Unreferenced (Doc, Level);
   begin
      if Engine.Syntax = SYNTAX_MEDIA_WIKI then
         Engine.Output.Write ("__TOC__");
      end if;
   end Render_TOC;

   --  ------------------------------
   --  Render a table component such as N_TABLE, N_ROW or N_COLUMN.
   --  ------------------------------
   procedure Render_Table (Engine : in out Wiki_Renderer;
                           Doc    : in Documents.Document;
                           Node   : in Nodes.Node_Type) is
   begin
      Engine.Close_Paragraph;
      Engine.Need_Paragraph := False;
      Engine.Has_Paragraph := False;
      Engine.In_Table := True;
      if Engine.Html_Table then
         Engine.Output.Write (LF & "<table>");
         Engine.New_Line;
      end if;

      Engine.Render (Doc, Node.Children);

      Engine.Close_Paragraph;
      Engine.In_Table := False;
      Engine.Has_Paragraph := False;
      Engine.Need_Paragraph := True;

      if Engine.Html_Table then
         Engine.Output.Write ("</table>");
         Engine.New_Line;
      end if;
   end Render_Table;

   --  ------------------------------
   --  Render a table row component such as N_ROW.
   --  ------------------------------
   procedure Render_Row (Engine : in out Wiki_Renderer;
                         Doc    : in Documents.Document;
                         Node   : in Nodes.Node_Type) is
   begin
      if Engine.Html_Table then
         Engine.Output.Write ("<tr>");
      else
         Engine.Output.Write ("| ");
      end if;
      Engine.Col_Index := 0;
      Engine.Render (Doc, Node.Children);
      if Engine.Html_Table then
         Engine.Output.Write ("</tr>");
      else
         Engine.Output.Write ("|");
      end if;
      Engine.New_Line;
   end Render_Row;

   --  ------------------------------
   --  Render a table row component such as N_COLUMN.
   --  ------------------------------
   procedure Render_Column (Engine : in out Wiki_Renderer;
                            Doc    : in Documents.Document;
                            Node   : in Nodes.Node_Type) is
   begin
      if Engine.Html_Table then
         Engine.Output.Write ("<td>");
      else
         if Engine.Col_Index > 0 then
            if Engine.Format /= Empty_Formats then
               Engine.Set_Format (Empty_Formats);
            end if;
            Engine.Output.Write (" | ");
         end if;
      end if;
      Engine.Col_Index := Engine.Col_Index + 1;
      Engine.Render (Doc, Node.Children);

      if Engine.Html_Table then
         Engine.Output.Write ("</td>");
      end if;
   end Render_Column;

end Wiki.Render.Wiki;
