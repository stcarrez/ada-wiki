-----------------------------------------------------------------------
--  wiki-render-wiki -- Wiki to Wiki renderer
--  Copyright (C) 2015, 2016 Stephane Carrez
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
with Ada.Wide_Wide_Characters.Handling;

with Wiki.Filters.Html;
with Wiki.Helpers;
package body Wiki.Render.Wiki is

   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#0A#);

   HEADER_CREOLE             : aliased constant Wide_Wide_String := "=";
   BOLD_CREOLE               : aliased constant Wide_Wide_String := "**";
   LINE_BREAK_CREOLE         : aliased constant Wide_Wide_String := "%%%";
   IMG_START_CREOLE          : aliased constant Wide_Wide_String := "{{";
   IMG_END_CREOLE            : aliased constant Wide_Wide_String := "}}";
   LINK_START_CREOLE         : aliased constant Wide_Wide_String := "[[";
   LINK_END_CREOLE           : aliased constant Wide_Wide_String := "]]";
   PREFORMAT_START_CREOLE    : aliased constant Wide_Wide_String := "{{{";
   PREFORMAT_END_CREOLE      : aliased constant Wide_Wide_String := "}}}" & LF;
   HORIZONTAL_RULE_CREOLE    : aliased constant Wide_Wide_String := "----" & LF;
   LINK_SEPARATOR_CREOLE     : aliased constant Wide_Wide_String := "|";
   LIST_ITEM_CREOLE          : aliased constant Wide_Wide_String := "*";
   LIST_ORDERED_ITEM_CREOLE  : aliased constant Wide_Wide_String := "#";
   ESCAPE_CREOLE             : aliased constant Wide_Wide_String := "~";

   HEADER_DOTCLEAR           : aliased constant Wide_Wide_String := "!";
   IMG_START_DOTCLEAR        : aliased constant Wide_Wide_String := "((";
   IMG_END_DOTCLEAR          : aliased constant Wide_Wide_String := "))";
   LINK_START_DOTCLEAR       : aliased constant Wide_Wide_String := "[";
   LINK_END_DOTCLEAR         : aliased constant Wide_Wide_String := "]";
   PREFORMAT_START_DOTCLEAR  : aliased constant Wide_Wide_String := "///";
   PREFORMAT_END_DOTCLEAR    : aliased constant Wide_Wide_String := "///" & LF;
   ESCAPE_DOTCLEAR           : aliased constant Wide_Wide_String := "\";

   LINE_BREAK_MEDIAWIKI      : aliased constant Wide_Wide_String := "<br />";
   BOLD_MEDIAWIKI            : aliased constant Wide_Wide_String := "'''";
   PREFORMAT_START_MEDIAWIKI : aliased constant Wide_Wide_String := "<pre>";
   PREFORMAT_END_MEDIAWIKI   : aliased constant Wide_Wide_String := "</pre>";
   IMG_START_MEDIAWIKI       : aliased constant Wide_Wide_String := "[[File:";
   IMG_END_MEDIAWIKI         : aliased constant Wide_Wide_String := "]]";

   Empty_Formats : constant Format_Map := (others => False);

   --  Set the output writer.
   procedure Set_Output_Stream (Engine : in out Wiki_Renderer;
                                Stream : in Streams.Output_Stream_Access;
                                Format : in Parsers.Wiki_Syntax_Type) is
   begin
      Engine.Output := Stream;
      Engine.Syntax := Format;
      case Format is
         when Parsers.SYNTAX_DOTCLEAR =>
            Engine.Style_Start_Tags (BOLD)   := BOLD_CREOLE'Access;
            Engine.Style_End_Tags (BOLD)     := BOLD_CREOLE'Access;
            Engine.Tags (Header_Start) := HEADER_DOTCLEAR'Access;
            Engine.Tags (Line_Break)   := LINE_BREAK_CREOLE'Access;
            Engine.Tags (Img_Start)    := IMG_START_DOTCLEAR'Access;
            Engine.Tags (Img_End)      := IMG_END_DOTCLEAR'Access;
            Engine.Tags (Link_Start)   := LINK_START_DOTCLEAR'Access;
            Engine.Tags (Link_End)     := LINK_END_DOTCLEAR'Access;
            Engine.Tags (Link_Separator)  := LINK_SEPARATOR_CREOLE'Access;
            Engine.Tags (Preformat_Start) := PREFORMAT_START_DOTCLEAR'Access;
            Engine.Tags (Preformat_End)   := PREFORMAT_END_DOTCLEAR'Access;
            Engine.Tags (Horizontal_Rule) := HORIZONTAL_RULE_CREOLE'Access;
            Engine.Tags (List_Item)       := LIST_ITEM_CREOLE'Access;
            Engine.Tags (List_Ordered_Item) := LIST_ORDERED_ITEM_CREOLE'Access;
            Engine.Tags (Escape_Rule)       := ESCAPE_DOTCLEAR'Access;
            Engine.Invert_Header_Level := True;
            Engine.Allow_Link_Language := True;
            Engine.Escape_Set := Ada.Strings.Wide_Wide_Maps.To_Set ("-+_*{}][/=\");

         when Parsers.SYNTAX_MEDIA_WIKI =>
            Engine.Style_Start_Tags (BOLD)   := BOLD_MEDIAWIKI'Access;
            Engine.Style_End_Tags (BOLD)     := BOLD_MEDIAWIKI'Access;
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
            Engine.Tags (Header_Start) := HEADER_CREOLE'Access;
            Engine.Tags (Header_End)   := HEADER_CREOLE'Access;
            Engine.Tags (Line_Break)   := LINE_BREAK_CREOLE'Access;
            Engine.Tags (Img_Start)    := IMG_START_CREOLE'Access;
            Engine.Tags (Img_End)      := IMG_END_CREOLE'Access;
            Engine.Tags (Link_Start)   := LINK_START_CREOLE'Access;
            Engine.Tags (Link_End)     := LINK_END_CREOLE'Access;
            Engine.Tags (Link_Separator)  := LINK_SEPARATOR_CREOLE'Access;
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

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Wiki_Renderer;
                     Doc    : in Nodes.Document;
                     Node   : in Nodes.Node_Type) is
      use type Nodes.Html_Tag_Type;
      use type Nodes.Node_List_Access;
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
                            Header : in Wide_Wide_String;
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
   --  Add a link.
   --  ------------------------------
   procedure Add_Link (Engine : in out Wiki_Renderer;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Title);
   begin
      Engine.Output.Write (Engine.Tags (Link_Start).all);
      Engine.Output.Write (To_Wide_Wide_String (Link));
      if Length (Name) > 0 then
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
         Engine.Output.Write (To_Wide_Wide_String (Name));
      end if;
      if Engine.Allow_Link_Language and Length (Language) > 0 then
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
      end if;
      Engine.Output.Write (Engine.Tags (Link_End).all);
      Engine.Empty_Line := False;
   end Add_Link;

   --  Add an image.
   procedure Add_Image (Engine    : in out Wiki_Renderer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Position, Description);
   begin
      Engine.Output.Write (Engine.Tags (Img_Start).all);
      Engine.Output.Write (To_Wide_Wide_String (Link));
      if Length (Alt) > 0 then
         Engine.Output.Write (Engine.Tags (Link_Separator).all);
         Engine.Output.Write (To_Wide_Wide_String (Alt));
      end if;
      Engine.Output.Write (Engine.Tags (Img_End).all);
      Engine.Empty_Line := False;
   end Add_Image;

   --  Add a quote.
   procedure Add_Quote (Engine : in out Wiki_Renderer;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String) is
   begin
      null;
   end Add_Quote;

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
                          Text     : in Wide_Wide_String;
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
            if Ada.Wide_Wide_Characters.Handling.Is_Line_Terminator (Text (I)) then
               if Engine.Empty_Line = False then
                  if Apply_Format and then Engine.Format /= Empty_Formats then
                     Engine.Set_Format (Empty_Formats);
                  end if;
                  Engine.Output.Write (LF);
                  Engine.Empty_Line := True;
               end if;
            elsif not Engine.Empty_Line or else not Helpers.Is_Space (Text (I)) then
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
   --  Add a text block that is pre-formatted.
   --  ------------------------------
   procedure Add_Preformatted (Engine : in out Wiki_Renderer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Format);

      Content       : constant Wide_Wide_String := To_Wide_Wide_String (Text);
      Col           : Natural := 2;
   begin
      Engine.New_Line;
      Engine.Output.Write (Engine.Tags (Preformat_Start).all);
      for I in Content'Range loop
         if Ada.Wide_Wide_Characters.Handling.Is_Line_Terminator (Content (I)) then
            Col := 0;
         else
            Col := Col + 1;
         end if;
         if I = Content'First and then Col > 0 then
            Engine.Output.Write (LF);
            Col := 0;
         end if;
         Engine.Output.Write (Content (I));
      end loop;
      if Col /= 0 then
         Engine.New_Line;
      end if;
      Engine.Output.Write (Engine.Tags (Preformat_End).all);
      Engine.New_Line;
      Engine.Empty_Line := True;
   end Add_Preformatted;

   procedure Start_Keep_Content (Engine : in out Wiki_Renderer) is
   begin
      Engine.Keep_Content := True;
      Engine.Content := To_Unbounded_Wide_Wide_String ("");
   end Start_Keep_Content;

   procedure Render_Tag (Engine : in out Wiki_Renderer;
                         Doc    : in Nodes.Document;
                         Node   : in Nodes.Node_Type) is
      use type Nodes.Html_Tag_Type;

   begin
      case Node.Tag_Start is
         when Nodes.BR_TAG =>
            Engine.Output.Write (Engine.Tags (Line_Break).all);
            Engine.Empty_Line := False;
            return;

         when Nodes.HR_TAG =>
            Engine.Close_Paragraph;
            Engine.Output.Write (Engine.Tags (Horizontal_Rule).all);
            return;

         when Nodes.H1_TAG | Nodes.H2_TAG
            | Nodes.H3_TAG | Nodes.H4_TAG
            | Nodes.H5_TAG | Nodes.H6_TAG =>
            Engine.Start_Keep_Content;

         when Nodes.IMG_TAG =>
            Engine.Add_Image (Link        => Get_Attribute (Node.Attributes, "src"),
                              Alt         => Get_Attribute (Node.Attributes, "alt"),
                              Position    => Null_Unbounded_Wide_Wide_String,
                              Description => Null_Unbounded_Wide_Wide_String);

         when Nodes.A_TAG =>
            Engine.Link_Href := Get_Attribute (Node.Attributes, "href");
            Engine.Link_Title := Get_Attribute (Node.Attributes, "title");
            Engine.Link_Lang := Get_Attribute (Node.Attributes, "lang");
            Engine.Start_Keep_Content;

         when Nodes.B_TAG | Nodes.EM_TAG | Nodes.STRONG_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (BOLD) := True;
            end if;

         when Nodes.I_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (ITALIC) := True;
            end if;

         when Nodes.U_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (CODE) := True;
            end if;

         when Nodes.SUP_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUPERSCRIPT) := True;
            end if;

         when Nodes.SUB_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUBSCRIPT) := True;
            end if;

         when Nodes.P_TAG =>
            Engine.New_Line;

         when Nodes.PRE_TAG =>
            Engine.Start_Keep_Content;

         when Nodes.UL_TAG =>
            Engine.UL_List_Level := Engine.UL_List_Level + 1;

         when Nodes.OL_TAG =>
            Engine.OL_List_Level := Engine.OL_List_Level + 1;

         when Nodes.LI_TAG =>
            Engine.In_List := True;

         when others =>
            null;

      end case;
      Engine.Render (Doc, Node.Children);

      case Node.Tag_Start is
         when Nodes.H1_TAG =>
            Engine.Render_Header (To_Wide_Wide_String (Engine.Content), 1);
            Engine.Keep_Content := False;

         when Nodes.H2_TAG =>
            Engine.Render_Header (To_Wide_Wide_String (Engine.Content), 2);
            Engine.Keep_Content := False;

         when Nodes.H3_TAG =>
            Engine.Render_Header (To_Wide_Wide_String (Engine.Content), 3);
            Engine.Keep_Content := False;

         when Nodes.H4_TAG =>
            Engine.Render_Header (To_Wide_Wide_String (Engine.Content), 4);
            Engine.Keep_Content := False;

         when Nodes.H5_TAG =>
            Engine.Render_Header (To_Wide_Wide_String (Engine.Content), 5);
            Engine.Keep_Content := False;

         when Nodes.H6_TAG =>
            Engine.Render_Header (To_Wide_Wide_String (Engine.Content), 6);
            Engine.Keep_Content := False;

         when Nodes.A_TAG =>
            Engine.Add_Link (Name     => Engine.Content,
                               Link     => Engine.Link_Href,
                               Language => Engine.Link_Lang,
                               Title    => Engine.Link_Title);
            Engine.Keep_Content := False;

         when Nodes.B_TAG | Nodes.EM_TAG | Nodes.STRONG_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (BOLD) := False;
            end if;

         when Nodes.I_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (ITALIC) := False;
            end if;

         when Nodes.U_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (CODE) := False;
            end if;

         when Nodes.SUP_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUPERSCRIPT) := False;
            end if;

         when Nodes.SUB_TAG =>
            if not Engine.Keep_Content then
               Engine.Current_Style (SUBSCRIPT) := False;
            end if;

         when Nodes.PRE_TAG =>
            Engine.Add_Preformatted (Engine.Content, Null_Unbounded_Wide_Wide_String);
            Engine.Keep_Content := False;

         when Nodes.UL_TAG =>
            Engine.UL_List_Level := Engine.UL_List_Level - 1;

         when Nodes.OL_TAG =>
            Engine.OL_List_Level := Engine.OL_List_Level - 1;

         when Nodes.LI_TAG =>
            Engine.In_List := False;

         when others =>
            null;

      end case;
   end Render_Tag;

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Engine : in out Wiki_Renderer) is
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
