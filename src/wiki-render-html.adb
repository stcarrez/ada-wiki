-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021, 2022 Stephane Carrez
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
with Util.Strings;
with Wiki.Attributes;
with Wiki.Helpers;
with Wiki.Nodes.Lists;
package body Wiki.Render.Html is

   procedure Close_Paragraph (Engine : in out Html_Renderer);
   procedure Open_Paragraph (Engine : in out Html_Renderer);

   procedure Render_Tag (Engine : in out Html_Renderer;
                         Doc    : in Wiki.Documents.Document;
                         Node   : in Wiki.Nodes.Node_Type);

   --  Render a section header in the document.
   procedure Render_Header (Engine : in out Html_Renderer;
                            Doc    : in Wiki.Documents.Document;
                            Node   : in Wiki.Nodes.Node_Type);

   --  Render the table of content.
   procedure Render_TOC (Engine : in out Html_Renderer;
                         Doc    : in Wiki.Documents.Document;
                         Level  : in Natural);

   --  Render a link.
   procedure Render_Link (Engine : in out Html_Renderer;
                          Doc    : in Wiki.Documents.Document;
                          Title  : in Wiki.Strings.WString;
                          Attr   : in Wiki.Attributes.Attribute_List);

   procedure Render_Link_Ref (Engine : in out Html_Renderer;
                              Doc    : in Wiki.Documents.Document;
                              Label  : in Wiki.Strings.WString);

   --  Render an image.
   procedure Render_Image (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List);

   --  Render a quote.
   procedure Render_Quote (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List);

   --  Returns true if the HTML element being included is already contained in a paragraph.
   --  This include: a, em, strong, small, b, i, u, s, span, ins, del, sub, sup.
   function Has_Html_Paragraph (Engine : in Html_Renderer) return Boolean;

   procedure Newline (Engine : in out Html_Renderer);

   --  ------------------------------
   --  Set the output stream.
   --  ------------------------------
   procedure Set_Output_Stream (Engine : in out Html_Renderer;
                                Stream : in Wiki.Streams.Html.Html_Output_Stream_Access) is
   begin
      Engine.Output := Stream;
   end Set_Output_Stream;

   --  ------------------------------
   --  Set the link renderer.
   --  ------------------------------
   procedure Set_Link_Renderer (Engine : in out Html_Renderer;
                                Links    : in Wiki.Render.Links.Link_Renderer_Access) is
   begin
      Engine.Links := Links;
   end Set_Link_Renderer;

   --  ------------------------------
   --  Set the render TOC flag that controls the TOC rendering.
   --  ------------------------------
   procedure Set_Render_TOC (Engine : in out Html_Renderer;
                             State  : in Boolean) is
   begin
      Engine.Enable_Render_TOC := State;
   end Set_Render_TOC;

   --  ------------------------------
   --  Set the no-newline mode to avoid emitting newlines (disabled by default).
   --  ------------------------------
   procedure Set_No_Newline (Engine : in out Html_Renderer;
                             Enable : in Boolean) is
   begin
      Engine.No_Newline := Enable;
   end Set_No_Newline;

   --  ------------------------------
   --  Get the current section number.
   --  ------------------------------
   function Get_Section_Number (Engine    : in Html_Renderer;
                                Prefix    : in Wiki.Strings.WString;
                                Separator : in Wiki.Strings.WChar) return Wiki.Strings.WString is
      Result : Wiki.Strings.UString;
      Value  : Natural;
      Empty  : Boolean := True;
   begin
      if Engine.Section_Level = 0 then
         return "";
      end if;
      Wiki.Strings.Append (Result, Prefix);
      for I in 1 .. Engine.Section_Level loop
         Value := Engine.Current_Section (I);
         if Value > 0 or else not Empty then
            declare
               N : constant Strings.WString := Positive'Wide_Wide_Image (Value);
            begin
               if not Empty then
                  Wiki.Strings.Append (Result, Separator);
               end if;
               Empty := False;
               Wiki.Strings.Append (Result, N (N'First + 1 .. N'Last));
            end;
         end if;
      end loop;
      return Wiki.Strings.To_WString (Result);
   end Get_Section_Number;

   --  ------------------------------
   --  Returns true if the HTML element being included is already contained in a paragraph.
   --  This include: a, em, strong, small, b, i, u, s, span, ins, del, sub, sup.
   --  ------------------------------
   function Has_Html_Paragraph (Engine : in Html_Renderer) return Boolean is
   begin
      return Engine.Html_Tag in Wiki.SPAN_TAG
        | Wiki.A_TAG | Wiki.EM_TAG | Wiki.STRONG_TAG
        | Wiki.SMALL_TAG | Wiki.B_TAG | Wiki.I_TAG
        | Wiki.U_TAG | Wiki.S_TAG | Wiki.SUB_TAG
        | Wiki.SUP_TAG | Wiki.INS_TAG | Wiki.DEL_TAG;
   end Has_Html_Paragraph;

   --  ------------------------------
   --  Render the node instance from the document.
   --  ------------------------------
   overriding
   procedure Render (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Documents.Document;
                     Node   : in Wiki.Nodes.Node_Type) is
   begin
      case Node.Kind is
         when Wiki.Nodes.N_NONE =>
            null;

         when Wiki.Nodes.N_HEADER =>
            Engine.Render_Header (Doc   => Doc,
                                  Node  => Node);

         when Wiki.Nodes.N_LINE_BREAK =>
            Engine.Output.Write ("<br>");
            Engine.Output.Newline;

         when Wiki.Nodes.N_NEWLINE =>
            Engine.Newline;

         when Wiki.Nodes.N_DEFINITION =>
            if not Engine.In_Definition then
               Engine.Close_Paragraph;
               Engine.In_Definition := True;
               Engine.Output.Start_Element ("dl");
            else
               Engine.Output.End_Element ("dd");
            end if;
            Engine.Output.Start_Element ("dt");
            Engine.Render (Doc, Node.Content);
            --  Engine.Output.Write_Wide_Text (Node.Header);
            Engine.Output.End_Element ("dt");
            Engine.Output.Start_Element ("dd");
            Engine.Has_Paragraph := True;
            Engine.Need_Paragraph := False;

         when Wiki.Nodes.N_END_DEFINITION =>
            if Engine.In_Definition then
               Engine.In_Definition := False;
               Engine.Output.End_Element ("dd");
               Engine.Output.End_Element ("dl");
               Engine.Has_Paragraph := False;
               Engine.Need_Paragraph := True;
            end if;

         when Wiki.Nodes.N_HORIZONTAL_RULE =>
            if Engine.Html_Level = 0 or else not Engine.Has_Html_Paragraph then
               Engine.Close_Paragraph;
               Engine.Add_Blockquote (0);
            end if;
            Engine.Output.Write ("<hr>");

         when Wiki.Nodes.N_PARAGRAPH =>
            --  Close the paragraph and start a new one except if the current HTML
            --  element is within a paragraph (ex: a, b, i, u, span, ...).
            if Engine.Html_Level = 0 or else not Engine.Has_Html_Paragraph then
               Engine.Close_Paragraph;
               Engine.Need_Paragraph := True;
            end if;

         when Wiki.Nodes.N_PREFORMAT =>
            Engine.Render_Preformatted (Node.Preformatted,
                                        Strings.To_WString (Node.Language));

         when Wiki.Nodes.N_INDENT =>
            null;

         when Wiki.Nodes.N_LIST_END =>
            Engine.Render_List_End ("ul");

         when Wiki.Nodes.N_NUM_LIST_END =>
            Engine.Render_List_End ("ol");

         when Wiki.Nodes.N_LIST_ITEM_END =>
            Engine.Render_List_End ("li");

         when Wiki.Nodes.N_LIST_START =>
            Engine.Render_List_Start ("ul", Node.Level);

         when Wiki.Nodes.N_NUM_LIST_START =>
            Engine.Render_List_Start ("ol", Node.Level);

         when Wiki.Nodes.N_LIST_ITEM =>
            Engine.Render_List_Item;

         when Wiki.Nodes.N_TEXT =>
            Engine.Add_Text (Text   => Node.Text,
                             Format => Node.Format);

         when Wiki.Nodes.N_QUOTE =>
            Engine.Render_Quote (Doc, Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_LINK =>
            Engine.Render_Link (Doc, Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_LINK_REF =>
            Engine.Render_Link_Ref (Doc, Node.Title);

         when Wiki.Nodes.N_LINK_REF_END =>
            Engine.Output.End_Element ("a");

         when Wiki.Nodes.N_IMAGE =>
            Engine.Render_Image (Doc, Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_BLOCKQUOTE =>
            Engine.Add_Blockquote (Node.Level);

         when Wiki.Nodes.N_TAG_START =>
            Engine.Render_Tag (Doc, Node);

         when Wiki.Nodes.N_TOC =>
            Engine.Render_TOC (Doc, Node.Level);

         when Wiki.Nodes.N_TOC_ENTRY =>
            null;

         when Wiki.Nodes.N_TOC_DISPLAY =>
            Engine.Render_TOC (Doc, 3);

         when Wiki.Nodes.N_TABLE =>
            Engine.Render_Table (Doc, Node, "table");

         when Wiki.Nodes.N_ROW =>
            Engine.Render_Table (Doc, Node, "tr");

         when Wiki.Nodes.N_COLUMN =>
            Engine.Render_Table (Doc, Node, "td");

      end case;
   end Render;

   procedure Render_Tag (Engine : in out Html_Renderer;
                         Doc    : in Wiki.Documents.Document;
                         Node   : in Wiki.Nodes.Node_Type) is
      Name         : constant Wiki.String_Access := Wiki.Get_Tag_Name (Node.Tag_Start);
      Iter         : Wiki.Attributes.Cursor := Wiki.Attributes.First (Node.Attributes);
      Previous_Tag : constant Wiki.Html_Tag := Engine.Html_Tag;
      Prev_Para    : Boolean := Engine.Has_Paragraph;
   begin
      if Node.Tag_Start = Wiki.P_TAG then
         Engine.Has_Paragraph := True;
         Engine.Need_Paragraph := False;
      elsif Node.Tag_Start in Wiki.UL_TAG
        | Wiki.OL_TAG | Wiki.DL_TAG | Wiki.DT_TAG
        | Wiki.DD_TAG | Wiki.LI_TAG | Wiki.H1_TAG
        | Wiki.H2_TAG | Wiki.H3_TAG | Wiki.H4_TAG
        | Wiki.H5_TAG | Wiki.H6_TAG | Wiki.DIV_TAG | Wiki.TABLE_TAG
      then
         Engine.Close_Paragraph;
         Engine.Need_Paragraph := False;
         Engine.Has_Paragraph := False;
         Engine.Open_Paragraph;
      elsif Node.Tag_Start in Wiki.B_TAG
        | Wiki.I_TAG | Wiki.SPAN_TAG
        | Wiki.INS_TAG | Wiki.DEL_TAG | Wiki.A_TAG
      then
         Engine.Open_Paragraph;
         Prev_Para := Engine.Has_Paragraph;
      elsif Node.Tag_Start = Wiki.BR_TAG then
         Engine.Output.Write ("<br>");
         return;
      elsif Node.Tag_Start = Wiki.HR_TAG then
         Engine.Output.Write ("<hr>");
         return;
      else
         Engine.Has_Paragraph := False;
         Engine.Need_Paragraph := False;
         Engine.Open_Paragraph;
      end if;
      Engine.Output.Start_Element (Name.all);
      while Wiki.Attributes.Has_Element (Iter) loop
         Engine.Output.Write_Wide_Attribute (Name    => Wiki.Attributes.Get_Name (Iter),
                                             Content => Wiki.Attributes.Get_Wide_Value (Iter));
         Wiki.Attributes.Next (Iter);
      end loop;
      Engine.Html_Tag := Node.Tag_Start;
      Engine.Html_Level := Engine.Html_Level + 1;
      Engine.Render (Doc, Node.Children);
      Engine.Html_Tag := Previous_Tag;
      Engine.Html_Level := Engine.Html_Level - 1;
      if Node.Tag_Start = Wiki.P_TAG then
         Engine.Has_Paragraph := False;
         Engine.Need_Paragraph := True;
      elsif Node.Tag_Start in Wiki.UL_TAG | Wiki.OL_TAG | Wiki.DL_TAG
        | Wiki.DT_TAG | Wiki.DD_TAG | Wiki.LI_TAG
        | Wiki.H1_TAG | Wiki.H2_TAG | Wiki.H3_TAG
        | Wiki.H4_TAG | Wiki.H5_TAG
        | Wiki.H6_TAG | Wiki.DIV_TAG | Wiki.TABLE_TAG
      then
         Engine.Close_Paragraph;
         Engine.Has_Paragraph := False;
         Engine.Need_Paragraph := True;
      elsif not Engine.Has_Html_Paragraph then
         --  Leaving the HTML text-element, restore the previous paragraph state.
         Engine.Has_Paragraph := Prev_Para;
      end if;
      Engine.Output.End_Element (Name.all);
   end Render_Tag;

   --  ------------------------------
   --  Render a section header in the document.
   --  ------------------------------
   procedure Render_Header (Engine : in out Html_Renderer;
                            Doc    : in Wiki.Documents.Document;
                            Node   : in Wiki.Nodes.Node_Type) is
      Level : constant Natural := Node.Level;
      Tag   : String_Access;
   begin
      if Engine.Enable_Render_TOC
        and then not Engine.TOC_Rendered
        and then not Doc.Is_Using_TOC
        and then Doc.Is_Visible_TOC
      then
         Engine.Render_TOC (Doc, 3);
      end if;
      Engine.Close_Paragraph;
      Engine.Current_Section (Level) := Engine.Current_Section (Level) + 1;
      for I in Level + 1 .. Engine.Current_Section'Last loop
         Engine.Current_Section (I) := 0;
      end loop;
      Engine.Section_Level := Level;
      case Level is
         when 1 =>
            Tag := Get_Tag_Name (H1_TAG);

         when 2 =>
            Tag := Get_Tag_Name (H2_TAG);

         when 3 =>
            Tag := Get_Tag_Name (H3_TAG);

         when 4 =>
            Tag := Get_Tag_Name (H4_TAG);

         when 5 =>
            Tag := Get_Tag_Name (H5_TAG);

         when others =>
            Tag := Get_Tag_Name (H6_TAG);

      end case;
      Engine.Output.Start_Element (Tag.all);
      if Engine.Enable_Render_TOC then
         Engine.Output.Write_Wide_Attribute ("id", Engine.Get_Section_Number ("section_", '_'));
      end if;
      Engine.Need_Paragraph := False;
      Engine.Render (Doc, Node.Content);
      Engine.Output.End_Element (Tag.all);
      Engine.Newline;
   end Render_Header;

   --  ------------------------------
   --  Render the table of content.
   --  ------------------------------
   procedure Render_TOC (Engine : in out Html_Renderer;
                         Doc    : in Wiki.Documents.Document;
                         Level  : in Natural) is

      procedure Render_Entry (Node : in Wiki.Nodes.Node_Type);
      procedure Set_Current_Level (New_Level : in Natural);

      use Wiki.Nodes;

      procedure Set_Current_Level (New_Level : in Natural) is
      begin
         if New_Level = Engine.Section_Level and then New_Level /= 0 then
            Engine.Output.End_Element ("li");
            Engine.Output.Start_Element ("li");
            Engine.Output.Write_Attribute ("class", "wiki-toc-entry");
         end if;

         --  Close the ul/li lists up to the expected level.
         while New_Level < Engine.Section_Level loop
            Engine.Output.End_Element ("li");
            Engine.Output.End_Element ("ul");
            Engine.Section_Level := Engine.Section_Level - 1;
         end loop;
         while New_Level > Engine.Section_Level loop
            Engine.Output.Start_Element ("ul");
            Engine.Output.Write_Attribute ("class", "wiki-toc-entry");
            Engine.Output.Start_Element ("li");
            Engine.Output.Write_Attribute ("class", "wiki-toc-entry");
            Engine.Section_Level := Engine.Section_Level + 1;
            Engine.Current_Section (Engine.Section_Level) := 0;
         end loop;
      end Set_Current_Level;

      procedure Render_Entry (Node : in Wiki.Nodes.Node_Type) is
      begin
         if Node.Kind /= Wiki.Nodes.N_TOC_ENTRY or else Node.Toc_Level > Level then
            return;
         end if;
         Set_Current_Level (Node.Toc_Level);
         Engine.Current_Section (Engine.Section_Level)
           := Engine.Current_Section (Engine.Section_Level) + 1;
         Engine.Output.Start_Element ("a");
         Engine.Output.Write_Attribute ("class", "wiki-toc-ref");
         Engine.Output.Write_Wide_Attribute ("href", Engine.Get_Section_Number ("#section_", '_'));
         Engine.Output.Start_Element ("span");
         Engine.Output.Write_Attribute ("class", "wiki-toc-level");
         Engine.Output.Write_Wide_Text (Engine.Get_Section_Number ("", '.'));
         Engine.Output.End_Element ("span");
         Engine.Output.Start_Element ("span");
         Engine.Output.Write_Attribute ("class", "wiki-toc-title");
         Engine.Output.Write_Wide_Text (Node.Header);
         Engine.Output.End_Element ("span");
         Engine.Output.End_Element ("a");
      end Render_Entry;

      Toc : constant Wiki.Nodes.Lists.Node_List_Ref := Doc.Get_TOC;
   begin
      if Wiki.Nodes.Lists.Length (Toc) <= 3 then
         Engine.Enable_Render_TOC := False;

      elsif not Engine.TOC_Rendered then
         Engine.Section_Level := 0;
         Engine.Current_Section := (others => 0);
         Engine.Output.Start_Element ("div");
         Engine.Output.Write_Attribute ("class", "wiki-toc");
         Wiki.Nodes.Lists.Iterate (Toc, Render_Entry'Access);
         Set_Current_Level (0);
         Engine.Output.End_Element ("div");
         Engine.TOC_Rendered := True;
         Engine.Current_Section := (others => 0);
      end if;
   end Render_TOC;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Add_Blockquote (Engine : in out Html_Renderer;
                             Level  : in Natural) is
   begin
      if Engine.Quote_Level /= Level then
         Engine.Close_Paragraph;
         Engine.Need_Paragraph := True;
      end if;
      while Engine.Quote_Level < Level loop
         Engine.Output.Start_Element ("blockquote");
         Engine.Quote_Level := Engine.Quote_Level + 1;
      end loop;
      while Engine.Quote_Level > Level loop
         Engine.Output.End_Element ("blockquote");
         Engine.Quote_Level := Engine.Quote_Level - 1;
      end loop;
   end Add_Blockquote;

   --  ------------------------------
   --  Render a list item end (</ul> or </ol>).
   --  Close the previous paragraph and list item if any.
   --  ------------------------------
   procedure Render_List_End (Engine : in out Html_Renderer;
                              Tag    : in String) is
   begin
      if Engine.Has_Paragraph then
         Engine.Output.End_Element ("p");
         Engine.Has_Paragraph := False;
      end if;
      Engine.Has_Item := False;
      Engine.Need_Paragraph := False;
      Engine.Output.End_Element (Tag);
      Engine.Has_Item := False;
   end Render_List_End;

   --  ------------------------------
   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Render_List_Start (Engine   : in out Html_Renderer;
                                Tag      : in String;
                                Level    : in Natural) is
   begin
      if Engine.Has_Paragraph then
         Engine.Output.End_Element ("p");
         Engine.Has_Paragraph := False;
      end if;
      Engine.Need_Paragraph := False;
      Engine.Output.Start_Element (Tag);
      if Tag = "ol" and then Level /= 1 then
         Engine.Output.Write_Attribute ("start", Util.Strings.Image (Level));
      end if;
      Engine.Has_Item := False;
   end Render_List_Start;

   --  ------------------------------
   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Render_List_Item (Engine   : in out Html_Renderer) is
   begin
      if Engine.Has_Paragraph then
         Engine.Output.End_Element ("p");
         Engine.Has_Paragraph := False;
      end if;
      Engine.Output.Start_Element ("li");
      Engine.Has_Item := False;
   end Render_List_Item;

   procedure Newline (Engine : in out Html_Renderer) is
   begin
--      if not Engine.No_Newline then
--         Engine.Output.Write_Wide_Text (Wiki.Helpers.LF & "");
--      end if;
      null;
   end Newline;

   procedure Close_Paragraph (Engine : in out Html_Renderer) is
   begin
      --  Don't close a paragraph if we are within a HTML text-level element.
      if Engine.Has_Html_Paragraph then
         return;
      end if;
      if Engine.Has_Paragraph then
         Engine.Output.End_Element ("p");
         if not Engine.No_Newline then
            Engine.Output.Newline;
         end if;
      end if;
      if Engine.Has_Item then
         Engine.Output.End_Element ("li");
         Engine.Newline;
      end if;
      while Engine.Current_Level > 0 loop
         if Engine.List_Styles (Engine.Current_Level) then
            Engine.Output.End_Element ("ol");
         else
            Engine.Output.End_Element ("ul");
         end if;
         Engine.Current_Level := Engine.Current_Level - 1;
         Engine.Newline;
      end loop;
      Engine.Has_Paragraph := False;
      Engine.Has_Item := False;
   end Close_Paragraph;

   procedure Open_Paragraph (Engine : in out Html_Renderer) is
   begin
      if Engine.Need_Paragraph then
         Engine.Output.Start_Element ("p");
         Engine.Has_Paragraph  := True;
         Engine.Need_Paragraph := False;
      end if;
      if Engine.Current_Level > 0 and then not Engine.Has_Item then
         Engine.Output.Start_Element ("li");
         Engine.Has_Item := True;
      end if;
   end Open_Paragraph;

   --  ------------------------------
   --  Render a link.
   --  ------------------------------
   procedure Render_Link (Engine : in out Html_Renderer;
                          Doc    : in Wiki.Documents.Document;
                          Title  : in Wiki.Strings.WString;
                          Attr   : in Wiki.Attributes.Attribute_List) is

      procedure Render_Attribute (Name  : in String;
                                  Value : in Wiki.Strings.WString);

      procedure Render_Attribute (Name  : in String;
                                  Value : in Wiki.Strings.WString) is
      begin
         if Name = "href" then
            declare
               URI    : Wiki.Strings.UString;
               Exists : Boolean;
            begin
               Engine.Links.Make_Page_Link (Value, URI, Exists);
               Engine.Output.Write_Wide_Attribute ("href", URI);
            end;

         elsif Name = "label" then
            return;

         elsif Value'Length = 0 then
            return;

         elsif Name in "lang" | "title" | "rel" | "target" | "style" | "class" then
            Engine.Output.Write_Wide_Attribute (Name, Value);
         end if;
      end Render_Attribute;

      Label : constant Wiki.Strings.WString :=
           Wiki.Attributes.Get_Attribute (Attr, "label");
   begin
      Engine.Open_Paragraph;
      if Label'Length = 0 then
         Engine.Output.Start_Element ("a");
         Wiki.Attributes.Iterate (Attr, Render_Attribute'Access);
         Engine.Output.Write_Wide_Text (Title);
         Engine.Output.End_Element ("a");
      else
         declare
            Link : constant Wiki.Strings.WString
              := Doc.Get_Link (Label);
         begin
            if Link'Length = 0 then
               Engine.Output.Write_Wide_Text ("[");
               Engine.Output.Write_Wide_Text (Label);
               Engine.Output.Write_Wide_Text ("]");
            else
               Engine.Output.Start_Element ("a");
               declare
                  URI    : Wiki.Strings.UString;
                  Exists : Boolean;
                  Title  : constant Wiki.Strings.WString := Doc.Get_Link_Title (Label);
               begin
                  Engine.Links.Make_Page_Link (Link, URI, Exists);
                  Engine.Output.Write_Wide_Attribute ("href", URI);
                  if Title'Length > 0 then
                     Engine.Output.Write_Wide_Attribute ("title", Title);
                  end if;
               end;
               Engine.Output.Write_Wide_Text (Title);
               Engine.Output.End_Element ("a");
            end if;
         end;
      end if;
   end Render_Link;

   --  ------------------------------
   --  Render a link.
   --  ------------------------------
   procedure Render_Link_Ref (Engine : in out Html_Renderer;
                              Doc    : in Wiki.Documents.Document;
                              Label  : in Wiki.Strings.WString) is
      Link : constant Wiki.Strings.WString := Doc.Get_Link (Label);
   begin
      Engine.Open_Paragraph;
      if Link'Length = 0 then
         Engine.Output.Write_Wide_Text ("[");
         Engine.Output.Write_Wide_Text (Label);
         Engine.Output.Write_Wide_Text ("]");
      else
         Engine.Output.Start_Element ("a");
         declare
            URI    : Wiki.Strings.UString;
            Exists : Boolean;
            Title  : constant Wiki.Strings.WString := Doc.Get_Link_Title (Label);
         begin
            Engine.Links.Make_Page_Link (Link, URI, Exists);
            Engine.Output.Write_Wide_Attribute ("href", URI);
            if Title'Length > 0 then
               Engine.Output.Write_Wide_Attribute ("title", Title);
            end if;
         end;
         Engine.Output.Write_Wide_Text (Label);
         Engine.Output.End_Element ("a");
      end if;
   end Render_Link_Ref;

   --  ------------------------------
   --  Render an image.
   --  ------------------------------
   procedure Render_Image (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List) is
      pragma Unreferenced (Doc);

      Src        : constant Strings.WString := Attributes.Get_Attribute (Attr, "src");
      Alt        : constant Strings.WString := Attributes.Get_Attribute (Attr, "alt");
      Desc       : constant Strings.WString := Attributes.Get_Attribute (Attr, "longdesc");
      Title_Attr : constant Strings.WString := Attributes.Get_Attribute (Attr, "title");
      Class      : constant Strings.WString := Attributes.Get_Attribute (Attr, "class");
      Style      : constant Strings.WString := Attributes.Get_Attribute (Attr, "style");
      Size       : constant Strings.WString := Attributes.Get_Attribute (Attr, "size");
      Frame      : constant Strings.WString := Attributes.Get_Attribute (Attr, "frame");
      Align      : constant Strings.WString := Attributes.Get_Attribute (Attr, "align");
      Valign     : constant Strings.WString := Attributes.Get_Attribute (Attr, "valign");
      URI        : Wiki.Strings.UString;
      Frame_Attr : Wiki.Strings.UString;
      Width      : Natural := 0;
      Height     : Natural := 0;
   begin
      if Frame'Length > 0 then
         Strings.Append (Frame_Attr, "wiki-img-");
         Strings.Append (Frame_Attr, Frame);
      end if;
      if Align'Length > 0 then
         if Strings.Length (Frame_Attr) > 0 then
            Strings.Append (Frame_Attr, " ");
         end if;
         Strings.Append (Frame_Attr, "wiki-img-");
         Strings.Append (Frame_Attr, Align);
      end if;
      if Valign'Length > 0 then
         if Strings.Length (Frame_Attr) > 0 then
            Strings.Append (Frame_Attr, " ");
         end if;
         Strings.Append (Frame_Attr, "wiki-img-");
         Strings.Append (Frame_Attr, Valign);
      end if;
      Engine.Open_Paragraph;
      if Strings.Length (Frame_Attr) > 0 then
         Engine.Output.Start_Element ("div");
         Engine.Output.Write_Wide_Attribute ("class", Frame_Attr);
         Engine.Output.Start_Element ("div");
         Engine.Output.Write_Wide_Attribute ("class", "wiki-img-inner");
      end if;

      if Size'Length > 0 then
         Wiki.Helpers.Get_Sizes (Size, Width, Height);
      end if;
      Engine.Output.Start_Element ("img");
      Engine.Links.Make_Image_Link (Src, URI, Width, Height);
      Engine.Output.Write_Wide_Attribute ("src", URI);
      if Width > 0 then
         Engine.Output.Write_Attribute ("width", Util.Strings.Image (Width));
      end if;
      if Height > 0 then
         Engine.Output.Write_Attribute ("height", Util.Strings.Image (Height));
      end if;
      if Desc'Length > 0 then
         Engine.Output.Write_Wide_Attribute ("longdesc", Desc);
      end if;
      if Class'Length > 0 then
         Engine.Output.Write_Wide_Attribute ("class", Class);
      end if;
      if Style'Length > 0 then
         Engine.Output.Write_Wide_Attribute ("style", Style);
      end if;
      if Title'Length > 0 and then Alt'Length = 0 then
         Engine.Output.Write_Wide_Attribute ("alt", Title);
      elsif Alt'Length > 0 then
         Engine.Output.Write_Wide_Attribute ("alt", Alt);
      end if;
      if Title_Attr'Length > 0 then
         Engine.Output.Write_Wide_Attribute ("title", Title_Attr);
      end if;

      Engine.Output.End_Element ("img");
      if Strings.Length (Frame_Attr) > 0 then
         Engine.Output.End_Element ("div");
         if Title'Length > 0
           and then Frame /= "border"
           and then Frame /= "frameless"
           and then Frame /= ""
         then
            Engine.Output.Start_Element ("div");
            Engine.Output.Write_Wide_Attribute ("class", "wiki-img-caption");
            Engine.Output.Write_Wide_Text (Title);
            Engine.Output.End_Element ("div");
         end if;
         Engine.Output.End_Element ("div");
      end if;
   end Render_Image;

   --  ------------------------------
   --  Render a quote.
   --  ------------------------------
   procedure Render_Quote (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List) is
      pragma Unreferenced (Doc);

      procedure Render_Attribute (Name  : in String;
                                  Value : in Wiki.Strings.WString);

      procedure Render_Attribute (Name  : in String;
                                  Value : in Wiki.Strings.WString) is
      begin
         if Value'Length = 0 then
            return;

         elsif Name in "cite" | "title" | "lang" | "style" | "class" then
            Engine.Output.Write_Wide_Attribute (Name, Value);
         end if;
      end Render_Attribute;

   begin
      Engine.Open_Paragraph;
      Engine.Output.Start_Element ("q");
      Wiki.Attributes.Iterate (Attr, Render_Attribute'Access);
      Engine.Output.Write_Wide_Text (Title);
      Engine.Output.End_Element ("q");
   end Render_Quote;

   HTML_STRONG      : aliased constant String := "strong";
   HTML_BOLD        : aliased constant String := "b";
   HTML_ITALIC      : aliased constant String := "i";
   HTML_EM          : aliased constant String := "em";
   HTML_CODE        : aliased constant String := "code";
   HTML_SUPERSCRIPT : aliased constant String := "sup";
   HTML_SUBSCRIPT   : aliased constant String := "sub";
   HTML_STRIKEOUT   : aliased constant String := "del";
   HTML_INS         : aliased constant String := "ins";
   HTML_PREFORMAT   : aliased constant String := "pre";
   HTML_UNDERLINE   : aliased constant String := "u";
   HTML_CITE        : aliased constant String := "cite";

   type String_Array_Access is array (Format_Type) of Wiki.String_Access;

   HTML_ELEMENT     : constant String_Array_Access :=
     (STRONG      => HTML_STRONG'Access,
      BOLD        => HTML_BOLD'Access,
      ITALIC      => HTML_ITALIC'Access,
      EMPHASIS    => HTML_EM'Access,
      CODE        => HTML_CODE'Access,
      SUPERSCRIPT => HTML_SUPERSCRIPT'Access,
      SUBSCRIPT   => HTML_SUBSCRIPT'Access,
      STRIKEOUT   => HTML_STRIKEOUT'Access,
      PREFORMAT   => HTML_PREFORMAT'Access,
      INS         => HTML_INS'Access,
      CITE        => HTML_CITE'Access,
      UNDERLINE   => HTML_UNDERLINE'Access);

   --  ------------------------------
   --  Add a text block with the given format.
   --  ------------------------------
   procedure Add_Text (Engine   : in out Html_Renderer;
                       Text     : in Wiki.Strings.WString;
                       Format   : in Wiki.Format_Map) is
   begin
      if not Engine.Has_Html_Paragraph or else Engine.Html_Tag = Wiki.P_TAG then
         Engine.Open_Paragraph;
      elsif Engine.Need_Paragraph then
         Engine.Output.Write (' ');
         Engine.Need_Paragraph := False;
      end if;
      for I in Format'Range loop
         if Format (I) then
            Engine.Output.Start_Element (HTML_ELEMENT (I).all);
         end if;
      end loop;
      Engine.Output.Write_Wide_Text (Text);
      for I in reverse Format'Range loop
         if Format (I) then
            Engine.Output.End_Element (HTML_ELEMENT (I).all);
         end if;
      end loop;
   end Add_Text;

   --  ------------------------------
   --  Render a text block that is pre-formatted.
   --  ------------------------------
   procedure Render_Preformatted (Engine : in out Html_Renderer;
                                  Text   : in Wiki.Strings.WString;
                                  Format : in Wiki.Strings.WString) is
   begin
      Engine.Close_Paragraph;
      if Format = "html" then
         Engine.Output.Write (Text);
      else
         Engine.Output.Start_Element ("pre");
         if Format'Length > 0 then
            Engine.Output.Start_Element ("code");
            Engine.Output.Write_Attribute ("class", "lang-" & Strings.To_String (Format));
            Engine.Output.Write_Wide_Text (Text);
            Engine.Output.End_Element ("code");
         else
            Engine.Output.Start_Element ("code");
            Engine.Output.Write_Wide_Text (Text);
            Engine.Output.End_Element ("code");
         end if;
         Engine.Output.End_Element ("pre");
      end if;
   end Render_Preformatted;

   --  ------------------------------
   --  Render a table component such as N_TABLE, N_ROW or N_COLUMN.
   --  ------------------------------
   procedure Render_Table (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Documents.Document;
                           Node   : in Wiki.Nodes.Node_Type;
                           Tag    : in String) is
      Iter         : Wiki.Attributes.Cursor := Wiki.Attributes.First (Node.Attributes);
   begin
      Engine.Close_Paragraph;
      Engine.Need_Paragraph := False;
      Engine.Has_Paragraph := False;
      Engine.Open_Paragraph;

      Engine.Output.Start_Element (Tag);
      while Wiki.Attributes.Has_Element (Iter) loop
         Engine.Output.Write_Wide_Attribute (Name    => Wiki.Attributes.Get_Name (Iter),
                                             Content => Wiki.Attributes.Get_Wide_Value (Iter));
         Wiki.Attributes.Next (Iter);
      end loop;
      Engine.Render (Doc, Node.Children);

      Engine.Close_Paragraph;
      Engine.Has_Paragraph := False;
      Engine.Need_Paragraph := True;

      Engine.Output.End_Element (Tag);
   end Render_Table;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Documents.Document) is
   begin
      if Engine.Enable_Render_TOC and then Doc.Is_Visible_TOC then
         Engine.Render_TOC (Doc, 4);
      end if;
      Engine.Close_Paragraph;
      Engine.Add_Blockquote (0);
   end Finish;

end Wiki.Render.Html;
