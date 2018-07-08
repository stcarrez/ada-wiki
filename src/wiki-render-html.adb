-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2018 Stephane Carrez
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
package body Wiki.Render.Html is

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
         if Value > 0 or not Empty then
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
      return Engine.Html_Tag = Wiki.SPAN_TAG
        or Engine.Html_Tag = Wiki.A_TAG
        or Engine.Html_Tag = Wiki.EM_TAG
        or Engine.Html_Tag = Wiki.STRONG_TAG
        or Engine.Html_Tag = Wiki.SMALL_TAG
        or Engine.Html_Tag = Wiki.B_TAG
        or Engine.Html_Tag = Wiki.I_TAG
        or Engine.Html_Tag = Wiki.U_TAG
        or Engine.Html_Tag = Wiki.S_TAG
        or Engine.Html_Tag = Wiki.SUB_TAG
        or Engine.Html_Tag = Wiki.SUP_TAG
        or Engine.Html_Tag = Wiki.INS_TAG
        or Engine.Html_Tag = Wiki.DEL_TAG;
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
         when Wiki.Nodes.N_HEADER =>
            if Node.Level > 6 then
               Engine.Render_Header (Doc    => Doc,
                                     Header => Node.Header,
                                     Level  => 6);
            else
               Engine.Render_Header (Doc    => Doc,
                                     Header => Node.Header,
                                     Level  => Node.Level);
            end if;

         when Wiki.Nodes.N_LINE_BREAK =>
            Engine.Output.Start_Element ("br");
            Engine.Output.End_Element ("br");

         when Wiki.Nodes.N_HORIZONTAL_RULE =>
            if Engine.Html_Level = 0 or not Engine.Has_Html_Paragraph then
               Engine.Close_Paragraph;
               Engine.Add_Blockquote (0);
            end if;
            Engine.Output.Start_Element ("hr");
            Engine.Output.End_Element ("hr");

         when Wiki.Nodes.N_PARAGRAPH =>
            --  Close the paragraph and start a new one except if the current HTML
            --  element is within a paragraph (ex: a, b, i, u, span, ...).
            if Engine.Html_Level = 0 or not Engine.Has_Html_Paragraph then
               Engine.Close_Paragraph;
               Engine.Need_Paragraph := True;
            end if;

         when Wiki.Nodes.N_PREFORMAT =>
            Engine.Render_Preformatted (Node.Preformatted, "");

         when Wiki.Nodes.N_INDENT =>
            null;

         when Wiki.Nodes.N_LIST =>
            Engine.Render_List_Item (Node.Level, False);

         when Wiki.Nodes.N_NUM_LIST =>
            Engine.Render_List_Item (Node.Level, True);

         when Wiki.Nodes.N_TEXT =>
            Engine.Add_Text (Text   => Node.Text,
                             Format => Node.Format);

         when Wiki.Nodes.N_QUOTE =>
            Engine.Render_Quote (Doc, Node.Title, Node.Link_Attr);

         when Wiki.Nodes.N_LINK =>
            Engine.Render_Link (Doc, Node.Title, Node.Link_Attr);

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
      elsif Node.Tag_Start = Wiki.UL_TAG
        or Node.Tag_Start = Wiki.OL_TAG
        or Node.Tag_Start = Wiki.DL_TAG
        or Node.Tag_Start = Wiki.DT_TAG
        or Node.Tag_Start = Wiki.DD_TAG
        or Node.Tag_Start = Wiki.LI_TAG
        or Node.Tag_Start = Wiki.H1_TAG
        or Node.Tag_Start = Wiki.H2_TAG
        or Node.Tag_Start = Wiki.H3_TAG
        or Node.Tag_Start = Wiki.H4_TAG
        or Node.Tag_Start = Wiki.H5_TAG
        or Node.Tag_Start = Wiki.H6_TAG
        or Node.Tag_Start = Wiki.DIV_TAG
        or Node.Tag_Start = Wiki.TABLE_TAG
      then
         Engine.Close_Paragraph;
         Engine.Need_Paragraph := False;
         Engine.Has_Paragraph := False;
         Engine.Open_Paragraph;
      elsif Node.Tag_Start = Wiki.B_TAG
        or Node.Tag_Start = Wiki.I_TAG
        or Node.Tag_Start = Wiki.SPAN_TAG
        or Node.Tag_Start = Wiki.INS_TAG
        or Node.Tag_Start = Wiki.DEL_TAG
        or Node.Tag_Start = Wiki.A_TAG
      then
         Engine.Open_Paragraph;
         Prev_Para := Engine.Has_Paragraph;
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
      elsif Node.Tag_Start = Wiki.UL_TAG
        or Node.Tag_Start = Wiki.OL_TAG
        or Node.Tag_Start = Wiki.DL_TAG
        or Node.Tag_Start = Wiki.DT_TAG
        or Node.Tag_Start = Wiki.DD_TAG
        or Node.Tag_Start = Wiki.LI_TAG
        or Node.Tag_Start = Wiki.H1_TAG
        or Node.Tag_Start = Wiki.H2_TAG
        or Node.Tag_Start = Wiki.H3_TAG
        or Node.Tag_Start = Wiki.H4_TAG
        or Node.Tag_Start = Wiki.H5_TAG
        or Node.Tag_Start = Wiki.H6_TAG
        or Node.Tag_Start = Wiki.DIV_TAG
        or Node.Tag_Start = Wiki.TABLE_TAG
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
                            Header : in Wiki.Strings.WString;
                            Level  : in Positive) is
      Tag   : String_Access;
   begin
      if Engine.Enable_Render_TOC and not Engine.TOC_Rendered and not Doc.Is_Using_TOC
        and Doc.Is_Visible_TOC
      then
         Engine.Render_TOC (Doc, 3);
      end if;
      Engine.Close_Paragraph;
      Engine.Add_Blockquote (0);
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

         when 6 =>
            Tag := Get_Tag_Name (H6_TAG);

         when others =>
            Tag := Get_Tag_Name (H3_TAG);

      end case;
      Engine.Output.Start_Element (Tag.all);
      if Engine.Enable_Render_TOC then
         Engine.Output.Write_Wide_Attribute ("id", Engine.Get_Section_Number ("section_", '_'));
      end if;
      Engine.Output.Write_Wide_Text (Header);
      Engine.Output.End_Element (Tag.all);
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
         if Node.Kind /= Wiki.Nodes.N_TOC_ENTRY or else Node.Level > Level then
            return;
         end if;
         Set_Current_Level (Node.Level);
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

      Toc : constant Wiki.Nodes.Node_List_Ref := Doc.Get_TOC;
   begin
      if Wiki.Nodes.Length (Toc) <= 3 then
         Engine.Enable_Render_TOC := False;

      elsif not Engine.TOC_Rendered then
         Engine.Section_Level := 0;
         Engine.Current_Section := (others => 0);
         Engine.Output.Start_Element ("div");
         Engine.Output.Write_Attribute ("class", "wiki-toc");
         Wiki.Nodes.Iterate (Toc, Render_Entry'Access);
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
   --  Render a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Render_List_Item (Engine   : in out Html_Renderer;
                               Level    : in Positive;
                               Ordered  : in Boolean) is
   begin
      if Engine.Has_Paragraph then
         Engine.Output.End_Element ("p");
         Engine.Has_Paragraph := False;
      end if;
      if Engine.Has_Item then
         Engine.Output.End_Element ("li");
         Engine.Has_Item := False;
      end if;
      Engine.Need_Paragraph := False;
      Engine.Open_Paragraph;
      while Engine.Current_Level < Level loop
         if Ordered then
            Engine.Output.Start_Element ("ol");
         else
            Engine.Output.Start_Element ("ul");
         end if;
         Engine.Current_Level := Engine.Current_Level + 1;
         Engine.List_Styles (Engine.Current_Level) := Ordered;
      end loop;
   end Render_List_Item;

   procedure Close_Paragraph (Engine : in out Html_Renderer) is
   begin
      --  Don't close a paragraph if we are within a HTML text-level element.
      if Engine.Has_Html_Paragraph then
         return;
      end if;
      if Engine.Has_Paragraph then
         Engine.Output.End_Element ("p");
      end if;
      if Engine.Has_Item then
         Engine.Output.End_Element ("li");
      end if;
      while Engine.Current_Level > 0 loop
         if Engine.List_Styles (Engine.Current_Level) then
            Engine.Output.End_Element ("ol");
         else
            Engine.Output.End_Element ("ul");
         end if;
         Engine.Current_Level := Engine.Current_Level - 1;
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
      if Engine.Current_Level > 0 and not Engine.Has_Item then
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
      pragma Unreferenced (Doc);

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

         elsif Value'Length = 0 then
            return;

         elsif Name = "lang" or Name = "title" or Name = "rel" or Name = "target"
           or Name = "style" or Name = "class"
         then
            Engine.Output.Write_Wide_Attribute (Name, Value);
         end if;
      end Render_Attribute;

   begin
      Engine.Open_Paragraph;
      Engine.Output.Start_Element ("a");
      Wiki.Attributes.Iterate (Attr, Render_Attribute'Access);
      Engine.Output.Write_Wide_Text (Title);
      Engine.Output.End_Element ("a");
   end Render_Link;

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
         Engine.Output.Write_Attribute ("width", Natural'Image (Width));
      end if;
      if Height > 0 then
         Engine.Output.Write_Attribute ("height", Natural'Image (Height));
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
      if Title'Length > 0 and Alt'Length = 0 then
         Engine.Output.Write_Wide_Attribute ("alt", Title);
      elsif Alt'Length > 0 then
         Engine.Output.Write_Wide_Attribute ("alt", Alt);
      end if;

      Engine.Output.End_Element ("img");
      if Strings.Length (Frame_Attr) > 0 then
         Engine.Output.End_Element ("div");
         if Title'Length > 0 and Frame /= "border" and Frame /= "frameless" and Frame /= "" then
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

         elsif Name = "cite" or Name = "title" or Name = "lang"
           or Name = "style" or Name = "class"
         then
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

   HTML_BOLD        : aliased constant String := "b";
   HTML_ITALIC      : aliased constant String := "i";
   HTML_CODE        : aliased constant String := "tt";
   HTML_SUPERSCRIPT : aliased constant String := "sup";
   HTML_SUBSCRIPT   : aliased constant String := "sub";
   HTML_STRIKEOUT   : aliased constant String := "del";
   --  HTML_UNDERLINE   : aliased constant String := "ins";
   HTML_PREFORMAT   : aliased constant String := "pre";

   type String_Array_Access is array (Format_Type) of Wiki.String_Access;

   HTML_ELEMENT     : constant String_Array_Access :=
     (BOLD        => HTML_BOLD'Access,
      ITALIC      => HTML_ITALIC'Access,
      CODE        => HTML_CODE'Access,
      SUPERSCRIPT => HTML_SUPERSCRIPT'Access,
      SUBSCRIPT   => HTML_SUBSCRIPT'Access,
      STRIKEOUT   => HTML_STRIKEOUT'Access,
      PREFORMAT   => HTML_PREFORMAT'Access);

   --  ------------------------------
   --  Add a text block with the given format.
   --  ------------------------------
   procedure Add_Text (Engine   : in out Html_Renderer;
                       Text     : in Wiki.Strings.WString;
                       Format   : in Wiki.Format_Map) is
   begin
      if not Engine.Has_Html_Paragraph or Engine.Html_Tag = Wiki.P_TAG then
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
         Engine.Output.Write_Wide_Text (Text);
         Engine.Output.End_Element ("pre");
      end if;
   end Render_Preformatted;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Documents.Document) is
   begin
      if Engine.Enable_Render_TOC and Doc.Is_Visible_TOC then
         Engine.Render_TOC (Doc, 4);
      end if;
      Engine.Close_Paragraph;
      Engine.Add_Blockquote (0);
   end Finish;

end Wiki.Render.Html;
