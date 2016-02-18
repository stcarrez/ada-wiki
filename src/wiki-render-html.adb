-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
--  Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016 Stephane Carrez
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
with Ada.Characters.Conversions;
with Util.Strings;

package body Wiki.Render.Html is

   package ACC renames Ada.Characters.Conversions;

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
   procedure Set_Link_Renderer (Document : in out Html_Renderer;
                                Links    : in Link_Renderer_Access) is
   begin
      Document.Links := Links;
   end Set_Link_Renderer;

   --  ------------------------------
   --  Render the node instance from the document.
   --  ------------------------------
   overriding
   procedure Render (Engine : in out Html_Renderer;
                     Doc    : in Wiki.Nodes.Document;
                     Node   : in Wiki.Nodes.Node_Type) is
      use type Wiki.Nodes.Html_Tag_Type;
      use type Wiki.Nodes.Node_List_Access;
   begin
      case Node.Kind is
         when Wiki.Nodes.N_HEADER =>
            Engine.Render_Header (Header => Node.Header,
                                  Level  => Node.Level);

         when Wiki.Nodes.N_LINE_BREAK =>
            Engine.Output.Start_Element ("br");
            Engine.Output.End_Element ("br");

         when Wiki.Nodes.N_HORIZONTAL_RULE =>
            Engine.Close_Paragraph;
            Engine.Add_Blockquote (0);
            Engine.Output.Start_Element ("hr");
            Engine.Output.End_Element ("hr");

         when Wiki.Nodes.N_PARAGRAPH =>
            Engine.Close_Paragraph;
            Engine.Need_Paragraph := True;

         when Wiki.Nodes.N_PREFORMAT =>
            Engine.Render_Preformatted (Node.Preformatted, "");

         when Wiki.Nodes.N_INDENT =>
            -- Engine.Indent_Level := Node.Level;
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

      end case;
   end Render;

   procedure Render_Tag (Engine : in out Html_Renderer;
                         Doc    : in Wiki.Nodes.Document;
                         Node   : in Wiki.Nodes.Node_Type) is
      use type Wiki.Nodes.Html_Tag_Type;

      Name : constant Wiki.Nodes.String_Access := Wiki.Nodes.Get_Tag_Name (Node.Tag_Start);
      Iter : Wiki.Attributes.Cursor := Wiki.Attributes.First (Node.Attributes);
   begin
      if Node.Tag_Start = Wiki.Nodes.P_TAG then
         Engine.Has_Paragraph := True;
         Engine.Need_Paragraph := False;
      end if;
      Engine.Output.Start_Element (Name.all);
      while Wiki.Attributes.Has_Element (Iter) loop
         Engine.Output.Write_Wide_Attribute (Name    => Wiki.Attributes.Get_Name (Iter),
                                             Content => Wiki.Attributes.Get_Wide_Value (Iter));
         Wiki.Attributes.Next (Iter);
      end loop;
      Engine.Render (Doc, Node.Children);
      if Node.Tag_Start = Wiki.Nodes.P_TAG then
         Engine.Has_Paragraph := False;
         Engine.Need_Paragraph := True;
      end if;
      Engine.Output.End_Element (Name.all);
   end Render_Tag;

   --  ------------------------------
   --  Render a section header in the document.
   --  ------------------------------
   procedure Render_Header (Engine : in out Html_Renderer;
                            Header : in Wiki.Strings.WString;
                            Level  : in Positive) is
   begin
      Engine.Close_Paragraph;
      Engine.Add_Blockquote (0);
      case Level is
         when 1 =>
            Engine.Output.Write_Wide_Element ("h1", Header);

         when 2 =>
            Engine.Output.Write_Wide_Element ("h2", Header);

         when 3 =>
            Engine.Output.Write_Wide_Element ("h3", Header);

         when 4 =>
            Engine.Output.Write_Wide_Element ("h4", Header);

         when 5 =>
            Engine.Output.Write_Wide_Element ("h5", Header);

         when 6 =>
            Engine.Output.Write_Wide_Element ("h6", Header);

         when others =>
            Engine.Output.Write_Wide_Element ("h3", Header);
      end case;
   end Render_Header;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Add_Blockquote (Document : in out Html_Renderer;
                             Level    : in Natural) is
   begin
      if Document.Quote_Level /= Level then
         Document.Close_Paragraph;
         Document.Need_Paragraph := True;
      end if;
      while Document.Quote_Level < Level loop
         Document.Output.Start_Element ("blockquote");
         Document.Quote_Level := Document.Quote_Level + 1;
      end loop;
      while Document.Quote_Level > Level loop
         Document.Output.End_Element ("blockquote");
         Document.Quote_Level := Document.Quote_Level - 1;
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

   procedure Close_Paragraph (Document : in out Html_Renderer) is
   begin
      if Document.Html_Level > 0 then
         return;
      end if;
      if Document.Has_Paragraph then
         Document.Output.End_Element ("p");
      end if;
      if Document.Has_Item then
         Document.Output.End_Element ("li");
      end if;
      while Document.Current_Level > 0 loop
         if Document.List_Styles (Document.Current_Level) then
            Document.Output.End_Element ("ol");
         else
            Document.Output.End_Element ("ul");
         end if;
         Document.Current_Level := Document.Current_Level - 1;
      end loop;
      Document.Has_Paragraph := False;
      Document.Has_Item := False;
   end Close_Paragraph;

   procedure Open_Paragraph (Document : in out Html_Renderer) is
   begin
      if Document.Html_Level > 0 then
         return;
      end if;
      if Document.Need_Paragraph then
         Document.Output.Start_Element ("p");
         Document.Has_Paragraph  := True;
         Document.Need_Paragraph := False;
      end if;
      if Document.Current_Level > 0 and not Document.Has_Item then
         Document.Output.Start_Element ("li");
         Document.Has_Item := True;
      end if;
   end Open_Paragraph;

   --  ------------------------------
   --  Render a link.
   --  ------------------------------
   procedure Render_Link (Engine : in out Html_Renderer;
                          Doc    : in Wiki.Nodes.Document;
                          Title  : in Wiki.Strings.WString;
                          Attr   : in Wiki.Attributes.Attribute_List_Type) is

      procedure Render_Attribute (Name  : in String;
                                  Value : in Wide_Wide_String) is
      begin
         if Name = "href" then
            declare
               URI    : Unbounded_Wide_Wide_String;
               Exists : Boolean;
            begin
               Engine.Links.Make_Page_Link (Value, URI, Exists);
               Engine.Output.Write_Wide_Attribute ("href", URI);
            end;

         elsif Value'Length = 0 then
            return;

         elsif Name = "lang" or Name = "title" or Name = "rel" or Name = "target"
         or Name = "style" or Name = "class" then
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
                           Doc    : in Wiki.Nodes.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List_Type) is
      Link   : Unbounded_Wide_Wide_String := Wiki.Attributes.Get_Attribute (Attr, "href");
      URI    : Unbounded_Wide_Wide_String;
      Width  : Natural;
      Height : Natural;

      procedure Render_Attribute (Name  : in String;
                                  Value : in Wide_Wide_String) is
      begin
         if Name = "alt" or Name = "longdesc"
           or Name = "style" or Name = "class" then
            Engine.Output.Write_Wide_Attribute (Name, Value);
         end if;
      end Render_Attribute;

   begin
      Engine.Open_Paragraph;
      Engine.Output.Start_Element ("img");
      Engine.Links.Make_Image_Link (Link, URI, Width, Height);
      Engine.Output.Write_Wide_Attribute ("src", URI);
      if Width > 0 then
         Engine.Output.Write_Attribute ("width", Natural'Image (Width));
      end if;
      if Height > 0 then
         Engine.Output.Write_Attribute ("height", Natural'Image (Height));
      end if;
      Wiki.Attributes.Iterate (Attr, Render_Attribute'Access);
      Engine.Output.End_Element ("img");
   end Render_Image;

   --  ------------------------------
   --  Render a quote.
   --  ------------------------------
   procedure Render_Quote (Engine : in out Html_Renderer;
                           Doc    : in Wiki.Nodes.Document;
                           Title  : in Wiki.Strings.WString;
                           Attr   : in Wiki.Attributes.Attribute_List_Type) is

      procedure Render_Attribute (Name  : in String;
                                  Value : in Wide_Wide_String) is
      begin
         if Value'Length = 0 then
            return;

         elsif Name = "cite" or Name = "title" or Name = "lang" or Name = "style" or Name = "class" then
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

   type String_Array_Access is array (Format_Type) of Util.Strings.Name_Access;

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
      Engine.Open_Paragraph;
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
   procedure Finish (Document : in out Html_Renderer) is
   begin
      Document.Close_Paragraph;
      Document.Add_Blockquote (0);
   end Finish;

end Wiki.Render.Html;
