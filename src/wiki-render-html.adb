-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
--  Copyright (C) 2011, 2012, 2013, 2014, 2015 Stephane Carrez
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

   --  ------------------------------
   --  Set the output writer.
   --  ------------------------------
   procedure Set_Writer (Document : in out Html_Renderer;
                         Writer   : in Wiki.Writers.Html_Writer_Type_Access) is
   begin
      Document.Writer := Writer;
   end Set_Writer;

   --  ------------------------------
   --  Set the link renderer.
   --  ------------------------------
   procedure Set_Link_Renderer (Document : in out Html_Renderer;
                                Links    : in Link_Renderer_Access) is
   begin
      Document.Links := Links;
   end Set_Link_Renderer;

   --  ------------------------------
   --  Add a section header in the document.
   --  ------------------------------
   overriding
   procedure Add_Header (Document : in out Html_Renderer;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive) is
   begin
      Document.Close_Paragraph;
      Document.Add_Blockquote (0);
      case Level is
         when 1 =>
            Document.Writer.Write_Wide_Element ("h1", Header);

         when 2 =>
            Document.Writer.Write_Wide_Element ("h2", Header);

         when 3 =>
            Document.Writer.Write_Wide_Element ("h3", Header);

         when 4 =>
            Document.Writer.Write_Wide_Element ("h4", Header);

         when 5 =>
            Document.Writer.Write_Wide_Element ("h5", Header);

         when 6 =>
            Document.Writer.Write_Wide_Element ("h6", Header);

         when others =>
            Document.Writer.Write_Wide_Element ("h3", Header);
      end case;
   end Add_Header;

   --  ------------------------------
   --  Add a line break (<br>).
   --  ------------------------------
   overriding
   procedure Add_Line_Break (Document : in out Html_Renderer) is
   begin
      --  Document.Writer.Write_Raw ("<br />");
      Document.Writer.Write ("<br />");
   end Add_Line_Break;

   --  ------------------------------
   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   --  ------------------------------
   overriding
   procedure Add_Paragraph (Document : in out Html_Renderer) is
   begin
      Document.Close_Paragraph;
      Document.Need_Paragraph := True;
   end Add_Paragraph;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   overriding
   procedure Add_Blockquote (Document : in out Html_Renderer;
                             Level    : in Natural) is
   begin
      if Document.Quote_Level /= Level then
         Document.Close_Paragraph;
         Document.Need_Paragraph := True;
      end if;
      while Document.Quote_Level < Level loop
         Document.Writer.Start_Element ("blockquote");
         Document.Quote_Level := Document.Quote_Level + 1;
      end loop;
      while Document.Quote_Level > Level loop
         Document.Writer.End_Element ("blockquote");
         Document.Quote_Level := Document.Quote_Level - 1;
      end loop;
   end Add_Blockquote;

   --  ------------------------------
   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   overriding
   procedure Add_List_Item (Document : in out Html_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean) is
   begin
      if Document.Has_Paragraph then
         Document.Writer.End_Element ("p");
         Document.Has_Paragraph := False;
      end if;
      if Document.Has_Item then
         Document.Writer.End_Element ("li");
         Document.Has_Item := False;
      end if;
      Document.Need_Paragraph := False;
      Document.Open_Paragraph;
      while Document.Current_Level < Level loop
         if Ordered then
            Document.Writer.Start_Element ("ol");
         else
            Document.Writer.Start_Element ("ul");
         end if;
         Document.Current_Level := Document.Current_Level + 1;
         Document.List_Styles (Document.Current_Level) := Ordered;
      end loop;
   end Add_List_Item;

   procedure Close_Paragraph (Document : in out Html_Renderer) is
   begin
      if Document.Html_Level > 0 then
         return;
      end if;
      if Document.Has_Paragraph then
         Document.Writer.End_Element ("p");
      end if;
      if Document.Has_Item then
         Document.Writer.End_Element ("li");
      end if;
      while Document.Current_Level > 0 loop
         if Document.List_Styles (Document.Current_Level) then
            Document.Writer.End_Element ("ol");
         else
            Document.Writer.End_Element ("ul");
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
         Document.Writer.Start_Element ("p");
         Document.Has_Paragraph  := True;
         Document.Need_Paragraph := False;
      end if;
      if Document.Current_Level > 0 and not Document.Has_Item then
         Document.Writer.Start_Element ("li");
         Document.Has_Item := True;
      end if;
   end Open_Paragraph;

   --  ------------------------------
   --  Add an horizontal rule (<hr>).
   --  ------------------------------
   overriding
   procedure Add_Horizontal_Rule (Document : in out Html_Renderer) is
   begin
      Document.Close_Paragraph;
      Document.Add_Blockquote (0);
      Document.Writer.Start_Element ("hr");
      Document.Writer.End_Element ("hr");
   end Add_Horizontal_Rule;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   overriding
   procedure Add_Link (Document : in out Html_Renderer;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String) is
      Exists : Boolean;
      URI    : Unbounded_Wide_Wide_String;
   begin
      Document.Open_Paragraph;
      Document.Writer.Start_Element ("a");
      if Length (Title) > 0 then
         Document.Writer.Write_Wide_Attribute ("title", Title);
      end if;
      if Length (Language) > 0 then
         Document.Writer.Write_Wide_Attribute ("lang", Language);
      end if;
      Document.Links.Make_Page_Link (Link, URI, Exists);
      Document.Writer.Write_Wide_Attribute ("href", URI);
      Document.Writer.Write_Wide_Text (Name);
      Document.Writer.End_Element ("a");
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   overriding
   procedure Add_Image (Document    : in out Html_Renderer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Position);

      URI    : Unbounded_Wide_Wide_String;
      Width  : Natural;
      Height : Natural;
   begin
      Document.Open_Paragraph;
      Document.Writer.Start_Element ("img");
      if Length (Alt) > 0 then
         Document.Writer.Write_Wide_Attribute ("alt", Alt);
      end if;
      if Length (Description) > 0 then
         Document.Writer.Write_Wide_Attribute ("longdesc", Description);
      end if;
      Document.Links.Make_Image_Link (Link, URI, Width, Height);
      Document.Writer.Write_Wide_Attribute ("src", URI);
      if Width > 0 then
         Document.Writer.Write_Attribute ("width", Natural'Image (Width));
      end if;
      if Height > 0 then
         Document.Writer.Write_Attribute ("height", Natural'Image (Height));
      end if;
      Document.Writer.End_Element ("img");
   end Add_Image;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   overriding
   procedure Add_Quote (Document : in out Html_Renderer;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String) is
   begin
      Document.Open_Paragraph;
      Document.Writer.Start_Element ("q");
      if Length (Language) > 0 then
         Document.Writer.Write_Wide_Attribute ("lang", Language);
      end if;
      if Length (Link) > 0 then
         Document.Writer.Write_Wide_Attribute ("cite", Link);
      end if;
      Document.Writer.Write_Wide_Text (Quote);
      Document.Writer.End_Element ("q");
   end Add_Quote;

   HTML_BOLD        : aliased constant String := "b";
   HTML_ITALIC      : aliased constant String := "i";
   HTML_CODE        : aliased constant String := "tt";
   HTML_SUPERSCRIPT : aliased constant String := "sup";
   HTML_SUBSCRIPT   : aliased constant String := "sub";
   HTML_STRIKEOUT   : aliased constant String := "del";
   --  HTML_UNDERLINE   : aliased constant String := "ins";
   HTML_PREFORMAT   : aliased constant String := "pre";

   type String_Array_Access is array (Documents.Format_Type) of Util.Strings.Name_Access;

   HTML_ELEMENT     : constant String_Array_Access :=
     (Documents.BOLD        => HTML_BOLD'Access,
      Documents.ITALIC      => HTML_ITALIC'Access,
      Documents.CODE        => HTML_CODE'Access,
      Documents.SUPERSCRIPT => HTML_SUPERSCRIPT'Access,
      Documents.SUBSCRIPT   => HTML_SUBSCRIPT'Access,
      Documents.STRIKEOUT   => HTML_STRIKEOUT'Access,
      Documents.PREFORMAT   => HTML_PREFORMAT'Access);

   --  ------------------------------
   --  Add a text block with the given format.
   --  ------------------------------
   overriding
   procedure Add_Text (Document : in out Html_Renderer;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Wiki.Documents.Format_Map) is
   begin
      Document.Open_Paragraph;
      for I in Format'Range loop
         if Format (I) then
            Document.Writer.Start_Element (HTML_ELEMENT (I).all);
         end if;
      end loop;
      Document.Writer.Write_Wide_Text (Text);
      for I in reverse Format'Range loop
         if Format (I) then
            Document.Writer.End_Element (HTML_ELEMENT (I).all);
         end if;
      end loop;
   end Add_Text;

   --  ------------------------------
   --  Add a text block that is pre-formatted.
   --  ------------------------------
   procedure Add_Preformatted (Document : in out Html_Renderer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String) is
   begin
      Document.Close_Paragraph;
      if Format = "html" then
         Document.Writer.Write (Text);
      else
         Document.Writer.Start_Element ("pre");
         Document.Writer.Write_Wide_Text (Text);
         Document.Writer.End_Element ("pre");
      end if;
   end Add_Preformatted;

   overriding
   procedure Start_Element (Document   : in out Html_Renderer;
                            Name       : in Unbounded_Wide_Wide_String;
                            Attributes : in Wiki.Attributes.Attribute_List_Type) is
   begin
      Document.Html_Level := Document.Html_Level + 1;
      if Name = "p" then
         Document.Has_Paragraph := True;
         Document.Need_Paragraph := False;
      end if;
      Document.Writer.Start_Element (Ada.Characters.Conversions.To_String (To_Wide_Wide_String (Name)));
   end Start_Element;

   overriding
   procedure End_Element (Document : in out Html_Renderer;
                          Name     : in Unbounded_Wide_Wide_String) is
   begin
      Document.Html_Level := Document.Html_Level - 1;
      if Name = "p" then
         Document.Has_Paragraph := False;
         Document.Need_Paragraph := True;
      end if;
      Document.Writer.End_Element (Ada.Characters.Conversions.To_String (To_Wide_Wide_String (Name)));
   end End_Element;

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
