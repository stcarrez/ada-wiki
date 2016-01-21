-----------------------------------------------------------------------
--  wiki-render-text -- Wiki Text renderer
--  Copyright (C) 2011, 2012, 2013, 2015 Stephane Carrez
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
with Wiki.Filters.Html;
package body Wiki.Render.Text is

   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#0A#);

   --  ------------------------------
   --  Set the output writer.
   --  ------------------------------
   procedure Set_Writer (Document : in out Text_Renderer;
                         Writer   : in Wiki.Writers.Writer_Type_Access) is
   begin
      Document.Writer := Writer;
   end Set_Writer;

   --  ------------------------------
   --  Emit a new line.
   --  ------------------------------
   procedure New_Line (Document : in out Text_Renderer) is
   begin
      Document.Writer.Write (LF);
      Document.Empty_Line := True;
   end New_Line;

   --  ------------------------------
   --  Add a section header in the document.
   --  ------------------------------
   overriding
   procedure Add_Header (Document : in out Text_Renderer;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive) is
      pragma Unreferenced (Level);
   begin
      Document.Close_Paragraph;
      if not Document.Empty_Line then
         Document.Add_Line_Break;
      end if;
      Document.Writer.Write (Header);
      Document.Add_Line_Break;
   end Add_Header;

   --  ------------------------------
   --  Add a line break (<br>).
   --  ------------------------------
   overriding
   procedure Add_Line_Break (Document : in out Text_Renderer) is
   begin
      Document.Writer.Write (Ada.Characters.Conversions.To_Wide_Wide_Character (ASCII.LF));
      Document.Empty_Line := True;
   end Add_Line_Break;

   --  ------------------------------
   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   --  ------------------------------
   overriding
   procedure Add_Paragraph (Document : in out Text_Renderer) is
   begin
      Document.Close_Paragraph;
      Document.Need_Paragraph := True;
      Document.Add_Line_Break;
   end Add_Paragraph;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   overriding
   procedure Add_Blockquote (Document : in out Text_Renderer;
                             Level    : in Natural) is
   begin
      Document.Close_Paragraph;
      for I in 1 .. Level loop
         Document.Writer.Write ("  ");
      end loop;
   end Add_Blockquote;

   --  ------------------------------
   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   overriding
   procedure Add_List_Item (Document : in out Text_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean) is
      pragma Unreferenced (Level, Ordered);
   begin
      if not Document.Empty_Line then
         Document.Add_Line_Break;
      end if;
      Document.Need_Paragraph := False;
      Document.Open_Paragraph;
   end Add_List_Item;

   procedure Close_Paragraph (Document : in out Text_Renderer) is
   begin
      if Document.Has_Paragraph then
         Document.Add_Line_Break;
      end if;
      Document.Has_Paragraph := False;
   end Close_Paragraph;

   procedure Open_Paragraph (Document : in out Text_Renderer) is
   begin
      if Document.Need_Paragraph then
         Document.Has_Paragraph  := True;
         Document.Need_Paragraph := False;
      end if;
   end Open_Paragraph;

   --  ------------------------------
   --  Add an horizontal rule (<hr>).
   --  ------------------------------
   overriding
   procedure Add_Horizontal_Rule (Document : in out Text_Renderer) is
   begin
      Document.Close_Paragraph;
      Document.Writer.Write ("---------------------------------------------------------");
      Document.Add_Line_Break;
   end Add_Horizontal_Rule;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   overriding
   procedure Add_Link (Document : in out Text_Renderer;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Language);
   begin
      Document.Open_Paragraph;
      if Length (Title) > 0 then
         Document.Writer.Write (Title);
      end if;
      Document.Writer.Write (Link);
      if Link /= Name then
         Document.Writer.Write (Name);
      end if;
      Document.Empty_Line := False;
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   overriding
   procedure Add_Image (Document    : in out Text_Renderer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Position);
   begin
      Document.Open_Paragraph;
      if Length (Alt) > 0 then
         Document.Writer.Write (Alt);
      end if;
      if Length (Description) > 0 then
         Document.Writer.Write (Description);
      end if;
      Document.Writer.Write (Link);
      Document.Empty_Line := False;
   end Add_Image;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   overriding
   procedure Add_Quote (Document : in out Text_Renderer;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Link, Language);
   begin
      Document.Open_Paragraph;
      Document.Writer.Write (Quote);
      Document.Empty_Line := False;
   end Add_Quote;

   --  ------------------------------
   --  Add a text block with the given format.
   --  ------------------------------
   overriding
   procedure Add_Text (Document : in out Text_Renderer;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Wiki.Documents.Format_Map) is
      pragma Unreferenced (Format);
   begin
      if Document.Empty_Line and Document.Indent_Level /= 0 then
         for I in 1 .. Document.Indent_Level loop
            Document.Writer.Write (' ');
         end loop;
      end if;
      Document.Writer.Write (Text);
      Document.Empty_Line := False;
   end Add_Text;

   --  ------------------------------
   --  Add a text block that is pre-formatted.
   --  ------------------------------
   procedure Add_Preformatted (Document : in out Text_Renderer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String) is
      pragma Unreferenced (Format);
   begin
      Document.Close_Paragraph;
      Document.Writer.Write (Text);
      Document.Empty_Line := False;
   end Add_Preformatted;

   overriding
   procedure Start_Element (Document   : in out Text_Renderer;
                            Name       : in Unbounded_Wide_Wide_String;
                            Attributes : in Wiki.Attributes.Attribute_List_Type) is
      pragma Unreferenced (Attributes);
      use type Wiki.Filters.Html.Html_Tag_Type;

      Tag : constant Wiki.Filters.Html.Html_Tag_Type := Wiki.Filters.Html.Find_Tag (Name);
   begin
      if Tag = Wiki.Filters.Html.DT_TAG then
         Document.Close_Paragraph;
         Document.Indent_Level := 0;
      elsif Tag = Wiki.Filters.Html.DD_TAG then
         Document.Close_Paragraph;
         Document.Empty_Line := True;
         Document.Indent_Level := 4;
      end if;
   end Start_Element;

   overriding
   procedure End_Element (Document : in out Text_Renderer;
                          Name     : in Unbounded_Wide_Wide_String) is
      use type Wiki.Filters.Html.Html_Tag_Type;

      Tag : constant Wiki.Filters.Html.Html_Tag_Type := Wiki.Filters.Html.Find_Tag (Name);
   begin
      if Tag = Wiki.Filters.Html.DT_TAG then
         Document.Close_Paragraph;
         Document.Indent_Level := 0;
      elsif Tag = Wiki.Filters.Html.DD_TAG then
         Document.Close_Paragraph;
         Document.Indent_Level := 0;
      elsif Tag = Wiki.Filters.Html.DL_TAG then
         Document.Close_Paragraph;
         Document.New_Line;
      end if;
   end End_Element;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Document : in out Text_Renderer) is
   begin
      Document.Close_Paragraph;
   end Finish;

end Wiki.Render.Text;
