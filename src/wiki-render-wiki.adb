-----------------------------------------------------------------------
--  wiki-render-wiki -- Wiki to Wiki renderer
--  Copyright (C) 2015 Stephane Carrez
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
with Wiki.Filters.Html;
package body Wiki.Render.Wiki is

   HEADER_CREOLE     : aliased constant Wide_Wide_String := "=";
   BOLD_CREOLE       : aliased constant Wide_Wide_String := "**";
   LINE_BREAK_CREOLE : aliased constant Wide_Wide_String := "%%%";

   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#0A#);

   --  Set the output writer.
   procedure Set_Writer (Document : in out Wiki_Renderer;
                         Writer   : in Writers.Writer_Type_Access;
                         Format   : in Parsers.Wiki_Syntax_Type) is
   begin
      Document.Writer := Writer;
      Document.Syntax := Format;
      case Format is
         when others =>
            Document.Tags (Bold_Start)   := BOLD_CREOLE'Access;
            Document.Tags (Bold_End)     := BOLD_CREOLE'Access;
            Document.Tags (Header_Start) := HEADER_CREOLE'Access;
            Document.Tags (Header_End)   := HEADER_CREOLE'Access;
            Document.Tags (Line_Break)   := LINE_BREAK_CREOLE'Access;

      end case;
   end Set_Writer;

   --  ------------------------------
   --  Emit a new line.
   --  ------------------------------
   procedure New_Line (Document : in out Wiki_Renderer) is
   begin
      Document.Writer.Write (LF);
      Document.Empty_Line := True;
   end New_Line;

   --  ------------------------------
   --  Add a section header in the document.
   --  ------------------------------
   overriding
   procedure Add_Header (Document : in out Wiki_Renderer;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive) is
   begin
      Document.Close_Paragraph;
      if not Document.Empty_Line then
         Document.New_Line;
      end if;
      for I in 1 .. Level loop
         Document.Writer.Write (Document.Tags (Header_Start).all);
      end loop;
      Document.Writer.Write (' ');
      Document.Writer.Write (Header);
      if Document.Tags (Header_End)'Length > 0 then
         Document.Writer.Write (' ');
         for I in 1 .. Level loop
            Document.Writer.Write (Document.Tags (Header_End).all);
         end loop;
      end if;
      Document.New_Line;
   end Add_Header;

   --  ------------------------------
   --  Add a line break (<br>).
   --  ------------------------------
   overriding
   procedure Add_Line_Break (Document : in out Wiki_Renderer) is
   begin
      Document.Writer.Write (Document.Tags (Line_Break).all);
      Document.Empty_Line := False;
   end Add_Line_Break;

   --  ------------------------------
   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   --  ------------------------------
   overriding
   procedure Add_Paragraph (Document : in out Wiki_Renderer) is
   begin
      Document.Close_Paragraph;
      if not Document.Empty_Line then
         Document.New_Line;
      end if;
      Document.New_Line;
   end Add_Paragraph;

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   overriding
   procedure Add_Blockquote (Document : in out Wiki_Renderer;
                             Level    : in Natural) is
   begin
      null;
   end Add_Blockquote;

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   overriding
   procedure Add_List_Item (Document : in out Wiki_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean) is
   begin
      null;
   end Add_List_Item;

   --  Add an horizontal rule (<hr>).
   overriding
   procedure Add_Horizontal_Rule (Document : in out Wiki_Renderer) is
   begin
      Document.Close_Paragraph;
      Document.Writer.Write ("----");
   end Add_Horizontal_Rule;

   --  Add a link.
   overriding
   procedure Add_Link (Document : in out Wiki_Renderer;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String) is
   begin
      null;
   end Add_Link;

   --  Add an image.
   overriding
   procedure Add_Image (Document    : in out Wiki_Renderer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String) is
   begin
      null;
   end Add_Image;

   --  Add a quote.
   overriding
   procedure Add_Quote (Document : in out Wiki_Renderer;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String) is
   begin
      null;
   end Add_Quote;

   --  Add a text block with the given format.
   overriding
   procedure Add_Text (Document : in out Wiki_Renderer;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Documents.Format_Map) is
   begin
      Document.Writer.Write (Text);
   end Add_Text;

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Document : in out Wiki_Renderer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String) is
   begin
      null;
   end Add_Preformatted;

   overriding
   procedure Start_Element (Document   : in out Wiki_Renderer;
                            Name       : in Unbounded_Wide_Wide_String;
                            Attributes : in Attribute_List_Type) is
      use type Filters.Html.Html_Tag_Type;

      Tag : constant Filters.Html.Html_Tag_Type := Filters.Html.Find_Tag (Name);
   begin
      if Tag = Filters.Html.BR_TAG then
         Document.Add_Line_Break;
      elsif Tag = Filters.Html.HR_TAG then
         Document.Add_Horizontal_Rule;
      end if;
   end Start_Element;

   overriding
   procedure End_Element (Document : in out Wiki_Renderer;
                          Name     : in Unbounded_Wide_Wide_String) is
   begin
      null;
   end End_Element;

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Document : in out Wiki_Renderer) is
   begin
      null;
   end Finish;

   procedure Close_Paragraph (Document : in out Wiki_Renderer) is
   begin
      null;
   end Close_Paragraph;

   procedure Open_Paragraph (Document : in out Wiki_Renderer) is
   begin
      null;
   end Open_Paragraph;

end Wiki.Render.Wiki;
