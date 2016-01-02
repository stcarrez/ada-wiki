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
with Wiki.Filters.Html;
with Ada.Wide_Wide_Characters.Handling;
package body Wiki.Render.Wiki is

   LF : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#0A#);

   HEADER_CREOLE          : aliased constant Wide_Wide_String := "=";
   BOLD_CREOLE            : aliased constant Wide_Wide_String := "**";
   LINE_BREAK_CREOLE      : aliased constant Wide_Wide_String := "%%%";
   IMG_START_CREOLE       : aliased constant Wide_Wide_String := "{{";
   IMG_END_CREOLE         : aliased constant Wide_Wide_String := "}}";
   LINK_START_CREOLE      : aliased constant Wide_Wide_String := "[[";
   LINK_END_CREOLE        : aliased constant Wide_Wide_String := "]]";
   PREFORMAT_START_CREOLE : aliased constant Wide_Wide_String := "{{{";
   PREFORMAT_END_CREOLE   : aliased constant Wide_Wide_String := "}}}" & LF;

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
            Document.Tags (Img_Start)    := IMG_START_CREOLE'Access;
            Document.Tags (Img_End)      := IMG_END_CREOLE'Access;
            Document.Tags (Link_Start)   := LINK_START_CREOLE'Access;
            Document.Tags (Link_End)     := LINK_END_CREOLE'Access;
            Document.Tags (Preformat_Start) := PREFORMAT_START_CREOLE'Access;
            Document.Tags (Preformat_End)   := PREFORMAT_END_CREOLE'Access;

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
      Document.Writer.Write (Document.Tags (Link_Start).all);
      Document.Writer.Write (Link);
      if Length (Name) > 0 then
         Document.Writer.Write ("|");
         Document.Writer.Write (Name);
      end if;
      Document.Writer.Write (Document.Tags (Link_End).all);
      Document.Empty_Line := False;
   end Add_Link;

   --  Add an image.
   overriding
   procedure Add_Image (Document    : in out Wiki_Renderer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String) is
   begin
      Document.Writer.Write (Document.Tags (Img_Start).all);
      Document.Writer.Write (Link);
      if Length (Alt) > 0 then
         Document.Writer.Write ("|");
         Document.Writer.Write (Alt);
      end if;
      Document.Writer.Write (Document.Tags (Img_End).all);
      Document.Empty_Line := False;
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
      if Document.Keep_Content then
         declare
            use Ada.Wide_Wide_Characters.Handling;

            Content : constant Wide_Wide_String := To_Wide_Wide_String (Text);
            Start   : Natural := Content'First;
            Last    : Natural := Content'Last;
         begin
            while Start <= Content'Last and then Is_space (Content (Start)) loop
               Start := Start + 1;
            end loop;
            while Last >= Start and then Is_Space (Content (Last)) loop
               Last := Last - 1;
            end loop;
            Append (Document.Content, Content (Start .. Last));
         end;
      else
         Document.Writer.Write (Text);
         Document.Empty_Line := False;
      end if;
   end Add_Text;

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Document : in out Wiki_Renderer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String) is
      Content       : constant Wide_Wide_String := To_Wide_Wide_String (Text);
      Col           : Natural := 2;
   begin
      Document.New_Line;
      Document.Writer.Write (Document.Tags (Preformat_Start).all);
      for I in Content'Range loop
         if Content (I) = LF then
            Col := 0;
         end if;
         if I = Content'First and then Col > 0 then
            Document.Writer.Write (LF);
            Col := 0;
         end if;
         Document.Writer.Write (Content (I));
      end loop;
      if Col /= 0 then
         Document.New_Line;
      end if;
      Document.Writer.Write (Document.Tags (Preformat_End).all);
      Document.New_Line;
      Document.Empty_Line := True;
   end Add_Preformatted;

   procedure Start_Keep_Content (Document : in out Wiki_Renderer) is
   begin
      Document.Keep_Content := True;
      Document.Content := To_Unbounded_Wide_Wide_String ("");
   end Start_Keep_Content;

   overriding
   procedure Start_Element (Document   : in out Wiki_Renderer;
                            Name       : in Unbounded_Wide_Wide_String;
                            Attributes : in Attribute_List_Type) is
      use type Filters.Html.Html_Tag_Type;

      Tag : constant Filters.Html.Html_Tag_Type := Filters.Html.Find_Tag (Name);
   begin
      case Tag is
         when Filters.Html.BR_TAG =>
            Document.Add_Line_Break;

         when Filters.Html.HR_TAG =>
            Document.Add_Horizontal_Rule;

         when Filters.Html.H1_TAG | Filters.Html.H2_TAG
            | Filters.Html.H3_TAG | Filters.Html.H4_TAG
            | Filters.Html.H5_TAG | Filters.Html.H6_TAG =>
            Document.Start_Keep_Content;

         when Filters.Html.IMG_TAG =>
            Document.Add_Image (Link        => Get_Attribute (Attributes, "src"),
                                Alt         => Get_Attribute (Attributes, "alt"),
                                Position    => Null_Unbounded_Wide_Wide_String,
                                Description => Null_Unbounded_Wide_Wide_String);

         when Filters.Html.A_TAG =>
            Document.Link_Href := Get_Attribute (Attributes, "href");
            Document.Link_Title := Get_Attribute (Attributes, "title");
            Document.Link_Lang := Get_Attribute (Attributes, "lang");
            Document.Start_Keep_Content;

         when Filters.Html.B_TAG | Filters.Html.EM_TAG | Filters.Html.STRONG_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.BOLD) := True;
            end if;

         when Filters.Html.I_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.ITALIC) := True;
            end if;

         when Filters.Html.U_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.CODE) := True;
            end if;

         when Filters.Html.SUP_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.SUPERSCRIPT) := True;
            end if;

         when Filters.Html.SUB_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.SUBSCRIPT) := True;
            end if;

         when Filters.Html.P_TAG =>
            Document.New_Line;

         when Filters.Html.PRE_TAG =>
            Document.Start_Keep_Content;

         when others =>
            null;

      end case;
   end Start_Element;

   overriding
   procedure End_Element (Document : in out Wiki_Renderer;
                          Name     : in Unbounded_Wide_Wide_String) is
      use type Filters.Html.Html_Tag_Type;

      Tag : constant Filters.Html.Html_Tag_Type := Filters.Html.Find_Tag (Name);
   begin
      case Tag is
         when Filters.Html.H1_TAG =>
            Document.Add_Header (Document.Content, 1);
            Document.Keep_Content := False;

         when Filters.Html.H2_TAG =>
            Document.Add_Header (Document.Content, 2);
            Document.Keep_Content := False;

         when Filters.Html.H3_TAG =>
            Document.Add_Header (Document.Content, 3);
            Document.Keep_Content := False;

         when Filters.Html.H4_TAG =>
            Document.Add_Header (Document.Content, 4);
            Document.Keep_Content := False;

         when Filters.Html.H5_TAG =>
            Document.Add_Header (Document.Content, 5);
            Document.Keep_Content := False;

         when Filters.Html.H6_TAG =>
            Document.Add_Header (Document.Content, 6);
            Document.Keep_Content := False;

         when Filters.Html.A_TAG =>
            Document.Add_Link (Name     => Document.Content,
                               Link     => Document.Link_Href,
                               Language => Document.Link_Lang,
                               Title    => Document.Link_Title);
            Document.Keep_Content := False;

         when Filters.Html.B_TAG | Filters.Html.EM_TAG | Filters.Html.STRONG_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.BOLD) := False;
            end if;

         when Filters.Html.I_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.ITALIC) := False;
            end if;

         when Filters.Html.U_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.CODE) := False;
            end if;

         when Filters.Html.SUP_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.SUPERSCRIPT) := False;
            end if;

         when Filters.Html.SUB_TAG =>
            if not Document.Keep_Content then
               Document.Current_Style (Documents.SUBSCRIPT) := False;
            end if;

         when Filters.Html.PRE_TAG =>
            Document.Add_Preformatted (Document.Content, Null_Unbounded_Wide_Wide_String);
            Document.Keep_Content := False;

         when others =>
            null;

      end case;
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
