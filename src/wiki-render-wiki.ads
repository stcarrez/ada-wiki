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

with Wiki.Documents;
with Wiki.Attributes;
with Wiki.Writers;
with Wiki.Parsers;

--  == Wiki Renderer ==
--  The <tt>Wiki_Renderer</tt> allows to render a wiki document into another wiki content.
--  The formatting rules are ignored except for the paragraphs and sections.
package Wiki.Render.Wiki is

   use Standard.Wiki.Attributes;

   --  ------------------------------
   --  Wiki to HTML writer
   --  ------------------------------
   type Wiki_Renderer is new Standard.Wiki.Documents.Document_Reader with private;

   --  Set the output writer.
   procedure Set_Writer (Document : in out Wiki_Renderer;
                         Writer   : in Writers.Writer_Type_Access;
                         Format   : in Parsers.Wiki_Syntax_Type);

   --  Add a section header in the document.
   overriding
   procedure Add_Header (Document : in out Wiki_Renderer;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive);

   --  Add a line break (<br>).
   overriding
   procedure Add_Line_Break (Document : in out Wiki_Renderer);

   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   overriding
   procedure Add_Paragraph (Document : in out Wiki_Renderer);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   overriding
   procedure Add_Blockquote (Document : in out Wiki_Renderer;
                             Level    : in Natural);

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   overriding
   procedure Add_List_Item (Document : in out Wiki_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean);

   --  Add an horizontal rule (<hr>).
   overriding
   procedure Add_Horizontal_Rule (Document : in out Wiki_Renderer);

   --  Add a link.
   overriding
   procedure Add_Link (Document : in out Wiki_Renderer;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String);

   --  Add an image.
   overriding
   procedure Add_Image (Document    : in out Wiki_Renderer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String);

   --  Add a quote.
   overriding
   procedure Add_Quote (Document : in out Wiki_Renderer;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String);

   --  Add a text block with the given format.
   overriding
   procedure Add_Text (Document : in out Wiki_Renderer;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Documents.Format_Map);

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Document : in out Wiki_Renderer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String);

   overriding
   procedure Start_Element (Document   : in out Wiki_Renderer;
                            Name       : in Unbounded_Wide_Wide_String;
                            Attributes : in Attribute_List_Type);

   overriding
   procedure End_Element (Document : in out Wiki_Renderer;
                          Name     : in Unbounded_Wide_Wide_String);


   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Document : in out Wiki_Renderer);

   --  Set the text style format.
   procedure Set_Format (Document : in out Wiki_Renderer;
                         Format   : in Documents.Format_Map);

private

   type Wide_String_Access is access constant Wide_Wide_String;

   type Wiki_Tag_Type is (Header_Start, Header_End,
                          Img_Start, Img_End,
                          Link_Start, Link_End, Link_Separator,
                          Preformat_Start, Preformat_End,
                          List_Start, List_Item, List_Ordered_Item,
                          Line_Break, Escape_Rule,
                          Horizontal_Rule,
                          Blockquote_Start, Blockquote_End);

   type Wiki_Tag_Array is array (Wiki_Tag_Type) of Wide_String_Access;

   type Wiki_Format_Array is array (Documents.Format_Type) of Wide_String_Access;

   --  Emit a new line.
   procedure New_Line (Document : in out Wiki_Renderer);

   procedure Close_Paragraph (Document : in out Wiki_Renderer);
   procedure Open_Paragraph (Document : in out Wiki_Renderer);
   procedure Start_Keep_Content (Document : in out Wiki_Renderer);

   type List_Style_Array is array (1 .. 32) of Boolean;

   EMPTY_TAG : aliased constant Wide_Wide_String := "";

   type Wiki_Renderer is new Documents.Document_Reader with record
      Writer              : Writers.Writer_Type_Access := null;
      Syntax              : Parsers.Wiki_Syntax_Type := Parsers.SYNTAX_CREOLE;
      Format              : Documents.Format_Map := (others => False);
      Tags                : Wiki_Tag_Array := (others => EMPTY_TAG'Access);
      Style_Start_Tags    : Wiki_Format_Array := (others => EMPTY_TAG'Access);
      Style_End_Tags      : Wiki_Format_Array := (others => EMPTY_TAG'Access);
      Has_Paragraph       : Boolean := False;
      Has_Item            : Boolean := False;
      Need_Paragraph      : Boolean := False;
      Empty_Line          : Boolean := True;
      Keep_Content        : Boolean := False;
      In_List             : Boolean := False;
      Invert_Header_Level : Boolean := False;
      Allow_Link_Language : Boolean := False;
      Current_Level       : Natural := 0;
      Quote_Level         : Natural := 0;
      UL_List_Level       : Natural := 0;
      OL_List_Level       : Natural := 0;
      Current_Style       : Documents.Format_Map := (others => False);
      Content             : Unbounded_Wide_Wide_String;
      Link_Href           : Unbounded_Wide_Wide_String;
      Link_Title          : Unbounded_Wide_Wide_String;
      Link_Lang           : Unbounded_Wide_Wide_String;
   end record;

end Wiki.Render.Wiki;
