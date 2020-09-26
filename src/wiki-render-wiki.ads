-----------------------------------------------------------------------
--  wiki-render-wiki -- Wiki to Wiki renderer
--  Copyright (C) 2015, 2016, 2020 Stephane Carrez
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
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Wide_Wide_Unbounded;

with Wiki.Documents;
with Wiki.Attributes;
with Wiki.Streams;
with Wiki.Strings;

--  == Wiki Renderer ==
--  The <tt>Wiki_Renderer</tt> allows to render a wiki document into another wiki content.
--  The formatting rules are ignored except for the paragraphs and sections.
package Wiki.Render.Wiki is

   use Standard.Wiki.Attributes;

   --  ------------------------------
   --  Wiki to HTML writer
   --  ------------------------------
   type Wiki_Renderer is new Renderer with private;

   --  Set the output stream.
   procedure Set_Output_Stream (Engine   : in out Wiki_Renderer;
                                Stream   : in Streams.Output_Stream_Access;
                                Format   : in Wiki_Syntax);

   --  Render the node instance from the document.
   overriding
   procedure Render (Engine : in out Wiki_Renderer;
                     Doc    : in Documents.Document;
                     Node   : in Nodes.Node_Type);

   --  Add a section header in the document.
   procedure Render_Header (Engine : in out Wiki_Renderer;
                            Header : in Wide_Wide_String;
                            Level  : in Positive);

   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   procedure Add_Paragraph (Engine : in out Wiki_Renderer);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   procedure Add_Blockquote (Engine : in out Wiki_Renderer;
                             Level    : in Natural);

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   procedure Add_List_Item (Engine : in out Wiki_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean);

   --  Render a link.
   procedure Render_Link (Engine : in out Wiki_Renderer;
                          Name     : in Strings.WString;
                          Attrs    : in Attributes.Attribute_List);

   --  Render an image.
   procedure Render_Image (Engine : in out Wiki_Renderer;
                           Title  : in Strings.WString;
                           Attrs  : in Attributes.Attribute_List);

   --  Render a quote.
   procedure Render_Quote (Engine : in out Wiki_Renderer;
                           Title  : in Strings.WString;
                           Attrs  : in Attributes.Attribute_List);

   --  Add a text block with the given format.
   procedure Render_Text (Engine : in out Wiki_Renderer;
                          Text   : in Wide_Wide_String;
                          Format : in Format_Map);

   --  Render a text block that is pre-formatted.
   procedure Render_Preformatted (Engine : in out Wiki_Renderer;
                                  Text   : in Strings.WString;
                                  Format : in Strings.WString);

   procedure Render_Tag (Engine : in out Wiki_Renderer;
                         Doc    : in Documents.Document;
                         Node   : in Nodes.Node_Type);

   --  Finish the document after complete wiki text has been parsed.
   procedure Finish (Engine : in out Wiki_Renderer;
                     Doc    : in Documents.Document);

   --  Set the text style format.
   procedure Set_Format (Engine : in out Wiki_Renderer;
                         Format   : in Format_Map);

private

   use Ada.Strings.Wide_Wide_Unbounded;

   type Wide_String_Access is access constant Wide_Wide_String;

   type Wiki_Tag_Type is (Header_Start, Header_End,
                          Img_Start, Img_End,
                          Link_Start, Link_End, Link_Separator,
                          Quote_Start, Quote_End, Quote_Separator,
                          Preformat_Start, Preformat_End,
                          List_Start, List_Item, List_Ordered_Item,
                          Line_Break, Escape_Rule,
                          Horizontal_Rule,
                          Blockquote_Start, Blockquote_End);

   type Wiki_Tag_Array is array (Wiki_Tag_Type) of Wide_String_Access;

   type Wiki_Format_Array is array (Format_Type) of Wide_String_Access;

   --  Emit a new line.
   procedure New_Line (Engine   : in out Wiki_Renderer;
                       Optional : in Boolean := False);
   procedure Need_Separator_Line (Engine   : in out Wiki_Renderer);

   procedure Close_Paragraph (Engine : in out Wiki_Renderer);
   procedure Start_Keep_Content (Engine : in out Wiki_Renderer);

   type List_Style_Array is array (1 .. 32) of Boolean;

   EMPTY_TAG : aliased constant Wide_Wide_String := "";

   type Wiki_Renderer is new Renderer with record
      Output              : Streams.Output_Stream_Access := null;
      Syntax              : Wiki_Syntax := SYNTAX_CREOLE;
      Format              : Format_Map := (others => False);
      Tags                : Wiki_Tag_Array := (others => EMPTY_TAG'Access);
      Style_Start_Tags    : Wiki_Format_Array := (others => EMPTY_TAG'Access);
      Style_End_Tags      : Wiki_Format_Array := (others => EMPTY_TAG'Access);
      Escape_Set          : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
      Has_Paragraph       : Boolean := False;
      Has_Item            : Boolean := False;
      Need_Paragraph      : Boolean := False;
      Need_Newline        : Boolean := False;
      Need_Space          : Boolean := False;
      Empty_Line          : Boolean := True;
      Empty_Previous_Line : Boolean := True;
      Keep_Content        : Boolean := False;
      In_List             : Boolean := False;
      Invert_Header_Level : Boolean := False;
      Allow_Link_Language : Boolean := False;
      Link_First          : Boolean := False;
      Html_Blockquote     : Boolean := False;
      Line_Count          : Natural := 0;
      Current_Level       : Natural := 0;
      Quote_Level         : Natural := 0;
      UL_List_Level       : Natural := 0;
      OL_List_Level       : Natural := 0;
      Current_Style       : Format_Map := (others => False);
      Content             : Unbounded_Wide_Wide_String;
      Link_Href           : Unbounded_Wide_Wide_String;
      Link_Title          : Unbounded_Wide_Wide_String;
      Link_Lang           : Unbounded_Wide_Wide_String;
   end record;

end Wiki.Render.Wiki;
