-----------------------------------------------------------------------
--  wiki-render-html -- Wiki HTML renderer
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
with Ada.Strings.Wide_Wide_Unbounded;

with Wiki.Documents;
with Wiki.Writers;

--  == HTML Renderer ==
--  The <tt>Text_Renderer</tt> allows to render a wiki document into a text content.
--  The formatting rules are ignored except for the paragraphs and sections.
package Wiki.Render.Html is

   use Ada.Strings.Wide_Wide_Unbounded;

   --  ------------------------------
   --  Wiki to HTML writer
   --  ------------------------------
   type Html_Renderer is new Wiki.Documents.Document_Reader with private;

   --  Set the output writer.
   procedure Set_Writer (Document : in out Html_Renderer;
                         Writer   : in Wiki.Writers.Html_Writer_Type_Access);

   --  Set the link renderer.
   procedure Set_Link_Renderer (Document : in out Html_Renderer;
                                Links    : in Link_Renderer_Access);

   --  Add a section header in the document.
   overriding
   procedure Add_Header (Document : in out Html_Renderer;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive);

   --  Add a line break (<br>).
   overriding
   procedure Add_Line_Break (Document : in out Html_Renderer);

   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   overriding
   procedure Add_Paragraph (Document : in out Html_Renderer);

   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   overriding
   procedure Add_Blockquote (Document : in out Html_Renderer;
                             Level    : in Natural);

   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   overriding
   procedure Add_List_Item (Document : in out Html_Renderer;
                            Level    : in Positive;
                            Ordered  : in Boolean);

   --  Add an horizontal rule (<hr>).
   overriding
   procedure Add_Horizontal_Rule (Document : in out Html_Renderer);

   --  Add a link.
   overriding
   procedure Add_Link (Document : in out Html_Renderer;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String);

   --  Add an image.
   overriding
   procedure Add_Image (Document    : in out Html_Renderer;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String);

   --  Add a quote.
   overriding
   procedure Add_Quote (Document : in out Html_Renderer;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String);

   --  Add a text block with the given format.
   overriding
   procedure Add_Text (Document : in out Html_Renderer;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Wiki.Documents.Format_Map);

   --  Add a text block that is pre-formatted.
   procedure Add_Preformatted (Document : in out Html_Renderer;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String);

   --  Finish the document after complete wiki text has been parsed.
   overriding
   procedure Finish (Document : in out Html_Renderer);

private

   procedure Close_Paragraph (Document : in out Html_Renderer);
   procedure Open_Paragraph (Document : in out Html_Renderer);

   type List_Style_Array is array (1 .. 32) of Boolean;

   Default_Links : aliased Default_Link_Renderer;

   type Html_Renderer is new Wiki.Documents.Document_Reader with record
      Writer         : Wiki.Writers.Html_Writer_Type_Access := null;
      Format         : Wiki.Documents.Format_Map := (others => False);
      Links          : Link_Renderer_Access := Default_Links'Access;
      Has_Paragraph  : Boolean := False;
      Need_Paragraph : Boolean := False;
      Current_Level  : Natural := 0;
      List_Styles    : List_Style_Array := (others => False);
      Has_Item       : Boolean := False;
      Quote_Level    : Natural := 0;
   end record;

end Wiki.Render.Html;
