-----------------------------------------------------------------------
--  wiki-filters -- Wiki filters
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

package body Wiki.Filters is

   --  ------------------------------
   --  Add a simple node such as N_LINE_BREAK, N_HORIZONTAL_RULE or N_PARAGRAPH to the document.
   --  ------------------------------
   procedure Add_Node (Filter    : in out Filter_Type;
                       Document  : in out Wiki.Nodes.Document;
                       Kind      : in Wiki.Nodes.Simple_Node_Kind) is
   begin
      if Filter.Next /= null then
         Filter.Add_Node (Document, Kind);
      else
         Wiki.Nodes.Append (Document, Kind);
      end if;
   end Add_Node;

   --  ------------------------------
   --  Add a section header in the document.
   --  ------------------------------
   overriding
   procedure Add_Header (Document : in out Filter_Type;
                         Header   : in Unbounded_Wide_Wide_String;
                         Level    : in Positive) is
   begin
      Document.Document.Add_Header (Header, Level);
   end Add_Header;

   --  ------------------------------
   --  Add a line break (<br>).
   --  ------------------------------
   overriding
   procedure Add_Line_Break (Document : in out Filter_Type) is
   begin
      Document.Document.Add_Line_Break;
   end Add_Line_Break;

   --  ------------------------------
   --  Add a paragraph (<p>).  Close the previous paragraph if any.
   --  The paragraph must be closed at the next paragraph or next header.
   --  ------------------------------
   overriding
   procedure Add_Paragraph (Document : in out Filter_Type) is
   begin
      Document.Document.Add_Paragraph;
   end Add_Paragraph;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   overriding
   procedure Add_Blockquote (Document : in out Filter_Type;
                             Level    : in Natural) is
   begin
      Document.Document.Add_Blockquote (Level);
   end Add_Blockquote;

   --  ------------------------------
   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   overriding
   procedure Add_List_Item (Document : in out Filter_Type;
                            Level    : in Positive;
                            Ordered  : in Boolean) is
   begin
      Document.Document.Add_List_Item (Level, Ordered);
   end Add_List_Item;

   --  ------------------------------
   --  Add an horizontal rule (<hr>).
   --  ------------------------------
   overriding
   procedure Add_Horizontal_Rule (Document : in out Filter_Type) is
   begin
      Document.Document.Add_Horizontal_Rule;
   end Add_Horizontal_Rule;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   overriding
   procedure Add_Link (Document : in out Filter_Type;
                       Name     : in Unbounded_Wide_Wide_String;
                       Link     : in Unbounded_Wide_Wide_String;
                       Language : in Unbounded_Wide_Wide_String;
                       Title    : in Unbounded_Wide_Wide_String) is
   begin
      Document.Document.Add_Link (Name, Link, Language, Title);
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   overriding
   procedure Add_Image (Document    : in out Filter_Type;
                        Link        : in Unbounded_Wide_Wide_String;
                        Alt         : in Unbounded_Wide_Wide_String;
                        Position    : in Unbounded_Wide_Wide_String;
                        Description : in Unbounded_Wide_Wide_String) is
   begin
      Document.Document.Add_Image (Link, Alt, Position, Description);
   end Add_Image;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   overriding
   procedure Add_Quote (Document : in out Filter_Type;
                        Quote    : in Unbounded_Wide_Wide_String;
                        Link     : in Unbounded_Wide_Wide_String;
                        Language : in Unbounded_Wide_Wide_String) is
   begin
      Document.Document.Add_Quote (Quote, Link, Language);
   end Add_Quote;

   --  ------------------------------
   --  Add a text block with the given format.
   --  ------------------------------
   overriding
   procedure Add_Text (Document : in out Filter_Type;
                       Text     : in Unbounded_Wide_Wide_String;
                       Format   : in Wiki.Documents.Format_Map) is
   begin
      Document.Document.Add_Text (Text, Format);
   end Add_Text;

   --  ------------------------------
   --  Add a text block that is pre-formatted.
   --  ------------------------------
   overriding
   procedure Add_Preformatted (Document : in out Filter_Type;
                               Text     : in Unbounded_Wide_Wide_String;
                               Format   : in Unbounded_Wide_Wide_String) is
   begin
      Document.Document.Add_Preformatted (Text, Format);
   end Add_Preformatted;

   overriding
   procedure Start_Element (Document   : in out Filter_Type;
                            Name       : in Unbounded_Wide_Wide_String;
                            Attributes : in Wiki.Attributes.Attribute_List_Type) is
   begin
      Document.Document.Start_Element (Name, Attributes);
   end Start_Element;

   overriding
   procedure End_Element (Document : in out Filter_Type;
                          Name     : in Unbounded_Wide_Wide_String) is
   begin
      Document.Document.End_Element (Name);
   end End_Element;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   overriding
   procedure Finish (Document : in out Filter_Type) is
   begin
      Document.Document.Finish;
   end Finish;

   --  ------------------------------
   --  Set the document reader.
   --  ------------------------------
   procedure Set_Document (Filter   : in out Filter_Type;
                           Document : in Wiki.Documents.Document_Reader_Access) is
   begin
      Filter.Document := Document;
   end Set_Document;

end Wiki.Filters;
