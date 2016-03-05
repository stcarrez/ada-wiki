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
                       Document  : in out Wiki.Documents.Document;
                       Kind      : in Wiki.Nodes.Simple_Node_Kind) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Node (Document, Kind);
      else
         Document.Append (Kind);
      end if;
   end Add_Node;

   --  ------------------------------
   --  Add a text content with the given format to the document.
   --  ------------------------------
   procedure Add_Text (Filter    : in out Filter_Type;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Text (Document, Text, Format);
      else
         Wiki.Documents.Append (Document, Text, Format);
      end if;
   end Add_Text;

   --  ------------------------------
   --  Add a section header with the given level in the document.
   --  ------------------------------
   procedure Add_Header (Filter    : in out Filter_Type;
                         Document  : in out Wiki.Documents.Document;
                         Header    : in Wiki.Strings.WString;
                         Level     : in Natural) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Header (Document, Header, Level);
      else
         Wiki.Documents.Append (Document, Header, Level);
      end if;
   end Add_Header;

   --  ------------------------------
   --  Push a HTML node with the given tag to the document.
   --  ------------------------------
   procedure Push_Node (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Filter.Next /= null then
         Filter.Next.Push_Node (Document, Tag, Attributes);
      else
         Document.Push_Node (Tag, Attributes);
      end if;
   end Push_Node;

   --  ------------------------------
   --  Pop a HTML node with the given tag.
   --  ------------------------------
   procedure Pop_Node (Filter   : in out Filter_Type;
                       Document : in out Wiki.Documents.Document;
                       Tag     : in Wiki.Html_Tag) is
   begin
      if Filter.Next /= null then
         Filter.Next.Pop_Node (Document, Tag);
      else
         Document.Pop_Node (Tag);
      end if;
   end Pop_Node;

   --  ------------------------------
   --  Add a blockquote (<blockquote>).  The level indicates the blockquote nested level.
   --  The blockquote must be closed at the next header.
   --  ------------------------------
   procedure Add_Blockquote (Filter   : in out Filter_Type;
                             Document : in out Wiki.Documents.Document;
                             Level    : in Natural) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Blockquote (Document, Level);
      else
         Document.Add_Blockquote (Level);
      end if;
   end Add_Blockquote;

   --  ------------------------------
   --  Add a list item (<li>).  Close the previous paragraph and list item if any.
   --  The list item will be closed at the next list item, next paragraph or next header.
   --  ------------------------------
   procedure Add_List_Item (Filter   : in out Filter_Type;
                            Document : in out Wiki.Documents.Document;
                            Level    : in Positive;
                            Ordered  : in Boolean) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_List_Item (Document, Level, Ordered);
      else
         Document.Add_List_Item (Level, Ordered);
      end if;
   end Add_List_Item;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   procedure Add_Link (Filter     : in out Filter_Type;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Link (Document, Name, Attributes);
      else
         Document.Add_Link (Name, Attributes);
      end if;
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   procedure Add_Image (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Image (Document, Name, Attributes);
      else
         Document.Add_Image (Name, Attributes);
      end if;
   end Add_Image;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   procedure Add_Quote (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Quote (Document, Name, Attributes);
      else
         Document.Add_Quote (Name, Attributes);
      end if;
   end Add_Quote;

   --  ------------------------------
   --  Add a text block that is pre-formatted.
   --  ------------------------------
   procedure Add_Preformatted (Filter   : in out Filter_Type;
                               Document : in out Wiki.Documents.Document;
                               Text     : in Wiki.Strings.WString;
                               Format   : in Wiki.Strings.WString) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Preformatted (Document, Text, Format);
      else
         Document.Add_Preformatted (Text, Format);
      end if;
   end Add_Preformatted;

   --  ------------------------------
   --  Finish the document after complete wiki text has been parsed.
   --  ------------------------------
   procedure Finish (Filter   : in out Filter_Type;
                     Document : in out Wiki.Documents.Document) is
   begin
      if Filter.Next /= null then
         Filter.Next.Finish (Document);
      end if;
   end Finish;

   --  ------------------------------
   --  Add the filter at beginning of the filter chain.
   --  ------------------------------
   procedure Add_Filter (Chain  : in out Filter_Chain;
                         Filter : in Filter_Type_Access) is
   begin
      Filter.Next := Chain.Next;
      Chain.Next := Filter;
   end Add_Filter;

   --  ------------------------------
   --  Internal operation to copy the filter chain.
   --  ------------------------------
   procedure Set_Chain (Chain : in out Filter_Chain;
                        From  : in Filter_Chain'Class) is
   begin
      Chain.Next := From.Next;
   end Set_Chain;

end Wiki.Filters;
