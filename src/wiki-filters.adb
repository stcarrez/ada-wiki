-----------------------------------------------------------------------
--  wiki-filters -- Wiki filters
--  Copyright (C) 2015, 2016, 2020, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
   --  Add a definition item at end of the document.
   --  ------------------------------
   procedure Add_Definition (Filter     : in out Filter_Type;
                             Document   : in out Wiki.Documents.Document;
                             Definition : in Wiki.Strings.WString) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Definition (Document, Definition);
      else
         Wiki.Documents.Add_Definition (Document, Definition);
      end if;
   end Add_Definition;

   procedure Start_Block (Filter   : in out Filter_Type;
                          Document : in out Wiki.Documents.Document;
                          Kind     : in Wiki.Nodes.Node_Kind;
                          Level    : in Natural) is
   begin
      if Filter.Next /= null then
         Filter.Next.Start_Block (Document, Kind, Level);
      else
         Document.Start_Block (Kind, Level);
      end if;
   end Start_Block;

   procedure End_Block (Filter   : in out Filter_Type;
                        Document : in out Wiki.Documents.Document;
                        Kind     : in Wiki.Nodes.Node_Kind) is
   begin
      if Filter.Next /= null then
         Filter.Next.End_Block (Document, Kind);
      else
         Document.End_Block (Kind);
      end if;
   end End_Block;

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
                       Tag      : in Wiki.Html_Tag) is
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
   --  Add a list (<ul> or <ol>) starting at the given number.
   --  ------------------------------
   procedure Add_List (Filter   : in out Filter_Type;
                       Document : in out Wiki.Documents.Document;
                       Level    : in Natural;
                       Ordered  : in Boolean) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_List (Document, Level, Ordered);
      else
         Document.Add_List (Level, Ordered);
      end if;
   end Add_List;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   procedure Add_Link (Filter        : in out Filter_Type;
                       Document      : in out Wiki.Documents.Document;
                       Name          : in Wiki.Strings.WString;
                       Attributes    : in out Wiki.Attributes.Attribute_List;
                       Reference     : in Boolean;
                       With_Children : in Boolean := False) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Link (Document, Name, Attributes, Reference, With_Children);
      else
         Document.Add_Link (Name, Attributes, Reference, With_Children);
      end if;
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   procedure Add_Image (Filter     : in out Filter_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List;
                        Reference  : in Boolean) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Image (Document, Name, Attributes, Reference);
      else
         Document.Add_Image (Name, Attributes, Reference);
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
   --  Add a new table in the document with the column styles.
   --  ------------------------------
   procedure Add_Table (Filter   : in out Filter_Type;
                        Document : in out Wiki.Documents.Document;
                        Columns  : in Nodes.Column_Array_Style) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Table (Document, Columns);
      else
         Document.Add_Table (Columns);
      end if;
   end Add_Table;

   --  ------------------------------
   --  Add a new row to the current table.
   --  ------------------------------
   procedure Add_Row (Filter   : in out Filter_Type;
                      Document : in out Wiki.Documents.Document;
                      Kind     : in Nodes.Row_Kind) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Row (Document, Kind);
      else
         Document.Add_Row (Kind);
      end if;
   end Add_Row;

   --  ------------------------------
   --  Add a column to the current table row.  The column is configured with the
   --  given attributes.  The column content is provided through calls to Append.
   --  ------------------------------
   procedure Add_Column (Filter     : in out Filter_Type;
                         Document   : in out Wiki.Documents.Document;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Filter.Next /= null then
         Filter.Next.Add_Column (Document, Attributes);
      else
         Document.Add_Column (Attributes);
      end if;
   end Add_Column;

   --  ------------------------------
   --  Finish the creation of the table.
   --  ------------------------------
   procedure Finish_Table (Filter   : in out Filter_Type;
                           Document : in out Wiki.Documents.Document) is
   begin
      if Filter.Next /= null then
         Filter.Next.Finish_Table (Document);
      else
         Document.Finish_Table;
      end if;
   end Finish_Table;

   --  ------------------------------
   --  Finish the creation of the list.
   --  ------------------------------
   procedure Finish_List (Filter   : in out Filter_Type;
                          Document : in out Wiki.Documents.Document) is
   begin
      if Filter.Next /= null then
         Filter.Next.Finish_List (Document);
      else
         Document.Finish_List;
      end if;
   end Finish_List;

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
