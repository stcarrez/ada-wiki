-----------------------------------------------------------------------
--  wiki-filters-collectors -- Wiki word and link collectors
--  Copyright (C) 2016, 2020, 2022 Stephane Carrez
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

package body Wiki.Filters.Collectors is

   procedure Add_String (Into : in out WString_Maps.Map;
                         Item : in Wiki.Strings.WString);

   procedure Add_String (Into : in out WString_Maps.Map;
                         Item : in Wiki.Strings.WString) is

      procedure Increment (Key : in Wiki.Strings.WString;
                           Value : in out Natural);

      procedure Increment (Key : in Wiki.Strings.WString;
                           Value : in out Natural) is
         pragma Unreferenced (Key);
      begin
         Value := Value + 1;
      end Increment;

      Pos : constant WString_Maps.Cursor := Into.Find (Item);
   begin
      if WString_Maps.Has_Element (Pos) then
         Into.Update_Element (Pos, Increment'Access);
      else
         Into.Insert (Item, 1);
      end if;
   end Add_String;

   function Find (Map  : in Collector_Type;
                  Item : in Wiki.Strings.WString) return Cursor is
   begin
      return Map.Items.Find (Item);
   end Find;

   function Contains (Map  : in Collector_Type;
                      Item : in Wiki.Strings.WString) return Boolean is
   begin
      return Map.Items.Contains (Item);
   end Contains;

   procedure Iterate (Map : in Collector_Type;
                      Process : not null access procedure (Pos : in Cursor)) is
   begin
      Map.Items.Iterate (Process);
   end Iterate;

   procedure Collect_Attribute (Filter     : in out Collector_Type;
                                Attributes : in Wiki.Attributes.Attribute_List;
                                Name       : in String) is
      Value : constant Wiki.Strings.WString := Wiki.Attributes.Get_Attribute (Attributes, Name);
   begin
      if Value'Length > 0 then
         Add_String (Filter.Items, Value);
      end if;
   end Collect_Attribute;

   --  ------------------------------
   --  Word Collector type
   --  ------------------------------
   procedure Collect_Words (Filter  : in out Word_Collector_Type;
                            Content : in Wiki.Strings.WString) is
      Pos   : Natural := Content'First;
      Start : Natural := Content'First;
      C     : Wiki.Strings.WChar;
   begin
      while Pos <= Content'Last loop
         C := Content (Pos);
         if Wiki.Strings.Is_Alphanumeric (C) then
            null;
         else
            if Start + 1 < Pos - 1 then
               Add_String (Filter.Items, Content (Start .. Pos - 1));
            end if;
            Start := Pos + 1;
         end if;
         Pos := Pos + 1;
      end loop;
      if Start < Content'Last then
         Add_String (Filter.Items, Content (Start .. Content'Last));
      end if;
   end Collect_Words;

   --  ------------------------------
   --  Add a text content with the given format to the document.
   --  ------------------------------
   overriding
   procedure Add_Text (Filter    : in out Word_Collector_Type;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map) is
   begin
      Filter.Collect_Words (Text);
      Filter_Type (Filter).Add_Text (Document, Text, Format);
   end Add_Text;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   overriding
   procedure Add_Link (Filter     : in out Word_Collector_Type;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      Filter_Type (Filter).Add_Link (Document, Name, Attributes);
   end Add_Link;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   overriding
   procedure Add_Image (Filter     : in out Word_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      Filter_Type (Filter).Add_Image (Document, Name, Attributes);
   end Add_Image;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   overriding
   procedure Add_Quote (Filter     : in out Word_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      Filter_Type (Filter).Add_Quote (Document, Name, Attributes);
   end Add_Quote;

   --  ------------------------------
   --  Add a text block that is pre-formatted.
   --  ------------------------------
   overriding
   procedure Add_Preformatted (Filter   : in out Word_Collector_Type;
                               Document : in out Wiki.Documents.Document;
                               Text     : in Wiki.Strings.WString;
                               Format   : in Wiki.Strings.WString) is
   begin
      Filter.Collect_Words (Text);
      Filter_Type (Filter).Add_Preformatted (Document, Text, Format);
   end Add_Preformatted;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   overriding
   procedure Add_Link (Filter     : in out Link_Collector_Type;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      Collect_Attribute (Filter, Attributes, "href");
      Filter_Type (Filter).Add_Link (Document, Name, Attributes);
   end Add_Link;

   --  ------------------------------
   --  Push a HTML node with the given tag to the document.
   --  ------------------------------
   overriding
   procedure Push_Node (Filter     : in out Link_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Tag = A_TAG then
         Collect_Attribute (Filter, Attributes, "href");
      end if;
      Filter_Type (Filter).Push_Node (Document, Tag, Attributes);
   end Push_Node;

   --  ------------------------------
   --  Add an image.
   --  ------------------------------
   overriding
   procedure Add_Image (Filter     : in out Image_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      Collect_Attribute (Filter, Attributes, "src");
      Filter_Type (Filter).Add_Image (Document, Name, Attributes);
   end Add_Image;

   --  ------------------------------
   --  Push a HTML node with the given tag to the document.
   --  ------------------------------
   overriding
   procedure Push_Node (Filter     : in out Image_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Tag = IMG_TAG then
         Collect_Attribute (Filter, Attributes, "src");
      end if;
      Filter_Type (Filter).Push_Node (Document, Tag, Attributes);
   end Push_Node;

end Wiki.Filters.Collectors;
