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
with Ada.Containers.Indefinite_Ordered_Maps;

--  === Collector Filters ===
--  The `Wiki.Filters.Collectors` package defines three filters that can be used to
--  collect words, links or images contained in a Wiki document.  The collector filters are
--  inserted in the filter chain and they collect the data as the Wiki text is parsed.
--  After the parsing, the collector filters have collected either the words or the links
--  and they can be queried by using the `Find` or `Iterate` operations.
--  The following collectors are defined:
--
--  * The `Word_Collector_Type` collects words from text, headers, links,
--  * The `Link_Collector_Type` collects links,
--  * The `Image_Collector_Type` collects images,
--
--  The filter is inserted in the filter chain before parsing the Wiki document.
--
--    Words : aliased Wiki.Filters.Collectors.Word_Collector_Type;
--    ...
--    Engine.Add_Filter (Words'Unchecked_Access);
--
--  Once the document is parsed, the collector filters contains the data that was collected.
--  The `Iterate` procedure can be used to have a procedure called for each value
--  collected by the filter.
--
--    Words.Iterate (Print'Access);
--
package Wiki.Filters.Collectors is

   pragma Preelaborate;

   package WString_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => Wiki.Strings.WString,
                                                 Element_Type => Natural,
                                                 "<"          => "<",
                                                 "="          => "=");

   subtype Map is WString_Maps.Map;
   subtype Cursor is WString_Maps.Cursor;

   --  ------------------------------
   --  General purpose collector type
   --  ------------------------------
   type Collector_Type is new Filter_Type with private;
   type Collector_Type_Access is access all Collector_Type'Class;

   function Find (Map  : in Collector_Type;
                  Item : in Wiki.Strings.WString) return Cursor;

   function Contains (Map  : in Collector_Type;
                      Item : in Wiki.Strings.WString) return Boolean;

   procedure Iterate (Map : in Collector_Type;
                      Process : not null access procedure (Pos : in Cursor));

   --  ------------------------------
   --  Word Collector type
   --  ------------------------------
   type Word_Collector_Type is new Collector_Type with private;
   type Word_Collector_Type_Access is access all Word_Collector_Type'Class;

   --  Add a text content with the given format to the document.
   overriding
   procedure Add_Text (Filter    : in out Word_Collector_Type;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map);

   --  Add a link.
   overriding
   procedure Add_Link (Filter     : in out Word_Collector_Type;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add an image.
   overriding
   procedure Add_Image (Filter     : in out Word_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a quote.
   overriding
   procedure Add_Quote (Filter     : in out Word_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a text block that is pre-formatted.
   overriding
   procedure Add_Preformatted (Filter   : in out Word_Collector_Type;
                               Document : in out Wiki.Documents.Document;
                               Text     : in Wiki.Strings.WString;
                               Format   : in Wiki.Strings.WString);

   --  ------------------------------
   --  Link Collector type
   --  ------------------------------
   type Link_Collector_Type is new Collector_Type with private;
   type Link_Collector_Type_Access is access all Link_Collector_Type'Class;

   --  Add a link.
   overriding
   procedure Add_Link (Filter     : in out Link_Collector_Type;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List);

   --  Push a HTML node with the given tag to the document.
   overriding
   procedure Push_Node (Filter     : in out Link_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  ------------------------------
   --  Image Collector type
   --  ------------------------------
   type Image_Collector_Type is new Collector_Type with private;
   type Image_Collector_Type_Access is access all Image_Collector_Type'Class;

   --  Add an image.
   overriding
   procedure Add_Image (Filter     : in out Image_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Push a HTML node with the given tag to the document.
   overriding
   procedure Push_Node (Filter     : in out Image_Collector_Type;
                        Document   : in out Wiki.Documents.Document;
                        Tag        : in Wiki.Html_Tag;
                        Attributes : in out Wiki.Attributes.Attribute_List);

private

   type Collector_Type is new Filter_Type with record
      Items : WString_Maps.Map;
   end record;

   procedure Collect_Attribute (Filter     : in out Collector_Type;
                                Attributes : in Wiki.Attributes.Attribute_List;
                                Name       : in String);

   type Word_Collector_Type is new Collector_Type with null record;

   procedure Collect_Words (Filter  : in out Word_Collector_Type;
                            Content : in Wiki.Strings.WString);

   type Link_Collector_Type is new Collector_Type with null record;

   type Image_Collector_Type is new Collector_Type with null record;

end Wiki.Filters.Collectors;
