-----------------------------------------------------------------------
--  wiki-filters-variables -- Expand variables in text and links
--  Copyright (C) 2020, 2022 Stephane Carrez
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

--  === Variables Filters ===
--  The `Wiki.Filters.Variables` package defines a filter that replaces variables
--  in the text, links, quotes.  Variables are represented as `$name`, `$(name)`
--  or `${name}`.  When a variable is not found, the original string is not modified.
--  The list of variables is either configured programatically through the
--  `Add_Variable` procedures but it can also be set from the Wiki text by using
--  the `Wiki.Plugins.Variables` plugin.
--
--  The variable filter must be configured with the plugin by declaring the instance:
--
--    F : aliased Wiki.Filters.Html.Html_Filter_Type;
--
--     Engine.Add_Filter (F'Unchecked_Access);
--
--  And variables can be inserted by using the `Add_Variable` procedure:
--
--    F.Add_Variable ("username", "gandalf");
--
package Wiki.Filters.Variables is

   type Variable_Filter is new Filter_Type with private;
   type Variable_Filter_Access is access all Variable_Filter'Class;

   --  Add a variable to replace the given name by its value.
   procedure Add_Variable (Filter : in out Variable_Filter;
                           Name   : in Wiki.Strings.WString;
                           Value  : in Wiki.Strings.WString);

   procedure Add_Variable (Filter : in out Variable_Filter;
                           Name   : in String;
                           Value  : in Wiki.Strings.WString);

   procedure Add_Variable (Chain : in Wiki.Filters.Filter_Chain;
                           Name  : in Wiki.Strings.WString;
                           Value : in Wiki.Strings.WString);

   --  Add a text content with the given format to the document.  Replace variables
   --  that are contained in the text.
   overriding
   procedure Add_Text (Filter    : in out Variable_Filter;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map);

   --  Add a link.
   overriding
   procedure Add_Link (Filter     : in out Variable_Filter;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List);

   --  Add a quote.
   overriding
   procedure Add_Quote (Filter     : in out Variable_Filter;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List);

   --  Expand the variables contained in the text.
   function Expand (Filter : in Variable_Filter;
                    Text   : in Wiki.Strings.WString) return Wiki.Strings.WString;

   --  Iterate over the filter variables.
   procedure Iterate (Filter  : in Variable_Filter;
                      Process : not null
                        access procedure (Name, Value : in Strings.WString));

   procedure Iterate (Chain   : in Wiki.Filters.Filter_Chain;
                      Process : not null
                        access procedure (Name, Value : in Strings.WString));

private

   subtype Variable_Map is Wiki.Strings.Maps.Map;
   subtype Variable_Cursor is Wiki.Strings.Maps.Cursor;

   type Variable_Filter is new Filter_Type with record
      Variables : Variable_Map;
   end record;

end Wiki.Filters.Variables;
