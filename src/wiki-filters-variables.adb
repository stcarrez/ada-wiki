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

package body Wiki.Filters.Variables is

   function Need_Expand (Text : in Wiki.Strings.WString) return Boolean;

   function Need_Expand (Text : in Wiki.Strings.WString) return Boolean is
   begin
      return Wiki.Strings.Index (Text, "$") > 0;
   end Need_Expand;

   procedure Add_Variable (Chain : in Wiki.Filters.Filter_Chain;
                           Name  : in Wiki.Strings.WString;
                           Value : in Wiki.Strings.WString) is
      Filter : Filter_Type_Access := Chain.Next;
   begin
      while Filter /= null loop
         if Filter.all in Variable_Filter'Class then
            Variable_Filter'Class (Filter.all).Add_Variable (Name, Value);
            return;
         end if;
         Filter := Filter.Next;
      end loop;
   end Add_Variable;

   --  ------------------------------
   --  Add a variable to replace the given name by its value.
   --  ------------------------------
   procedure Add_Variable (Filter : in out Variable_Filter;
                           Name   : in Wiki.Strings.WString;
                           Value  : in Wiki.Strings.WString) is
   begin
      Filter.Variables.Include (Name, Value);
   end Add_Variable;

   procedure Add_Variable (Filter : in out Variable_Filter;
                           Name   : in String;
                           Value  : in Wiki.Strings.WString) is
   begin
      Filter.Variables.Include (Wiki.Strings.To_WString (Name), Value);
   end Add_Variable;

   --  ------------------------------
   --  Add a text content with the given format to the document.  Replace variables
   --  that are contained in the text.
   --  ------------------------------
   overriding
   procedure Add_Text (Filter    : in out Variable_Filter;
                       Document  : in out Wiki.Documents.Document;
                       Text      : in Wiki.Strings.WString;
                       Format    : in Wiki.Format_Map) is
   begin
      if Need_Expand (Text) then
         Filter_Type (Filter).Add_Text (Document, Filter.Expand (Text), Format);
      else
         Filter_Type (Filter).Add_Text (Document, Text, Format);
      end if;
   end Add_Text;

   --  ------------------------------
   --  Add a link.
   --  ------------------------------
   overriding
   procedure Add_Link (Filter     : in out Variable_Filter;
                       Document   : in out Wiki.Documents.Document;
                       Name       : in Wiki.Strings.WString;
                       Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Need_Expand (Name) then
         Filter_Type (Filter).Add_Link (Document, Filter.Expand (Name), Attributes);
      else
         Filter_Type (Filter).Add_Link (Document, Name, Attributes);
      end if;
   end Add_Link;

   --  ------------------------------
   --  Add a quote.
   --  ------------------------------
   overriding
   procedure Add_Quote (Filter     : in out Variable_Filter;
                        Document   : in out Wiki.Documents.Document;
                        Name       : in Wiki.Strings.WString;
                        Attributes : in out Wiki.Attributes.Attribute_List) is
   begin
      if Need_Expand (Name) then
         Filter_Type (Filter).Add_Quote (Document, Filter.Expand (Name), Attributes);
      else
         Filter_Type (Filter).Add_Quote (Document, Name, Attributes);
      end if;
   end Add_Quote;

   --  ------------------------------
   --  Expand the variables contained in the text.
   --  ------------------------------
   function Expand (Filter : in Variable_Filter;
                    Text   : in Wiki.Strings.WString) return Wiki.Strings.WString is
      First      : Natural := Text'First;
      Pos        : Natural;
      Last       : Natural;
      Name_Start : Natural;
      Name_End   : Natural;
      Result     : Wiki.Strings.BString (256);
      Item       : Variable_Cursor;
      Match_End  : Wiki.Strings.WChar;
   begin
      while First <= Text'Last loop
         Pos := First;
         while Pos <= Text'Last and then Text (Pos) /= '$' loop
            Pos := Pos + 1;
         end loop;
         exit when Pos >= Text'Last;
         Strings.Wide_Wide_Builders.Append (Result, Text (First .. Pos - 1));
         First := Pos;
         Name_Start := Pos + 1;
         if Text (Name_Start) in '(' | '{' then
            Match_End := (if Text (Name_Start) = '(' then ')' else '}');
            Name_Start := Name_Start + 1;
            Name_End := Name_Start;
            while Name_End <= Text'Last and then Text (Name_End) /= Match_End loop
               Name_End := Name_End + 1;
            end loop;
            exit when Name_End > Text'Last;
            exit when Text (Name_End) /= Match_End;
            Last := Name_End + 1;
            Name_End := Name_End - 1;
         else
            Name_End := Name_Start;
            while Name_End <= Text'Last and then Text (Name_End) /= ' ' loop
               Name_End := Name_End + 1;
            end loop;
            Last := Name_End;
            Name_End := Name_End - 1;
         end if;
         Item := Filter.Variables.Find (Text (Name_Start .. Name_End));
         if Strings.Maps.Has_Element (Item) then
            Strings.Wide_Wide_Builders.Append (Result, Strings.Maps.Element (Item));
            First := Last;
         elsif Last > Text'Last then
            exit;
         else
            Strings.Wide_Wide_Builders.Append (Result, Text (First .. Last));
            First := Last + 1;
         end if;
      end loop;
      if First <= Text'Last then
         Strings.Wide_Wide_Builders.Append (Result, Text (First .. Text'Last));
      end if;
      return Strings.Wide_Wide_Builders.To_Array (Result);
   end Expand;

   --  ------------------------------
   --  Iterate over the filter variables.
   --  ------------------------------
   procedure Iterate (Filter  : in Variable_Filter;
                      Process : not null
                        access procedure (Name, Value : in Strings.WString)) is
      Iter : Variable_Cursor := Filter.Variables.First;
   begin
      while Strings.Maps.Has_Element (Iter) loop
         Strings.Maps.Query_Element (Iter, Process);
         Strings.Maps.Next (Iter);
      end loop;
   end Iterate;

   procedure Iterate (Chain   : in Wiki.Filters.Filter_Chain;
                      Process : not null
                        access procedure (Name, Value : in Strings.WString)) is
      Filter : Filter_Type_Access := Chain.Next;
   begin
      while Filter /= null loop
         if Filter.all in Variable_Filter'Class then
            Variable_Filter'Class (Filter.all).Iterate (Process);
            return;
         end if;
         Filter := Filter.Next;
      end loop;
   end Iterate;

end Wiki.Filters.Variables;
