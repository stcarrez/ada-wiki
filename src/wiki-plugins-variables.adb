-----------------------------------------------------------------------
--  wiki-plugins-variables -- Variables plugin
--  Copyright (C) 2020, 2021 Stephane Carrez
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
with Wiki.Filters.Variables;
package body Wiki.Plugins.Variables is

   --  ------------------------------
   --  Set or update a variable in the `Wiki.Filters.Variable` filter.
   --  ------------------------------
   overriding
   procedure Expand (Plugin   : in out Variable_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context) is
      pragma Unreferenced (Plugin, Document);
   begin
      if Wiki.Attributes.Length (Params) >= 3 then
         declare
            First  : Wiki.Attributes.Cursor := Wiki.Attributes.First (Params);
            Second : Wiki.Attributes.Cursor;
         begin
            Wiki.Attributes.Next (First);
            Second := First;
            Wiki.Attributes.Next (Second);
            Wiki.Filters.Variables.Add_Variable (Context.Filters,
                                                 Wiki.Attributes.Get_Wide_Value (First),
                                                 Wiki.Attributes.Get_Wide_Value (Second));
         end;
      end if;
   end Expand;

   --  ------------------------------
   --  List the variables from the `Wiki.Filters.Variable` filter.
   --  ------------------------------
   overriding
   procedure Expand (Plugin   : in out List_Variable_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context) is
      pragma Unreferenced (Plugin, Params);
      procedure Print_Variable (Name  : in Wiki.Strings.WString;
                                Value : in Wiki.Strings.WString);

      Has_Table  : Boolean := False;
      Format     : constant Format_Map := (others => False);
      Attributes : Wiki.Attributes.Attribute_List;

      procedure Print_Variable (Name  : in Wiki.Strings.WString;
                                Value : in Wiki.Strings.WString) is
      begin
         Has_Table := True;
         Context.Filters.Add_Row (Document);
         Context.Filters.Add_Column (Document, Attributes);
         Context.Filters.Add_Text (Document, Name, Format);
         Context.Filters.Add_Column (Document, Attributes);
         Context.Filters.Add_Text (Document, Value, Format);
      end Print_Variable;

   begin
      Wiki.Filters.Variables.Iterate (Context.Filters, Print_Variable'Access);
      if Has_Table then
         Context.Filters.Finish_Table (Document);
      end if;
   end Expand;

end Wiki.Plugins.Variables;
