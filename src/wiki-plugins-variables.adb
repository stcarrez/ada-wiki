-----------------------------------------------------------------------
--  wiki-plugins-variables -- Variables plugin
--  Copyright (C) 2020, 2021, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Filters.Variables;
with Wiki.Nodes;
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
         if not Has_Table then
            Context.Filters.Add_Table (Document,
                                       (1 => (Width => 40, others => <>),
                                        2 => (Width => 60, others => <>)));
         end if;
         Has_Table := True;
         Context.Filters.Add_Row (Document, Nodes.N_ROW);
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
