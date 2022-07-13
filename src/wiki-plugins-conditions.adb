-----------------------------------------------------------------------
--  wiki-plugins-conditions -- Condition Plugin
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
package body Wiki.Plugins.Conditions is

   --  ------------------------------
   --  Append the attribute name/value to the condition plugin parameter list.
   --  ------------------------------
   procedure Append (Plugin   : in out Condition_Plugin;
                     Name     : in Wiki.Strings.WString;
                     Value    : in Wiki.Strings.WString) is
   begin
      Attributes.Append (Plugin.Params, Name, Value);
   end Append;

   --  ------------------------------
   --  Evaluate the condition and return it as a boolean status.
   --  ------------------------------
   function Evaluate (Plugin   : in Condition_Plugin;
                      Params   : in Wiki.Attributes.Attribute_List) return Boolean is
      procedure Check (Name : in String;
                       Value : in Wiki.Strings.WString);

      Result : Boolean := False;
      Index  : Natural := 0;

      procedure Check (Name : in String;
                       Value : in Wiki.Strings.WString) is
         pragma Unreferenced (Name);

         Pos : Wiki.Attributes.Cursor;
      begin
         Index := Index + 1;
         if Index > 1 and then not Result then
            Pos := Attributes.Find (Plugin.Params, Wiki.Strings.To_String (Value));
            Result := Attributes.Has_Element (Pos);
         end if;
      end Check;

   begin
      Attributes.Iterate (Params, Check'Access);
      return Result;
   end Evaluate;

   --  ------------------------------
   --  Get the type of condition (IF, ELSE, ELSIF, END) represented by the plugin.
   --  ------------------------------
   function Get_Condition_Kind (Plugin : in Condition_Plugin;
                                Params : in Wiki.Attributes.Attribute_List)
                                return Condition_Type is
      pragma Unreferenced (Plugin);

      Name : constant Strings.WString := Attributes.Get_Attribute (Params, "name");
   begin
      if Name = "if" then
         return CONDITION_IF;
      elsif Name = "else" then
         return CONDITION_ELSE;
      elsif Name = "elsif" then
         return CONDITION_ELSIF;
      elsif Name = "end" then
         return CONDITION_END;
      else
         return CONDITION_IF;
      end if;
   end Get_Condition_Kind;

   --  ------------------------------
   --  Evaluate the condition described by the parameters and hide or show the wiki
   --  content.
   --  ------------------------------
   overriding
   procedure Expand (Plugin   : in out Condition_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context) is
      pragma Unreferenced (Document);

      Kind : constant Condition_Type
        := Condition_Plugin'Class (Plugin).Get_Condition_Kind (Params);
   begin
      case Kind is
         when CONDITION_IF =>
            Plugin.Depth := Plugin.Depth + 1;
            Plugin.Values (Plugin.Depth) := not Condition_Plugin'Class (Plugin).Evaluate (Params);

         when CONDITION_ELSIF =>
            Plugin.Values (Plugin.Depth) := not Condition_Plugin'Class (Plugin).Evaluate (Params);

         when CONDITION_ELSE =>
            Plugin.Values (Plugin.Depth) := not Plugin.Values (Plugin.Depth);

         when CONDITION_END =>
            Plugin.Depth := Plugin.Depth - 1;

      end case;
      Context.Previous.Is_Hidden := Plugin.Values (Plugin.Depth);
   end Expand;

end Wiki.Plugins.Conditions;
