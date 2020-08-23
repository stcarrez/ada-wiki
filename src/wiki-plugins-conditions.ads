-----------------------------------------------------------------------
--  wiki-plugins-conditions -- Condition Plugin
--  Copyright (C) 2016, 2020 Stephane Carrez
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
with Wiki.Strings;

--  === Conditions Plugins ===
--  The <b>Wiki.Plugins.Conditions</b> package defines a set of conditional plugins
--  to show or hide wiki content according to some conditions evaluated during the parsing phase.
--
package Wiki.Plugins.Conditions is

   MAX_CONDITION_DEPTH : constant Natural := 31;

   type Condition_Depth is new Natural range 0 .. MAX_CONDITION_DEPTH;

   type Condition_Type is (CONDITION_IF, CONDITION_ELSIF, CONDITION_ELSE, CONDITION_END);

   type Condition_Plugin is new Wiki_Plugin with private;

   --  Evaluate the condition and return it as a boolean status.
   function Evaluate (Plugin   : in Condition_Plugin;
                      Params   : in Wiki.Attributes.Attribute_List) return Boolean;

   --  Get the type of condition (IF, ELSE, ELSIF, END) represented by the plugin.
   function Get_Condition_Kind (Plugin : in Condition_Plugin;
                                Params : in Wiki.Attributes.Attribute_List)
                                return Condition_Type;

   --  Evaluate the condition described by the parameters and hide or show the wiki
   --  content.
   overriding
   procedure Expand (Plugin   : in out Condition_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context);

   --  Append the attribute name/value to the condition plugin parameter list.
   procedure Append (Plugin   : in out Condition_Plugin;
                     Name     : in Wiki.Strings.WString;
                     Value    : in Wiki.Strings.WString);

private

   type Boolean_Array is array (Condition_Depth) of Boolean;
   pragma Pack (Boolean_Array);

   type Condition_Plugin is new Wiki_Plugin with record
      Depth  : Condition_Depth := 1;
      Values : Boolean_Array := (others => False);
      Params : Wiki.Attributes.Attribute_List;
   end record;

end Wiki.Plugins.Conditions;
