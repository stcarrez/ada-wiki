-----------------------------------------------------------------------
--  wiki-plugins-variables -- Variables plugin
--  Copyright (C) 2020 Stephane Carrez
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

--  === Variables Plugins ===
--  The `Wiki.Plugins.Variables` package defines a the variables plugin that allows
--  to set or update a variable that will be replaced by the `Wiki.Filters.Variables` filter.
package Wiki.Plugins.Variables is

   type Variable_Plugin is new Wiki_Plugin with null record;

   --  Set or update a variable in the `Wiki.Filters.Variable` filter.
   overriding
   procedure Expand (Plugin   : in out Variable_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context);

   type List_Variable_Plugin is new Wiki_Plugin with null record;

   --  List the variables from the `Wiki.Filters.Variable` filter.
   overriding
   procedure Expand (Plugin   : in out List_Variable_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context);

end Wiki.Plugins.Variables;
