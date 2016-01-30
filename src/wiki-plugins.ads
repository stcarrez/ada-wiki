-----------------------------------------------------------------------
--  wiki-plugins -- Wiki plugins
--  Copyright (C) 2016 Stephane Carrez
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

with Wiki.Attributes;
with Wiki.Documents;

--  == Plugins ==
--  The <b>Wiki.Plugins</b> package defines the plugin interface that is used by the wiki
--  engine to provide pluggable extensions in the Wiki.
--
package Wiki.Plugins is

   pragma Preelaborate;

   type Wiki_Plugin is limited interface;
   type Wiki_Plugin_Access is access all Wiki_Plugin'Class;

   --  Expand the plugin configured with the parameters for the document.
   procedure Expand (Plugin   : in out Wiki_Plugin;
                     Document : in out Wiki.Documents.Document_Reader'Class;
                     Params   : in out Wiki.Attributes.Attribute_List_Type) is abstract;

end Wiki.Plugins;
