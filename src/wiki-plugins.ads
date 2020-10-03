-----------------------------------------------------------------------
--  wiki-plugins -- Wiki plugins
--  Copyright (C) 2016, 2018, 2020 Stephane Carrez
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
with Wiki.Filters;
with Wiki.Strings;

--  == Plugins {#wiki-plugins} ==
--  The `Wiki.Plugins` package defines the plugin interface that is used by the wiki
--  engine to provide pluggable extensions in the Wiki.  The plugins works by using
--  a factory that finds and gives access to a plugin given its name.
--  The plugin factory is represented by the `Wiki_Plugin` limited interface which
--  must only implement the `Find` function.  A simple plugin factory can be created
--  by declaring a tagged record that implements the interface:
--
--    type Factory is new Wiki.Plugins.Plugin_Factory with null record;
--    overriding function
--    Find (Factory : in Factory;
--          Name    : in String) return Wiki.Plugins.Wiki_Plugin_Access;
--
--  @include wiki-plugins-variables.ads
--  @include wiki-plugins-conditions.ads
--  @include wiki-plugins-templates.ads
package Wiki.Plugins is

   pragma Preelaborate;

   type Plugin_Context;

   type Wiki_Plugin is limited interface;
   type Wiki_Plugin_Access is access all Wiki_Plugin'Class;
   type Plugin_Factory is limited interface;
   type Plugin_Factory_Access is access all Plugin_Factory'Class;

   --  Find a plugin knowing its name.
   function Find (Factory : in Plugin_Factory;
                  Name    : in String) return Wiki_Plugin_Access is abstract;

   type Plugin_Context is limited record
      Previous    : access Plugin_Context;
      Filters     : Wiki.Filters.Filter_Chain;
      Factory     : Plugin_Factory_Access;
      Variables   : Wiki.Attributes.Attribute_List;
      Syntax      : Wiki.Wiki_Syntax;
      Ident       : Wiki.Strings.UString;
      Is_Hidden   : Boolean := False;
      Is_Included : Boolean := False;
   end record;

   --  Expand the plugin configured with the parameters for the document.
   procedure Expand (Plugin   : in out Wiki_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context) is abstract;

end Wiki.Plugins;
