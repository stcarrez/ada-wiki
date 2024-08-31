-----------------------------------------------------------------------
--  wiki-plugins-variables -- Variables plugin
--  Copyright (C) 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
