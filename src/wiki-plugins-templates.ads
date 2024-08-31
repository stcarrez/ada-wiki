-----------------------------------------------------------------------
--  wiki-plugins-template -- Template Plugin
--  Copyright (C) 2016, 2020 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Wiki.Strings;

--  === Template Plugins ===
--  The `Wiki.Plugins.Templates` package defines an abstract template plugin.
--  To use the template plugin, the `Get_Template` procedure must be implemented.
--  It is responsible for getting the template content according to the plugin parameters.
--
package Wiki.Plugins.Templates is

   type Template_Plugin is abstract new Wiki_Plugin with null record;

   --  Get the template content for the plugin evaluation.
   procedure Get_Template (Plugin   : in out Template_Plugin;
                           Params   : in out Wiki.Attributes.Attribute_List;
                           Template : out Wiki.Strings.UString) is abstract;

   --  Expand the template configured with the parameters for the document.
   --  The <tt>Get_Template</tt> operation is called and the template content returned
   --  by that operation is parsed in the current document.  Template parameters are passed
   --  in the <tt>Context</tt> instance and they can be evaluated within the template
   --  while parsing the template content.
   overriding
   procedure Expand (Plugin   : in out Template_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context);

   type File_Template_Plugin is new Wiki_Plugin and Plugin_Factory with private;

   --  Find a plugin knowing its name.
   overriding
   function Find (Factory : in File_Template_Plugin;
                  Name    : in String) return Wiki_Plugin_Access;

   --  Set the directory path that contains template files.
   procedure Set_Template_Path (Plugin : in out File_Template_Plugin;
                                Path   : in String);

   --  Expand the template configured with the parameters for the document.
   --  Read the file whose basename correspond to the first parameter and parse that file
   --  in the current document.  Template parameters are passed
   --  in the <tt>Context</tt> instance and they can be evaluated within the template
   --  while parsing the template content.
   overriding
   procedure Expand (Plugin   : in out File_Template_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context);

private

   type File_Template_Plugin is new Wiki_Plugin and Plugin_Factory with record
      Path : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Wiki.Plugins.Templates;
