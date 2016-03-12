-----------------------------------------------------------------------
--  wiki-plugins-template -- Template Plugin
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
with Wiki.Parsers;

package body Wiki.Plugins.Templates is

   --  ------------------------------
   --  Expand the template configured with the parameters for the document.
   --  The <tt>Get_Template</tt> operation is called and the template content returned
   --  by that operation is parsed in the current document.  Template parameters are passed
   --  in the <tt>Context</tt> instance and they can be evaluated within the template
   --  while parsing the template content.
   --  ------------------------------
   overriding
   procedure Expand (Plugin   : in out Template_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in Plugin_Context) is
      P       : Wiki.Parsers.Parser;
      Content : Wiki.Strings.UString;
   begin
      Template_Plugin'Class (Plugin).Get_Template (Params, Content);
      P.Set_Context (Context);
      P.Parse (Content, Document);
   end Expand;

end Wiki.Plugins.Templates;
