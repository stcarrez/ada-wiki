-----------------------------------------------------------------------
--  wiki-plugins-template -- Template Plugin
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
with Ada.IO_Exceptions;
with Ada.Directories;
with Wiki.Parsers;
with Wiki.Streams.Text_IO;

package body Wiki.Plugins.Templates is

   use Ada.Strings.Unbounded;

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
                     Context  : in out Plugin_Context) is
      P       : Wiki.Parsers.Parser;
      Content : Wiki.Strings.UString;
   begin
      Template_Plugin'Class (Plugin).Get_Template (Params, Content);
      P.Set_Context (Context);
      P.Parse (Content, Document);
   end Expand;

   --  ------------------------------
   --  Find a plugin knowing its name.
   --  ------------------------------
   overriding
   function Find (Factory : in File_Template_Plugin;
                  Name    : in String) return Wiki_Plugin_Access is
      Path  : constant String := Ada.Directories.Compose (To_String (Factory.Path), Name);
   begin
      if Ada.Directories.Exists (Path) then
         return Factory'Unrestricted_Access;
      else
         return null;
      end if;
   end Find;

   --  ------------------------------
   --  Set the directory path that contains template files.
   --  ------------------------------
   procedure Set_Template_Path (Plugin : in out File_Template_Plugin;
                                Path   : in String) is
   begin
      Plugin.Path := Ada.Strings.Unbounded.To_Unbounded_String (Path);
   end Set_Template_Path;

   --  ------------------------------
   --  Expand the template configured with the parameters for the document.
   --  Read the file whose basename correspond to the first parameter and parse that file
   --  in the current document.  Template parameters are passed
   --  in the <tt>Context</tt> instance and they can be evaluated within the template
   --  while parsing the template content.
   --  ------------------------------
   overriding
   procedure Expand (Plugin   : in out File_Template_Plugin;
                     Document : in out Wiki.Documents.Document;
                     Params   : in out Wiki.Attributes.Attribute_List;
                     Context  : in out Plugin_Context) is
      First : constant Wiki.Attributes.Cursor := Wiki.Attributes.First (Params);
      Name  : constant String := Wiki.Attributes.Get_Value (First);
      Input : aliased Wiki.Streams.Text_IO.File_Input_Stream;
      P     : Wiki.Parsers.Parser;
      Path  : constant String := Ada.Directories.Compose (To_String (Plugin.Path), Name);
   begin
      Input.Open (Path, "WCEM=8");
      P.Set_Context (Context);
      P.Parse (Input'Unchecked_Access, Document);
      Input.Close;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         null;
   end Expand;

end Wiki.Plugins.Templates;
