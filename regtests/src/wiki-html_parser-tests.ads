-----------------------------------------------------------------------
--  wiki-html_parser-tests -- Unit tests for the HTML parser
--  Copyright (C) 2022 Stephane Carrez
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

with Util.Tests;

package Wiki.Html_Parser.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with record
      Attributes : Wiki.Attributes.Attribute_List;
   end record;

   --  Test Parse_Element
   procedure Test_Parse_Element (T : in out Test);

   --  Test parsing HTML with errors.
   procedure Test_Parse_Error (T : in out Test);

   --  Test Parse_Element with attributes
   procedure Test_Parse_Element_Attributes (T : in out Test);

   --  Test Parse_Doctype
   procedure Test_Parse_Doctype (T : in out Test);

   --  Test Parse_Entity.
   procedure Test_Parse_Entity (T : in out Test);

end Wiki.Html_Parser.Tests;
