-----------------------------------------------------------------------
--  wiki-html_parser-tests -- Unit tests for the HTML parser
--  Copyright (C) 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
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
