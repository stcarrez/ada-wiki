-----------------------------------------------------------------------
--  wiki-parsers-tests -- Unit tests for wiki parsing
--  Copyright (C) 2011, 2012, 2013, 2016, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Wiki.Parsers.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test bold rendering.
   procedure Test_Wiki_Bold (T : in out Test);

   --  Test italic rendering.
   procedure Test_Wiki_Italic (T : in out Test);

   --  Test various format rendering.
   procedure Test_Wiki_Formats (T : in out Test);

   --  Test heading rendering.
   procedure Test_Wiki_Section (T : in out Test);

   --  Test list rendering.
   procedure Test_Wiki_List (T : in out Test);

   --  Test link rendering.
   procedure Test_Wiki_Link (T : in out Test);

   --  Test quote rendering.
   procedure Test_Wiki_Quote (T : in out Test);

   --  Test line break rendering.
   procedure Test_Wiki_Line_Break (T : in out Test);

   --  Test image rendering.
   procedure Test_Wiki_Image (T : in out Test);

   --  Test preformatted rendering.
   procedure Test_Wiki_Preformatted (T : in out Test);

   --  Test the text renderer.
   procedure Test_Wiki_Text_Renderer (T : in out Test);

   --  Test the string parser with UTF-8 support.
   procedure Test_Wiki_UTF_8 (T : in out Test);

   procedure Test_Buffer (T : in out Test);

   procedure Test_Wiki_Parse (T : in out Test);

end Wiki.Parsers.Tests;
