-----------------------------------------------------------------------
--  wiki-samples_tests -- Unit tests for samples
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

generic
   Syntax : String;
   Option : String;
package Wiki.Samples_Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   procedure Test_Render (T : in out Test);
   procedure Test_Import (T : in out Test);
   procedure Test_Convert (T : in out Test);
   procedure Test_Words (T : in out Test);

end Wiki.Samples_Tests;
