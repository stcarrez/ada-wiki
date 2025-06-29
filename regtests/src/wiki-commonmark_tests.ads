-----------------------------------------------------------------------
--  CommonMark specification tests - Unit tests from CommonMark specification
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

with Ada.Strings.Unbounded;
package Wiki.CommonMark_Tests is

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test_Case with record
      Name       : UString;
      Content    : UString;
      Expect     : UString;
      Normalized : UString;
      Result     : UString;
      Section    : UString;
      Start_Line : Natural;
      End_Line   : Natural;
      Example    : Natural;
      XFail      : Boolean;
   end record;
   type Test_Case_Access is access all Test;

   --  Test case name
   overriding
   function Name (T : Test) return Util.Tests.Message_String;

   --  Perform the test.
   overriding
   procedure Run_Test (T : in out Test);

   --  Test rendering the CommonMark test with Markdown parser.
   procedure Test_Render (T : in out Test);

end Wiki.CommonMark_Tests;
