-----------------------------------------------------------------------
--  Render Tests - Unit tests for AWA Wiki rendering
--  Copyright (C) 2013, 2016, 2020 Stephane Carrez
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

with Ada.Strings.Unbounded;
package Wiki.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test_Case with record
      Writer : Integer;
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      File    : Ada.Strings.Unbounded.Unbounded_String;
      Expect  : Ada.Strings.Unbounded.Unbounded_String;
      Result  : Ada.Strings.Unbounded.Unbounded_String;
      Collect : Ada.Strings.Unbounded.Unbounded_String;
      Expect_Collect : Ada.Strings.Unbounded.Unbounded_String;
      Source  : Wiki.Wiki_Syntax;
      Format  : Wiki.Wiki_Syntax;
      Is_Html : Boolean := False;
      Is_Cvt  : Boolean := False;
   end record;
   type Test_Case_Access is access all Test;

   --  Test case name
   overriding
   function Name (T : Test) return Util.Tests.Message_String;

   --  Perform the test.
   overriding
   procedure Run_Test (T : in out Test);

   --  Test rendering a wiki text in HTML or text.
   procedure Test_Render (T : in out Test);

end Wiki.Tests;
