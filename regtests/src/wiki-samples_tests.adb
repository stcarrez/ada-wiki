-----------------------------------------------------------------------
--  wiki-samples_tests -- Unit tests for samples
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Ada.Strings.Unbounded;
with Ada.Directories;
with Util.Test_Caller;
with Util.Files;
with GNAT.Source_Info;
package body Wiki.Samples_Tests is

   package Caller is new Util.Test_Caller (Test, "Samples." & Syntax);

   procedure Test_Execute (T : in out Test;
                           Command : in String;
                           Name    : in String;
                           Source  : String := GNAT.Source_Info.File;
                           Line    : Natural := GNAT.Source_Info.Line);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test render",
                       Test_Render'Access);
      Caller.Add_Test (Suite, "Test import",
                       Test_Import'Access);
      Caller.Add_Test (Suite, "Test convert",
                       Test_Convert'Access);
      Caller.Add_Test (Suite, "Test words",
                       Test_Words'Access);
   end Add_Tests;

   procedure Test_Execute (T : in out Test;
                           Command : in String;
                           Name    : in String;
                           Source  : String := GNAT.Source_Info.File;
                           Line    : Natural := GNAT.Source_Info.Line) is
      Expect_Dir  : constant String := Util.Tests.Get_Path ("regtests/expect/samples");
      Result_Dir  : constant String := Util.Tests.Get_Test_Path ("samples");
      Result_File : constant String := Util.Files.Compose (Result_Dir, Name);
      Expect_File : constant String := Util.Files.Compose (Expect_Dir, Name);
      Result      : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if not Ada.Directories.Exists (Result_Dir) then
         Ada.Directories.Create_Directory (Result_Dir);
      end if;
      T.Execute (Command, "", Result_File, Result, "", 0, Source, Line);
      Util.Tests.Assert_Equal_Files (T, Expect_File, Result_File,
                                     "Command '" & Command & "' failed");
   end Test_Execute;

   --  ------------------------------
   --  Tests the render example.
   --  ------------------------------
   procedure Test_Render (T : in out Test) is
   begin
      Test_Execute (T, "./bin/render " & Option & " regtests/expect/samples/article-2." & Syntax,
                    "article-2-" & Syntax & ".html");
   end Test_Render;

   --  ------------------------------
   --  Tests the import example.
   --  ------------------------------
   procedure Test_Import (T : in out Test) is
   begin
      Test_Execute (T, "./bin/import " & Option & " regtests/files/html/article-2.html",
                    "article-2." & Syntax);
   end Test_Import;

   --  ------------------------------
   --  Tests the convert example.
   --  ------------------------------
   procedure Test_Convert (T : in out Test) is
   begin
      Test_Execute (T, "./bin/convert -d " & Syntax & " -s markdown "
                    & "regtests/expect/samples/article-2.markdown",
                    "convert-article-2." & Syntax);
   end Test_Convert;

   --  ------------------------------
   --  Tests the words example.
   --  ------------------------------
   procedure Test_Words (T : in out Test) is
   begin
      Test_Execute (T, "./bin/words " & Option & " regtests/expect/samples/article-2.markdown",
                    "words-article-2-" & Syntax & ".txt");
   end Test_Words;

end Wiki.Samples_Tests;
