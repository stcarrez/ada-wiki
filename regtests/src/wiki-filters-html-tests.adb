-----------------------------------------------------------------------
--  wiki-filters-html-tests -- Unit tests for wiki HTML filters
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Test_Caller;
with Util.Assertions;
with Util.Strings;
with Util.Log.Loggers;

package body Wiki.Filters.Html.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Wiki.Filters");

   package Caller is new Util.Test_Caller (Test, "Wikis.Filters.Html");

   procedure Assert_Equals is
      new Util.Assertions.Assert_Equals_T (Html_Tag);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Wiki.Filters.Html.Find_Tag",
                       Test_Find_Tag'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test Find_Tag operation.
   --  ------------------------------
   procedure Test_Find_Tag (T : in out Test) is
   begin
      for I in Html_Tag'Range loop
         declare
            Name  : constant String := Html_Tag'Image (I);
            Wname : constant Wide_Wide_String := Html_Tag'Wide_Wide_Image (I);
            Pos   : constant Natural := Util.Strings.Index (Name, '_');
            Tname : constant Wide_Wide_String := Wname (Wname'First .. Pos - 1);
            Tag   : constant Html_Tag := Find_Tag (Tname);
         begin
            Log.Info ("Checking tag {0}", Name);
            if I /= ROOT_HTML_TAG then
               Assert_Equals (T, I, Tag, "Find_Tag failed");
            end if;

            Assert_Equals (T, UNKNOWN_TAG, Find_Tag (Tname & "x"),
                           "Find_Tag must return UNKNOWN_TAG");

            for K in Tname'Range loop
               if K = Tname'First then
                  Assert_Equals (T, UNKNOWN_TAG,
                                 Find_Tag ("_" & Tname (Tname'First + 1 .. Tname'Last)),
                                 "Find_Tag must return UNKNOWN_TAG");
               elsif K = Tname'Last then
                  Assert_Equals (T, UNKNOWN_TAG,
                                 Find_Tag (Tname (Tname'First .. K - 1) & "_"),
                                 "Find_Tag must return UNKNOWN_TAG");
               else
                  Assert_Equals (T, UNKNOWN_TAG,
                                 Find_Tag (Tname (Tname'First .. K - 1) & "_"
                                   & Tname (K + 1 .. Tname'Last)),
                                 "Find_Tag must return UNKNOWN_TAG");
               end if;
            end loop;
         end;
      end loop;
   end Test_Find_Tag;

end Wiki.Filters.Html.Tests;
