-----------------------------------------------------------------------
--  wiki-filters-html-tests -- Unit tests for wiki HTML filters
--  Copyright (C) 2015 Stephane Carrez
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

with Util.Test_Caller;
with Util.Assertions;
with Util.Strings;
with Util.Log.Loggers;

package body Wiki.Filters.Html.Tests is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Wiki.Filters");

   package Caller is new Util.Test_Caller (Test, "Wikis.Filters.Html");

   procedure Assert_Equals is
      new Util.Assertions.Assert_Equals_T (Html_Tag_Type);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Wiki.Filters.Html.Find_Tag",
                       Test_Find_Tag'Access);
   end Add_Tests;

   --  Test Find_Tag operation.
   procedure Test_Find_Tag (T : in out Test) is
   begin
      for I in Html_Tag_Type'Range loop
         declare
            Name : constant String := Html_Tag_Type'Image (I);
            Wname : constant Wide_Wide_String := Html_Tag_Type'Wide_Wide_Image (I);
            Pos  : constant Natural := Util.Strings.Index (Name, '_');
            Tag  : constant Html_Tag_Type := Find_Tag (WName (WName'First .. Pos - 1));
         begin
            Log.Info ("Checking tag {0}", Name);
            Assert_Equals (T, I, Tag, "Find_Tag failed");
         end;
      end loop;
   end Test_Find_Tag;

end Wiki.Filters.Html.Tests;
