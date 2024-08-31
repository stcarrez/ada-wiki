-----------------------------------------------------------------------
--  wiki-filters-html-tests -- Unit tests for wiki HTML filters
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Util.Tests;

package Wiki.Filters.Html.Tests is

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite);

   type Test is new Util.Tests.Test with null record;

   --  Test Find_Tag operation.
   procedure Test_Find_Tag (T : in out Test);

end Wiki.Filters.Html.Tests;
