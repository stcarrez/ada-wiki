-----------------------------------------------------------------------
--  Wiki testsuite - Ada Wiki Test suite
--  Copyright (C) 2015, 2016, 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Parsers.Tests;
with Wiki.Tests;
with Wiki.Filters.Html.Tests;
with Wiki.Html_Parser.Tests;
package body Wiki.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      Wiki.Filters.Html.Tests.Add_Tests (Ret);
      Wiki.Parsers.Tests.Add_Tests (Ret);
      Wiki.Tests.Add_Tests (Ret);
      Wiki.Html_Parser.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end Wiki.Testsuite;
