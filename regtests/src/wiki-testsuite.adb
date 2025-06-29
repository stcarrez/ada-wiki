-----------------------------------------------------------------------
--  Wiki testsuite - Ada Wiki Test suite
--  Copyright (C) 2015, 2016, 2022, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Parsers.Tests;
with Wiki.Tests;
with Wiki.Filters.Html.Tests;
with Wiki.Html_Parser.Tests;
with Wiki.Samples_Tests;
with Wiki.CommonMark_Tests;
package body Wiki.Testsuite is

   package Markdown_Samples is
     new Wiki.Samples_Tests (Syntax => "markdown", Option => "-m");
   package Dotclear_Samples is
     new Wiki.Samples_Tests (Syntax => "dotclear", Option => "-d");
   package Mediawiki_Samples is
     new Wiki.Samples_Tests (Syntax => "mediawiki", Option => "-M");
   package Creole_Samples is
     new Wiki.Samples_Tests (Syntax => "creole", Option => "-c");
   package Textile_Samples is
     new Wiki.Samples_Tests (Syntax => "textile", Option => "-T");

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
      Ret : constant Util.Tests.Access_Test_Suite := Tests'Access;
   begin
      CommonMark_Tests.Add_Tests (Ret);
      Markdown_Samples.Add_Tests (Ret);
      Dotclear_Samples.Add_Tests (Ret);
      Mediawiki_Samples.Add_Tests (Ret);
      Creole_Samples.Add_Tests (Ret);
      Textile_Samples.Add_Tests (Ret);
      Wiki.Filters.Html.Tests.Add_Tests (Ret);
      Wiki.Parsers.Tests.Add_Tests (Ret);
      Wiki.Tests.Add_Tests (Ret);
      Wiki.Html_Parser.Tests.Add_Tests (Ret);
      return Ret;
   end Suite;

end Wiki.Testsuite;
