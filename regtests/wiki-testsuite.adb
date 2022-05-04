-----------------------------------------------------------------------
--  Wiki testsuite - Ada Wiki Test suite
--  Copyright (C) 2015, 2016, 2022 Stephane Carrez
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
