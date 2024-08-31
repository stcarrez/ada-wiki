-----------------------------------------------------------------------
--  Wiki -- Unit tests
--  Copyright (C) 2015 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Wiki.Testsuite;
with Util.Tests;

procedure Wiki_Harness is
   procedure Harness is new Util.Tests.Harness (Wiki.Testsuite.Suite);
begin
   Harness ("wiki-tests.xml");
end Wiki_Harness;
