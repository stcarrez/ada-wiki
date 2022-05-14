-----------------------------------------------------------------------
--  wiki-html_parsers-tests -- Unit tests for the HTML parser
--  Copyright (C) 2022 Stephane Carrez
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
with GNAT.Source_Info;

package body Wiki.Html_Parser.Tests is

   package Caller is new Util.Test_Caller (Test, "Wikis.Html_Parser");

   procedure Assert_Equals is
      new Util.Assertions.Assert_Equals_T (State_Type);

   procedure Assert_Equals is
      new Util.Assertions.Assert_Equals_T (Html_Parser_State);

   procedure Assert_Equals is
      new Util.Assertions.Assert_Equals_T (Entity_State_Type);

   procedure Assert_Equals is
      new Util.Assertions.Assert_Equals_T (Wiki.Strings.WChar);

   procedure Check_Parser (T           : in out Test;
                           Expect      : in Wiki.Strings.WString;
                           Expect_Kind : in State_Type;
                           Content     : in Wiki.Strings.WString;
                           Source      : in String := GNAT.Source_Info.File;
                           Line        : in Natural := GNAT.Source_Info.Line);

   procedure Check_Error (T           : in out Test;
                          Last_Pos    : in Natural;
                          Content     : in Wiki.Strings.WString;
                          Source      : in String := GNAT.Source_Info.File;
                          Line        : in Natural := GNAT.Source_Info.Line);

   procedure Check_Attribute (T     : in out Test;
                              Name  : in String;
                              Value : in Wiki.Strings.WString);

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Wiki.Html_Parsers.Parse_Element",
                       Test_Parse_Element'Access);
      Caller.Add_Test (Suite, "Test Wiki.Html_Parsers.Parse_Element (Attributes)",
                       Test_Parse_Element_Attributes'Access);
      Caller.Add_Test (Suite, "Test Wiki.Html_Parsers.Parse_Element (Errors)",
                       Test_Parse_Error'Access);
      Caller.Add_Test (Suite, "Test Wiki.Html_Parsers.Parse_Element (Doctype)",
                       Test_Parse_Doctype'Access);
      Caller.Add_Test (Suite, "Test Wiki.Html_Parsers.Parse_Entity",
                       Test_Parse_Entity'Access);
   end Add_Tests;

   procedure Check_Parser (T           : in out Test;
                           Expect      : in Wiki.Strings.WString;
                           Expect_Kind : in State_Type;
                           Content     : in Wiki.Strings.WString;
                           Source      : in String := GNAT.Source_Info.File;
                           Line        : in Natural := GNAT.Source_Info.Line) is
      procedure Process (Kind       : in State_Type;
                         Name       : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List);

      P    : Parser_Type;
      Last : Natural;

      procedure Process (Kind       : in State_Type;
                         Name       : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
      begin
         Assert_Equals (T, Expect_Kind, Kind, "Invalid state type", Source, Line);
         if Kind /= HTML_ERROR then
            T.Assert (Name = Expect, "Invalid name", Source, Line);
         end if;
         T.Attributes := Attributes;
      end Process;

   begin
      Parse_Element (P, Content, Content'First, Process'Access, Last);
      Util.Tests.Assert_Equals (T, Content'Last + 1, Last, "Invalid last index", Source, Line);
      Assert_Equals (T, P.State, State_None, "Invalid parser state", Source, Line);

      Parse_Element (P, Content, Content'First, Process'Access, Last);
      Util.Tests.Assert_Equals (T, Content'Last + 1, Last, "Invalid last index", Source, Line);
      Assert_Equals (T, P.State, State_None, "Invalid parser state", Source, Line);

      for I in 1 .. Content'Length loop
         declare
            First   : Natural := Content'First;
            End_Pos : Natural := Content'First;
            Retry   : Natural := 0;
         begin
            loop
               Parse_Element (P, Content (Content'First .. End_Pos), First, Process'Access, Last);
               exit when Last = Content'Last + 1;
               Retry := Retry + 1;
               First := Last;
               End_Pos := End_Pos + I;
               if End_Pos > Content'Last then
                  End_Pos := Content'Last;
               end if;
               T.Assert (Retry <= Content'Length, "Too many retries", Source, Line);
            end loop;
            Assert_Equals (T, P.State, State_None, "Invalid parser state", Source, Line);
         end;
      end loop;
   end Check_Parser;

   procedure Check_Error (T           : in out Test;
                          Last_Pos    : in Natural;
                          Content     : in Wiki.Strings.WString;
                          Source      : in String := GNAT.Source_Info.File;
                          Line        : in Natural := GNAT.Source_Info.Line) is
      procedure Process (Kind       : in State_Type;
                         Name       : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List);

      P    : Parser_Type;
      Last : Natural;

      procedure Process (Kind       : in State_Type;
                         Name       : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
         pragma Unreferenced (Name, Attributes);
      begin
         Assert_Equals (T, HTML_ERROR, Kind, "Invalid state type", Source, Line);
      end Process;

   begin
      Parse_Element (P, Content, Content'First, Process'Access, Last);
      Util.Tests.Assert_Equals (T, Last_Pos, Last, "Invalid last index", Source, Line);
   end Check_Error;

   --  ------------------------------
   --  Test Parse_Element operation.
   --  ------------------------------
   procedure Test_Parse_Element (T : in out Test) is
   begin
      Check_Parser (T, "a", HTML_START, "a>");
      Check_Parser (T, "a", HTML_START, "   a   >");
      Check_Parser (T, "input", HTML_START, "input>");
      Check_Parser (T, "a", HTML_END, "/a>");
      Check_Parser (T, "input", HTML_START_END, "  input  />");
      Check_Parser (T, "br", HTML_START_END, "br/>");
      Check_Parser (T, "img", HTML_END, "/img   >");
   end Test_Parse_Element;

   --  ------------------------------
   --  Test parsing HTML with errors.
   --  ------------------------------
   procedure Test_Parse_Error (T : in out Test) is
   begin
      Check_Error (T, 2, "a'");
      Check_Error (T, 9, "input   """);
      Check_Error (T, 7, "input =");
      Check_Error (T, 17, "input abcd efgh =");
   end Test_Parse_Error;

   procedure Check_Attribute (T     : in out Test;
                              Name  : in String;
                              Value : in Wiki.Strings.WString) is
      V : constant Wiki.Strings.WString := Wiki.Attributes.Get_Attribute (T.Attributes, Name);
   begin
      T.Assert (Value = V, "Invalid attribute " & Name);
   end Check_Attribute;

   --  ------------------------------
   --  Test Parse_Element with attributes
   --  ------------------------------
   procedure Test_Parse_Element_Attributes (T : in out Test) is
   begin
      Check_Parser (T, "a", HTML_START, "a href='toto' title=' bla bla '>");

      Util.Tests.Assert_Equals (T, 2, Wiki.Attributes.Length (T.Attributes),
                                "invalid number of attributes");
      Check_Attribute (T, "href", "toto");
      Check_Attribute (T, "title", " bla bla ");

      Check_Parser (T, "img", HTML_START_END, "img src=""plop"" width=4 height=5/>");

      Util.Tests.Assert_Equals (T, 3, Wiki.Attributes.Length (T.Attributes),
                                "invalid number of attributes");
      Check_Attribute (T, "src", "plop");
      Check_Attribute (T, "width", "4");
      Check_Attribute (T, "height", "5");

      Check_Parser (T, "select", HTML_START, "select id=a name=test >");

      Util.Tests.Assert_Equals (T, 2, Wiki.Attributes.Length (T.Attributes),
                                "invalid number of attributes");
      Check_Attribute (T, "id", "a");
      Check_Attribute (T, "name", "test");

   end Test_Parse_Element_Attributes;

   --  ------------------------------
   --  Test Parse_Doctype
   --  ------------------------------
   procedure Test_Parse_Doctype (T : in out Test) is
      procedure Process (Kind       : in State_Type;
                         Name       : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List);

      P    : Parser_Type;
      Last : Natural;

      procedure Process (Kind       : in State_Type;
                         Name       : in Wiki.Strings.WString;
                         Attributes : in out Wiki.Attributes.Attribute_List) is
         pragma Unreferenced (Kind, Name, Attributes);
      begin
         T.Fail ("Should not be called");
      end Process;
   begin
      Parse_Element (P, "!doctype html>", 1, Process'Access, Last);
      Util.Tests.Assert_Equals (T, 15, Last, "Invalid last index");

      Parse_Element (P, "!doctype html>  ", 1, Process'Access, Last);
      Util.Tests.Assert_Equals (T, 15, Last, "Invalid last index");

      Parse_Element (P, "!-- blq blq < > -->", 1, Process'Access, Last);
      Util.Tests.Assert_Equals (T, 20, Last, "Invalid last index");

      Parse_Element (P, "!-- blq blq < > --", 1, Process'Access, Last);
      Util.Tests.Assert_Equals (T, 19, Last, "Invalid last index");
      Assert_Equals (T, P.State, State_Comment, "Invalid parser state");

      Parse_Element (P, "bla -- blq blq < > -->", 1, Process'Access, Last);
      Util.Tests.Assert_Equals (T, 23, Last, "Invalid last index");
      Assert_Equals (T, P.State, State_None, "Invalid parser state");

      Parse_Element (P, "!doctype html  ", 1, Process'Access, Last);
      Util.Tests.Assert_Equals (T, 16, Last, "Invalid last index");
      Assert_Equals (T, P.State, State_Doctype, "Invalid parser state");

      Parse_Element (P, "bla -- blq blq < > --x", 1, Process'Access, Last);
      Util.Tests.Assert_Equals (T, 19, Last, "Invalid last index");
      Assert_Equals (T, P.State, State_None, "Invalid parser state");

   end Test_Parse_Doctype;

   --  ------------------------------
   --  Test Parse_Entity.
   --  ------------------------------
   procedure Test_Parse_Entity (T : in out Test) is
      P      : Parser_Type;
      Last   : Natural;
      Entity : Wiki.Strings.WChar;
      Status : Entity_State_Type := ENTITY_NONE;
   begin
      Parse_Entity (P, "amp; ", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 5, Last, "Invalid last index");
      Assert_Equals (T, '&', Entity, "Invalid parsed entity");

      Parse_Entity (P, "dagger;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 8, Last, "Invalid last index");
      Assert_Equals (T, Wiki.Strings.WChar'Val (8224), Entity, "Invalid parsed entity");

      Parse_Entity (P, "Dagger;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 8, Last, "Invalid last index");
      Assert_Equals (T, Wiki.Strings.WChar'Val (8225), Entity, "Invalid parsed entity");

      Parse_Entity (P, "#1234;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 7, Last, "Invalid last index");
      Assert_Equals (T, Wiki.Strings.WChar'Val (1234), Entity, "Invalid parsed entity");

      Parse_Entity (P, "#x1234;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 8, Last, "Invalid last index");
      Assert_Equals (T, Wiki.Strings.WChar'Val (16#1234#), Entity, "Invalid parsed entity");

      Parse_Entity (P, "#xabCD;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 8, Last, "Invalid last index");
      Assert_Equals (T, Wiki.Strings.WChar'Val (16#abcd#), Entity, "Invalid parsed entity");
      Assert_Equals (T, ENTITY_VALID, Status, "Invalid status");

      Parse_Entity (P, "dagobert;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 1, Last, "Invalid last index");
      Assert_Equals (T, NUL, Entity, "Invalid parsed entity");
      Assert_Equals (T, ENTITY_NONE, Status, "Invalid status");

      Parse_Entity (P, "#x000000;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 10, Last, "Invalid last index");
      Assert_Equals (T, Wiki.Strings.WChar'Val (16#0FFFD#), Entity, "Invalid parsed entity");

      Parse_Entity (P, "#000000;", 1, Status, Entity, Last);
      Util.Tests.Assert_Equals (T, 9, Last, "Invalid last index");
      Assert_Equals (T, Wiki.Strings.WChar'Val (16#0FFFD#), Entity, "Invalid parsed entity");

      Parse_Entity (P, "#00000b;", 1, Status, Entity, Last);
      Assert_Equals (T, ENTITY_NONE, Status, "Invalid status");
      Util.Tests.Assert_Equals (T, 1, Last, "Invalid last index");
      Assert_Equals (T, NUL, Entity, "Invalid parsed entity");

      Parse_Entity (P, "#x0000g0;", 1, Status, Entity, Last);
      Assert_Equals (T, ENTITY_NONE, Status, "Invalid status");
      Util.Tests.Assert_Equals (T, 1, Last, "Invalid last index");
      Assert_Equals (T, NUL, Entity, "Invalid parsed entity");

      Parse_Entity (P, "bla bla bla bla bla ;", 1, Status, Entity, Last);
      Assert_Equals (T, ENTITY_NONE, Status, "Invalid status");
      Util.Tests.Assert_Equals (T, 1, Last, "Invalid last index");
      Assert_Equals (T, NUL, Entity, "Invalid parsed entity");

   end Test_Parse_Entity;

end Wiki.Html_Parser.Tests;
