-----------------------------------------------------------------------
--  CommonMark specification tests - Unit tests from CommonMark specification
--  Copyright (C) 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Directories;

with Util.Measures;
with Util.Beans.Objects;
with Util.Beans.Objects.Iterators;
with Util.Serialize.IO.JSON;
with Util.Strings.Sets;
with Util.Files;

with Wiki.Render.Html;
with Wiki.Streams.Html.Builders;
with Wiki.Documents;
with Wiki.Parsers;
package body Wiki.CommonMark_Tests is

   use Ada.Strings.Unbounded;
   function Normalize (Content : in String) return UString;

   function Starts_With_Block (Content : in String) return Boolean is
     (Util.Strings.Starts_With (Content, "<ul>")
      or else Util.Strings.Starts_With (Content, "<li>")
      or else Util.Strings.Starts_With (Content, "<tr>")
      or else Util.Strings.Starts_With (Content, "<td>")
      or else Util.Strings.Starts_With (Content, "<div>")
      or else Util.Strings.Starts_With (Content, "<script>")
      or else Util.Strings.Starts_With (Content, "</script>")
      or else Util.Strings.Starts_With (Content, "</div>"));

   function Normalize (Content : in String) return UString is
      use Util.Strings;

      Result : UString;
      Pos    : Natural := Content'First;
      Last   : constant Natural := Content'Last;
      Ignore : Boolean;
      In_Pre : Boolean := False;
   begin
      while Pos <= Last loop
         if Starts_With (Content (Pos .. Last), "<pre>") then
            In_Pre := True;
         elsif In_Pre and then Starts_With (Content (Pos .. Last), "</pre>") then
            In_Pre := False;
         end if;

         if Starts_With (Content (Pos .. Last), "<br />") then
            Append (Result, "<br>");
            Pos := Pos + 6 - 1;
            if Pos < Last and then Content (Pos + 1) = ASCII.LF then
               Pos := Pos + 1;
            end if;

         elsif not In_Pre and then Content (Pos) = ASCII.LF then
            exit when Pos = Last;
            if Content (Pos + 1) = '<' then
               Ignore := (Pos > Content'First and then Content (Pos - 1) = '>')
                 or else (Starts_With_Block (Content (Pos + 1 .. Last)));
               if not Ignore then
                  Append (Result, ' ');
               end if;
            else
               Ignore := Content (Pos + 1) in ' ' | ASCII.LF;
               if not Ignore then
                  Append (Result, ' ');
               end if;
            end if;
         elsif not In_Pre and then Content (Pos) = ' ' and then Pos < Last then
            Ignore := (Content (Pos + 1) in ' ' | ASCII.LF)
              or else Starts_With_Block (Content (Pos + 1 .. Last));
            if not Ignore then
               Append (Result, Content (Pos));
            end if;
         else
            Append (Result, Content (Pos));
         end if;
         Pos := Pos + 1;
      end loop;
      return Result;
   end Normalize;

   --  ------------------------------
   --  Test rendering a wiki text in HTML or text.
   --  ------------------------------
   procedure Test_Render (T : in out Test) is
      Doc         : Wiki.Documents.Document;
      Engine      : Wiki.Parsers.Parser;
      Output      : aliased Wiki.Streams.Html.Builders.Html_Output_Stream;
   begin
      declare
         Time : Util.Measures.Stamp;
      begin
         Engine.Set_Syntax (Wiki.SYNTAX_MARKDOWN);
         Engine.Parse (To_String (T.Content), Doc);
         Output.Set_Indent_Level (0);
         Output.Set_Strict_XML (True);
         Util.Measures.Report (Time, "Parse " & To_String (T.Name));
         declare
            Renderer : aliased Wiki.Render.Html.Html_Renderer;
         begin
            Renderer.Set_Output_Stream (Output'Unchecked_Access);
            Renderer.Set_No_Newline (False);
            Renderer.Render (Doc);
            Util.Measures.Report (Time, "Render HTML " & To_String (T.Name));
         end;
      end;
      declare
         Result      : constant String := Output.To_String;
         Normalized  : constant String := To_String (T.Normalized);
         Result_Norm : constant UString := Normalize (Result);
         Pass        : constant Boolean := To_String (Result_Norm) = Normalized;
      begin
         if not Pass then
            if not T.XFail then
               Ada.Text_IO.Put_Line ("Markdown:");
               Ada.Text_IO.Put_Line (To_String (T.Content));
               Ada.Text_IO.Put_Line ("Expect:");
               Ada.Text_IO.Put_Line (To_String (T.Expect));
               Ada.Text_IO.Put_Line ("Normalized:");
               Ada.Text_IO.Put_Line (Normalized);
               Ada.Text_IO.Put_Line ("Result:");
               Ada.Text_IO.Put_Line (Result);
               T.Assert (To_String (Result_Norm) = Normalized,
                         "Render " & T.Get_Name);
            end if;
         elsif T.XFail then
            Ada.Text_IO.Put_Line ("Now succeeding:");
            T.Assert (not Pass,
                      "Render " & T.Get_Name);
         end if;
      end;
   end Test_Render;

   --  ------------------------------
   --  Test case name
   --  ------------------------------
   overriding
   function Name (T : in Test) return Util.Tests.Message_String is
   begin
      return "CMark " & To_String (T.Section) & T.Example'Image;
   end Name;

   --  ------------------------------
   --  Perform the test.
   --  ------------------------------
   overriding
   procedure Run_Test (T : in out Test) is
   begin
      T.Test_Render;
   end Run_Test;

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
      package UBO renames Util.Beans.Objects;
      procedure Add_Test (Node : in UBO.Object);
      procedure Read_XFail (Line : in String);

      XFail : Util.Strings.Sets.Set;

      procedure Add_Test (Node : in UBO.Object) is
         Markdown   : constant UBO.Object := UBO.Get_Value (Node, "markdown");
         Html       : constant UBO.Object := UBO.Get_Value (Node, "html");
         Example    : constant UBO.Object := UBO.Get_Value (Node, "example");
         Start_Line : constant UBO.Object := UBO.Get_Value (Node, "start_line");
         End_Line   : constant UBO.Object := UBO.Get_Value (Node, "end_Line");
         Section    : constant UBO.Object := UBO.Get_Value (Node, "section");
         Tst        : Test_Case_Access;
      begin
         if UBO.Is_Null (UBO.Get_Value (Node, "skip")) then
            Tst := new Test;
            Tst.Example := UBO.To_Integer (Example);
            Tst.Start_Line := UBO.To_Integer (Start_Line);
            Tst.End_Line := UBO.To_Integer (End_Line);
            Tst.Section := UBO.To_Unbounded_String (Section);
            Tst.Content := UBO.To_Unbounded_String (Markdown);
            Tst.Expect := UBO.To_Unbounded_String (Html);
            Tst.Normalized := Normalize (UBO.To_String (Html));
            Tst.XFail := XFail.Contains (Tst.Get_Name);
            Suite.Add_Test (Tst.all'Access);
         end if;
      end Add_Test;

      procedure Read_XFail (Line : in String) is
      begin
         XFail.Include (Line);
      end Read_XFail;

      Root : UBO.Object;
      Iter : UBO.Iterators.Iterator;
   begin
      if Ada.Directories.Exists ("xfail.txt") then
         Util.Files.Read_File ("xfail.txt", Read_XFail'Access);
      end if;
      if Ada.Directories.Exists ("spec.json") then
         Root := Util.Serialize.IO.JSON.Read ("spec.json");
         Iter := UBO.Iterators.First (Root);
         while UBO.Iterators.Has_Element (Iter) loop
            Add_Test (UBO.Iterators.Element (Iter));
            UBO.Iterators.Next (Iter);
         end loop;
      end if;
   end Add_Tests;

end Wiki.CommonMark_Tests;
