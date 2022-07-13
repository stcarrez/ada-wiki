-----------------------------------------------------------------------
--  Render Tests - Unit tests for AWA Wiki rendering
--  Copyright (C) 2013, 2016, 2020, 2021, 2022 Stephane Carrez
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

with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Directories;

with Util.Measures;

with Wiki.Strings;
with Wiki.Render.Wiki;
with Wiki.Render.Html;
with Wiki.Render.Text;
with Wiki.Filters.Html;
with Wiki.Filters.TOC;
with Wiki.Filters.Autolink;
with Wiki.Filters.Variables;
with Wiki.Filters.Collectors;
with Wiki.Plugins.Templates;
with Wiki.Plugins.Conditions;
with Wiki.Plugins.Variables;
with Wiki.Streams.Text_IO;
with Wiki.Streams.Html.Text_IO;
with Wiki.Documents;
with Wiki.Parsers;
package body Wiki.Tests is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Test rendering a wiki text in HTML or text.
   --  ------------------------------
   procedure Test_Render (T : in out Test) is

      use Ada.Directories;

      Result_File : constant String := To_String (T.Result);
      Dir         : constant String := Containing_Directory (Result_File);
      Doc         : Wiki.Documents.Document;
      Engine      : Wiki.Parsers.Parser;
      Toc_Filter  : aliased Wiki.Filters.TOC.TOC_Filter;
      Html_Filter : aliased Wiki.Filters.Html.Html_Filter_Type;
      Var_Filter  : aliased Wiki.Filters.Variables.Variable_Filter;
      Auto_Filter : aliased Wiki.Filters.Autolink.Autolink_Filter;
      Words       : aliased Wiki.Filters.Collectors.Word_Collector_Type;
      Links       : aliased Wiki.Filters.Collectors.Link_Collector_Type;
      Images      : aliased Wiki.Filters.Collectors.Image_Collector_Type;
      Template    : aliased Wiki.Plugins.Templates.File_Template_Plugin;
      Condition   : aliased Wiki.Plugins.Conditions.Condition_Plugin;
      Variables   : aliased Wiki.Plugins.Variables.Variable_Plugin;
      List_Vars   : aliased Wiki.Plugins.Variables.List_Variable_Plugin;
      Input       : aliased Wiki.Streams.Text_IO.File_Input_Stream;
      Output      : aliased Wiki.Streams.Html.Text_IO.Html_Output_Stream;

      type Test_Factory is new Wiki.Plugins.Plugin_Factory with null record;

      --  Find a plugin knowing its name.
      overriding
      function Find (Factory : in Test_Factory;
                     Name    : in String) return Wiki.Plugins.Wiki_Plugin_Access;

      overriding
      function Find (Factory : in Test_Factory;
                     Name    : in String) return Wiki.Plugins.Wiki_Plugin_Access is
         pragma Unreferenced (Factory);
      begin
         if Name in "if" | "else" | "elsif" | "end" then
            return Condition'Unchecked_Access;
         elsif Name = "set" then
            return Variables'Unchecked_Access;
         elsif Name = "list" then
            return List_Vars'Unchecked_Access;
         else
            return Template.Find (Name);
         end if;
      end Find;

      Local_Factory : aliased Test_Factory;
      Has_Collector : constant Boolean := Length (T.Collect) > 0;
   begin
      if not Exists (Dir) then
         Create_Path (Dir);
      end if;
      Input.Open (Path => To_String (T.File),
                  Form => "WCEM=8");
      Output.Create (Result_File, "WCEM=8");
      Template.Set_Template_Path (Containing_Directory (To_String (T.File)));
      Condition.Append ("public", "");
      Condition.Append ("user", "admin");
      declare
         Time : Util.Measures.Stamp;
      begin
         Engine.Set_Syntax (T.Source);
         Engine.Set_Plugin_Factory (Local_Factory'Unchecked_Access);
         if Has_Collector then
            Engine.Add_Filter (Words'Unchecked_Access);
            Engine.Add_Filter (Links'Unchecked_Access);
            Engine.Add_Filter (Images'Unchecked_Access);
         end if;
         Engine.Add_Filter (Toc_Filter'Unchecked_Access);
         Engine.Add_Filter (Auto_Filter'Unchecked_Access);
         Engine.Add_Filter (Html_Filter'Unchecked_Access);
         Engine.Add_Filter (Var_Filter'Unchecked_Access);
         Engine.Parse (Input'Unchecked_Access, Doc);
         Var_Filter.Add_Variable (String '("file"), Wiki.Strings.To_WString (To_String (T.Name)));
         Util.Measures.Report (Time, "Parse " & To_String (T.Name));
         if T.Source = Wiki.SYNTAX_HTML or else T.Is_Cvt then
            declare
               Renderer    : aliased Wiki.Render.Wiki.Wiki_Renderer;
            begin
               Renderer.Set_Output_Stream (Output'Unchecked_Access, T.Format);
               Renderer.Render (Doc);
               Output.Close;
               Util.Measures.Report (Time, "Render Wiki " & To_String (T.Name));
            end;
         elsif T.Is_Html then
            declare
               Renderer : aliased Wiki.Render.Html.Html_Renderer;
            begin
               Renderer.Set_Output_Stream (Output'Unchecked_Access);
               Renderer.Set_Render_TOC (True);
               Renderer.Set_No_Newline (False);
               Renderer.Render (Doc);
               Output.Close;
               Util.Measures.Report (Time, "Render HTML " & To_String (T.Name));
            end;
         else
            declare
               Renderer : aliased Wiki.Render.Text.Text_Renderer;
            begin
               Renderer.Set_Output_Stream (Output'Unchecked_Access);
               Renderer.Render (Doc);
               Output.Close;
               Util.Measures.Report (Time, "Render Text " & To_String (T.Name));
            end;
         end if;
      end;
      Input.Close;
      Util.Tests.Assert_Equal_Files (T       => T,
                                     Expect  => To_String (T.Expect),
                                     Test    => Result_File,
                                     Message => "Render");

      if Has_Collector then
         declare
            procedure Print (Pos : in Wiki.Filters.Collectors.Cursor);

            Path : constant String := To_String (T.Collect);
            Dir  : constant String := Containing_Directory (Path);
            File : Ada.Wide_Wide_Text_IO.File_Type;

            procedure Print (Pos : in Wiki.Filters.Collectors.Cursor) is
               use Wiki.Filters.Collectors;

               Word : constant Wiki.Strings.WString := WString_Maps.Key (Pos);
               Count : constant Natural := WString_Maps.Element (Pos);
            begin
               Ada.Wide_Wide_Text_IO.Put (File, Word);
               Ada.Wide_Wide_Text_IO.Put_Line (File, Natural'Wide_Wide_Image (Count));
            end Print;

         begin
            if not Exists (Dir) then
               Create_Path (Dir);
            end if;
            Ada.Wide_Wide_Text_IO.Create (File, Ada.Wide_Wide_Text_IO.Out_File, Path);
            Ada.Wide_Wide_Text_IO.Put_Line (File, "== Words ==");
            Words.Iterate (Print'Access);
            Ada.Wide_Wide_Text_IO.Put_Line (File, "== Links ==");
            Links.Iterate (Print'Access);
            Ada.Wide_Wide_Text_IO.Put_Line (File, "== Images ==");
            Images.Iterate (Print'Access);
            Ada.Wide_Wide_Text_IO.Close (File);

            Util.Tests.Assert_Equal_Files (T       => T,
                                           Expect  => To_String (T.Expect_Collect),
                                           Test    => Path,
                                           Message => "Collect");
         end;
      end if;
   end Test_Render;

   --  ------------------------------
   --  Test case name
   --  ------------------------------
   overriding
   function Name (T : in Test) return Util.Tests.Message_String is
   begin
      if T.Source = Wiki.SYNTAX_HTML then
         return Util.Tests.Format ("Test IMPORT " & To_String (T.Name));
      elsif T.Is_Html then
         return Util.Tests.Format ("Test HTML " & To_String (T.Name));
      else
         return Util.Tests.Format ("Test TEXT " & To_String (T.Name));
      end if;
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
      use Ada.Directories;

      procedure Add_Import_Tests;
      procedure Add_Wiki_Tests;
      procedure Add_Convert_Tests;
      function Create_Test (Name    : in String;
                            Path    : in String;
                            Format  : in Wiki.Wiki_Syntax;
                            Prefix  : in String;
                            Collect : in String;
                            Is_Html : in Boolean) return Test_Case_Access;

      Result_Dir  : constant String := "";
      Expect_Dir  : constant String := "regtests/expect";
      Expect_Path : constant String := Util.Tests.Get_Path (Expect_Dir);
      Result_Path : constant String := Util.Tests.Get_Test_Path (Result_Dir);
      Search      : Search_Type;
      Filter      : constant Filter_Type := (others => True);
      Ent         : Directory_Entry_Type;

      function Create_Test (Name    : in String;
                            Path    : in String;
                            Format  : in Wiki.Wiki_Syntax;
                            Prefix  : in String;
                            Collect : in String;
                            Is_Html : in Boolean) return Test_Case_Access is
         Tst    : Test_Case_Access;
      begin
         Tst := new Test;
         Tst.Is_Html := Is_Html;
         Tst.Name    := To_Unbounded_String (Name);
         Tst.File    := To_Unbounded_String (Path);
         Tst.Expect  := To_Unbounded_String (Expect_Path & Prefix & Name);
         Tst.Result  := To_Unbounded_String (Result_Path & Prefix & Name);
         if Collect'Length > 0 then
            Tst.Collect := To_Unbounded_String (Result_Path & Collect & Name);
            Tst.Expect_Collect  := To_Unbounded_String (Expect_Path & Collect & Name);
         end if;
         Tst.Format  := Format;
         Tst.Source  := Format;
         return Tst;
      end Create_Test;

      procedure Add_Wiki_Tests is
         Dir         : constant String := "regtests/files/wiki";
         Path        : constant String := Util.Tests.Get_Path (Dir);
      begin
         if Kind (Path) /= Directory then
            Ada.Text_IO.Put_Line ("Cannot read test directory: " & Path);
         end if;

         Start_Search (Search, Directory => Path, Pattern => "*.*", Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Simple : constant String := Simple_Name (Ent);
               Ext    : constant String := Ada.Directories.Extension (Simple);
               Tst    : Test_Case_Access;
               Format : Wiki.Wiki_Syntax;
            begin
               if Simple /= "." and then Simple /= ".."
                 and then Simple /= ".svn" and then Simple (Simple'Last) /= '~'
               then
                  if Ext = "wiki" then
                     Format := Wiki.SYNTAX_GOOGLE;
                  elsif Ext = "dotclear" then
                     Format := Wiki.SYNTAX_DOTCLEAR;
                  elsif Ext = "creole" then
                     Format := Wiki.SYNTAX_CREOLE;
                  elsif Ext = "phpbb" then
                     Format := Wiki.SYNTAX_PHPBB;
                  elsif Ext = "mediawiki" then
                     Format := Wiki.SYNTAX_MEDIA_WIKI;
                  elsif Ext = "markdown" then
                     Format := Wiki.SYNTAX_MARKDOWN;
                  elsif Ext = "textile" then
                     Format := Wiki.SYNTAX_TEXTILE;
                  else
                     Format := Wiki.SYNTAX_MARKDOWN;
                  end if;

                  Tst := Create_Test (Simple, Path & "/" & Simple, Format, "/wiki-html/",
                                      "/wiki-collect/", True);
                  Suite.Add_Test (Tst.all'Access);

                  Tst := Create_Test (Simple, Path & "/" & Simple, Format, "/wiki-txt/",
                                      "", False);
                  Suite.Add_Test (Tst.all'Access);
               end if;
            end;
         end loop;
      end Add_Wiki_Tests;

      procedure Add_Import_Tests is
         Dir         : constant String := "regtests/files/html";
         Path        : constant String := Util.Tests.Get_Path (Dir);
      begin
         if Kind (Path) /= Directory then
            Ada.Text_IO.Put_Line ("Cannot read test directory: " & Path);
         end if;

         Start_Search (Search, Directory => Path, Pattern => "*.*", Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Simple : constant String := Simple_Name (Ent);
               Name   : constant String := Base_Name (Simple);
               Tst    : Test_Case_Access;
            begin
               if Simple /= "." and then Simple /= ".."
                 and then Simple /= ".svn" and then Simple (Simple'Last) /= '~'
               then
                  for Syntax in Wiki.Wiki_Syntax'Range loop
                     case Syntax is
                        when Wiki.SYNTAX_CREOLE =>
                           Tst := Create_Test (Name & ".creole", Path & "/" & Simple,
                                               Syntax, "/wiki-import/", "", True);

                        when Wiki.SYNTAX_DOTCLEAR =>
                           Tst := Create_Test (Name & ".dotclear", Path & "/" & Simple,
                                               Syntax, "/wiki-import/", "", True);

                        when Wiki.SYNTAX_MEDIA_WIKI =>
                           Tst := Create_Test (Name & ".mediawiki", Path & "/" & Simple,
                                               Syntax, "/wiki-import/", "", True);

                        when Wiki.SYNTAX_MARKDOWN =>
                           Tst := Create_Test (Name & ".markdown", Path & "/" & Simple,
                                               Syntax, "/wiki-import/", "", True);

                        when others =>
                           Tst := null;

                     end case;
                     if Tst /= null then
                        Tst.Source := Wiki.SYNTAX_HTML;
                        Suite.Add_Test (Tst.all'Access);
                     end if;
                  end loop;
               end if;
            end;
         end loop;
      end Add_Import_Tests;

      procedure Add_Convert_Tests is
         Dir         : constant String := "regtests/files/convert";
         Path        : constant String := Util.Tests.Get_Path (Dir);
      begin
         if Kind (Path) /= Directory then
            Ada.Text_IO.Put_Line ("Cannot read test directory: " & Path);
         end if;

         Start_Search (Search, Directory => Path, Pattern => "*.*", Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Simple : constant String := Simple_Name (Ent);
               Ext    : constant String := Ada.Directories.Extension (Simple);
               Name   : constant String := Base_Name (Simple);
               Tst    : Test_Case_Access;
               Format : Wiki.Wiki_Syntax;
            begin
               if Simple /= "." and then Simple /= ".."
                 and then Simple /= ".svn" and then Simple (Simple'Last) /= '~'
               then
                  if Ext = "wiki" then
                     Format := Wiki.SYNTAX_GOOGLE;
                  elsif Ext = "dotclear" then
                     Format := Wiki.SYNTAX_DOTCLEAR;
                  elsif Ext = "creole" then
                     Format := Wiki.SYNTAX_CREOLE;
                  elsif Ext = "phpbb" then
                     Format := Wiki.SYNTAX_PHPBB;
                  elsif Ext = "mediawiki" then
                     Format := Wiki.SYNTAX_MEDIA_WIKI;
                  elsif Ext = "markdown" then
                     Format := Wiki.SYNTAX_MARKDOWN;
                  else
                     Format := Wiki.SYNTAX_MARKDOWN;
                  end if;

                  for Syntax in Wiki.Wiki_Syntax'Range loop
                     case Syntax is
                        when Wiki.SYNTAX_CREOLE =>
                           Tst := Create_Test (Name & ".creole", Path & "/" & Simple,
                                               Syntax, "/wiki-convert/", "", True);

                        when Wiki.SYNTAX_DOTCLEAR =>
                           Tst := Create_Test (Name & ".dotclear", Path & "/" & Simple,
                                               Syntax, "/wiki-convert/", "", True);

                        when Wiki.SYNTAX_MEDIA_WIKI =>
                           Tst := Create_Test (Name & ".mediawiki", Path & "/" & Simple,
                                               Syntax, "/wiki-convert/", "", True);

                        when Wiki.SYNTAX_MARKDOWN =>
                           Tst := Create_Test (Name & ".markdown", Path & "/" & Simple,
                                               Syntax, "/wiki-convert/", "", True);

                        when Wiki.SYNTAX_TEXTILE =>
                           Tst := Create_Test (Name & ".textile", Path & "/" & Simple,
                                               Syntax, "/wiki-convert/", "", True);

                        when others =>
                           Tst := null;

                     end case;
                     if Tst /= null then
                        Tst.Is_Cvt := True;
                        Tst.Source := Format;
                        Suite.Add_Test (Tst.all'Access);
                     end if;
                  end loop;
               end if;
            end;
         end loop;
      end Add_Convert_Tests;

   begin
      Add_Wiki_Tests;
      Add_Import_Tests;
      Add_Convert_Tests;
   end Add_Tests;

end Wiki.Tests;
