-----------------------------------------------------------------------
--  wiki-parsers-tests -- Unit tests for wiki parsing
--  Copyright (C) 2011, 2012, 2013, 2015 Stephane Carrez
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

with Wiki.Utils;
package body Wiki.Parsers.Tests is

   package Caller is new Util.Test_Caller (Test, "Wikis.Parsers");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (bold)",
                       Test_Wiki_Bold'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (italic)",
                       Test_Wiki_Italic'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (italic, bold)",
                       Test_Wiki_Formats'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (headings)",
                       Test_Wiki_Section'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (lists)",
                       Test_Wiki_List'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (links)",
                       Test_Wiki_Link'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (quote)",
                       Test_Wiki_Quote'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (line break)",
                       Test_Wiki_Line_Break'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (image)",
                       Test_Wiki_Image'Access);
      Caller.Add_Test (Suite, "Test Wiki.Parsers.Parse (preformatted)",
                       Test_Wiki_Preformatted'Access);
      Caller.Add_Test (Suite, "Test Wiki.Text.Renderer",
                       Test_Wiki_Text_Renderer'Access);
   end Add_Tests;

   --  ------------------------------
   --  Test bold rendering.
   --  ------------------------------
   procedure Test_Wiki_Bold (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p><b>bold</b></p>",
                                Wiki.Utils.To_Html ("*bold*", SYNTAX_GOOGLE),
                                "Bold rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <b>bold</b> y</p>",
                                Wiki.Utils.To_Html ("x *bold* y", SYNTAX_GOOGLE),
                                "Bold rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <b>bold y</b></p>",
                                Wiki.Utils.To_Html ("x *bold y", SYNTAX_MIX),
                                "Bold rendering invalid (MIX)");
      Util.Tests.Assert_Equals (T, "<p>x <b>item y</b> p</p>",
                                Wiki.Utils.To_Html ("x __item y__ p", SYNTAX_DOTCLEAR),
                                "Bold rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x _item y_ p</p>",
                                Wiki.Utils.To_Html ("x _item y_ p", SYNTAX_DOTCLEAR),
                                "No bold rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <b>bold</b> y</p>",
                                Wiki.Utils.To_Html ("x '''bold''' y", SYNTAX_PHPBB),
                                "Bold rendering invalid (PHPBB)");
   end Test_Wiki_Bold;

   --  ------------------------------
   --  Test italic rendering.
   --  ------------------------------
   procedure Test_Wiki_Italic (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p><i>item</i></p>",
                                Wiki.Utils.To_Html ("_item_", SYNTAX_GOOGLE),
                                "Italic rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <i>item</i> y</p>",
                                Wiki.Utils.To_Html ("x _item_ y", SYNTAX_GOOGLE),
                                "Italic rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <i>item y</i></p>",
                                Wiki.Utils.To_Html ("x _item y", SYNTAX_MIX),
                                "Italic rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <i>item y</i> p</p>",
                                Wiki.Utils.To_Html ("x ''item y'' p", SYNTAX_DOTCLEAR),
                                "Italic rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <i>item y</i> p</p>",
                                Wiki.Utils.To_Html ("x ''item y'' p", SYNTAX_PHPBB),
                                "Italic rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x 'item y<i> p</i></p>",
                                Wiki.Utils.To_Html ("x 'item y'' p", SYNTAX_PHPBB),
                                "Italic rendering invalid");
   end Test_Wiki_Italic;

   --  ------------------------------
   --  Test various format rendering.
   --  ------------------------------
   procedure Test_Wiki_Formats (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p><i>it</i><b><i>bold</i></b><i>em</i></p>",
                                Wiki.Utils.To_Html ("_it*bold*em_", SYNTAX_GOOGLE),
                                "Italic+Bold rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <i>item</i> y</p>",
                                Wiki.Utils.To_Html ("x _item_ y", SYNTAX_GOOGLE),
                                "Italic rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>x <i>item y</i></p>",
                                Wiki.Utils.To_Html ("x _item y", SYNTAX_GOOGLE),
                                "Italic rendering invalid");
   end Test_Wiki_Formats;

   --  ------------------------------
   --  Test heading rendering.
   --  ------------------------------
   procedure Test_Wiki_Section (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<h1>item</h1>",
                                Wiki.Utils.To_Html ("= item =", SYNTAX_GOOGLE),
                                "H1 rendering invalid");
      Util.Tests.Assert_Equals (T, "<h2>item</h2>",
                                Wiki.Utils.To_Html ("== item == ", SYNTAX_GOOGLE),
                                "H2 rendering invalid");
      Util.Tests.Assert_Equals (T, "<h3>item</h3>",
                                Wiki.Utils.To_Html ("=== item  ===  ", SYNTAX_GOOGLE),
                                "H3 rendering invalid");
      Util.Tests.Assert_Equals (T, "<h4>item</h4>",
                                Wiki.Utils.To_Html ("==== item ==== ", SYNTAX_GOOGLE),
                                "H4 rendering invalid");
      Util.Tests.Assert_Equals (T, "<h5>item</h5>",
                                Wiki.Utils.To_Html ("===== item =====", SYNTAX_GOOGLE),
                                "H5 rendering invalid");
      Util.Tests.Assert_Equals (T, "<h6>item</h6>",
                                Wiki.Utils.To_Html ("====== item ===", SYNTAX_GOOGLE),
                                "H6 rendering invalid");
      Util.Tests.Assert_Equals (T, "<h1>item</h1><h2>item2</h2>",
                                Wiki.Utils.To_Html ("= item =" & CR & "== item2 ==", SYNTAX_GOOGLE),
                                "H1 rendering invalid");
      Util.Tests.Assert_Equals (T, "<h1>item</h1><h2>item2</h2><h1>item3</h1>",
        Wiki.Utils.To_Html ("= item =" & CR & "== item2 ==" & CR & "= item3 =",
          SYNTAX_GOOGLE),
        "H1 rendering invalid");
   end Test_Wiki_Section;

   --  ------------------------------
   --  Test list rendering.
   --  ------------------------------
   procedure Test_Wiki_List (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<ol><li>item</li></ol>",
                                Wiki.Utils.To_Html ("# item", SYNTAX_GOOGLE),
                                "Ordered list rendering invalid");
      Util.Tests.Assert_Equals (T, "<ol><li>item item " & ASCII.LF &
                                "</li><li>item2 item2" & ASCII.LF &
                                "</li><li><ol>item3</li></ol></ol>",
        Wiki.Utils.To_Html ("# item item " & LF & "# item2 item2" & LF & "## item3",
         SYNTAX_GOOGLE),
        "Ordered rendering invalid");

      Util.Tests.Assert_Equals (T, "<ul><li>item</li></ul>",
                                Wiki.Utils.To_Html (" * item", SYNTAX_GOOGLE),
                                "Bullet list rendering invalid");

      Util.Tests.Assert_Equals (T, "<ul><li>item</li></ul>",
                                Wiki.Utils.To_Html ("* item", SYNTAX_DOTCLEAR),
                                "Bullet list rendering invalid");

   end Test_Wiki_List;

   --  ------------------------------
   --  Test link rendering.
   --  ------------------------------
   procedure Test_Wiki_Link (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p><a href=""name"">name</a></p>",
                                Wiki.Utils.To_Html ("[name]", SYNTAX_GOOGLE),
                                "Link rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><a title=""some"" lang=""en"" " &
                                "href=""http://www.joe.com/item"">name </a></p>",
                                Wiki.Utils.To_Html ("[name |http://www.joe.com/item|en|some]",
                                SYNTAX_DOTCLEAR),
                                "Link rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><a href=""name"">name</a></p>",
                                Wiki.Utils.To_Html ("[[name]]", SYNTAX_CREOLE),
                                "Link rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>[d</p>",
                                Wiki.Utils.To_Html ("[d", SYNTAX_CREOLE),
                                "No link rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><a " &
                                "href=""http://www.joe.com/item"">http://www.joe.com/item</a></p>",
                                Wiki.Utils.To_Html ("[http://www.joe.com/item]",
                                SYNTAX_DOTCLEAR),
                                "Link rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><a href=""name"">name</a></p>",
                                Wiki.Utils.To_Html ("[[name]]", SYNTAX_MEDIA_WIKI),
                                "Link rendering invalid");
   end Test_Wiki_Link;

   --  ------------------------------
   --  Test quote rendering.
   --  ------------------------------
   procedure Test_Wiki_Quote (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p><q>quote</q></p>",
                                Wiki.Utils.To_Html ("{{quote}}", SYNTAX_DOTCLEAR),
                                "Quote rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><q lang=""en"">quote</q></p>",
                                Wiki.Utils.To_Html ("{{quote|en}}", SYNTAX_DOTCLEAR),
                                "Quote rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><q lang=""en"" cite=""http://www.sun.com"">quote</q></p>",
                                Wiki.Utils.To_Html ("{{quote|en|http://www.sun.com}}",
                                                 SYNTAX_DOTCLEAR),
                                "Quote rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>{quote}}</p>",
                                Wiki.Utils.To_Html ("{quote}}", SYNTAX_DOTCLEAR),
                                "No quote rendering invalid");
   end Test_Wiki_Quote;

   --  ------------------------------
   --  Test line break rendering.
   --  ------------------------------
   procedure Test_Wiki_Line_Break (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p>a<br />b</p>",
                                Wiki.Utils.To_Html ("a%%%b", SYNTAX_DOTCLEAR),
                                "Line break rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>a<br />b</p>",
                                Wiki.Utils.To_Html ("a\\b", SYNTAX_CREOLE),
                                "Line break rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>a%%b</p>",
                                Wiki.Utils.To_Html ("a%%b", SYNTAX_DOTCLEAR),
                                "No line break rendering invalid");
      Util.Tests.Assert_Equals (T, "<p>a%b</p>",
                                Wiki.Utils.To_Html ("a%b", SYNTAX_DOTCLEAR),
                                "No line break rendering invalid");
   end Test_Wiki_Line_Break;

   --  ------------------------------
   --  Test image rendering.
   --  ------------------------------
   procedure Test_Wiki_Image (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p><img src=""/image/t.png""></img></p>",
                                Wiki.Utils.To_Html ("((/image/t.png))", SYNTAX_DOTCLEAR),
                                "Image rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><img alt=""title"" src=""/image/t.png""></img></p>",
                                Wiki.Utils.To_Html ("((/image/t.png|title))", SYNTAX_DOTCLEAR),
                                "Image rendering invalid");
      Util.Tests.Assert_Equals (T, "<p><img alt=""title"" longdesc=""describe"" " &
                                "src=""/image/t.png""></img></p>",
                                Wiki.Utils.To_Html ("((/image/t.png|title|D|describe))",
                                                 SYNTAX_DOTCLEAR),
                                "Image rendering invalid");
   end Test_Wiki_Image;

   --  ------------------------------
   --  Test preformatted rendering.
   --  ------------------------------
   procedure Test_Wiki_Preformatted (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, "<p><tt>code</tt></p>",
                                Wiki.Utils.To_Html ("{{{code}}}", SYNTAX_GOOGLE),
                                "Preformat rendering invalid");
      Util.Tests.Assert_Equals (T, "<pre>* code *" & ASCII.LF & "</pre>",
                                Wiki.Utils.To_Html ("///" & LF & "* code *" & LF & "///",
                                                 SYNTAX_DOTCLEAR),
                                "Preformat rendering invalid");
      Util.Tests.Assert_Equals (T, "<pre>item1 x" & ASCII.LF & "item2 x" & ASCII.LF & "item3 x"
                                & ASCII.LF & "</pre>",
                                Wiki.Utils.To_Html (" item1 x" & LF & " item2 x" & LF & " item3 x",
                                                 SYNTAX_DOTCLEAR),
                                "Preformat rendering invalid");
      Util.Tests.Assert_Equals (T, "<pre>item1 x" & ASCII.LF & "item2 x"
                                & ASCII.LF & "item3 x"
                                & ASCII.LF & "</pre>",
                                Wiki.Utils.To_Html (" item1 x" & CR & LF & " item2 x"
                                  & CR & LF & " item3 x",
                                                 SYNTAX_DOTCLEAR),
                                "Preformat rendering invalid");
   end Test_Wiki_Preformatted;

   --  ------------------------------
   --  Test the text renderer.
   --  ------------------------------
   procedure Test_Wiki_Text_Renderer (T : in out Test) is
   begin
      Util.Tests.Assert_Equals (T, ASCII.LF & "code",
                                Wiki.Utils.To_Text ("{{{code}}}", SYNTAX_GOOGLE),
                                "Preformat rendering invalid");
      Util.Tests.Assert_Equals (T, ASCII.LF & "bold item my_title" & ASCII.LF,
                                Wiki.Utils.To_Text ("_bold_ __item__ [my_title]", SYNTAX_GOOGLE),
                                "Preformat rendering invalid");

   end Test_Wiki_Text_Renderer;

end Wiki.Parsers.Tests;
