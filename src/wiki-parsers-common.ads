-----------------------------------------------------------------------
--  wiki-parsers-common -- Common operations with several wiki parsers
--  Copyright (C) 2011 - 2022 Stephane Carrez
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
private package Wiki.Parsers.Common is

   pragma Preelaborate;

   --  Escape a single character and append it to the wiki text buffer.
   procedure Parse_Escape (P     : in out Parser;
                           Token : in Wiki.Strings.WChar);

   --  Parse a wiki heading.  The heading could start with '=' or '!'.
   --  The trailing equals are ignored.
   --  Example:
   --    == Level 2 ==
   --    !!! Level 3
   procedure Parse_Header (P     : in out Parser;
                           Token : in Wiki.Strings.WChar);

   --  Parse a line break.
   --  Example:
   --     \\    (Creole)
   --     %%%   (Dotclear)
   procedure Parse_Line_Break (P     : in out Parser;
                               Token : in Wiki.Strings.WChar);

   procedure Parse_Preformatted (P     : in out Parser;
                                 Token : in Wiki.Strings.WChar);

   procedure Parse_Preformatted_Block (P     : in out Parser;
                                       Token : in Wiki.Strings.WChar);

   --  Parse a template parameter and expand it to the target buffer.
   --  Example:
   --    {{{1}}}                      MediaWiki
   --    <<<1>>>                      Creole extension
   procedure Expand_Parameter (P     : in out Parser;
                               Into  : in out Wiki.Strings.BString);

   --  Extract a list of parameters separated by the given separator (ex: '|').
   procedure Parse_Parameters (P          : in out Parser;
                               Separator  : in Wiki.Strings.WChar;
                               Terminator : in Wiki.Strings.WChar;
                               Names      : in String_Array;
                               Max        : in Positive := 200);

   --  Parse a link.
   --  Example:
   --    [name]
   --    [name|url]
   --    [name|url|language]
   --    [name|url|language|title]
   --    [[link]]
   --    [[link|name]]
   --  ------------------------------
   procedure Parse_Link (P     : in out Parser;
                         Token : in Wiki.Strings.WChar);

   procedure Parse_List (P     : in out Parser;
                         Token : in Wiki.Strings.WChar);

   procedure Parse_Single_Italic is new Parse_Single_Format (ITALIC);
   procedure Parse_Single_Bold is new Parse_Single_Format (BOLD);
   procedure Parse_Single_Code is new Parse_Single_Format (CODE);
   procedure Parse_Single_Superscript is new Parse_Single_Format (SUPERSCRIPT);
   --  procedure Parse_Single_Subscript is new Parse_Single_Format (SUBSCRIPT);
   --  procedure Parse_Single_Strikeout is new Parse_Single_Format (STRIKEOUT);

   procedure Parse_Double_Italic is new Parse_Double_Format (ITALIC);
   procedure Parse_Double_Bold is new Parse_Double_Format (BOLD);
   procedure Parse_Double_Code is new Parse_Double_Format (CODE);
   --  procedure Parse_Double_Superscript is new Parse_Double_Format (SUPERSCRIPT);
   procedure Parse_Double_Subscript is new Parse_Double_Format (SUBSCRIPT);
   procedure Parse_Double_Superscript is new Parse_Double_Format (SUPERSCRIPT);
   procedure Parse_Double_Strikeout is new Parse_Double_Format (STRIKEOUT);

end Wiki.Parsers.Common;
