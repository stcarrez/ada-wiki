-----------------------------------------------------------------------
--  wiki-parsers-mediawiki -- Media Wiki parser operations
--  Copyright (C) 2011- 2022 Stephane Carrez
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
with Wiki.Parsers.Html;
with Wiki.Parsers.Common;
private package Wiki.Parsers.MediaWiki is

   pragma Preelaborate;

   --  Parse an italic, bold or bold + italic sequence.
   --  Example:
   --    ''name''         (italic)
   --    '''name'''       (bold)
   --    '''''name'''''   (bold+italic)
   --  ------------------------------
   procedure Parse_Bold_Italic (P     : in out Parser;
                                Token : in Wiki.Strings.WChar);

   --  Parse a list definition:
   --    ;item 1
   --    : definition 1
   procedure Parse_Definition (P     : in out Parser;
                               Token : in Wiki.Strings.WChar);

   --  Parse a list definition:
   --    ;item 1
   --    : definition 1
   procedure Parse_Item (P     : in out Parser;
                         Token : in Wiki.Strings.WChar);

   --  Parse a horizontal rule.
   --  Example:
   --    ----
   procedure Parse_Horizontal_Rule (P     : in out Parser;
                                    Token : in Wiki.Strings.WChar);

   --  Parse a template parameter and expand it.
   --  Example:
   --    {{{1}}}                      MediaWiki
   procedure Parse_Parameter (P     : in out Parser;
                              Token : in Wiki.Strings.WChar);

   --  Parse a template with parameters.
   --  Example:
   --    {{Name|param|...}}           MediaWiki
   --    {{Name|param=value|...}}     MediaWiki
   --    <<Name param=value ...>>     Creole
   --    [{Name param=value ...}]     JSPWiki
   procedure Parse_Template (P     : in out Parser;
                             Token : in Wiki.Strings.WChar);

   Wiki_Table : aliased constant Parser_Table
     := (
         16#0A# => Parse_End_Line'Access,
         16#0D# => Parse_End_Line'Access,
         Character'Pos (' ') => Parse_Space'Access,
         Character'Pos ('=') => Common.Parse_Header'Access,
         Character'Pos (''') => Parse_Bold_Italic'Access,
         Character'Pos ('[') => Common.Parse_Link'Access,
         Character'Pos ('\') => Common.Parse_Line_Break'Access,
         Character'Pos ('#') => Common.Parse_List'Access,
         Character'Pos ('*') => Common.Parse_List'Access,
         Character'Pos ('<') => Parse_Maybe_Html'Access,
         Character'Pos ('&') => Html.Parse_Entity'Access,
         Character'Pos ('-') => Parse_Horizontal_Rule'Access,
         Character'Pos (';') => Parse_Item'Access,
         Character'Pos ('{') => Parse_Template'Access,
         Character'Pos (':') => Parse_Definition'Access,
         Character'Pos ('~') => Common.Parse_Escape'Access,
         others => Parse_Text'Access
        );

end Wiki.Parsers.MediaWiki;
