-----------------------------------------------------------------------
--  wiki-parsers-markdown -- Markdown parser operations
--  Copyright (C) 2016 - 2022 Stephane Carrez
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
with Wiki.Parsers.Common;
private package Wiki.Parsers.Markdown is

   pragma Preelaborate;

   --  Parse a blockquote.
   --  Example:
   --    >>>quote level 3
   --    >>quote level 2
   --    >quote level 1
   procedure Parse_Blockquote (P     : in out Parser;
                               Token : in Wiki.Strings.WChar);

   --  Parse a markdown image.
   --  Example:
   --    ![title](link)
   procedure Parse_Markdown_Image (P     : in out Parser;
                                   Token : in Wiki.Strings.WChar);

   procedure Parse_Markdown_Link (P     : in out Parser;
                                  Token : in Wiki.Strings.WChar);

   procedure Parse_Markdown_Escape (P     : in out Parser;
                                    Token : in Wiki.Strings.WChar);

   --  Parse an italic or bold sequence or a list.
   --  Example:
   --    *name*         (italic)
   --    **name**       (bold)
   --    * item         (list)
   procedure Parse_Markdown_Bold_Italic (P     : in out Parser;
                                         Token : in Wiki.Strings.WChar);

   --  Parse a markdown table/column.
   --  Example:
   --    | col1 | col2 | ... | colN |
   procedure Parse_Table (P     : in out Parser;
                          Token : in Wiki.Strings.WChar);

   procedure Parse_Markdown_Horizontal_Rule (P     : in out Parser;
                                             Token : in Wiki.Strings.WChar);
   pragma Unreferenced (Parse_Markdown_Horizontal_Rule);

   procedure Parse_Single_Superscript is new Parse_Single_Format (SUPERSCRIPT);

   Wiki_Table : aliased constant Parser_Table
     := (
         16#0A# => Parse_End_Line'Access,
         16#0D# => Parse_End_Line'Access,
         Character'Pos (' ') => Parse_Space'Access,
         Character'Pos ('#') => Common.Parse_Header'Access,
         Character'Pos ('*') => Parse_Markdown_Bold_Italic'Access,
         Character'Pos ('_') => Parse_Markdown_Bold_Italic'Access,
         Character'Pos ('-') => Common.Parse_List'Access,
         Character'Pos ('^') => Common.Parse_Single_Superscript'Access,
         Character'Pos ('!') => Parse_Markdown_Image'Access,
         Character'Pos ('[') => Parse_Markdown_Link'Access,
         Character'Pos ('\') => Parse_Markdown_Escape'Access,
         Character'Pos ('>') => Parse_Blockquote'Access,
         Character'Pos ('<') => Parse_Maybe_Html'Access,
         Character'Pos ('`') => Common.Parse_Preformatted'Access,
         Character'Pos ('|') => Parse_Table'Access,
         others => Parse_Text'Access
        );

end Wiki.Parsers.Markdown;
