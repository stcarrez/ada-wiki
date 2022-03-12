-----------------------------------------------------------------------
--  wiki-parsers-textile -- Textile parser operations
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
with Wiki.Parsers.Html;
with Wiki.Parsers.Common;
private package Wiki.Parsers.Textile is

   pragma Preelaborate;

   --  Parse a textile wiki heading in the form 'h<N>.'.
   --  Example:
   --    h1. Level 1
   --    h2. Level 2
   procedure Parse_Header (P     : in out Parser;
                           Token : in Wiki.Strings.WChar);

   --  Parse a textile image.
   --  Example:
   --    !image-link!
   --    !image-link(title)!
   --    !image-path|:http-link
   procedure Parse_Image (P     : in out Parser;
                          Token : in Wiki.Strings.WChar);

   --  Parse an external link:
   --  Example:
   --    "title":http-link
   procedure Parse_Link (P     : in out Parser;
                         Token : in Wiki.Strings.WChar);

   --  Parse an italic or bold sequence or a list.
   --  Example:
   --    *name*         (italic)
   --    **name**       (bold)
   --    * item         (list)
   procedure Parse_Bold_Or_List (P     : in out Parser;
                                 Token : in Wiki.Strings.WChar);

   --  Parse a markdown table/column.
   --  Example:
   --    | col1 | col2 | ... | colN |
   procedure Parse_Table (P     : in out Parser;
                          Token : in Wiki.Strings.WChar);

   procedure Parse_Deleted_Or_Horizontal_Rule (P     : in out Parser;
                                               Token : in Wiki.Strings.WChar);

   Wiki_Table : aliased constant Parser_Table
     := (
         16#0A# => Parse_End_Line'Access,
         16#0D# => Parse_End_Line'Access,
         Character'Pos (' ') => Parse_Space'Access,
         Character'Pos ('[') => Common.Parse_Link'Access,
         Character'Pos ('#') => Common.Parse_List'Access,
         Character'Pos ('*') => Parse_Bold_Or_List'Access,
         Character'Pos ('<') => Parse_Maybe_Html'Access,
         Character'Pos ('&') => Html.Parse_Entity'Access,
         Character'Pos ('-') => Parse_Deleted_Or_Horizontal_Rule'Access,
         Character'Pos ('!') => Parse_Image'Access,
         Character'Pos ('"') => Parse_Link'Access,
         Character'Pos ('@') => Common.Parse_Single_Code'Access,
         Character'Pos ('_') => Common.Parse_Single_Italic'Access,
         Character'Pos ('|') => Parse_Table'Access,
         Character'Pos ('h') => Parse_Header'Access,
         others => Parse_Text'Access
        );

end Wiki.Parsers.Textile;
