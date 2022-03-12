-----------------------------------------------------------------------
--  wiki-parsers-google -- Google Code parser operations
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
with Wiki.Parsers.Common;
private package Wiki.Parsers.Google is

   pragma Preelaborate;

   Wiki_Table : aliased constant Parser_Table
     := (
         16#0A# => Parse_End_Line'Access,
         16#0D# => Parse_End_Line'Access,
         Character'Pos (' ') => Parse_Space'Access,
         Character'Pos ('=') => Common.Parse_Header'Access,
         Character'Pos ('*') => Common.Parse_Single_Bold'Access,
         Character'Pos ('_') => Common.Parse_Single_Italic'Access,
         Character'Pos ('`') => Common.Parse_Single_Code'Access,
         Character'Pos ('^') => Common.Parse_Single_Superscript'Access,
         Character'Pos ('~') => Common.Parse_Double_Strikeout'Access,
         Character'Pos (',') => Common.Parse_Double_Subscript'Access,
         Character'Pos ('[') => Common.Parse_Link'Access,
         Character'Pos ('\') => Common.Parse_Line_Break'Access,
         Character'Pos ('#') => Common.Parse_List'Access,
         Character'Pos ('{') => Common.Parse_Preformatted'Access,
         Character'Pos ('}') => Common.Parse_Preformatted'Access,
         Character'Pos ('<') => Parse_Maybe_Html'Access,
         others => Parse_Text'Access
        );

end Wiki.Parsers.Google;
