-----------------------------------------------------------------------
--  wiki-parsers-html -- Wiki HTML parser
--  Copyright (C) 2015, 2016, 2022 Stephane Carrez
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

--  This is a small HTML parser that is called to deal with embedded HTML in wiki text.
--  The parser is intended to handle incorrect HTML and clean the result as much as possible.
--  We cannot use a real XML/Sax parser (such as XML/Ada) because we must handle errors and
--  backtrack from HTML parsing to wiki or raw text parsing.
--
--  When parsing HTML content, we call the <tt>Start_Element</tt> or <tt>End_Element</tt>
--  operations.  The renderer is then able to handle the HTML tag according to its needs.
private package Wiki.Parsers.Html with Preelaborate is

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access);

end Wiki.Parsers.Html;
