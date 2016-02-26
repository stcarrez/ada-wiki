-----------------------------------------------------------------------
--  wiki-parsers -- Wiki parser
--  Copyright (C) 2011, 2015, 2016 Stephane Carrez
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

with Wiki.Attributes;
with Wiki.Plugins;
with Wiki.Filters;
with Wiki.Strings;
with Wiki.Documents;
with Wiki.Streams;

--  === Wiki Parsers ===
--  The <b>Wikis.Parsers</b> package implements a parser for several well known wiki formats
--  but also for HTML.  While reading the input, the parser populates a wiki <tt>Document</tt>
--  instance with headers, paragraphs, links, and other elements.
--
--     Doc    : Wiki.Documents.Document;
--     Engine : Wiki.Parsers.Parser;
--
--  Before using the parser, it must be configured to choose the syntax by using the
--  <tt>Set_Syntax</tt> procedure:
--
--    Engine.Set_Syntax (Wiki.SYNTAX_HTML);
--
--  The parser can be configured to use filters.  A filter is added by using the
--  <tt>Add_Filter</tt> procedure.  A filter is added at begining of the chain so that
--  the filter added last is called first.
--
--  The <tt>Parse</tt> procedure is then used to parse either a string content or some stream
--  represented by the <tt>Input_Stream</tt> interface.  After the <tt>Parse</tt> procedure
--  completes, the <tt>Document</tt> instance holds the wiki document.
--
--    Engine.Parse (Some_Text, Doc);
--
package Wiki.Parsers is

   pragma Preelaborate;

   type Parser is tagged limited private;

   --  Add the plugin to the wiki engine.
   procedure Add_Plugin (Engine : in out Parser;
                         Name   : in String;
                         Plugin : in Wiki.Plugins.Wiki_Plugin_Access);

   --  Set the wiki syntax that the wiki engine must use.
   procedure Set_Syntax (Engine : in out Parser;
                         Syntax : in Wiki_Syntax := SYNTAX_MIX);

   --  Add a filter in the wiki engine.
   procedure Add_Filter (Engine : in out Parser;
                         Filter : in Wiki.Filters.Filter_Type_Access);

   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  defined on the parser.
   procedure Parse (Engine : in out Parser;
                    Text   : in Wiki.Strings.WString;
                    Doc    : in out Wiki.Documents.Document);

   --  Parse the wiki stream managed by <tt>Stream</tt> according to the wiki syntax configured
   --  on the wiki engine.
   procedure Parse (Engine : in out Parser;
                    Stream : in Wiki.Streams.Input_Stream_Access;
                    Doc    : in out Wiki.Documents.Document);

private

   type Parser is tagged limited record
      Pending             : Wiki.Strings.WChar;
      Has_Pending         : Boolean;
      Syntax              : Wiki_Syntax;
      Document            : Wiki.Documents.Document;
      Filters             : Wiki.Filters.Filter_Chain;
      Format              : Wiki.Format_Map;
      Text                : Wiki.Strings.BString (512);
      Token               : Wiki.Strings.BString (512);
      Empty_Line          : Boolean := True;
      Is_Eof              : Boolean := False;
      In_Paragraph        : Boolean := False;
      In_List             : Boolean := False;
      Need_Paragraph      : Boolean := True;
      Link_Double_Bracket : Boolean := False;
      Is_Dotclear         : Boolean := False;
      Link_Title_First    : Boolean := False;
      Check_Image_Link    : Boolean := False;
      Header_Offset       : Integer := 0;
      Quote_Level         : Natural := 0;
      Escape_Char         : Wiki.Strings.WChar;
      List_Level          : Natural := 0;
      Reader              : Wiki.Streams.Input_Stream_Access := null;
      Attributes          : Wiki.Attributes.Attribute_List;
   end record;

   type Parser_Handler is access procedure (P     : in out Parser;
                                            Token : in Wiki.Strings.WChar);

   type Parser_Table is array (0 .. 127) of Parser_Handler;
   type Parser_Table_Access is access Parser_Table;

   --  Peek the next character from the wiki text buffer.
   procedure Peek (P     : in out Parser;
                   Token : out Wiki.Strings.WChar);

   --  Put back the character so that it will be returned by the next call to Peek.
   procedure Put_Back (P     : in out Parser;
                       Token : in Wiki.Strings.WChar);

   --  Skip all the spaces and tabs as well as end of the current line (CR+LF).
   procedure Skip_End_Of_Line (P : in out Parser);

   --  Skip white spaces and tabs.
   procedure Skip_Spaces (P : in out Parser);

   --  Flush the wiki text that was collected in the text buffer.
   procedure Flush_Text (P : in out Parser);

   --  Flush the wiki dl/dt/dd definition list.
   procedure Flush_List (P : in out Parser);

   --  Append a character to the wiki text buffer.
   procedure Parse_Text (P     : in out Parser;
                         Token : in Wiki.Strings.WChar);

   --  Check if the link refers to an image and must be rendered as an image.
   function Is_Image (P    : in Parser;
                      Link : in Wiki.Strings.WString) return Boolean;

   type String_Array is array (Positive range <>) of Wiki.String_Access;

   --  Extract a list of parameters separated by the given separator (ex: '|').
   procedure Parse_Parameters (P          : in out Parser;
                               Separator  : in Wiki.Strings.WChar;
                               Terminator : in Wiki.Strings.WChar;
                               Names      : in String_Array);

   procedure Start_Element (P          : in out Parser;
                            Tag        : in Wiki.Html_Tag;
                            Attributes : in out Wiki.Attributes.Attribute_List);

   procedure End_Element (P    : in out Parser;
                          Tag  : in Wiki.Html_Tag);

end Wiki.Parsers;
