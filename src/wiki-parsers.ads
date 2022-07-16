-----------------------------------------------------------------------
--  wiki-parsers -- Wiki parser
--  Copyright (C) 2011, 2015, 2016, 2018, 2020, 2022 Stephane Carrez
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
private with Wiki.Buffers;
private with Util.Stacks;
private with Wiki.Nodes;
private with Wiki.Html_Parser;

--  == Wiki Parsers {#wiki-parsers} ==
--  The `Wikis.Parsers` package implements a parser for several well known wiki formats
--  but also for HTML.  While reading the input, the parser populates a wiki `Document`
--  instance with headers, paragraphs, links, and other elements.
--
--     Engine : Wiki.Parsers.Parser;
--
--  Before using the parser, it must be configured to choose the syntax by using the
--  `Set_Syntax` procedure:
--
--    Engine.Set_Syntax (Wiki.SYNTAX_HTML);
--
--  The parser can be configured to use filters.  A filter is added by using the
--  `Add_Filter` procedure.  A filter is added at begining of the chain so that
--  the filter added last is called first.  The wiki `Document` is always built through
--  the filter chain so this allows filters to change or alter the content that was parsed.
--
--    Engine.Add_Filter (TOC'Unchecked_Access);
--    Engine.Add_Filter (Filter'Unchecked_Access);
--
--  The `Parse` procedure is then used to parse either a string content or some stream
--  represented by the `Input_Stream` interface.  After the `Parse` procedure
--  completes, the `Document` instance holds the wiki document.
--
--    Engine.Parse (Some_Text, Doc);
--
package Wiki.Parsers is

   pragma Preelaborate;

   type Parser is tagged limited private;
   subtype Parser_Type is Parser;

   --  Set the plugin factory to find and use plugins.
   procedure Set_Plugin_Factory (Engine  : in out Parser;
                                 Factory : in Wiki.Plugins.Plugin_Factory_Access);

   --  Set the wiki syntax that the wiki engine must use.
   procedure Set_Syntax (Engine : in out Parser;
                         Syntax : in Wiki_Syntax := SYNTAX_MARKDOWN);

   --  Add a filter in the wiki engine.
   procedure Add_Filter (Engine : in out Parser;
                         Filter : in Wiki.Filters.Filter_Type_Access);

   --  Set the plugin context.
   procedure Set_Context (Engine  : in out Parser;
                          Context : in Wiki.Plugins.Plugin_Context);

   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  defined on the parser.  The string is assumed to be in UTF-8 format.
   procedure Parse (Engine : in out Parser;
                    Text   : in String;
                    Doc    : in out Wiki.Documents.Document);

   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  defined on the parser.
   procedure Parse (Engine : in out Parser;
                    Text   : in Wiki.Strings.WString;
                    Doc    : in out Wiki.Documents.Document);

   --  Parse the wiki text contained in <b>Text</b> according to the wiki syntax
   --  defined on the parser.
   procedure Parse (Engine : in out Parser;
                    Text   : in Wiki.Strings.UString;
                    Doc    : in out Wiki.Documents.Document);

   --  Parse the wiki stream managed by <tt>Stream</tt> according to the wiki syntax configured
   --  on the wiki engine.
   procedure Parse (Engine : in out Parser;
                    Stream : in Wiki.Streams.Input_Stream_Access;
                    Doc    : in out Wiki.Documents.Document);

private

   type Trim_End is (None, Left, Right, Both);

   use Wiki.Strings.Wide_Wide_Builders;

   type Block_Type is record
      Kind   : Wiki.Nodes.Node_Kind := Wiki.Nodes.N_PARAGRAPH;
      Level  : Natural := 0;
      Marker : Wiki.Strings.WChar := ' ';
      Number : Integer := 0;
      Quote_Level : Natural := 0;
   end record;

   type Parser_State_Type is (State_Html_Doctype,
                              State_Html_Comment,
                              State_Html_Attribute,
                              State_Html_Element);

   type Block_Access is access all Block_Type;

   package Block_Stack is new Util.Stacks (Element_Type        => Block_Type,
                                           Element_Type_Access => Block_Access);

   type Parser_Handler is access procedure (Parser : in out Parser_Type;
                                            Text   : in Wiki.Buffers.Buffer_Access);

   type Parser is tagged limited record
      Context             : aliased Wiki.Plugins.Plugin_Context;
      Previous_Syntax     : Wiki_Syntax;
      Document            : Wiki.Documents.Document;
      Parse_Block         : Parser_Handler;
      Parse_Inline        : Parser_Handler;
      Format              : Wiki.Format_Map;
      Text                : Wiki.Strings.BString (512);
      Line_Buffer         : Wiki.Buffers.Builder (512);
      Text_Buffer         : Wiki.Buffers.Builder (512);
      Empty_Line          : Boolean := True;
      Is_Last_Line        : Boolean := False;
      Is_Eof              : Boolean := False;
      In_Paragraph        : Boolean := False;
      In_Table            : Boolean := False;
      In_Html             : Boolean := False;
      In_Blockquote       : Boolean := False;
      Link_Double_Bracket : Boolean := False;
      Link_No_Space       : Boolean := False;
      Link_Title_First    : Boolean := False;
      Check_Image_Link    : Boolean := False;
      Header_Offset       : Integer := 0;
      Preformat_Column    : Natural := 1;
      Quote_Level         : Natural := 0;
      Escape_Char         : Wiki.Strings.WChar;
      Param_Char          : Wiki.Strings.WChar;
      List_Level          : Natural := 0;
      Previous_Tag        : Html_Tag := UNKNOWN_TAG;
      Reader              : Wiki.Streams.Input_Stream_Access := null;
      Attributes          : Wiki.Attributes.Attribute_List;
      Current_Node        : Wiki.Nodes.Node_Kind := Wiki.Nodes.N_NONE;
      Blocks              : Block_Stack.Stack;
      Previous_Line_Empty : Boolean := False;
      Header_Level        : Natural := 0;
      Is_Empty_Paragraph  : Boolean := True;
      Pre_Tag_Counter     : Natural := 0;

      --  Pre-format code block
      Preformat_Fence     : Wiki.Strings.WChar;
      Preformat_Indent    : Natural := 0;
      Preformat_Fcount    : Natural := 0;
      Preformat_Format    : Wiki.Strings.BString (32);

      Html                : Wiki.Html_Parser.Parser_Type;
   end record;

   --  Read the next wiki input line in the line buffer.
   procedure Read_Line (Parser : in out Parser_Type'Class;
                        Buffer : out Wiki.Buffers.Buffer_Access);

   function Is_List_Item (P     : in Parser;
                          Level : in Natural) return Boolean;

   function Is_Single_Token (Text : in Wiki.Buffers.Buffer_Access;
                             From : in Positive;
                             Token : in Wiki.Strings.WChar) return Boolean;

   --  Flush the wiki text that was collected in the text buffer.
   procedure Flush_Text (P    : in out Parser;
                         Trim : in Trim_End := None);

   procedure Flush_Block (Parser : in out Parser_Type;
                          Trim   : in Trim_End := None);

   --  Flush the wiki dl/dt/dd definition list.
   procedure Flush_List (P : in out Parser);
   procedure Pop_List (P      : in out Parser;
                       Level  : in Natural;
                       Marker : in Wiki.Strings.WChar;
                       Number : in Natural);
   procedure Pop_List (P      : in out Parser);

   function Get_Current_Level (Parser : in Parser_Type) return Natural;

   --  Check if the link refers to an image and must be rendered as an image.
   --  Returns a positive index of the start the the image link.
   function Is_Image (P    : in Parser;
                      Link : in Wiki.Strings.WString) return Natural;

   --  Returns true if we are included from another wiki content.
   function Is_Included (P : in Parser) return Boolean;

   --  Find the plugin with the given name.
   --  Returns null if there is no such plugin.
   function Find (P    : in Parser;
                  Name : in Wiki.Strings.WString) return Wiki.Plugins.Wiki_Plugin_Access;

   type String_Array is array (Positive range <>) of Wiki.String_Access;

   procedure Process_Html (P          : in out Parser;
                           Kind       : in Wiki.Html_Parser.State_Type;
                           Name       : in Wiki.Strings.WString;
                           Attributes : in out Wiki.Attributes.Attribute_List);

   procedure Start_Element (P          : in out Parser;
                            Tag        : in Wiki.Html_Tag;
                            Attributes : in out Wiki.Attributes.Attribute_List);

   procedure End_Element (P    : in out Parser;
                          Tag  : in Wiki.Html_Tag);

   procedure Toggle_Format (P      : in out Parser;
                            Format : in Format_Type);

   --  Parse the beginning or the end of a single character sequence.
   --  Example:
   --    _name_    *bold*   `code`
   procedure Parse_Format (P      : in out Parser;
                           Text   : in out Wiki.Buffers.Buffer_Access;
                           From   : in out Positive;
                           Expect : in Wiki.Strings.WChar;
                           Format : in Format_Type);

   --  Parse the beginning or the end of a double character sequence.
   --  Example:
   --    --name--  **bold** ~~strike~~
   procedure Parse_Format_Double (P      : in out Parser;
                                  Text   : in out Wiki.Buffers.Buffer_Access;
                                  From   : in out Positive;
                                  Expect : in Wiki.Strings.WChar;
                                  Format : in Format_Type);

   --  Push a new block kind on the block stack.
   procedure Push_Block (P      : in out Parser;
                         Kind   : in Wiki.Nodes.Node_Kind;
                         Level  : in Integer := 0;
                         Marker : in Wiki.Strings.WChar := ' ';
                         Number : in Integer := 0);

   --  Pop the current block stack.
   procedure Pop_Block (Parser  : in out Parser_Type;
                        Trim    : in Trim_End := None);
   procedure Pop_All (P : in out Parser);

   procedure Pop_Block_Until (P     : in out Parser;
                              Kind  : in Wiki.Nodes.Node_Kind;
                              Level : in Integer);

   --  Flush current block and add an horizontal rule in the document.
   procedure Add_Horizontal_Rule (Parser : in out Parser_Type);

   NAME_ATTR  : aliased constant String := "name";
   HREF_ATTR  : aliased constant String := "href";
   LANG_ATTR  : aliased constant String := "lang";
   TITLE_ATTR : aliased constant String := "title";

end Wiki.Parsers;
