-----------------------------------------------------------------------
--  wiki-html_parser -- Wiki HTML parser
--  Copyright (C) 2015, 2016, 2022, 2023, 2025 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
with Wiki.Strings;
with Wiki.Attributes;

--  This is a small HTML parser that is called to deal with embedded HTML in wiki text.
--  The parser is intended to handle incorrect HTML and clean the result as much as possible.
--  We cannot use a real XML/Sax parser (such as XML/Ada) because we must handle errors and
--  backtrack from HTML parsing to wiki or raw text parsing.
--
--  When parsing HTML content, we call the `Parse_Element` and it calls back the `Process`
--  parameter procedure with the element name and the optional attributes.
private package Wiki.Html_Parser is

   pragma Preelaborate;

   MAX_ENTITY_LENGTH : constant := 32;

   subtype Length_Type is Natural range 0 .. MAX_ENTITY_LENGTH;

   type State_Type is (HTML_NONE, HTML_START, HTML_END, HTML_START_END, HTML_ERROR);

   type Entity_State_Type is (ENTITY_NONE, ENTITY_MIDDLE, ENTITY_VALID);

   type Html_Parser_State is (State_None,
                              State_Start,
                              State_Comment_Or_Doctype,
                              State_Doctype,
                              State_Comment,
                              State_Element,
                              State_Check_Attribute,
                              State_Parse_Attribute,
                              State_Check_Attribute_Value,
                              State_Parse_Attribute_Value,
                              State_Valid_Attribute_Value,
                              State_No_Attribute_Value,
                              State_Expect_Start_End_Element,
                              State_Expect_End_Element,
                              State_End_Element,
                              State_Error);

   type Parser_Type is limited record
      State       : Html_Parser_State := State_None;
      Elt_Name    : Wiki.Strings.BString (64);
      Attr_Name   : Wiki.Strings.BString (64);
      Attr_Value  : Wiki.Strings.BString (256);
      Entity_Name : String (1 .. MAX_ENTITY_LENGTH);
      Attributes  : Wiki.Attributes.Attribute_List;
      Counter     : Length_Type := 0;
      Separator   : Wiki.Strings.WChar;
   end record;

   function Is_Empty (Parser : in Parser_Type) return Boolean;

   --  Parse a HTML element `<XXX attributes>` or parse an end of HTML element </XXX>
   --  The first '<' is assumed to have been already verified.  When the HTML element
   --  is scanned, the `Result` value will indicate the value that was found.
   procedure Parse_Element (Parser  : in out Parser_Type;
                            Text    : in Wiki.Strings.WString;
                            From    : in Positive;
                            Result  : out State_Type;
                            Last    : out Positive);

   --  Parse an HTML entity such as `&nbsp;` and return its value with the last position
   --  if it was correct.  The first `&` is assumed to have been already verified.
   --  When `Entity` is not 0, the parsing is finished.  When `Last` exceeds the input
   --  text position, it is necessary to call `Parse_Entity` with the next input chunk.
   procedure Parse_Entity (Parser  : in out Parser_Type;
                           Text    : in Wiki.Strings.WString;
                           From    : in Positive;
                           Status  : in out Entity_State_Type;
                           Entity  : out Wiki.Strings.WChar;
                           Last    : out Natural);

   --  Copy the string represented by `From` in `Into` and replace the HTML
   --  entities when they are found and valid.  Return in `Last` the index
   --  of the last valid character in `Into`.
   procedure Replace_Entities (Parser : in out Parser_Type;
                               From   : in Wiki.Strings.WString;
                               Into   : in out Wiki.Strings.WString;
                               Last   : out Natural);

   NUL : constant Wiki.Strings.WChar := Wiki.Strings.WChar'Val (0);

end Wiki.Html_Parser;
