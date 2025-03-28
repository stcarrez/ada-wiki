-----------------------------------------------------------------------
--  wiki-parsers-common -- Common operations with several wiki parsers
--  Copyright (C) 2011 - 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
private package Wiki.Parsers.Common with Preelaborate is

   --  Check if this is a list item composed of '*' and '#'
   --  and terminated by a space.
   function Is_List (Text : in Wiki.Buffers.Cursor) return Boolean;

   procedure Append (Into : in out Wiki.Strings.BString;
                     Text : in Wiki.Buffers.Cursor);

   procedure Parse_Token (Text        : in out Wiki.Buffers.Buffer_Access;
                          From        : in out Positive;
                          Escape_Char : in Wiki.Strings.WChar;
                          Marker1     : in Wiki.Strings.WChar;
                          Marker2     : in Wiki.Strings.WChar;
                          Into        : in out Wiki.Strings.BString);

   --  Parse a single text character and add it to the text buffer.
   procedure Parse_Text (Parser : in out Parser_Type;
                         Text   : in out Wiki.Buffers.Cursor;
                         Count  : in Positive := 1);

   procedure Parse_Paragraph (Parser : in out Parser_Type;
                              Text   : in out Wiki.Buffers.Cursor);

   procedure Parse_Horizontal_Rule (Parser : in out Parser_Type;
                                    Text   : in out Wiki.Buffers.Cursor;
                                    Marker : in Wiki.Strings.WChar);

   --  Parse a preformatted header block.
   --  Example:
   --    ///
   --    ///html
   --    ///[Ada]
   --    {{{
   --    ```
   procedure Parse_Preformatted (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Cursor;
                                 Marker : in Wiki.Strings.WChar;
                                 Keep_Block : in Boolean := False);

   --  Parse a wiki heading.  The heading could start with '=' or '!'.
   --  The trailing equals are ignored.
   --  Example:
   --    ==== Level 4       Creole
   --    == Level 2 ==      MediaWiki
   --    !!! Level 3        Dotclear
   procedure Parse_Header (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor;
                           Marker : in Wiki.Strings.WChar);

   --  Parse a template with parameters.
   --  Example:
   --    {{Name|param|...}}           MediaWiki
   --    {{Name|param=value|...}}     MediaWiki
   --    <<Name param=value ...>>     Creole
   --    [{Name param=value ...}]     JSPWiki
   procedure Parse_Template (Parser  : in out Parser_Type;
                             Text    : in out Wiki.Buffers.Cursor;
                             Token   : in Wiki.Strings.WChar);

   procedure Parse_Entity (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor;
                           Status : out Wiki.Html_Parser.Entity_State_Type;
                           Entity : out Wiki.Strings.WChar);

   procedure Parse_Entity (Parser : in out Parser_Type;
                           Text   : in out Wiki.Buffers.Cursor);

   --  Parse a quote.
   --  Example:
   --    {{name}}
   --    {{name|language}}
   --    {{name|language|url}}
   --    ??citation??
   procedure Parse_Quote (Parser  : in out Parser_Type;
                          Text    : in out Wiki.Buffers.Cursor;
                          Marker  : in Wiki.Strings.WChar);

   procedure Parse_Html_Element (Parser : in out Parser_Type;
                                 Text   : in out Wiki.Buffers.Cursor;
                                 Start  : in Boolean);

   procedure Parse_Html_Preformatted (Parser  : in out Parser_Type;
                                      Text    : in out Wiki.Buffers.Cursor);

   --  Parse a link.
   --  Example:
   --    [name]
   --    [name|url]
   --    [name|url|language]
   --    [name|url|language|title]
   --    [[link]]
   --    [[link|name]]
   procedure Parse_Link (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Cursor);

   procedure Parse_List (Parser  : in out Parser_Type;
                         Text    : in out Wiki.Buffers.Cursor);

   --  Parse a list definition that starts with ';':
   --    ;item 1
   --    : definition 1
   procedure Parse_Definition (Parser  : in out Parser_Type;
                               Text    : in out Wiki.Buffers.Cursor;
                               Is_Term : in Boolean);

end Wiki.Parsers.Common;
