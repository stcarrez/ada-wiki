-----------------------------------------------------------------------
--  wiki-parsers-markdown -- Markdown parser operations
--  Copyright (C) 2016 - 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
private package Wiki.Parsers.Markdown with Preelaborate is

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor);

   procedure Parse_Inline_Text (Parser : in out Parser_Type;
                                Text   : in Wiki.Buffers.Cursor);

   function Get_Setext_Heading (Parser : in out Parser_Type;
                                Text   : in Wiki.Buffers.Cursor) return Natural;

end Wiki.Parsers.Markdown;
