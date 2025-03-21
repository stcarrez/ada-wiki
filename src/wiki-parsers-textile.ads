-----------------------------------------------------------------------
--  wiki-parsers-textile -- Textile parser operations
--  Copyright (C) 2022, 2024 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
private package Wiki.Parsers.Textile with Preelaborate is

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Cursor);

end Wiki.Parsers.Textile;
