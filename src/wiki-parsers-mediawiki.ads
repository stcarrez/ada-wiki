-----------------------------------------------------------------------
--  wiki-parsers-mediawiki -- Media Wiki parser operations
--  Copyright (C) 2011- 2022 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------
private package Wiki.Parsers.MediaWiki with Preelaborate is

   procedure Parse_Line (Parser : in out Parser_Type;
                         Text   : in Wiki.Buffers.Buffer_Access);

end Wiki.Parsers.MediaWiki;
