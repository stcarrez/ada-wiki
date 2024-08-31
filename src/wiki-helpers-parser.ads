-----------------------------------------------------------------------
--  wiki-helpers-parser -- Generic procedure for the wiki parser
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Wiki.Strings;
with Wiki.Documents;
with Wiki.Streams;
generic
   type Engine_Type is limited private;
   type Element_Type (<>) is limited private;
   with procedure Element (Item : in Element_Type;
                           Pos  : in out Natural;
                           Char : out Wiki.Strings.WChar) is <>;
   with function Length (Item  : in Element_Type) return Natural is <>;
   with procedure Parse (Engine : in out Engine_Type;
                         Stream : in Wiki.Streams.Input_Stream_Access;
                         Doc    : in out Wiki.Documents.Document) is <>;
procedure Wiki.Helpers.Parser (Engine  : in out Engine_Type;
                               Content : in Element_Type;
                               Doc     : in out Wiki.Documents.Document);
pragma Preelaborate (Wiki.Helpers.Parser);
