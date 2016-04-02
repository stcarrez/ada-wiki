-----------------------------------------------------------------------
--  wiki-helpers-parser -- Generic procedure for the wiki parser
--  Copyright (C) 2016 Stephane Carrez
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
