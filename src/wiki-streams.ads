-----------------------------------------------------------------------
--  wiki-streams -- Wiki input and output streams
--  Copyright (C) 2011, 2012, 2013, 2015, 2016, 2020, 2022 Stephane Carrez
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

--  == Input and Output streams {#wiki-streams} ==
--  The `Wiki.Streams` package defines the interfaces used by
--  the parser or renderer to read and write their outputs.
--
--  The `Input_Stream` interface defines the interface that must be implemented to
--  read the source Wiki content.  The `Read` procedure is called by the parser
--  repeatedly while scanning the Wiki content.
--
--  The `Output_Stream` interface is the interface used by the renderer
--  to write their outputs.  It defines the `Write` procedure to write
--  a single character or a string.
--
--  @include wiki-streams-html.ads
--  @include wiki-streams-builders.ads
--  @include wiki-streams-html-builders.ads
--  @include wiki-streams-text_io.ads
--  @include wiki-streams-html-text_io.ads
package Wiki.Streams is

   pragma Preelaborate;

   type Input_Stream is limited interface;
   type Input_Stream_Access is access all Input_Stream'Class;

   --  Read the input stream and fill the `Into` buffer until either it is full or
   --  we reach the end of line.  Returns in `Last` the last valid position in the
   --  `Into` buffer.  When there is no character to read, return True in
   --  the `Eof` indicator.
   procedure Read (Input : in out Input_Stream;
                   Into  : in out Wiki.Strings.WString;
                   Last  : out Natural;
                   Eof   : out Boolean) is abstract;

   type Output_Stream is limited interface;
   type Output_Stream_Access is access all Output_Stream'Class;

   --  Write the string to the output stream.
   procedure Write (Stream  : in out Output_Stream;
                    Content : in Wiki.Strings.WString) is abstract;

   --  Write a single character to the output stream.
   procedure Write (Stream : in out Output_Stream;
                    Char   : in Wiki.Strings.WChar) is abstract;

end Wiki.Streams;
