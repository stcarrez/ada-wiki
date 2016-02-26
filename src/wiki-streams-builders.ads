-----------------------------------------------------------------------
--  wiki-streams-builders -- Wiki writer to a string builder
--  Copyright (C) 2011, 2012, 2013, 2015, 2016 Stephane Carrez
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

--  === Output Builder Stream ==
--  The <tt>Output_Builder_Stream</tt> is a concrete in-memory output stream.
--  It collects the output in a <tt>Wiki.Strings.Bstring</tt> object and the
--  content can be retrieved at the end by using the <tt>To_String</tt>
--  or <tt>Iterate</tt> operation.
package Wiki.Streams.Builders is

   type Output_Builder_Stream is limited new Output_Stream with private;
   type Output_Builder_Stream_Access is access all Output_Builder_Stream'Class;

   --  Write the content to the string builder.
   overriding
   procedure Write (Stream  : in out Output_Builder_Stream;
                    Content : in Wiki.Strings.WString);

   --  Write a single character to the string builder.
   overriding
   procedure Write (Stream  : in out Output_Builder_Stream;
                    Content : in Wiki.Strings.WChar);

   --  Write the string to the string builder.
   procedure Write_String (Stream  : in out Output_Builder_Stream;
                           Content : in String);

   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   procedure Iterate (Source  : in Output_Builder_Stream;
                      Process : not null access procedure (Chunk : in Wiki.Strings.WString));

   --  Convert what was collected in the writer builder to a string and return it.
   function To_String (Source : in Output_Builder_Stream) return String;

private

   BLOCK_SIZE : constant Positive := 512;

   type Output_Builder_Stream is limited new Output_Stream with record
      Content : Wiki.Strings.BString (BLOCK_SIZE);
   end record;

end Wiki.Streams.Builders;
