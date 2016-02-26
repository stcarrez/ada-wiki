-----------------------------------------------------------------------
--  wiki-writers-builders -- Wiki writer to a string builder
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

with GNAT.Encode_UTF8_String;
package body Wiki.Streams.Builders is

--   use Wiki.Strings;

   package WBS renames Wiki.Strings.Wide_Wide_Builders;

   --  ------------------------------
   --  Write the content to the string builder.
   --  ------------------------------
   overriding
   procedure Write (Stream  : in out Output_Builder_Stream;
                    Content : in Wiki.Strings.WString) is
   begin
      WBS.Append (Stream.Content, Content);
   end Write;

   --  ------------------------------
   --  Write the content to the string builder.
   --  ------------------------------
   overriding
   procedure Write (Stream  : in out Output_Builder_Stream;
                    Content : in Wiki.Strings.WChar) is
   begin
      WBS.Append (Stream.Content, Content);
   end Write;

   --  Write the string to the string builder.
   procedure Write_String (Stream  : in out Output_Builder_Stream;
                           Content : in String) is
   begin
      for I in Content'Range loop
         WBS.Append (Stream.Content, Wiki.Strings.To_WChar (Content (I)));
      end loop;
   end Write_String;

   --  ------------------------------
   --  Iterate over the buffer content calling the <tt>Process</tt> procedure with each
   --  chunk.
   --  ------------------------------
   procedure Iterate (Source  : in Output_Builder_Stream;
                      Process : not null access procedure (Chunk : in Wiki.Strings.WString)) is
   begin
      Strings.Wide_Wide_Builders.Iterate (Source.Content, Process);
   end Iterate;

   --  ------------------------------
   --  Convert what was collected in the writer builder to a string and return it.
   --  ------------------------------
   function To_String (Source : in Output_Builder_Stream) return String is
      procedure Convert (Chunk : in Wiki.Strings.WString);

      Pos    : Natural := 1;
      Result : String (1 .. 5 * Strings.Wide_Wide_Builders.Length (Source.Content));

      procedure Convert (Chunk : in Wiki.Strings.WString) is
      begin
         for I in Chunk'Range loop
            GNAT.Encode_UTF8_String.Encode_Wide_Wide_Character (Char   => Chunk (I),
                                                                Result => Result,
                                                                Ptr    => Pos);
         end loop;
      end Convert;

   begin
      Strings.Wide_Wide_Builders.Iterate (Source.Content, Convert'Access);
      return Result (1 .. Pos - 1);
   end To_String;

end Wiki.Streams.Builders;
