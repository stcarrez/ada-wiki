-----------------------------------------------------------------------
--  wiki-streams-text_io -- Text_IO input output streams
--  Copyright (C) 2016, 2020, 2022 Stephane Carrez
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
with Ada.Wide_Wide_Text_IO;
with Ada.Finalization;

--  === Text_IO Input and Output streams ===
--  The `Wiki.Streams.Text_IO` package defines the `File_Input_Stream` and
--  the `File_Output_Stream` types which use the `Ada.Wide_Wide_Text_IO` package
--  to read or write the output streams.
--
--  By default the `File_Input_Stream` is configured to read the standard input.
--  The `Open` procedure can be used to read from a file knowing its name.
--
--  The `File_Output_Stream` is configured to write on the standard output.
--  The `Open` and `Create` procedure can be used to write on a file.
--
package Wiki.Streams.Text_IO is

   type File_Input_Stream is limited new Ada.Finalization.Limited_Controlled
     and Wiki.Streams.Input_Stream with private;
   type File_Input_Stream_Access is access all File_Input_Stream'Class;

   --  Open the file and prepare to read the input stream.
   procedure Open (Stream : in out File_Input_Stream;
                   Path   : in String;
                   Form   : in String := "");

   --  Read the input stream and fill the `Into` buffer until either it is full or
   --  we reach the end of line.  Returns in `Last` the last valid position in the
   --  `Into` buffer.  When there is no character to read, return True in
   --  the `Eof` indicator.
   overriding
   procedure Read (Input : in out File_Input_Stream;
                   Into  : in out Wiki.Strings.WString;
                   Last  : out Natural;
                   Eof   : out Boolean);

   --  Close the file.
   procedure Close (Stream : in out File_Input_Stream);

   --  Close the stream.
   overriding
   procedure Finalize (Stream : in out File_Input_Stream);

   type File_Output_Stream is limited new Ada.Finalization.Limited_Controlled
     and Wiki.Streams.Output_Stream with private;
   type File_Output_Stream_Access is access all File_Output_Stream'Class;

   --  Open the file and prepare to write the output stream.
   procedure Open (Stream : in out File_Output_Stream;
                   Path   : in String;
                   Form   : in String := "");

   --  Create the file and prepare to write the output stream.
   procedure Create (Stream : in out File_Output_Stream;
                     Path   : in String;
                     Form   : in String := "");

   --  Close the file.
   procedure Close (Stream : in out File_Output_Stream);

   --  Write the string to the output stream.
   overriding
   procedure Write (Stream  : in out File_Output_Stream;
                    Content : in Wiki.Strings.WString);

   --  Write a single character to the output stream.
   overriding
   procedure Write (Stream : in out File_Output_Stream;
                    Char   : in Wiki.Strings.WChar);

   --  Close the stream.
   overriding
   procedure Finalize (Stream : in out File_Output_Stream);

private

   type File_Input_Stream is limited new Ada.Finalization.Limited_Controlled
     and Wiki.Streams.Input_Stream with record
      File  : Ada.Wide_Wide_Text_IO.File_Type;
      Stdin : Boolean := True;
   end record;

   type File_Output_Stream is limited new Ada.Finalization.Limited_Controlled
     and Wiki.Streams.Output_Stream with record
      File   : Ada.Wide_Wide_Text_IO.File_Type;
      Stdout : Boolean := True;
   end record;

end Wiki.Streams.Text_IO;
