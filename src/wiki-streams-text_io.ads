-----------------------------------------------------------------------
--  wiki-streams-text_io -- Text_IO input output streams
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
with Ada.Wide_Wide_Text_IO;

--  === Text_IO Input and Output streams ===
--  The <tt>Wiki.Streams.Text_IO</tt> package defines the <tt>File_Input_Stream</tt> and
--  the <tt>File_Output_Stream</tt/ types which use the <tt>Ada.Wide_Wide_Text_IO</tt> package
--  to read or write the output streams.
--
--  By default the <tt>File_Input_Stream</tt> is configured to read the standard input.
--  The <tt>Open</tt> procedure can be used to read from a file knowing its name.
--
--  The <tt>File_Output_Stream</tt> is configured to write on the standard output.
--  The <tt>Open</tt> and <tt>Create</tt> procedure can be used to write on a file.
--
package Wiki.Streams.Text_IO is

   type File_Input_Stream is limited new Wiki.Streams.Input_Stream with private;
   type File_Input_Stream_Access is access all File_Input_Stream'Class;

   --  Open the file and prepare to read the input stream.
   procedure Open (Stream : in out File_Input_Stream;
                   Path   : in String;
                   Form   : in String := "");

   --  Read one character from the input stream and return False to the <tt>Eof</tt> indicator.
   --  When there is no character to read, return True in the <tt>Eof</tt> indicator.
   overriding
   procedure Read (Input : in out File_Input_Stream;
                   Char  : out Wiki.Strings.WChar;
                   Eof   : out Boolean);

   type File_Output_Stream is limited new Wiki.Streams.Output_Stream with private;
   type File_Output_Stream_Access is access all File_Output_Stream'Class;

   --  Open the file and prepare to write the output stream.
   procedure Open (Stream : in out File_Output_Stream;
                   Path   : in String;
                   Form   : in String := "");

   --  Close the file.
   procedure Close (Stream : in out File_Input_Stream);

   --  Create the file and prepare to write the output stream.
   procedure Create (Stream : in out File_Output_Stream;
                     Path   : in String;
                     Form   : in String := "");

   --  Write the string to the output stream.
   overriding
   procedure Write (Stream  : in out File_Output_Stream;
                    Content : in Wiki.Strings.WString);

   --  Write a single character to the output stream.
   overriding
   procedure Write (Stream : in out File_Output_Stream;
                    Char   : in Wiki.Strings.WChar);

private

   type File_Input_Stream is limited new Wiki.Streams.Input_Stream with record
      File : Ada.Wide_Wide_Text_IO.File_Type;
   end record;

   type File_Output_Stream is limited new Wiki.Streams.Output_Stream with record
      File : Ada.Wide_Wide_Text_IO.File_Type := Ada.Wide_Wide_Text_IO.Current_Output;
   end record;

end Wiki.Streams.Text_IO;
