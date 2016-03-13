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
with Ada.IO_Exceptions;

with Wiki.Helpers;
package body Wiki.Streams.Text_IO is

   --  ------------------------------
   --  Open the file and prepare to read the input stream.
   --  ------------------------------
   procedure Open (Stream : in out File_Input_Stream;
                   Path   : in String;
                   Form   : in String := "") is
   begin
      Ada.Wide_Wide_Text_IO.Open (Stream.File, Ada.Wide_Wide_Text_IO.In_File, Path, Form);
   end Open;

   --  ------------------------------
   --  Close the file.
   --  ------------------------------
   procedure Close (Stream : in out File_Input_Stream) is
   begin
      if Ada.Wide_Wide_Text_IO.Is_Open (Stream.File) then
         Ada.Wide_Wide_Text_IO.Close (Stream.File);
      end if;
   end Close;

   --  ------------------------------
   --  Close the stream.
   --  ------------------------------
   overriding
   procedure Finalize (Stream : in out File_Input_Stream) is
   begin
      Stream.Close;
   end Finalize;

   --  ------------------------------
   --  Read one character from the input stream and return False to the <tt>Eof</tt> indicator.
   --  When there is no character to read, return True in the <tt>Eof</tt> indicator.
   --  ------------------------------
   overriding
   procedure Read (Input : in out File_Input_Stream;
                   Char  : out Wiki.Strings.WChar;
                   Eof   : out Boolean) is
      Available : Boolean;
   begin
      Eof := False;
      Ada.Wide_Wide_Text_IO.Get_immediate (Input.File, Char, Available);

   exception
      when Ada.IO_Exceptions.End_Error =>
         Char := Wiki.Helpers.LF;
         Eof  := True;

   end Read;

   --  ------------------------------
   --  Open the file and prepare to write the output stream.
   --  ------------------------------
   procedure Open (Stream : in out File_Output_Stream;
                   Path   : in String;
                   Form   : in String := "") is
   begin
      if Ada.Wide_Wide_Text_IO.Is_Open (Stream.File) then
         Ada.Wide_Wide_Text_IO.Close (Stream.File);
      end if;
      Ada.Wide_Wide_Text_IO.Open (Stream.File, Ada.Wide_Wide_Text_IO.Out_File, Path, Form);
      Ada.Wide_Wide_Text_IO.Set_Line_Length (Ada.Wide_Wide_Text_IO.Count'Last);
      Stream.Stdout := False;
   end Open;

   --  ------------------------------
   --  Create the file and prepare to write the output stream.
   --  ------------------------------
   procedure Create (Stream : in out File_Output_Stream;
                     Path   : in String;
                     Form   : in String := "") is
   begin
      Ada.Wide_Wide_Text_IO.Create (Stream.File, Ada.Wide_Wide_Text_IO.Out_File, Path, Form);
      Ada.Wide_Wide_Text_IO.Set_Line_Length (Ada.Wide_Wide_Text_IO.Count'Last);
      Stream.Stdout := False;
   end Create;

   --  ------------------------------
   --  Close the file.
   --  ------------------------------
   procedure Close (Stream : in out File_Output_Stream) is
   begin
      if Ada.Wide_Wide_Text_IO.Is_Open (Stream.File) then
         Ada.Wide_Wide_Text_IO.Close (Stream.File);
      end if;
   end Close;

   --  ------------------------------
   --  Write the string to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream  : in out File_Output_Stream;
                    Content : in Wiki.Strings.WString) is
   begin
      if not Stream.Stdout then
         Ada.Wide_Wide_Text_IO.Put (Stream.File, Content);
      else
         Ada.Wide_Wide_Text_IO.Put (Content);
      end if;
   end Write;

   --  ------------------------------
   --  Write a single character to the output stream.
   --  ------------------------------
   overriding
   procedure Write (Stream : in out File_Output_Stream;
                    Char   : in Wiki.Strings.WChar) is
   begin
      if not Stream.Stdout then
         Ada.Wide_Wide_Text_IO.Put (Stream.File, Char);
      else
         Ada.Wide_Wide_Text_IO.Put (Char);
      end if;
   end Write;

   --  ------------------------------
   --  Close the stream.
   --  ------------------------------
   overriding
   procedure Finalize (Stream : in out File_Output_Stream) is
   begin
      Stream.Close;
   end Finalize;

end Wiki.Streams.Text_IO;
