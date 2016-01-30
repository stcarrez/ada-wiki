-----------------------------------------------------------------------
--  wiki-streams -- Wiki input and output streams
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

--  == Writer interfaces ==
--  The <tt>Wiki.Writers</tt> package defines the interfaces used by the renderer to write
--  their outputs.
--
--  The <tt>Input_Stream</tt> interface defines the interface that must be implemented to
--  read the source Wiki content.  The <tt>Read</tt> procedure is called by the parser
--  repeatedly while scanning the Wiki content.
package Wiki.Streams is

   type Input_Stream is limited interface;

   --  Read one character from the input stream and return False to the <tt>Eof</tt> indicator.
   --  When there is no character to read, return True in the <tt>Eof</tt> indicator.
   procedure Read (Input : in out Input_Stream;
                   Char  : out Wide_Wide_Character;
                   Eof   : out Boolean) is abstract;

   type Output_Stream is limited interface;
   type Output_Stream_Type_Access is access all Output_Stream'Class;

   procedure Write (Stream  : in out Output_Stream;
                    Content : in Wide_Wide_String) is abstract;

   --  Write a single character to the string builder.
   procedure Write (Stream : in out Output_Stream;
                    Char   : in Wide_Wide_Character) is abstract;

end Wiki.Streams;
