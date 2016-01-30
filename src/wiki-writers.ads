-----------------------------------------------------------------------
--  wiki-writers -- Wiki writers
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
with Ada.Strings.Wide_Wide_Unbounded;

--  == Writer interfaces ==
--  The <tt>Wiki.Writers</tt> package defines the interfaces used by the renderer to write
--  their outputs.
--
--  The <tt>Input_Stream</tt> interface defines the interface that must be implemented to
--  read the source Wiki content.  The <tt>Read</tt> procedure is called by the parser
--  repeatedly while scanning the Wiki content.
package Wiki.Writers is

   use Ada.Strings.Wide_Wide_Unbounded;

   type Input_Stream is limited interface;

   --  Read one character from the input stream and return False to the <tt>Eof</tt> indicator.
   --  When there is no character to read, return True in the <tt>Eof</tt> indicator.
   procedure Read (Input : in out Input_Stream;
                   Char  : out Wide_Wide_Character;
                   Eof   : out Boolean) is abstract;

   type Writer_Type is limited interface;
   type Writer_Type_Access is access all Writer_Type'Class;

   procedure Write (Writer  : in out Writer_Type;
                    Content : in Wide_Wide_String) is abstract;

   --  Write a single character to the string builder.
   procedure Write (Writer : in out Writer_Type;
                    Char   : in Wide_Wide_Character) is abstract;

   procedure Write (Writer  : in out Writer_Type;
                    Content : in Unbounded_Wide_Wide_String) is abstract;

   type Html_Writer_Type is limited interface and Writer_Type;
   type Html_Writer_Type_Access is access all Html_Writer_Type'Class;

   --  Write an XML element using the given name and with the content.
   --  This is similar to calling <b>Start_Element</b>, <b>Write_Text</b>
   --  and <b>End_Element</b>.
   procedure Write_Wide_Element (Writer  : in out Html_Writer_Type;
                                 Name    : in String;
                                 Content : in Unbounded_Wide_Wide_String) is abstract;

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Unbounded_Wide_Wide_String) is abstract;

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Wide_Attribute (Writer  : in out Html_Writer_Type;
                                   Name    : in String;
                                   Content : in Wide_Wide_String) is abstract;

   --  Start an XML element with the given name.
   procedure Start_Element (Writer : in out Html_Writer_Type;
                            Name   : in String) is abstract;

   --  Closes an XML element of the given name.
   procedure End_Element (Writer : in out Html_Writer_Type;
                          Name   : in String) is abstract;

   --  Write a text escaping any character as necessary.
   procedure Write_Wide_Text (Writer  : in out Html_Writer_Type;
                              Content : in Unbounded_Wide_Wide_String) is abstract;

   --  Write an XML attribute within an XML element.
   --  The attribute value is escaped according to the XML escape rules.
   procedure Write_Attribute (Writer  : in out Html_Writer_Type'Class;
                              Name    : in String;
                              Content : in String);

end Wiki.Writers;
