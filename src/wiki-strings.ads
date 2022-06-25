-----------------------------------------------------------------------
--  wiki-strings -- Wiki string types and operations
--  Copyright (C) 2016, 2017, 2020, 2022 Stephane Carrez
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
with Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Maps;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Characters.Conversions;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Containers.Indefinite_Ordered_Maps;
with Util.Texts.Builders;

package Wiki.Strings is

   pragma Preelaborate;

   subtype WChar is Wide_Wide_Character;
   subtype WString is Wide_Wide_String;
   subtype UString is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
   subtype WChar_Mapping is Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Mapping;

   function To_WChar (C : in Character) return WChar
                      renames Ada.Characters.Conversions.To_Wide_Wide_Character;

   function To_Char (C : in WChar; Substitute : in Character := ' ') return Character
                     renames Ada.Characters.Conversions.To_Character;

   function To_String (S : in WString; Output_BOM : in Boolean := False) return String
                       renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode;

   function To_UString (S : in WString) return UString
                        renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   function To_WString (S : in UString) return WString
                        renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   function To_WString (S : in String) return WString
                        renames Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Decode;

   function Hash (S : in WString) return Ada.Containers.Hash_Type
     renames Ada.Strings.Wide_Wide_Hash;

   procedure Append (Into : in out UString; S : in WString)
                     renames Ada.Strings.Wide_Wide_Unbounded.Append;

   procedure Append (Into : in out UString; S : in WChar)
                     renames Ada.Strings.Wide_Wide_Unbounded.Append;

   function Length (S : in UString) return Natural
                    renames Ada.Strings.Wide_Wide_Unbounded.Length;

   function Element (S : in UString; Pos : in Positive) return WChar
                     renames Ada.Strings.Wide_Wide_Unbounded.Element;

   function Is_Alphanumeric (C : in WChar) return Boolean
     renames Ada.Wide_Wide_Characters.Handling.Is_Alphanumeric;

   function Index (S       : in WString;
                   P       : in WString;
                   Going   : in Ada.Strings.Direction := Ada.Strings.Forward;
                   Mapping : in WChar_Mapping := Ada.Strings.Wide_Wide_Maps.Identity)
     return Natural renames Ada.Strings.Wide_Wide_Fixed.Index;

   Null_UString : UString
   renames Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;

   package Wide_Wide_Builders is new Util.Texts.Builders (Element_Type => WChar,
                                                          Input        => WString,
                                                          Chunk_Size   => 512);

   subtype BString is Wide_Wide_Builders.Builder;

   function Length (Source : in BString) return Natural renames Wide_Wide_Builders.Length;

   function Element (Source   : in BString;
                     Position : in Positive) return WChar renames Wide_Wide_Builders.Element;

   function To_WString (Source : in BString) return WString renames Wide_Wide_Builders.To_Array;

   procedure Clear (Source   : in out BString) renames Wide_Wide_Builders.Clear;

   procedure Append_String (Source   : in out BString;
                            Content  : in WString) renames Wide_Wide_Builders.Append;

   procedure Append (Source   : in out BString;
                     Content  : in BString;
                     From     : in Positive;
                     To       : in Positive) renames Wide_Wide_Builders.Append;

   procedure Append_Char (Source   : in out BString;
                          Item     : in WChar) renames Wide_Wide_Builders.Append;

   package Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => WString,
                                                  Element_Type => WString);

   --  Search for the first occurrence of the character in the builder and
   --  starting after the from index.  Returns the index of the first occurence or 0.
   function Index (Source : in BString;
                   Char   : in WChar;
                   From   : in Positive := 1) return Natural;

   --  Find the last position of the character in the string and starting
   --  at the given position.  Stop at the first character different than `Char`.
   function Last_Position (Source : in BString;
                           Char   : in WChar;
                           From   : in Positive := 1) return Natural;

   --  Count the the number of consecutive occurence of the given character
   --  and starting at the given position.
   function Count_Occurence (Source : in BString;
                             Char   : in WChar;
                             From   : in Positive := 1) return Natural;
   function Count_Occurence (Source : in WString;
                             Char   : in WChar;
                             From   : in Positive := 1) return Natural;

   function Skip_Spaces (Source : in BString;
                         From   : in Positive;
                         Last   : in Positive) return Positive;

   procedure Scan_Line_Fragment (Source  : in BString;
                                 Process : not null
                                    access procedure (Text   : in WString;
                                                      Offset : in Natural));

end Wiki.Strings;
