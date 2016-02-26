-----------------------------------------------------------------------
--  wiki-strings -- Wiki string types and operations
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
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Characters.Conversions;
with Util.Texts.Builders;

package Wiki.Strings is

   pragma Preelaborate;

   subtype WChar is Wide_Wide_Character;
   subtype WString is Wide_Wide_String;
   subtype UString is Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

   function To_WChar (C : in Character) return WChar
                      renames Ada.Characters.Conversions.To_Wide_Wide_Character;

   function To_Char (C : in WChar; Substitute : in Character := ' ') return Character
                     renames Ada.Characters.Conversions.To_Character;

   function To_String (S : in WString; Substitute : in Character := ' ') return String
                       renames Ada.Characters.Conversions.To_String;

   function To_UString (S : in WString) return UString
                        renames Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String;

   function To_WString (S : in UString) return WString
                        renames Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String;

   function To_WString (S : in String) return WString
                        renames Ada.Characters.Conversions.To_Wide_Wide_String;

   procedure Append (Into : in out UString; S : in WString)
                     renames Ada.Strings.Wide_Wide_Unbounded.Append;

   Null_UString : UString
   renames Ada.Strings.Wide_Wide_Unbounded.Null_Unbounded_Wide_Wide_String;

   package Wide_Wide_Builders is new Util.Texts.Builders (Element_Type => WChar,
                                                          Input        => WString,
                                                          Chunk_Size   => 512);

   subtype BString is Wide_Wide_Builders.Builder;

end Wiki.Strings;
