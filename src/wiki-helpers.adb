-----------------------------------------------------------------------
--  wiki-helpers -- Helper operations for wiki parsers and renderer
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
with Ada.Wide_Wide_Characters.Handling;

package body Wiki.Helpers is

   --  ------------------------------
   --  Returns True if the character is a space or tab.
   --  ------------------------------
   function Is_Space (C : in Wide_Wide_Character) return Boolean is
   begin
      return Ada.Wide_Wide_Characters.Handling.Is_Space (C) or C = HT;
   end Is_Space;

   --  ------------------------------
   --  Returns True if the character is a space, tab or a newline.
   --  ------------------------------
   function Is_Space_Or_Newline (C : in Wide_Wide_Character) return Boolean is
   begin
      return Is_Space (C) or Ada.Wide_Wide_Characters.Handling.Is_Line_Terminator (C);
   end Is_Space_Or_Newline;

end Wiki.Helpers;
