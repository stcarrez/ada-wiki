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
with Util.Texts.Builders;

package Wiki.Strings is

   pragma Preelaborate;

   subtype WChar is Wide_Wide_Character;
   subtype WString is Wide_Wide_String;

   package Wide_Wide_Builders is new Util.Texts.Builders (Element_Type => WChar,
                                                          Input        => WString,
                                                          Chunk_Size   => 512);

   subtype BString is Wide_Wide_Builders.Builder;

end Wiki.Strings;
