-----------------------------------------------------------------------
--  wiki-utils -- Wiki utility operations
--  Copyright (C) 2015, 2016 Stephane Carrez
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
with Wiki.Strings;
package Wiki.Utils is

   --  Render the wiki text according to the wiki syntax in HTML into a string.
   function To_Html (Text   : in Wiki.Strings.WString;
                     Syntax : in Wiki.Wiki_Syntax) return String;

   --  Render the wiki text according to the wiki syntax in text into a string.
   --  Wiki formatting and decoration are removed.
   function To_Text (Text   : in Wiki.Strings.WString;
                     Syntax : in Wiki.Wiki_Syntax) return String;

end Wiki.Utils;
