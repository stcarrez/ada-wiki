-----------------------------------------------------------------------
--  wiki-utils -- Wiki utility operations
--  Copyright (C) 2015, 2016, 2020 Stephane Carrez
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
with Wiki.Parsers;
with Wiki.Render.Text;
with Wiki.Render.Html;
with Wiki.Filters.Html;
with Wiki.Filters.TOC;
with Wiki.Streams.Builders;
with Wiki.Streams.Html.Builders;
with Wiki.Documents;
package body Wiki.Utils is

   --  ------------------------------
   --  Render the wiki text according to the wiki syntax in HTML into a string.
   --  ------------------------------
   function To_Html (Text   : in Wiki.Strings.WString;
                     Syntax : in Wiki.Wiki_Syntax) return String is
      Stream   : aliased Wiki.Streams.Html.Builders.Html_Output_Stream;
      Renderer : aliased Wiki.Render.Html.Html_Renderer;
      Doc      : Wiki.Documents.Document;
      Filter   : aliased Wiki.Filters.Html.Html_Filter_Type;
      TOC      : aliased Wiki.Filters.TOC.TOC_Filter;
      Engine   : Wiki.Parsers.Parser;
   begin
      Renderer.Set_Output_Stream (Stream'Unchecked_Access);
      Renderer.Set_Render_TOC (True);
      Engine.Add_Filter (TOC'Unchecked_Access);
      Engine.Add_Filter (Filter'Unchecked_Access);
      Engine.Set_Syntax (Syntax);
      Engine.Parse (Text, Doc);
      Renderer.Render (Doc);
      return Stream.To_String;
   end To_Html;

   --  ------------------------------
   --  Render the wiki text according to the wiki syntax in text into a string.
   --  Wiki formatting and decoration are removed.
   --  ------------------------------
   function To_Text (Text   : in Wiki.Strings.WString;
                     Syntax : in Wiki.Wiki_Syntax) return String is
      Stream   : aliased Wiki.Streams.Builders.Output_Builder_Stream;
      Doc      : Wiki.Documents.Document;
      Renderer : aliased Wiki.Render.Text.Text_Renderer;
      Engine   : Wiki.Parsers.Parser;
   begin
      Renderer.Set_Output_Stream (Stream'Unchecked_Access);
      Engine.Set_Syntax (Syntax);
      Engine.Parse (Text, Doc);
      Renderer.Render (Doc);
      return Stream.To_String;
   end To_Text;

end Wiki.Utils;
