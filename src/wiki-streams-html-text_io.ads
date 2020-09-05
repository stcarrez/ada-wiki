-----------------------------------------------------------------------
--  wiki-streams-html-text_io -- Wiki HTML output stream on Ada Text_IO
--  Copyright (C) 2016, 2020 Stephane Carrez
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
with Wiki.Streams.Text_IO;
with Wiki.Streams.Html.Stream;

package Wiki.Streams.Html.Text_IO is
   new Wiki.Streams.Html.Stream (Wiki.Streams.Text_IO.File_Output_Stream);
