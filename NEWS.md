Version 1.5.0   - Under development
  - Feature #12: Improvement of HTML renderer
  - Feature #10: Text renderer improvement
  - Fix #11: Newline in link titles should be ignored and replaced by space
  - Fix parsing MediaWiki definition lists
  - Many fixes in the Markdown parser to better follow [CommonMark Spec](https://spec.commonmark.org/)

Version 1.4.3   - Nov 2024
  - Feature #7: Add a style on HTML table
  - Feature #8: Add support for strikeout in markdown

Version 1.4.2   - Aug 2024
  - Cleanup build environment to drop configure

Version 1.4.1   - Jul 2023
  - Fix parsing some HTML document when we reach end of line buffer
  - Fix \<pre\> HTML blocks to keep CR in order to preserve the end of lines
  - Fix releasing memory for some document nodes

Version 1.4.0   - Aug 2022
  - Add support for Textile markup language
  - Rewrote the Markdown parser to better follow the Common Mark Specification

Version 1.3.2   - Jul 2021
  - Fix \<hr\> and \<br\> generation to follow HTML5 convention.
  - Add option -H to the render example
  - Fix for GNAT 2021

Version 1.3.1   - Feb 2021
  - Minor cleanup for the build

Version 1.3.0   - Nov 2020
  - New plugin and filter to setup and expand variables in Wiki texts
  - Add support for Markdown tables
  - Fixes in the Markdown and Creole syntax parsers
  - New Ada Wiki Engine Programmer's Guide

Version 1.2.1   - May 2020
  - Minor configuration and code coverage support
  - Corrections in the Markdown syntax parser

Version 1.2.0   - Dec 2019
  - Rename GNAT project into wikiada

Version 1.1.0   - Jul 2018
  - New condition plugins for the conditional inclusion of wiki content
  - Added support for __NOTOC__ by the TOC filter

Version 1.0.1   - Apr 2016
  - Bug fix in Autolink filter and pre-formatted block parsing

Version 1.0.0   - Apr 2016
  - Implement a wiki parser with several Wiki formats (moved from AWA)
  - Support for Dotclear2, Mediawiki, Creole, Markdown, PhpBB syntax
  - Add support for HTML embedded in wiki text
  - Define a framework to plug wiki filters
  - Add support to convert HTML into some wiki format
  - Provide a TOC, Autolink and Collectors filters
