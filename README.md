# Ada Wiki

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/wikiada.json)](https://alire.ada.dev/crates/wikiada)
[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Wiki.svg)](https://jenkins.vacs.fr/job/Ada-Wiki/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Wiki.svg)](https://jenkins.vacs.fr/job/Ada-Wiki/)
[![codecov](https://codecov.io/gh/stcarrez/ada-wiki/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-wiki)
[![Download](https://img.shields.io/badge/download-1.2.1-brightgreen.svg)](http://download.vacs.fr/ada-wiki/ada-wiki-1.2.1.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-wiki/1.2.1.svg)

Ada Wiki is a small library that provides and focuses only on the Wiki engine.

The library allows to:

* Parse a wiki text such as Markdown, Mediawiki, Creole, PhpBB, Dotclear and Google Code
* Parse HTML content in embedded wiki text,
* Filter out the wiki, HTML or text through customizable filters,
* Render the wiki text in HTML, text or another wiki format

The Ada Wiki library is used by [Ada Web Application](https://github.com/stcarrez/ada-awa)
for the implementation of the blog and wiki online plugins.

## Version 1.2.1 - May 2020

- Minor configuration and code coverage support
- Corrections in the Markdown syntax parser

[List all versions](https://github.com/stcarrez/ada-wiki/blob/master/NEWS.md)

# Build

To use Ada Wiki library, configure as follows:
```
   ./configure
   make
```

By default the configure is setup to use the Ada Utility Library.  You can disable that
by using the 'with-ada-util=no' configure option.  When disabled, the build will use some
locally imported files (in src/util) but the unit tests will not be compiled.
```
   ./configure --with-ada-util=no
   make
```

The unit tests are built and executed with:
```
   make test
```
For the installation, use the following command:
```
   make install
```

# Samples

A first example shows how to render a Wiki text into HTML or text.
Another one takes some HTML content and render a Wiki text in one of the supported
Wiki format.  To build the samples, use the following command:
```
   gnatmake -Psamples
```

To import a HTML content and produce a Wiki text, use the following:
```
   bin/import -M https://en.wikibooks.org/wiki/Ada_Programming > content.wiki
```

And to render the Wiki text into HTML use:
```
   bin/render -M content.wiki
```

The 'words' samples illustrates the use of filters to collect information stored
in the documents.  It collects words and links and report their usage in the
document.  The next command reports the links used in the document:
```
   bin/words -l -M content.wiki
```

A complete online Wiki application is part of AWA (https://github.com/stcarrez/ada-awa).

# Documentation

The Ada Wiki sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-wiki/wiki

