# Ada Wiki

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/wikiada.json)](https://alire.ada.dev/crates/wikiada)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-wiki/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-wiki/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-wiki/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-wiki/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-wiki/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-wiki/summary)
[![Download](https://img.shields.io/badge/download-1.4.2-brightgreen.svg)](http://download.vacs.fr/ada-wiki/ada-wiki-1.4.2.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-wiki/1.4.2.svg)](Commits)

Ada Wiki is a small library that provides a Wiki engine supporting several Wiki syntaxes.

The library allows to:

* Parse a wiki text such as Markdown, Mediawiki, Creole, PhpBB, Dotclear and Google Code,
* Parse HTML content in embedded wiki text,
* Filter out the wiki, HTML or text through customizable filters,
* Render the wiki text in HTML, text or another wiki format.

The Ada Wiki library is used by [Ada Web Application](https://gitlab.com/stcarrez/ada-awa)
for the implementation of the blog and wiki online plugins.

You can play with the Wiki engine by using [Wi2wic](https://gitlab.com/stcarrez/wi2wic) on https://wi2wic.vacs.fr/wi2wic/index.html

## Version 1.4.3   - Under development
  - Feature #7: Add a style on HTML table
  - Feature #8: Add support for strikeout in markdown

## Version 1.4.2   - Aug 2024
  - Cleanup build environment to drop configure

[List all versions](https://gitlab.com/stcarrez/ada-wiki/blob/master/NEWS.md)

## Using with Alire

If you are using [Alire](https://alire.ada.dev/) in your project, run the following command
within your [Alire](https://alire.ada.dev/) project to use the library:

```
alr with wikiada
```

## Using without Alire

If you don't have [Alire](https://alire.ada.dev/) or want to build and install the library
on a specific place, run a `setup` command to configure the build as well as installation
directory (note: you must have built and installed [Ada Utility Library](https://gitlab.com/stcarrez/ada-util/)):

```
make setup BUILD=debug PREFIX=/build/install HAVE_ALIRE=no
```

Then build, run the unit tests and install by using:

```
make build
make test
make install
```

# Samples

A first example shows how to render a Wiki text into HTML or text.
Another one takes some HTML content and render a Wiki text in one of the supported
Wiki format.  To build the samples, use the following command:

```
cd samples
alr build
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

A complete online Wiki application is part of [AWA](https://gitlab.com/stcarrez/ada-awa)
and can be tried online with [Atlas](https://demo.vacs.fr/atlas/index.html)

# Documentation

* [Ada Wiki Programmer's Guide](https://ada-wiki.readthedocs.io/en/latest/) [PDF](https://gitlab.com/stcarrez/ada-wiki/blob/master/docs/wiki-book.pdf)

* [Using the Ada Wiki Engine](https://blog.vacs.fr/vacs/blogs/post.html?post=2016/04/30/Using-the-Ada-Wiki-Engine)

# Sites Using Ada Wiki

  * [Java 2 Ada](https://blog.vacs.fr/)
  * [Ada France](https://www.ada-france.org/adafr/index.html)
  * [Atlas](https://demo.vacs.fr/atlas/index.html)
  * [Wi2wic](https://wi2wic.vacs.fr/wi2wic/index.html)
  * [Jason Project Manager](https://vdo.vacs.fr/vdo/index.html)
