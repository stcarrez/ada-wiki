# Ada Wiki

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Wiki.svg)](http://jenkins.vacs.fr/job/Ada-Wiki/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Wiki.svg)](http://jenkins.vacs.fr/job/Ada-Wiki/)
[![Download](https://img.shields.io/badge/download-1.0.0-brightgreen.svg)](http://download.vacs.fr/ada-wiki/ada-wiki-1.0.0.tar.gz)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-wiki/ada-wiki-1.0.0.svg)

Ada Wiki is a small library that provides and focuses only on the Wiki engine.

The library allows to:

* Parse a wiki text such as Mediawiki, Creole, PhpBB, Dotclear and Google Code
* Parse HTML content in embedded wiki text,
* Filter out the wiki, HTML or text through customizable filters,
* Render the wiki text in HTML or text

To use Ada Wiki library, configure as follows:
```
   ./configure
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
# Documentation

The Ada Wiki sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-wiki
