# Installation

This chapter explains how to build and install the library.

## Alire setup

To use `wikiada` in your project, run the following command to add the dependency
to `wikiada`:

```
alr with wikiada
```

## Build for wikiada development

You can also build and install the `wikiada` library and install it as follows
(but the Alire setup is prefered):

```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:
```
make test
```
And unit tests are executed by running the `bin/wiki_harness` test program.

## Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install prefix=/opt
```

## Using

To use the library in an Ada project, add the following line at the beginning of your
GNAT project file:

```
with "wikiada";
```

