# Installation

This chapter explains how to build and install the library.

## Before Building

Before building the library, you will need:

* The [GNAT Ada compiler](https://libre.adacore.com/tools/gnat-gpl-edition/),
* [Ada Utility Library](https://github.com/stcarrez/ada-util).

The build process may also need the following commands:

* make (GNU make),
* gprbuild,
* gprinstall,
* gcc (with Ada language support)

## Configuration

The library uses the `configure` script to detect the build environment,
check for [Ada Utility Library](https://github.com/stcarrez/ada-util) library.
The `configure` script provides several standard options
and you may use:

  * `--prefix=DIR` to control the installation directory,
  * `--disable-static` to disable the build of static libraries,
  * `--enable-distrib` to build for a distribution and strip symbols,
  * `--disable-distrib` to build with debugging support,
  * `--enable-coverage` to build with code coverage support (`-fprofile-arcs -ftest-coverage`),
  * `--with-ada-util=PATH` to control the installation path of [Ada Utility Library](https://github.com/stcarrez/ada-util),
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

By default, the library will be installed in `/usr/local` directory.
If you want to install the library in a specific directory, use the `--prefix` option as follows:

```
./configure --prefix=/opt
```

## Build

After configuration is successful, you can build the library by running:
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

