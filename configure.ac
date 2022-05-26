dnl Autoconf configure script for Ada Wiki
dnl Copyright (C) 2015, 2016, 2018, 2019, 2020, 2021, 2022 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT
AC_CONFIG_SRCDIR([src/wiki.ads])

# Current release settings
WIKI_MAJOR_VERSION=1
WIKI_MINOR_VERSION=4
WIKI_MICRO_VERSION=0
WIKI_VERSION=$WIKI_MAJOR_VERSION.$WIKI_MINOR_VERSION.$WIKI_MICRO_VERSION

# Set the version number of Ada Wiki
AC_SUBST(WIKI_VERSION)
AC_SUBST(WIKI_MAJOR_VERSION)
AC_SUBST(WIKI_MINOR_VERSION)
AC_SUBST(WIKI_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(wikiada)

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

AC_MSG_CHECKING([Ada Utility Library])
AC_ARG_WITH(ada-util,
AS_HELP_STRING([--with-ada-util=x], [Path for Ada Utility Library]),
[
  if test T${withval} = Tno ; then
    WIKI_USE_ADA_UTIL=no;
  else
    WIKI_USE_ADA_UTIL='yes';
  fi
],
[
  WIKI_USE_ADA_UTIL=yes;
])
AC_MSG_RESULT(${WIKI_USE_ADA_UTIL})

##########################################
# Ada Util library
##########################################
if test T${WIKI_USE_ADA_UTIL} = Tyes; then
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_base],
  [git@github.com:stcarrez/ada-util.git],
  [Building Ada Wiki requires the Ada Utility Library.],
  [
    UTIL_DIR=${ac_cv_gnat_project_dir_utilada_base}
  ])
  WITH_ADA_UTIL="with \"${UTIL_DIR}utilada_core\";";
  WITH_ADA_UTIL_SYS="with \"${UTIL_DIR}utilada_sys\";";

else
  WITH_ADA_UTIL=''
  WITH_ADA_UTIL_SYS=''
  UTIL_DIR=''
fi
AC_SUBST(UTIL_DIR)
AC_SUBST(WITH_ADA_UTIL)
AC_SUBST(WITH_ADA_UTIL_SYS)
AC_SUBST(WIKI_USE_ADA_UTIL)

AC_CONFIG_FILES([Makefile.conf wikiada.gpr wikiada_tests.gpr samples.gpr
])
AC_OUTPUT
