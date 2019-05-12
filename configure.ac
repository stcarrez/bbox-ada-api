dnl Autoconf configure script for Bbox Ada Library
dnl Copyright (C) 2017, 2019 Stephane Carrez
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

AC_INIT(src/bbox.ads)

AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE

# Current release settings
BBOX_MAJOR_VERSION=0
BBOX_MINOR_VERSION=2
BBOX_MICRO_VERSION=0
BBOX_VERSION=$BBOX_MAJOR_VERSION.$BBOX_MINOR_VERSION.$BBOX_MICRO_VERSION

AM_GNAT_CHECK_GPRBUILD

# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S

# Set the version number of Ada Server Faces
AC_SUBST(BBOX_VERSION)
AC_SUBST(BBOX_MAJOR_VERSION)
AC_SUBST(BBOX_MINOR_VERSION)
AC_SUBST(BBOX_MICRO_VERSION)

BBOX_LIBDIR="lib"
AC_SUBST(BBOX_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

##########################################
# Check to enable/disable shared library
##########################################
AM_SHARED_LIBRARY_SUPPORT

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_sys],
  [git@github.com:stcarrez/ada-util.git],
  [Building Ada CSS requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_utilada_sys}
  ])
AC_SUBST(UTIL_DIR)

AM_UTIL_CHECK_INSTALL

AC_OUTPUT(
Makefile bbox.gpr bbox_tests.gpr bbox_tools.gpr
)
