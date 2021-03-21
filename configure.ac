dnl Autoconf configure script for Bbox Ada Library
dnl Copyright (C) 2017, 2019, 2021 Stephane Carrez
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

# Current release settings
BBOX_MAJOR_VERSION=0
BBOX_MINOR_VERSION=3
BBOX_MICRO_VERSION=0
BBOX_VERSION=$BBOX_MAJOR_VERSION.$BBOX_MINOR_VERSION.$BBOX_MICRO_VERSION

# Set the version number of Ada Server Faces
AC_SUBST(BBOX_VERSION)
AC_SUBST(BBOX_MAJOR_VERSION)
AC_SUBST(BBOX_MINOR_VERSION)
AC_SUBST(BBOX_MICRO_VERSION)

AM_GNAT_LIBRARY_PROJECT(bboxada)

BBOX_LIBDIR="lib"
AC_SUBST(BBOX_LIBDIR)

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_ADA_UTIL([Bbox Ada])

AC_OUTPUT(
Makefile.conf bboxada.gpr bboxada_tools.gpr
)
