AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include
ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS =
bin_SCRIPTS =
dist_bin_SCRIPTS =
noinst_PROGRAMS =
lib_LTLIBRARIES =
EXTRA_DIST =



lib_LTLIBRARIES += libclzygote.la

libclzygote_la_SOURCES = \
	src/zygote.c

noinst_PROGRAMS = testsrv

testsrv_SOURCES = src/testsrv.c
testsrv_LDADD = libclzygote.la

bin_PROGRAMS = clz
clz_SOURCES = src/clz.c
clz_LDADD = libclzygote.la
