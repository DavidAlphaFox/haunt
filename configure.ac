dnl -*- Autoconf -*-

AC_INIT(Haunt, 0.2.6)
AC_CONFIG_SRCDIR(haunt)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile example/Makefile website/Makefile haunt/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([test-env], [chmod +x test-env])
AC_CONFIG_FILES([scripts/haunt], [chmod +x scripts/haunt])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS

dnl Guile-reader is needed for Skribe support
GUILE_MODULE_AVAILABLE([have_guile_reader], [(system reader)])
AM_CONDITIONAL([HAVE_GUILE_READER], [test "x$have_guile_reader" = "xyes"])

GUILE_MODULE_AVAILABLE([have_guile_commonmark], [(commonmark)])
AM_CONDITIONAL([HAVE_GUILE_COMMONMARK], [test "x$have_guile_commonmark" = "xyes"])

AC_CHECK_FUNC([inotify_init], [AM_CONDITIONAL(HAVE_INOTIFY, true)], [AM_CONDITIONAL(HAVE_INOTIFY, false)])

AC_OUTPUT
