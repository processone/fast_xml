#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_PACKAGE_VERSION(0.1.0)
AC_INIT(xml, 0.1.0, [], [])

# Checks for programs.
AC_PROG_CC
AC_PROG_MAKE_SET

if test "x$GCC" = "xyes"; then
    CFLAGS="$CFLAGS -Wall"
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_FUNC_MALLOC
AC_HEADER_STDC

# Checks Erlang runtime and compiler
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC

# Checks and sets ERLANG_ROOT_DIR and ERLANG_LIB_DIR variable
# AC_ERLANG_SUBST_ROOT_DIR
# AC_ERLANG_SUBST_LIB_DIR

AC_CHECK_LIB(expat, XML_ParserCreate, [EXPAT_LIB=yes], [], [])
AC_CHECK_HEADER(expat.h, [EXPAT_HEADER=yes], [], [])

if test "x$EXPAT_LIB" = "x"; then
   AC_MSG_ERROR([Expat library was not found])
fi

if test "x$EXPAT_HEADER" = "x"; then
   AC_MSG_ERROR([Expat header file "expat.h" was not found])
fi

AC_OUTPUT
