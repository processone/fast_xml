#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.53)
AC_PACKAGE_VERSION(0.1.0)
AC_INIT(xml, 0.1.0, [], [])

AC_CONFIG_MACRO_DIR([m4])

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

AX_LIB_EXPAT([2.0.0])

if test "$HAVE_EXPAT" = "no"; then
   AC_MSG_ERROR([Expat XML Parser was not found])
fi

AC_SUBST(EXPAT_CFLAGS)
AC_SUBST(EXPAT_LIBS)
AC_SUBST(EXPAT_LDFLAGS)

AC_OUTPUT
