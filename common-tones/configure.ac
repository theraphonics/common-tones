# Configuration script for CLM (taken from Snd)

AC_INIT(CLM, 4, bil@ccrma.stanford.edu)
CLM_VERSION=4
AC_CONFIG_SRCDIR(clm.c)
AC_CANONICAL_HOST
AC_CONFIG_HEADERS(mus-config.h)
# no makefile -- actual compilation goes through all.lisp
AC_PROG_CC

#--------------------------------------------------------------------------------
# configuration options
#   --with-alsa           use ALSA (the default in Linux)
#   --with-oss            use OSS
#   --with-jack           use Jack
#   --with-audio          include audio support
#--------------------------------------------------------------------------------

AC_ARG_WITH(oss,	[  --with-oss		  use OSS])
AC_ARG_WITH(alsa,	[  --with-alsa		  use ALSA])
AC_ARG_WITH(audio,       [  --without-audio       don't include any audio functionality])
AC_ARG_WITH(jack,       [  --with-jack		  use JACK])

AUDIO_SYSTEM="None"
LIBS=""

AC_PROG_INSTALL
AC_C_BIGENDIAN
AC_CHECK_SIZEOF(void *)

AC_PATH_PROG(PKG_CONFIG, pkg-config, no)


#--------------------------------------------------------------------------------
# Audio library
#--------------------------------------------------------------------------------

AUDIO_LIB=""
JACK_LIBS=""
JACK_FLAGS=""

if test "$with_audio" != no ; then

  if test "$with_jack" = yes ; then 
    AUDIO_SYSTEM=JACK
    AC_DEFINE(MUS_JACK)
    if test x$PKG_CONFIG != xno ; then
      if $PKG_CONFIG jack --exists ; then    
        JACK_LIBS="`$PKG_CONFIG jack --libs`"
        JACK_FLAGS="`$PKG_CONFIG jack --cflags`"
        if $PKG_CONFIG samplerate --exists ; then    
  	  JACK_LIBS="$JACK_LIBS `$PKG_CONFIG samplerate --libs`"
  	  JACK_FLAGS="$JACK_FLAGS `$PKG_CONFIG samplerate --cflags`"
        else
          JACK_LIBS="$JACK_LIBS -lsamplerate"
        fi
      else
        JACK_LIBS="-ljack -lsamplerate"
      fi
    else
      JACK_LIBS="-ljack -lsamplerate"
    fi
  fi

  if test "$with_alsa" = yes ; then
    AC_DEFINE(HAVE_ALSA)
    AUDIO_LIB="-lasound"
    AUDIO_SYSTEM=ALSA
  fi

  if test "$with_oss" = yes ; then
    AC_DEFINE(HAVE_OSS)
    AUDIO_SYSTEM=OSS
  fi

  if test "$AUDIO_SYSTEM" = None ; then
    case "$host" in
      *-*-linux*) 
        AUDIO_SYSTEM=ALSA
        AC_DEFINE(HAVE_ALSA)
        AUDIO_LIB="-lasound"
	;;
      *-*-sunos4*) 
	AUDIO_SYSTEM=Sun
        ;;
      *-*-solaris*) 
	AUDIO_SYSTEM=Sun
        ;;
      *-*-hpux*) 
	AUDIO_SYSTEM=Hpux
        ;;
      *-*-bsdi*) 
	AC_DEFINE(HAVE_OSS)
	AUDIO_SYSTEM=OSS
        ;;
      *-*-freebsd*)
	AC_DEFINE(HAVE_OSS)
	AUDIO_SYSTEM=OSS
	;;
      *-*-openbsd*) 
	AUDIO_SYSTEM=OpenBSD
        ;;
      *-*-netbsd*) 
	AUDIO_SYSTEM=NetBSD
        ;;
      *-*-cygwin*)
	if test "$with_jack" != yes ; then 
	  AUDIO_SYSTEM=Windows
	fi
	;;
      *-*-mingw*)
	audio_system=Windows
	;;
      *-apple-*)
	if test "$with_jack" != yes ; then 
	  AUDIO_SYSTEM=MacOSX
	  AUDIO_LIB="-framework CoreAudio -framework CoreFoundation -framework CoreMIDI"
        else
          AUDIO_SYSTEM=JACK
	  JACK_LIBS="-framework CoreAudio -framework CoreServices -framework AudioUnit -L/usr/local/lib -ljack -lsamplerate"
          JACK_FLAGS="-I/usr/local/include"
	fi
	;;
    esac
  fi
fi

AC_MSG_CHECKING([for audio system])
AC_MSG_RESULT($AUDIO_SYSTEM)

if test "$AUDIO_SYSTEM" != None ; then
  AC_DEFINE(WITH_AUDIO)
fi


AC_SUBST(AUDIO_LIB)
AC_SUBST(JACK_LIBS)
AC_SUBST(JACK_FLAGS)


#--------------------------------------------------------------------------------
# compiler/loader flags
#--------------------------------------------------------------------------------

LDSO_FLAGS=""
SO_FLAGS=""
SO_LD="ld"
SO_INSTALL="install"
A_INSTALL="install"
# A_LD="ld"
# A_LD_FLAGS=""
A_LD="ar"
A_LD_FLAGS="-rc"
SO_NAME="libsndlib.so"
RANLIB=":"
LD_FLAGS="-r"

case "$host" in
  *-*-linux*) 
    LDSO_FLAGS="-shared"
    LIBS="$LIBS -lm -ldl"
    if test "$GCC" = yes ; then
      SO_FLAGS="-fPIC $SO_FLAGS"
      SO_LD="$CC"
    fi
    if test "$with_jack" = yes ; then 
      A_INSTALL=":"
      A_LD=":"
    fi
    ;;

  *-*-sunos4*) 
    LIBS="$LIBS -lm"
    ;;

  *-*-solaris*) 
    LIBS="$LIBS -lm"
    LDSO_FLAGS="-G"
    ;;

  *-*-hpux*) 
    LDSO_FLAGS="+z -Ae +DA1.1"
    if test "$GCC" = yes ; then
      SO_FLAGS="-fPIC $SO_FLAGS"
    fi
    ;;

  *-*-bsdi*) 
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_FLAGS="-fPIC $SO_FLAGS"
    fi
    ;;

  *-*-freebsd*)
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_LD="$CC"
      SO_FLAGS="-fPIC $SO_FLAGS"
      LDSO_FLAGS="-shared"
    fi
    ;;

  *-*-openbsd*) 
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_LD="$CC"
      SO_FLAGS="-fPIC $SO_FLAGS"
      CFLAGS="-ftrampolines $CFLAGS"
      LDSO_FLAGS="-shared"
    fi
    ;;

  *-*-netbsd*) 
    LIBS="$LIBS -lm"
    if test "$GCC" = yes ; then
      SO_LD="$CC"
      SO_FLAGS="-fPIC $SO_FLAGS"
      LDSO_FLAGS="-shared"
    fi
    ;;

  *-*-cygwin*)
    A_LD_FLAGS="-rc"
    SO_INSTALL=":"
    SO_LD=":"
    ;;

   *-*-mingw*)
    LIBS="$LIBS -lwinmm -lwsock32"
    LDFLAGS="$LDFLAGS -mwindows"
    SO_INSTALL=":"
    SO_LD=":"
    ;;

  *-apple-*)
    SO_LD="$CC"	
    LDSO_FLAGS="-dynamic -bundle -undefined suppress -flat_namespace"
    ;;
esac

if test "$enable_shared" = no; then
  SO_LD=":"
  SO_INSTALL=":"
fi

CFLAGS="-I. $CFLAGS"

AC_SUBST(LDSO_FLAGS)
AC_SUBST(SO_FLAGS)
AC_SUBST(SO_INSTALL)
AC_SUBST(A_INSTALL)
AC_SUBST(SO_LD)
AC_SUBST(A_LD)
AC_SUBST(A_LD_FLAGS)
AC_SUBST(LD_FLAGS)
AC_SUBST(SO_NAME)
AC_SUBST(JACK_LIBS)
AC_SUBST(JACK_FLAGS)
AC_SUBST(RANLIB)

AC_OUTPUT
