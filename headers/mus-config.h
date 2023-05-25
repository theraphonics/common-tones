/* mus-config.h.  Generated from mus-config.h.in by configure.  */
#ifndef CONFIG_H_LOADED
#define CONFIG_H_LOADED

/* #undef WORDS_BIGENDIAN */
#define SIZEOF_VOID_P 8
/* #undef HAVE_OSS */
/* #undef HAVE_ALSA */
/* #undef MUS_JACK */
#define WITH_AUDIO 1

/* ---------------------------------------- */

#define USE_SND 0
#define CLM 1

#define HAVE_EXTENSION_LANGUAGE (HAVE_SCHEME || HAVE_RUBY || HAVE_FORTH)

#define HAVE_COMPLEX_NUMBERS 0
#define HAVE_COMPLEX_TRIG 0
#define HAVE_MAKE_RATIO ((HAVE_SCHEME) || (HAVE_FORTH))

#endif