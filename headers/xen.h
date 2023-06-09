#ifndef XEN_H
#define XEN_H

/* macros for extension language support
 *
 * Ruby:      covers 1.8.0 to present
 * Forth:     covers 1.0 to present
 * s7:        all versions
 * None:      all versions
 */

#define XEN_MAJOR_VERSION 3
#define XEN_MINOR_VERSION 28
#define XEN_VERSION "3.28"

/* HISTORY:
 *
 *  14-May-20: g++ xen.h|c changes for Ruby 2.7 and gcc version 10.
 *  --------
 *  26-Apr-18: changed XEN_MAKE_OBJECT_TYPE in s7 again...
 *  --------
 *  2-Aug-17:  changed XEN_MAKE_OBJECT_TYPE in s7.
 *  --------
 *  29-Jul-16: Xen_define_unsafe_typed_procedure.
 *  --------
 *  20-Aug-15: Xen_define_typed_procedure, Xen_define_typed_dilambda.
 *  --------
 *  27-Dec:    Xen_arity in s7 now uses s7_arity. Xen_define_integer_procedure, Xen_define_dilambda.
 *  21-Feb:    Xen_is_number and friends.
 *  7-Jan-14:  in s7, C_TO_XEN_STRING and XEN_TO_C_STRING now treat a null string as a string (not #f).
 *  --------
 *  9-Nov:     removed XEN_DEFINE_PROCEDURE_WITH_REVERSED_SETTER.
 *  11-Oct:    removed XEN_EXACT_P.
 *  23-Sep:    removed *_OR_ELSE, XEN_ARG_*, and OFF_T* macros; added XEN_ARGIFY* to the Forth section.
 *  7-Jul-13:  removed int64 stuff (it was not used anywhere). Made various Ruby changes (NUM2ULL etc).
 *  --------
 *  5-Nov:     minor s7-related changes.
 *  9-July:    XEN_VECTOR_ELEMENTS and XEN_VECTOR_COPY.
 *  4-June:    XEN_PROVIDE
 *  8-May:     added description arg to XEN_DEFINE_SIMPLE_HOOK and XEN_DEFINE_HOOK, only used in scheme.
 *  12-Jan-12: added reverse argument to s7 version of XEN_MAKE_OBJECT_TYPE.
 *  --------
 *  20-Oct:    XEN_LONG_LONG_P.
 *  5-Jun-11:  XEN_DEFINE_SAFE_PROCEDURE, an experiment with s7.
 *  --------
 *  25-Nov:    updates for Ruby 1.9.*.
 *  7-Nov:     XEN_ADD_HOOK.
 *  23-Oct:    use s7_call_with_location, rather than s7_call, for better error reporting.
 *  19-Mar:    removed s7_define_set_function (removed encapsulation from s7, so it's not useful anymore).
 *  17-Feb:    various s7 changes.
 *  5-Feb-10:  XEN_ASSOC_REF and XEN_ASSOC_SET.  XEN_ASSOC_REF returns the value, not the key/value pair.
 *  --------
 *  16-Dec:    removed Guile support. removed xen_return_first (a guile-ism).
 *  2-Nov:     XEN_VECTOR_RANK.
 *  5-Oct:     use s7_c_pointer etc.
 *  7-Aug:     use s7_new_type_x in XEN_MAKE_OBJECT_TYPE.  XEN_DEFINE_SET_PROCEDURE.
 *  27-Jul:    INT64_T cases paralleling OFF_T (the latter may go away someday).
 *  14-Jul:    s7_define_function_star via XEN_DEFINE_PROCEDURE_STAR.
 *  6-Jul:     cleaned up XEN_WRAP_C_POINTER et al (Mike Scholz).
 *  29-Jun:    some fth changes.
 *  30-Mar:    added a bunch of file-oriented functions for s7 (xen.c).
 *  14-Mar:    removed XEN_LOCAL_GC_PROTECT and XEN_LOCAL_GC_UNPROTECT.
 *  14-Jan-09: s7_xen_initialize.
 *  --------
 *  17-Nov:    use s7_define_constant in XEN_DEFINE_CONSTANT.
 *  1-Nov:     changed s7 and Guile C_TO_XEN_STRING slightly.
 *  16-Oct:    removed Gauche support.
 *  10-Aug:    S7, a TinyScheme derivative.
 *             changed XEN_NUMERATOR and XEN_DENOMINATOR to return off_t not XEN.
 *  23-Jul:    be more careful about wrapping POINTERs (they say 64-bit MS C void* == unsigned long long, but not unsigned long).
 *  30-Jun:    XEN_OFF_T_IF_BOUND_P.
 *  19-May:    more const char* arg declarations.
 *  14-May:    changed XEN_ARITY in Guile to use scm_procedure_property.
 *  1-May:     XEN_NAN_P and XEN_INF_P (Guile).
 *  23-Apr:    try to get old Gauche (8.7) to work again.
 *  1-Mar-08:  no ext case now checks arg consistency.
 *  --------
 *  12-Dec:    Gauche uses COMPNUM, not COMPLEX (after 0.8.7?), NUMBERP for complex?
 *  21-Nov:    XEN_HAVE_COMPLEX_NUMBERS.
 *  18-Jul:    Gauche error handling changes.
 *  28-Apr:    Gauche API changes in versions 0.8.8, 0.8.10, and 0.9.
 *  14-Feb:    XEN_PUTS and friends for fth (Mike).
 *  17-Jan-07: rb_errinfo changes (Mike Scholz).
 *  --------
 *  14-Nov:    check for Scm_EvalRec (Gauche 0.8.8).
 *  9-Sep:     XEN_LOAD_PATH and XEN_ADD_TO_LOAD_PATH
 *  1-Sep:     string and array changes for Ruby (from Mike).
 *  7-Aug:     more careful list length handling in Ruby (from Mike).
 *  23-May:    added xen_rb_repl_set_prompt to set (no-gui) Ruby repl prompt.
 *  12-May:    changed HAVE_RATIOS to XEN_HAVE_RATIOS.
 *  17-Apr:    removed XEN_MAKE_OBJECT.
 *  15-Apr:    Gauche support.
 *  28-Mar-06: Forth support thanks to Mike Scholz.
 *  --------
 *  7-Nov:     xen_rb_defined_p (Mike Scholz).
 *  16-Sep:    removed some debugging extras that caused confusion on 64-bit machines.
 *  12-Aug:    include guile setter procedure names for better error reporting.
 *  14-Jun:    XEN_DEFINE (XEN value, not assumed to be int as in XEN_DEFINE_CONSTANT).
 *             XEN_ASSOC, XEN_MEMBER, and XEN_PROCEDURE_NAME for Scheme side.
 *             XEN_DEFINE_HOOK and XEN_DEFINE_SIMPLE_HOOK no longer take the "Var" arg.
 *  18-May:    deprecate XEN_NUMBER_OR_BOOLEAN_IF_BOUND_P and XEN_NUMBER_OR_BOOLEAN_P.
 *  29-Mar:    C_TO_XEN_STRINGN changes.
 *  24-Mar:    Ruby properties (Mike Scholz).
 *  8-Mar:     Ruby improvements in keywords and hooks (Mike Scholz).
 *  7-Mar:     C99 complex number changes (creal, _Complex_I) (Steve Bankowitz).
 *  2-Mar:     Ruby support for off_t (Mike Scholz).
 *  4-Jan-05:  more guile changes.
 *  --------
 *  31-Dec:    removed "caller" arg from *_NO_CATCH.
 *  10-Nov:    scm_c_vector* (new Guile functions)
 *  21-Oct:    XEN_LIST_REVERSE, (using rb_ary_dup available in 1.8)
 *  7-Oct:     keyword changes for new Guile.
 *  28-Sep:    deprecated *_WITH_CALLER -- these no longer do anything useful in Guile.
 *             NaNs and Infs -> 0 or 0.0 in XEN_TO_C_INT|DOUBLE -- perhaps I should add another set of macros?
 *  23-Aug:    more Guile name changes.
 *  12-Aug:    more Guile name changes, C_TO_XEN_STRINGN (Guile)
 *  3-Aug:     xen_to_c_int bugfix thanks to Kjetil S. Matheussen.
 *  29-Jul:    deprecated XEN_TO_C_BOOLEAN_OR_TRUE.
 *  21-Jul:    deprecated XEN_TO_SMALL_C_INT and C_TO_SMALL_XEN_INT.
 *             use new Guile 1.7 numerical function names (under flag HAVE_SCM_TO_SIGNED_INTEGER).
 *  28-Jun:    XEN_REQUIRED_ARGS_OK to make it easier to turn off this check.
 *  9-June:    complex number conversions (Guile) -- Ruby complex numbers are an optional module?
 *  21-May:    plug some memory leaks in Ruby cases.
 *  23-Feb:    changed DEBUGGING to XEN_DEBUGGING, added redefinition checks under that switch.
 *  2-Feb:     C_TO_XEN_CHAR, ratio support (Guile), XEN_CONS_P, XEN_PAIR_P, etc
 *  6-Jan:     XEN_VARIABLE_REF in Guile changed to support 1.4 and older versions.
 *  5-Jan-04:  hook support in Ruby thanks to Michael Scholz.
 *  --------
 *  1-Nov:     protect several macros from hidden double evaluations.
 *  29-Sep:    fixed incorrect assumption in xen_rb_cons (xen.c) that arg2 was list.
 *  8-Sep:     removed xen_malloc -- can't remember now why this existed.
 *  19-Aug:    xen_rb_str_new2 to avoid unwanted side-effects.
 *  12-Aug:    various changes for ISO C99.
 *  30-Jul:    use new SCM_VECTOR_REF/SET macros if they're defined.
 *  7-Apr:     changes to error handlers for more perspicuous error messages
 *             changed XEN_PROTECT_FROM_GC in Ruby to use rb_gc_register_address, added XEN_UNPROTECT_FROM_GC (rb_gc_unregister_address)
 *  10-Mar:    XEN_OUT_OF_RANGE_ERROR, XEN_BAD_ARITY_ERROR
 *  17-Feb:    XEN_HOOK_P
 *  20-Jan-03: added Windows case for auto-import loader bugfix.
 *  --------
 *  19-Dec:    proc arg checks for Ruby (to make sure XEN_[N|V]ARGIFY|DEFINE_PROCEDURE[etc] agree)
 *  29-Jul:    SCM_WRITABLE_VELTS for current CVS Guile
 *  28-May:    off_t equivalents in Ruby 1.7
 *  6-May:     off_t (long long) macros.
 *  2-Jan-02:  removed TIMING and MCHECK debugging stuff, VARIABLE_REF -> XEN_VARIABLE_REF
 *  --------
 *  22-Sep-01: removed (redundant) UNSIGNED_LONG macros -- use ULONG instead
*/

#ifndef __cplusplus
#include <sys/types.h>
#ifndef _MSC_VER
  #include <stdbool.h>
#else
#ifndef true
  #define bool  unsigned char
  #define true	1
  #define false	0
#endif
#endif
#endif


#if ((!__NetBSD__) && ((_MSC_VER) || (!defined(__STC__)) || (defined(__STDC_VERSION__) && (__STDC_VERSION__ < 199901L))))
  #define __func__ __FUNCTION__
#endif



/* ------------------------------ RUBY ------------------------------ */

/* other possibilities:
 *    XEN_DEFINE_METHOD, XEN_DEFINE_ALIAS, rb_ary_unsift = XEN_LIST_PREPEND?,
 *    various property macros -- in Scheme as well, rb_const_defined, rb_yield, XEN_INCLUDE_MODULE,
 *    rb_id2name (XEN_SYMBOL...), rb_raise.
 */

#if HAVE_RUBY

#ifdef _GNU_SOURCE
  #undef _GNU_SOURCE
#endif
#include <ruby.h>
#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #ifndef _GNU_SOURCE
    #define _GNU_SOURCE
  #endif
#endif

#define XEN_OK 1

#define XEN                             VALUE
#define XEN_FILE_EXTENSION              "rb"
#define XEN_COMMENT_STRING              "#"
#define XEN_LANGUAGE_NAME               "Ruby"

#define XEN_FALSE                       Qfalse
#define XEN_TRUE                        Qtrue
#define XEN_TRUE_P(a)                   ((a) == Qtrue)
#define XEN_FALSE_P(a)                  ((a) == Qfalse)
#define C_TO_XEN_BOOLEAN(a)             ((a) ? Qtrue : Qfalse)
#define XEN_TO_C_BOOLEAN(a)             (!(XEN_FALSE_P(a)))

/* #define XEN_UNDEFINED                   Qundef */
#define XEN_UNDEFINED                   ID2SYM(rb_intern("undefined"))

#define XEN_BOUND_P(Arg)                ((Arg) != XEN_UNDEFINED)

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_BOOLEAN_P(Arg)            ({ XEN _xen_h_7_ = Arg;        (XEN_TRUE_P(_xen_h_7_) || XEN_FALSE_P(_xen_h_7_)); })
  #define XEN_NUMBER_P(Arg)             ({ int _xen_h_8_ = TYPE(Arg);  ((_xen_h_8_ == T_FLOAT) || (_xen_h_8_ == T_FIXNUM) || (_xen_h_8_ == T_BIGNUM)); })
  #define XEN_INTEGER_P(Arg)            ({ int _xen_h_9_ = TYPE(Arg);  ((_xen_h_9_ == T_FIXNUM) || (_xen_h_9_ == T_BIGNUM)); })
  #define XEN_PROCEDURE_P(Arg)          ({ XEN _xen_h_10_ = Arg;       (XEN_BOUND_P(_xen_h_10_) && (rb_obj_is_kind_of(_xen_h_10_, rb_cProc))); })
  #define XEN_KEYWORD_P(Obj)            ({ XEN _xen_h_12_ = Obj;       (XEN_BOUND_P(_xen_h_12_) && SYMBOL_P(_xen_h_12_)); })
#else
  #define XEN_BOOLEAN_P(Arg)            (XEN_TRUE_P(Arg) || XEN_FALSE_P(Arg))
  #define XEN_NUMBER_P(Arg)             ((TYPE(Arg) == T_FLOAT) || (TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_INTEGER_P(Arg)            ((TYPE(Arg) == T_FIXNUM) || (TYPE(Arg) == T_BIGNUM))
  #define XEN_PROCEDURE_P(Arg)          (XEN_BOUND_P(Arg) && (rb_obj_is_kind_of(Arg, rb_cProc)))
  #define XEN_KEYWORD_P(Obj)            (XEN_BOUND_P(Obj) && SYMBOL_P(Obj))
#endif

/* ---- lists ---- */
#define XEN_EMPTY_LIST                  Qnil
#define XEN_NULL_P(a)                   (XEN_LIST_LENGTH(a) == 0)

#define XEN_CONS_P(Arg)                 (TYPE(Arg) == T_ARRAY)
#define XEN_PAIR_P(Arg)                 (TYPE(Arg) == T_ARRAY)
#define XEN_CONS(Arg1, Arg2)            xen_rb_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)    xen_rb_cons2(Arg1, Arg2, Arg3)
#define XEN_CAR(a)                      xen_rb_list_ref(a, 0)
#define XEN_CADR(a)                     xen_rb_list_ref(a, 1)
#define XEN_CADDR(a)                    xen_rb_list_ref(a, 2)
#define XEN_CADDDR(a)                   xen_rb_list_ref(a, 3)
#define XEN_CDR(a)                      xen_rb_cdr(a)
#define XEN_CDDR(a)                     XEN_CDR(XEN_CDR(a))
#define XEN_CDDDR(a)                    XEN_CDR(XEN_CDR(XEN_CDR(a)))

#define XEN_LIST_P(Arg)                 ((Arg) == XEN_EMPTY_LIST || XEN_CONS_P(Arg))
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = XEN_LIST_LENGTH(Arg)) >= 0)
#define XEN_LIST_LENGTH(Arg)            xen_rb_list_length(Arg)
#define XEN_EQ_P(a, b)                  ((a) == (b))
#define XEN_LIST_1(a)                   rb_ary_new3(1, a)
#define XEN_LIST_2(a, b)                rb_ary_new3(2, a, b)
#define XEN_LIST_3(a, b, c)             rb_ary_new3(3, a, b, c)
#define XEN_LIST_4(a, b, c, d)          rb_ary_new3(4, a, b, c, d)
#define XEN_LIST_5(a, b, c, d, e)       rb_ary_new3(5, a, b, c, d, e)
#define XEN_LIST_6(a, b, c, d, e, f)    rb_ary_new3(6, a, b, c, d, e, f)
#define XEN_LIST_7(a, b, c, d, e, f, g) rb_ary_new3(7, a, b, c, d, e, f, g)
#define XEN_LIST_8(a, b, c, d, e, f, g, h) rb_ary_new3(8, a, b, c, d, e, f, g, h)
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) rb_ary_new3(9, a, b, c, d, e, f, g, h, i)
#define XEN_COPY_ARG(Lst)               xen_rb_copy_list(Lst)
#define XEN_LIST_REF(Lst, Num)          xen_rb_list_ref(Lst, Num)
#define XEN_LIST_SET(Lst, Num, Val)     xen_rb_list_set(Lst, Num, Val)
#define XEN_APPEND(X, Y)                rb_ary_concat(X, Y)
#define XEN_LIST_REVERSE(Lst)           ((Lst == XEN_EMPTY_LIST) ? XEN_EMPTY_LIST : rb_ary_reverse(XEN_COPY_ARG(Lst)))

/* ---- numbers ---- */
#define XEN_ZERO                        INT2NUM(0)
#define XEN_DOUBLE_P(Arg)               XEN_NUMBER_P(Arg)
#define XEN_TO_C_DOUBLE(a)              NUM2DBL(a)
#define C_TO_XEN_DOUBLE(a)              rb_float_new(a)
#define XEN_TO_C_INT(a)                 rb_num2long(a)

/* apparently no complex numbers (built-in) in Ruby? */
#define XEN_COMPLEX_P(Arg)              1
#define C_TO_XEN_COMPLEX(a)             XEN_ZERO
#define XEN_TO_C_COMPLEX(a)             0.0

#define XEN_ULONG_P(Arg1)               XEN_INTEGER_P(Arg1)
#define XEN_WRAPPED_C_POINTER_P(Arg1)   XEN_INTEGER_P(Arg1)
#define C_TO_XEN_INT(a)                 INT2NUM(a)
#define XEN_TO_C_ULONG(a)               NUM2ULONG(a)
#ifdef ULONG2NUM
  #define C_TO_XEN_ULONG(a)             ULONG2NUM((unsigned long)a)
#else
  #define C_TO_XEN_ULONG(a)             UINT2NUM((unsigned long)a)
#endif

#ifdef NUM2ULL
/* ruby 1.9.3 */
  #define C_TO_XEN_LONG_LONG(a)           LL2NUM(a)
  #define XEN_TO_C_LONG_LONG(a)           NUM2LL(a)

  #define XEN_ULONG_LONG_P(Arg)           XEN_INTEGER_P(Arg)
  #define XEN_TO_C_ULONG_LONG(Arg)        NUM2ULL(Arg) /* NUM2ULONG(Arg) */
  #define C_TO_XEN_ULONG_LONG(Arg)        ULL2NUM(Arg) /* INT2NUM(Arg) */
#else
/* older versions -- no dependable version number in ruby -- these macros may not work on a 64-bit machine */

  #ifndef OFFT2NUM
    #define OFFT2NUM(a)                   INT2NUM(a)
  #endif
  #ifndef NUM2OFFT
    #define NUM2OFFT(a)                   NUM2LONG(a)
  #endif
  #define C_TO_XEN_LONG_LONG(a)           OFFT2NUM(a)
  #define XEN_TO_C_LONG_LONG(a)           NUM2OFFT(a)

  #define XEN_ULONG_LONG_P(Arg)           XEN_INTEGER_P(Arg)
  #define XEN_TO_C_ULONG_LONG(Arg)        NUM2OFFT(Arg)
  #define C_TO_XEN_ULONG_LONG(Arg)        OFFT2NUM(Arg)
#endif

/* ---- strings ---- */
#define XEN_STRING_P(Arg)               ((TYPE(Arg) == T_STRING) && (!SYMBOL_P(Arg)))
#define C_TO_XEN_STRING(a)              xen_rb_str_new2((char *)a)
#define C_TO_XEN_STRINGN(a, len)        rb_str_new((char *)a, len)
#ifndef RSTRING_PTR
  #define XEN_TO_C_STRING(Str)          RSTRING(Str)->ptr
#else
  #define XEN_TO_C_STRING(Str)          RSTRING_PTR(Str)
#endif

#define XEN_CHAR_P(Arg)                 XEN_STRING_P(Arg)
#define XEN_TO_C_CHAR(Arg)              XEN_TO_C_STRING(Arg)[0]
#define C_TO_XEN_CHAR(Arg)              rb_str_new((char *)(&(Arg)), 1)

#define XEN_NAME_AS_C_STRING_TO_VALUE(a) xen_rb_gv_get(a)
#define XEN_EVAL_C_STRING(Arg)          xen_rb_eval_string_with_error(Arg)
#define XEN_TO_STRING(Obj)              xen_rb_obj_as_string(Obj)
#define XEN_LOAD_FILE(a)                xen_rb_load_file_with_error(a)
#define XEN_LOAD_PATH                   XEN_NAME_AS_C_STRING_TO_VALUE("$LOAD_PATH")
#define XEN_ADD_TO_LOAD_PATH(Path)      xen_rb_add_to_load_path(Path)

/* ---- hooks ---- */
#define XEN_HOOK_P(Arg)                 xen_rb_hook_p(Arg)
#define XEN_HOOK_PROCEDURES(a)          xen_rb_hook_to_a(a)
#define XEN_CLEAR_HOOK(a)               xen_rb_hook_reset_hook(a)
#define XEN_HOOKED(a)                   (!xen_rb_hook_empty_p(a))
#define XEN_DEFINE_HOOK(Name, Descr, Arity, Help) xen_rb_create_hook((char *)(Name), Arity, (char *)Help)
#define XEN_DEFINE_SIMPLE_HOOK(Descr, Arity) xen_rb_create_simple_hook(Arity);
#define XEN_ADD_HOOK(Hook, Func, Name, Doc) xen_rb_add_hook(Hook, (XEN (*)(ANYARGS))Func, Name, Doc)

/* ---- vectors ---- */
#define XEN_VECTOR_P(Arg)               (TYPE(Arg) == T_ARRAY)
#define XEN_VECTOR_LENGTH(Arg)          xen_rb_list_length(Arg)
#define XEN_VECTOR_REF(Vect, Num)       xen_rb_list_ref(Vect, Num)
#define XEN_VECTOR_SET(Vect, Num, Val)  xen_rb_list_set(Vect, Num, Val)
#define XEN_MAKE_VECTOR(Num, Fill)      xen_rb_ary_new_with_initial_element(Num, Fill)
#define XEN_VECTOR_TO_LIST(a)           a
#define XEN_VECTOR_COPY(Vect)           rb_ary_dup(Vect)

#define XEN_ASSOC_REF(Item, Lst)        xen_assoc(Item, Lst)
#define XEN_ASSOC_SET(Sym, Val, Lst)    xen_set_assoc(Sym, Val, Lst)


/* ---- symbols ---- */
#define XEN_SYMBOL_P(Arg)               SYMBOL_P(Arg)
#define XEN_SYMBOL_TO_C_STRING(a)       ((char *)rb_id2name(SYM2ID(a)))
#define C_STRING_TO_XEN_SYMBOL(a)       ID2SYM(rb_intern(a))
#define XEN_SYMBOL_TO_STRING(Sym)       C_TO_XEN_STRING(XEN_SYMBOL_TO_C_STRING(Sym))
#define XEN_DOCUMENTATION_SYMBOL        C_STRING_TO_XEN_SYMBOL("documentation")
#define XEN_OBJECT_HELP(Name)           rb_documentation(Name)
#define XEN_SET_OBJECT_HELP(Name, Help) rb_set_documentation(Name, Help)
#define C_SET_OBJECT_HELP(name, help)   XEN_SET_OBJECT_HELP(C_TO_XEN_STRING(name), C_TO_XEN_STRING(help))

#define XEN_VARIABLE_SET(a, b)          xen_rb_gv_set(a, b)
#define XEN_VARIABLE_REF(a)             xen_rb_gv_get(a)
#define XEN_DEFINE_CONSTANT(Name, Value, Help) \
  do { \
      char *temp; \
      temp = xen_scheme_constant_to_ruby(Name); \
      rb_define_global_const(temp, C_TO_XEN_INT(Value)); \
      if ((Name) && (Help)) C_SET_OBJECT_HELP(temp, Help); \
      if (temp) free(temp); \
    } while (0)

#define XEN_DEFINE_VARIABLE(Name, Var, Value) \
  { \
    char *temp; \
    Var = Value; \
    temp = xen_scheme_global_variable_to_ruby(Name); \
    rb_define_variable(temp, (VALUE *)(&Var)); \
    if (temp) free(temp); \
  }
#define XEN_DEFINE(Name, Value)         xen_rb_define(Name, Value)
#define XEN_DEFINED_P(Name)             xen_rb_defined_p(Name)

/* ---- C structs ---- */
#define XEN_MARK_OBJECT_TYPE            void *
#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Mark, Free) return(Data_Wrap_Struct(Tag, Mark, Free, Val))
#define XEN_MAKE_OBJECT(Tag, Val, Mark, Free) Data_Wrap_Struct(Tag, Mark, Free, Val)
#define XEN_OBJECT_REF(a)               DATA_PTR(a)
#define XEN_OBJECT_TYPE                 VALUE
#define XEN_OBJECT_TYPE_P(OBJ, TAG)     (XEN_BOUND_P(OBJ) && (rb_obj_is_instance_of(OBJ, TAG)))
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz)  xen_rb_define_class(Typ)

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) \
  static void *Wrapped_Free(XEN obj) \
  { \
    Original_Free((Type *)obj); \
    return(NULL); \
  }

#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) \
  static XEN Wrapped_Print(XEN obj) \
  { \
    XEN val; \
    char *str; \
    str = Original_Print((Type *)XEN_OBJECT_REF(obj)); \
    val = C_TO_XEN_STRING(str); \
    free(str); \
    return(val); \
  }

/* ---- procedures ---- */
#if defined(__cplusplus) || (defined(__GNUC__) && (__GNUC__ >= 10)) || (defined(__clang__) && (__clang_major__ >= 10))
  #ifdef ANYARGS
    #define XEN_PROCEDURE_CAST (XEN (*)(ANYARGS))
    #define XEN_VALUE_ARG_PROCEDURE_CAST (XEN (*)(VALUE))
  #else
    #define XEN_PROCEDURE_CAST (XEN (*)())
    #define XEN_VALUE_ARG_PROCEDURE_CAST (XEN (*)())
  #endif
#else
  #define XEN_PROCEDURE_CAST
  #define XEN_VALUE_ARG_PROCEDURE_CAST
#endif

#define XEN_ARITY(Func)                  rb_funcall(Func, rb_intern("arity"), 0)
#define XEN_REQUIRED_ARGS(Func)          xen_rb_required_args(XEN_ARITY(Func))
#define XEN_REQUIRED_ARGS_OK(Func, Args) (xen_rb_required_args(XEN_ARITY(Func)) == Args)

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  do { \
      char *temp; \
      temp = xen_scheme_procedure_to_ruby(Name); \
      rb_define_global_function(temp, XEN_PROCEDURE_CAST Func, ((RstArg > 0) ? -2 : (OptArg > 0) ? -1 : ReqArg)); \
      if ((Name) && (Doc)) C_SET_OBJECT_HELP(temp, Doc); \
      if (temp) free(temp); \
    } while (0)

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  do { \
      XEN_DEFINE_PROCEDURE(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Req, Get_Opt, 0, Get_Help); \
      XEN_DEFINE_PROCEDURE(Set_Name, XEN_PROCEDURE_CAST Set_Func, Set_Req, Set_Opt, 0, Get_Help); \
   } while (0)

#define XEN_DEFINE_SAFE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)

#define XEN_CALL_0(Func, Caller)                   xen_rb_funcall_0(Func)
#define XEN_CALL_1(Func, Arg1, Caller)             rb_funcall(Func, rb_intern("call"), 1, Arg1)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller)       rb_funcall(Func, rb_intern("call"), 2, Arg1, Arg2)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) rb_funcall(Func, rb_intern("call"), 3, Arg1, Arg2, Arg3)
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) rb_funcall(Func, rb_intern("call"), 4, Arg1, Arg2, Arg3, Arg4)
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) rb_funcall(Func, rb_intern("call"), 5, Arg1, Arg2, Arg3, Arg4, Arg5)
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) rb_funcall(Func, rb_intern("call"), 6, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)
#define XEN_APPLY(Func, Args, Caller)              xen_rb_apply(Func, Args)
#define XEN_CALL_0_NO_CATCH(Func)                   xen_rb_funcall_0(Func)
#define XEN_CALL_1_NO_CATCH(Func, Arg1)             rb_funcall(Func, rb_intern("call"), 1, Arg1)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2)       rb_funcall(Func, rb_intern("call"), 2, Arg1, Arg2)
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) rb_funcall(Func, rb_intern("call"), 3, Arg1, Arg2, Arg3)
#define XEN_APPLY_NO_CATCH(Func, Args)              xen_rb_apply(Func, Args)

/* ---- keywords, etc ---- */
#define XEN_KEYWORD_EQ_P(k1, k2)        ((k1) == (k2))
#define XEN_MAKE_KEYWORD(Arg)           xen_rb_make_keyword(Arg)
#define XEN_PROVIDE(a)                  rb_provide(xen_strdup(a))
#define XEN_PROTECT_FROM_GC(Var)        rb_gc_register_address(&(Var))
#define XEN_UNPROTECT_FROM_GC(Var)      rb_gc_unregister_address(&(Var))

/* ---- errors ---- */
#define XEN_ERROR_TYPE(Name)            xen_rb_intern(Name)


#if USE_SND

#define XEN_ERROR(Type, Info)           snd_rb_raise(Type, Info)

#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  snd_rb_raise(XEN_ERROR_TYPE("out-of-range"), \
           XEN_LIST_5(C_TO_XEN_STRING("~A: argument ~A, ~A, is out of range (~A)"), \
                          C_TO_XEN_STRING(xen_scheme_procedure_to_ruby(Caller)), \
                          C_TO_XEN_INT(ArgN), \
                          Arg, \
                          C_TO_XEN_STRING(Descr)))

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  snd_rb_raise(XEN_ERROR_TYPE("wrong-type-arg"), \
               XEN_LIST_5(C_TO_XEN_STRING("~A: argument ~A, ~A, should be ~A"), \
                          C_TO_XEN_STRING(xen_scheme_procedure_to_ruby(Caller)), \
                          C_TO_XEN_INT(ArgN), \
                            Arg, \
                          C_TO_XEN_STRING(Descr)))

#else

#define XEN_ERROR(Type, Info)           xen_rb_raise(Type, Info)

#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  rb_raise(rb_eRangeError, "%s: argument %d, %s, is out of range (%s)\n", \
       Caller, (int)ArgN, XEN_AS_STRING(Arg), Descr)

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  rb_raise(rb_eTypeError, "%s: argument %d, %s, should be %s\n", \
       Caller, (int)ArgN, XEN_AS_STRING(Arg), Descr)

#endif

#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
  if (!(Assertion)) \
    XEN_WRONG_TYPE_ARG_ERROR(Caller, Position, Arg, Correct_Type)

#define XEN_THROW(Type, Info)           xen_rb_raise(Type, Info)

#define XEN_ARGIFY_1(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_2(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_3(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_4(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_5(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_6(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_7(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_8(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED)); \
  }

#define XEN_ARGIFY_9(OutName, InName) \
  static XEN OutName(int argc, XEN *argv, XEN self) \
  { \
    return(InName((argc > 0) ? argv[0] : XEN_UNDEFINED, \
		  (argc > 1) ? argv[1] : XEN_UNDEFINED, \
		  (argc > 2) ? argv[2] : XEN_UNDEFINED, \
		  (argc > 3) ? argv[3] : XEN_UNDEFINED, \
		  (argc > 4) ? argv[4] : XEN_UNDEFINED, \
		  (argc > 5) ? argv[5] : XEN_UNDEFINED, \
		  (argc > 6) ? argv[6] : XEN_UNDEFINED, \
		  (argc > 7) ? argv[7] : XEN_UNDEFINED, \
		  (argc > 8) ? argv[8] : XEN_UNDEFINED)); \
  }

#define XEN_NARGIFY_0(OutName, InName) \
  static XEN OutName(void) {return(InName());}

#define XEN_NARGIFY_1(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg) {return(InName(Arg));}

#define XEN_NARGIFY_2(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2) {return(InName(Arg1, Arg2));}

#define XEN_NARGIFY_3(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3) {return(InName(Arg1, Arg2, Arg3));}

#define XEN_NARGIFY_4(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4) {return(InName(Arg1, Arg2, Arg3, Arg4));}

#define XEN_NARGIFY_5(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5) {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5));}

#define XEN_NARGIFY_6(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6) {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6));}

#define XEN_NARGIFY_7(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7) \
    {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7));}

#define XEN_NARGIFY_8(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7, XEN Arg8) \
    {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8));}

#define XEN_NARGIFY_9(OutName, InName) \
  static XEN OutName(XEN self, XEN Arg1, XEN Arg2, XEN Arg3, XEN Arg4, XEN Arg5, XEN Arg6, XEN Arg7, XEN Arg8, XEN Arg9) \
    {return(InName(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, Arg9));}

#define XEN_VARGIFY(OutName, InName) \
  static XEN OutName(XEN self, XEN Args) {return(InName(Args));}

#ifdef __cplusplus
extern "C" {
#endif

XEN xen_rb_gv_get(const char *name);
XEN xen_rb_gv_set(const char *name, XEN new_val);
XEN xen_rb_intern(const char *name);
XEN xen_rb_make_keyword(const char *name);
void xen_rb_define(const char *name, XEN value);
XEN xen_rb_cdr(XEN val);
XEN xen_rb_cons(XEN arg1, XEN arg2);
XEN xen_rb_cons2(XEN arg1, XEN arg2, XEN arg3);
char *xen_scheme_constant_to_ruby(const char *name);
char *xen_scheme_procedure_to_ruby(const char *name);
char *xen_scheme_global_variable_to_ruby(const char *name);
bool xen_rb_defined_p(const char *name);
XEN xen_rb_define_class(const char *name);
int xen_rb_list_length(XEN obj);
XEN xen_rb_list_ref(XEN obj, int index);
XEN xen_rb_list_set(XEN obj, int index, XEN value);
void xen_rb_raise(XEN type, XEN info);
XEN xen_rb_obj_as_string(XEN obj);
XEN xen_rb_eval_string_with_error(const char *str);
void xen_rb_load_file_with_error(const char *file);
XEN xen_rb_ary_new_with_initial_element(long num, XEN element);
XEN xen_rb_apply(XEN func, XEN args);
XEN xen_rb_funcall_0(XEN func);
int xen_rb_required_args(XEN val);
XEN xen_rb_copy_list(XEN val);
XEN xen_rb_str_new2(char *arg);
void xen_add_help(char *name, const char *help);
char *xen_help(char *name);
/* class Hook */
bool xen_rb_hook_p(XEN hook);
bool xen_rb_hook_empty_p(XEN hook);
XEN xen_rb_hook_c_new(char *name, int arity, char *help);
XEN xen_rb_hook_reset_hook(XEN hook);
XEN xen_rb_hook_to_a(XEN hook);
void Init_Hook(void);
XEN xen_rb_create_hook(char *name, int arity, char *help);
XEN xen_rb_create_simple_hook(int arity);
XEN xen_rb_add_hook(XEN hook, VALUE (*func)(ANYARGS), const char *name, const char *doc);
typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);

XEN rb_properties(void);
XEN rb_property(XEN obj, XEN prop);
XEN rb_set_property(XEN obj, XEN prop, XEN val);
XEN rb_documentation(XEN name);
XEN rb_set_documentation(XEN name, XEN help);
bool xen_rb_arity_ok(int rargs, int args);
void xen_rb_repl_set_prompt(const char *prompt);
XEN xen_rb_add_to_load_path(char *path);
XEN xen_set_assoc(XEN key, XEN val, XEN alist);
XEN xen_assoc(XEN key, XEN alist);

#ifdef __cplusplus
}
#endif

#endif
/* end HAVE_RUBY */



/* ------------------------------ FORTH ------------------------------ */

#if HAVE_FORTH

#include <fth.h>

#if USE_SND
# undef gettext_noop
# undef _
# undef N_
#endif

#define XEN_OK                          true

#define XEN                             FTH
#define XEN_FILE_EXTENSION              FTH_FILE_EXTENSION
#define XEN_COMMENT_STRING              "\\"
#define XEN_LANGUAGE_NAME               "Forth"

#define XEN_FALSE                       FTH_FALSE
#define XEN_TRUE                        FTH_TRUE
#define XEN_EMPTY_LIST                  FTH_NIL
#define XEN_UNDEFINED                   FTH_UNDEF
#define XEN_DOCUMENTATION_SYMBOL        FTH_DOCUMENTATION_SYMBOL

#define XEN_DEFINED_P(name)             fth_defined_p((char *)name)
#define XEN_PROVIDE(feature)            fth_add_feature(feature)

/* === Boolean, Bound, Equal === */
#define XEN_BOOLEAN_P(Arg)              FTH_BOOLEAN_P(Arg)
#define XEN_TRUE_P(a)                   FTH_TRUE_P(a)
#define XEN_FALSE_P(a)                  FTH_FALSE_P(a)
#define C_TO_XEN_BOOLEAN(a)             BOOL_TO_FTH(a)
#define XEN_TO_C_BOOLEAN(a)             FTH_TO_BOOL(a)

#define XEN_BOUND_P(Arg)                FTH_BOUND_P(Arg)
#define XEN_EQ_P(a, b)                  ((a) == (b))

/* === Number === */
#define XEN_ZERO                        FTH_ZERO
#define XEN_NUMBER_P(Arg)               FTH_NUMBER_P(Arg)
#define XEN_WRAPPED_C_POINTER_P(Arg)    FTH_EXACT_P(Arg)

#define XEN_INTEGER_P(Arg)              FTH_INTEGER_P(Arg)
#define C_TO_XEN_INT(a)                 fth_make_int(a)
#define XEN_TO_C_INT(a)                 fth_int_ref(a)

#define XEN_ULONG_P(Arg)                FTH_UNSIGNED_P(Arg)
#define C_TO_XEN_ULONG(a)               fth_make_unsigned((unsigned long)(a))
#define XEN_TO_C_ULONG(a)               fth_unsigned_ref(a)

#define XEN_ULONG_LONG_P(Arg)           XEN_ULONG_P(Arg)
#define XEN_TO_C_ULONG_LONG(Arg)        fth_ulong_long_ref(Arg)
#define C_TO_XEN_ULONG_LONG(Arg)        fth_make_ulong_long((unsigned long long)Arg)

#define C_TO_XEN_LONG_LONG(a)           fth_make_long_long(a)
#define XEN_TO_C_LONG_LONG(a)           fth_long_long_ref(a)

#define XEN_DOUBLE_P(Arg)               FTH_FLOAT_P(Arg)
#define C_TO_XEN_DOUBLE(a)              fth_make_float(a)
#define XEN_TO_C_DOUBLE(a)              fth_float_ref(a)

#if HAVE_COMPLEX_NUMBERS
# define XEN_COMPLEX_P(Arg)             FTH_NUMBER_P(Arg)
# define C_TO_XEN_COMPLEX(a)            fth_make_complex(a)
# define XEN_TO_C_COMPLEX(a)            fth_complex_ref(a)
# define XEN_HAVE_COMPLEX_NUMBERS 1
#else
# define XEN_COMPLEX_P(Arg)             false
# define C_TO_XEN_COMPLEX(a)            XEN_ZERO
# define XEN_TO_C_COMPLEX(a)            0.0
#endif

#if HAVE_MAKE_RATIO
# define XEN_HAVE_RATIOS                    true
# define XEN_RATIO_P(Arg)               FTH_RATIO_P(Arg)
# define XEN_MAKE_RATIO(Num, Den)       fth_make_ratio(Num, Den)
# define XEN_NUMERATOR(Arg)             XEN_TO_C_LONG_LONG(fth_numerator(Arg))
# define XEN_DENOMINATOR(Arg)           XEN_TO_C_LONG_LONG(fth_denominator(Arg))
# define XEN_RATIONALIZE(Arg1, Arg2)    fth_rationalize(Arg1, Arg2)
#endif

/* === String, Symbol, Keyword, Eval === */
#define XEN_CHAR_P(Arg)                 FTH_CHAR_P(Arg)
#define C_TO_XEN_CHAR(Arg)              CHAR_TO_FTH(Arg)
#define XEN_TO_C_CHAR(Arg)              FTH_TO_CHAR(Arg)

#define XEN_STRING_P(Arg)               FTH_STRING_P(Arg)
#define C_TO_XEN_STRING(str)            fth_make_string(str)
#define C_TO_XEN_STRINGN(str, len)      fth_make_string_len(str, len)
#define XEN_TO_C_STRING(Str)            fth_string_ref(Str)

#if HAVE_FTH_PORT_PUTS
/* port = XEN_FALSE means default output handler (snd-print). */
#define XEN_PUTS(Str, Port)             fth_port_puts(Port, Str)
#define XEN_DISPLAY(Val, Port)          fth_port_display(Port, Val)
#define XEN_FLUSH_PORT(Port)            fth_port_flush(Port)
#define XEN_CLOSE_PORT(Port)            fth_port_close(Port)
#define XEN_PORT_TO_STRING(Port)        fth_port_to_string(Port)
#endif

#define XEN_TO_STRING(Obj)              fth_object_to_string(Obj)

#define XEN_SYMBOL_P(Arg)               FTH_SYMBOL_P(Arg)
#define C_STRING_TO_XEN_SYMBOL(a)       fth_symbol(a)
#define XEN_SYMBOL_TO_C_STRING(Sym)     fth_symbol_ref(Sym)

#define XEN_KEYWORD_P(Obj)              FTH_KEYWORD_P(Obj)
#define XEN_MAKE_KEYWORD(arg)           fth_keyword(arg)
#define XEN_KEYWORD_EQ_P(K1, K2)        XEN_EQ_P(K1, K2)

#define XEN_EVAL_C_STRING(arg)          fth_eval(arg)
#define XEN_LOAD_FILE(a)                fth_load_file(a)
#define XEN_LOAD_PATH                   XEN_NAME_AS_C_STRING_TO_VALUE("*load-path*")
#define XEN_ADD_TO_LOAD_PATH(Path)      fth_add_load_path(Path)

/* === Vector (Array) === */
#define XEN_MAKE_VECTOR(Num, Fill)      fth_make_array_with_init(Num, Fill)
#define XEN_VECTOR_P(Arg)               FTH_ARRAY_P(Arg)
#define XEN_VECTOR_LENGTH(Arg)          ((int)fth_array_length(Arg))
#define XEN_VECTOR_TO_LIST(Vect)        fth_array_to_list(Vect)
#define XEN_VECTOR_REF(Vect, Num)       fth_array_ref(Vect, Num)
#define XEN_VECTOR_SET(Vect, Num, Val)  fth_array_set(Vect, Num, Val)
#define XEN_VECTOR_COPY(Vect)           fth_array_copy(Vect)

/* === List === */
#define XEN_NULL_P(a)                   FTH_NIL_P(a)
#define XEN_LIST_P(Arg)                 FTH_LIST_P(Arg)
#define XEN_CONS_P(Arg)                 FTH_CONS_P(Arg)
#define XEN_PAIR_P(Arg)                 FTH_PAIR_P(Arg)
#define XEN_CONS(Arg1, Arg2)            fth_cons(Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)    fth_cons_2(Arg1, Arg2, Arg3)
#define XEN_LIST_REF(Lst, Num)          fth_list_ref(Lst, Num)
#define XEN_LIST_SET(Lst, Num, Val)     fth_list_set(Lst, Num, Val)
#define XEN_LIST_REVERSE(Lst)           fth_list_reverse(Lst)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) ((Len = XEN_LIST_LENGTH(Arg)) >= 0)
#define XEN_LIST_LENGTH(Arg)            ((int)fth_list_length(Arg))
#define XEN_LIST_1(a)                   FTH_LIST_1(a)
#define XEN_LIST_2(a, b)                FTH_LIST_2(a, b)
#define XEN_LIST_3(a, b, c)             FTH_LIST_3(a, b, c)
#define XEN_LIST_4(a, b, c, d)          FTH_LIST_4(a, b, c, d)
#define XEN_LIST_5(a, b, c, d, e)       FTH_LIST_5(a, b, c, d, e)
#define XEN_LIST_6(a, b, c, d, e, f)    FTH_LIST_6(a, b, c, d, e, f)
#define XEN_LIST_7(a, b, c, d, e, f, g) FTH_LIST_7(a, b, c, d, e, f, g)
#define XEN_LIST_8(a, b, c, d, e, f, g, h)    FTH_LIST_8(a, b, c, d, e, f, g, h)
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) FTH_LIST_9(a, b, c, d, e, f, g, h, i)
#define XEN_CAR(a)                      fth_car(a)
#define XEN_CADR(a)                     FTH_CADR(a)
#define XEN_CADDR(a)                    FTH_CADDR(a)
#define XEN_CADDDR(a)                   FTH_CADDDR(a)
#define XEN_CDR(a)                      fth_cdr(a)
#define XEN_CDDR(a)                     FTH_CDDR(a)
#define XEN_CDDDR(a)                    FTH_CDDDR(a)
#define XEN_COPY_ARG(Lst)               fth_list_copy(Lst)
#define XEN_APPEND(a, b)                fth_list_append(XEN_LIST_2(a, b))
#define XEN_ASSOC_REF(Item, Lst)        fth_list_assoc_ref(Lst, Item)
#define XEN_ASSOC_SET(Sym, Val, Lst)    fth_list_assoc_set(Lst, Sym, Val)
#define XEN_ASSOC(Item, Lst)            fth_list_assoc_ref(Lst, Item)  /* perhaps fth_list_assoc? */
#define XEN_MEMBER(Item, Lst)           fth_list_member_p(Lst, Item)

/* === Hook, Procedure === */
#define XEN_HOOK_P(Arg)                 FTH_HOOK_P(Arg)
#define XEN_HOOKED(a)                   (!fth_hook_empty_p(a))
#define XEN_DEFINE_HOOK(name, descr, arity, help) fth_make_hook(name, arity, help)
#define XEN_DEFINE_SIMPLE_HOOK(descr, arity) fth_make_simple_hook(arity)
#define XEN_CLEAR_HOOK(Arg)             fth_hook_clear(Arg)
#define XEN_HOOK_PROCEDURES(Obj)        fth_hook_procedure_list(Obj)
#define XEN_ADD_HOOK(Hook, Func, Name, Doc)  fth_add_hook(Hook, (FTH)fth_define_procedure(Name, Func, fth_hook_arity(Hook), 0, false, Doc))

#define XEN_PROCEDURE_P(Arg)            FTH_PROC_P(Arg)
#define XEN_PROCEDURE_NAME(Func)        C_TO_XEN_STRING(fth_proc_name(Func))
#define XEN_PROCEDURE_HELP(Name)        fth_documentation_ref(Name)
#define XEN_ARITY(Func)                 INT_TO_FIX(XEN_REQUIRED_ARGS(Func))
#define XEN_REQUIRED_ARGS(Func)         fth_proc_arity(Func)
#define XEN_REQUIRED_ARGS_OK(Func, args) (XEN_REQUIRED_ARGS(Func) == (args))

#define XEN_CALL_0(Func, Caller)                    fth_proc_call(Func, Caller, 0)
#define XEN_CALL_1(Func, Arg1, Caller)              fth_proc_call(Func, Caller, 1, Arg1)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller)        fth_proc_call(Func, Caller, 2, Arg1, Arg2)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller)  fth_proc_call(Func, Caller, 3, Arg1, Arg2, Arg3)
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) \
  fth_proc_call(Func, Caller, 4, Arg1, Arg2, Arg3, Arg4)
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) \
  fth_proc_call(Func, Caller, 5, Arg1, Arg2, Arg3, Arg4, Arg5)
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) \
  fth_proc_call(Func, Caller, 6, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6)
#define XEN_APPLY(Func, Args, Caller)               fth_proc_apply(Func, Args, Caller)
#define XEN_CALL_0_NO_CATCH(Func)                   XEN_CALL_0(Func, NULL)
#define XEN_CALL_1_NO_CATCH(Func, Arg1)             XEN_CALL_1(Func, Arg1, NULL)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2)       XEN_CALL_2(Func, Arg1, Arg2, NULL)
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) XEN_CALL_3(Func, Arg1, Arg2, Arg3, NULL)
#define XEN_APPLY_NO_CATCH(Func, Args)              XEN_APPLY(Func, Args, NULL)

/* === Define === */
#define XEN_DEFINE(name, Value)                fth_define(name, Value)
#define XEN_DEFINE_CONSTANT(name, Value, help) fth_define_constant(name, Value, help)
#define XEN_DEFINE_VARIABLE(name, Var, Value)  (Var = fth_define_variable(name, Value, NULL))
#define XEN_VARIABLE_SET(name, Value)          fth_variable_set((char *)(name), Value)
#define XEN_VARIABLE_REF(name)                 fth_variable_ref((char *)(name))
#define XEN_NAME_AS_C_STRING_TO_VARIABLE(name) fth_word_ref((char *)(name))
#define XEN_NAME_AS_C_STRING_TO_VALUE(name)    XEN_VARIABLE_REF(name)

#ifdef __cplusplus
# define XEN_PROCEDURE_CAST (XEN (*)())
#else
# define XEN_PROCEDURE_CAST
#endif

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  fth_define_procedure(Name, XEN_PROCEDURE_CAST Func, ReqArg, OptArg, RstArg, Doc)

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  do { \
    XEN_DEFINE_PROCEDURE(Get_Name, XEN_PROCEDURE_CAST Get_Func, Get_Req, Get_Opt, 0, Get_Help); \
    XEN_DEFINE_PROCEDURE(Set_Name, XEN_PROCEDURE_CAST Set_Func, Set_Req, Set_Opt, 0, Get_Help); \
  } while (0)

#define XEN_DEFINE_SAFE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)

/* === Object === */
#define XEN_OBJECT_TYPE                 FTH
#define XEN_MARK_OBJECT_TYPE            void

#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, Mark, Free) return(fth_make_instance(Tag, Val))
#define XEN_MAKE_OBJECT(Tag, Val, Mark, Free) fth_make_instance(Tag, Val)

#define XEN_OBJECT_TYPE_P(Obj, Tag)     fth_object_is_instance_of(Obj, Tag)
#define XEN_OBJECT_REF(Obj)             fth_instance_ref_gen(Obj)
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz)  fth_make_object_type(Typ)
#define XEN_OBJECT_HELP(Name)           fth_documentation_ref(Name)

#define XEN_PROTECT_FROM_GC(Obj)        fth_gc_protect(Obj)
#define XEN_UNPROTECT_FROM_GC(Obj)      fth_gc_unprotect(Obj)

#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) \
  static XEN Wrapped_Print(XEN obj) \
  { \
    char * str = Original_Print((Type *)XEN_OBJECT_REF(obj)); \
    XEN val = C_TO_XEN_STRING(str); \
    free(str); \
    return val; \
  }

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) \
  static void Wrapped_Free(XEN obj) \
  { \
    Original_Free((Type *)XEN_OBJECT_REF(obj)); \
  }

/* === Error === */
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) \
  FTH_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_ERROR_TYPE(Typ)             fth_exception(Typ)

#define XEN_ERROR(Type, Info)           fth_throw_list(Type, Info)
#define XEN_THROW(Type, Info)           XEN_ERROR(Type, Info)

#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr) \
  FTH_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr)
#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) \
  FTH_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr)

typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);

#define XEN_NARGIFY_0(OutName, InName) static XEN (*OutName)(void) = InName;
#define XEN_NARGIFY_1(OutName, InName) static XEN (*OutName)(XEN a1) = InName;
#define XEN_NARGIFY_2(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2) = InName;
#define XEN_NARGIFY_3(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2, XEN a3) = InName;
#define XEN_NARGIFY_4(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4) = InName;
#define XEN_NARGIFY_5(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5) = InName;
#define XEN_NARGIFY_6(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6) = InName;
#define XEN_NARGIFY_7(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6, XEN a7) = InName;
#define XEN_NARGIFY_8(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6, XEN a7, XEN a8) = InName;
#define XEN_NARGIFY_9(OutName, InName) static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6, XEN a7, XEN a8, XEN a9) = InName;
#define XEN_ARGIFY_1(OutName, InName)  static XEN (*OutName)(XEN a1) = InName;
#define XEN_ARGIFY_2(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2) = InName;
#define XEN_ARGIFY_3(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2, XEN a3) = InName;
#define XEN_ARGIFY_4(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4) = InName;
#define XEN_ARGIFY_5(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5) = InName;
#define XEN_ARGIFY_6(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6) = InName;
#define XEN_ARGIFY_7(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6, XEN a7) = InName;
#define XEN_ARGIFY_8(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6, XEN a7, XEN a8) = InName;
#define XEN_ARGIFY_9(OutName, InName)  static XEN (*OutName)(XEN a1, XEN a2, XEN a3, XEN a4, XEN a5, XEN a6, XEN a7, XEN a8, XEN a9) = InName;
#define XEN_VARGIFY(OutName, InName)   static XEN (*OutName)(XEN a1) = InName;

#endif /* end HAVE_FORTH */


/* ------------------------------ s7 ------------------------------ */

#if HAVE_SCHEME

#define XEN_OK 1

#include "s7.h"


#ifdef __cplusplus
extern "C" {
#endif
extern s7_scheme *s7;  /* s7 is a pointer to the current scheme */
#ifdef __cplusplus
}
#endif


#define XEN                                        s7_pointer
#define XEN_FILE_EXTENSION                         "scm"
#define XEN_LANGUAGE_NAME                          "s7"
#define XEN_COMMENT_STRING                         ";"

extern XEN xen_false, xen_true, xen_nil, xen_undefined, xen_zero;
extern size_t xen_s7_number_location, xen_s7_denominator_location;

#define XEN_FALSE                                  xen_false
#define XEN_TRUE                                   xen_true
#define XEN_TRUE_P(Arg)                            ((Arg) == XEN_TRUE)  /* not scheme-wise, but Snd-wise (#t as special arg) */
#define XEN_FALSE_P(Arg)                           ((Arg) == XEN_FALSE)
#define XEN_BOOLEAN_P(Arg)                         s7_is_boolean(Arg)
#define C_TO_XEN_BOOLEAN(Arg)                      ((Arg) ? XEN_TRUE : XEN_FALSE)
#define XEN_TO_C_BOOLEAN(Arg)                      ((XEN_TRUE_P(Arg)) ? true : false)

#define XEN_NULL_P(Arg)                            ((Arg) == xen_nil)
#define XEN_BOUND_P(Arg)                           ((Arg) != xen_undefined)
#define XEN_EMPTY_LIST                             xen_nil
#define XEN_UNDEFINED                              xen_undefined
#define XEN_EQ_P(Arg1, Arg2)                       ((Arg1) == (Arg2))

#define XEN_CONS_P(Arg)                            s7_cons_p(Arg)
#define XEN_CONS(Arg1, Arg2)                       s7_cons(s7, Arg1, Arg2)
#define XEN_CONS_2(Arg1, Arg2, Arg3)               s7_cons(s7, Arg1, s7_cons(s7, Arg2, Arg3))
#define XEN_PAIR_P(Arg)                            s7_is_pair(Arg)
#define XEN_CAR(Arg)                               s7_car(Arg)
#define XEN_CDR(Arg)                               s7_cdr(Arg)
#define XEN_CADR(Arg)                              s7_cadr(Arg)
#define XEN_CADDR(Arg)                             s7_caddr(Arg)
#define XEN_CADDDR(Arg)                            s7_cadddr(Arg)
#define XEN_CDDR(Arg)                              s7_cddr(Arg)
#define XEN_CDDDR(Arg)                             s7_cdddr(Arg)
#define XEN_LIST_P(Arg)                            s7_is_list(s7, Arg) /* not pair? because we want '() to return #t here */
#define XEN_LIST_LENGTH(Arg)                       s7_list_length(s7, Arg)
#define XEN_LIST_P_WITH_LENGTH(Arg, Len)           ((s7_is_list(s7, Arg)) && ((Len = XEN_LIST_LENGTH(Arg)) >= 0))
#define XEN_LIST_1(a)                              s7_list(s7, 1, a)
#define XEN_LIST_2(a, b)                           s7_list(s7, 2, a, b)
#define XEN_LIST_3(a, b, c)                        s7_list(s7, 3, a, b, c)
#define XEN_LIST_4(a, b, c, d)                     s7_list(s7, 4, a, b, c, d)
#define XEN_LIST_5(a, b, c, d, e)                  s7_list(s7, 5, a, b, c, d, e)
#define XEN_LIST_6(a, b, c, d, e, f)               s7_list(s7, 6, a, b, c, d, e, f)
#define XEN_LIST_7(a, b, c, d, e, f, g)            s7_list(s7, 7, a, b, c, d, e, f, g)
#define XEN_LIST_8(a, b, c, d, e, f, g, h)         s7_list(s7, 8, a, b, c, d, e, f, g, h)
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i)      s7_list(s7, 9, a, b, c, d, e, f, g, h, i)
#define XEN_LIST_REF(Lst, Num)                     s7_list_ref(s7, Lst, Num)
#define XEN_LIST_SET(Lst, Num, Val)                s7_list_set(s7, Lst, Num, Val)
#define XEN_LIST_REVERSE(Lst)                      s7_reverse(s7, Lst)
#define XEN_COPY_ARG(Lst)                          Lst
#define XEN_APPEND(Arg1, Arg2)                     s7_append(s7, Arg1, Arg2)
#define XEN_ASSOC_REF(Sym, Lst)                    xen_assoc(s7, Sym, Lst)
#define XEN_ASSOC_SET(Sym, Val, Lst)               xen_set_assoc(s7, Sym, Val, Lst)
#define XEN_ASSOC(Sym, Lst)                        s7_assoc(s7, Sym, Lst)
#define XEN_MEMBER(Sym, Lst)                       s7_member(s7, Sym, Lst)

#define XEN_STRING_P(Arg)                          s7_is_string(Arg)
#define XEN_NAME_AS_C_STRING_TO_VALUE(Arg)         s7_name_to_value(s7, Arg)
#define XEN_TO_C_STRING(Str)                       s7_string(Str)
#define C_TO_XEN_STRING(Str)                       s7_make_string(s7, Str)
#define C_TO_XEN_STRINGN(Str, Len)                 s7_make_string_with_length(s7, Str, Len)

#define XEN_ZERO                                   xen_zero
#define XEN_INTEGER_P(Arg)                         s7_is_integer(Arg)
#define C_TO_XEN_INT(Arg)                          s7_make_integer(s7, Arg)
#define XEN_TO_C_INT(Arg)                          s7_integer(Arg)

#define XEN_ULONG_P(Arg)                           s7_is_integer(Arg)
#define XEN_TO_C_ULONG(Arg)                        (uint64_t)s7_integer(Arg)
#define C_TO_XEN_ULONG(Arg)                        s7_make_integer(s7, (s7_int)((intptr_t)Arg))

#define XEN_ULONG_LONG_P(Arg)                      s7_is_c_pointer(Arg)
#define XEN_TO_C_ULONG_LONG(Arg)                   (uint64_t)s7_c_pointer(Arg)
#define C_TO_XEN_ULONG_LONG(Arg)                   s7_make_c_pointer(s7, (void *)Arg)

#define C_TO_XEN_LONG_LONG(Arg)                    s7_make_integer(s7, Arg)
#define XEN_TO_C_LONG_LONG(Arg)                    s7_integer(Arg)

#define XEN_NUMBER_P(Arg)                          s7_is_real(Arg)
#define XEN_WRAPPED_C_POINTER_P(Arg)               s7_is_c_pointer(Arg)

#define XEN_DOUBLE_P(Arg)                          s7_is_real(Arg)
#define XEN_TO_C_DOUBLE(Arg)                       ((double)s7_number_to_real(s7, Arg))
#define C_TO_XEN_DOUBLE(Arg)                       s7_make_real(s7, Arg)

#if HAVE_COMPLEX_NUMBERS
  #define XEN_HAVE_COMPLEX_NUMBERS                 1
  #define XEN_COMPLEX_P(Arg)                       s7_is_complex(Arg)
  #define XEN_TO_C_COMPLEX(a)                      (s7_real_part(a) + s7_imag_part(a) * _Complex_I)
  #define C_TO_XEN_COMPLEX(a)                      s7_make_complex(s7, creal(a), cimag(a))
#else
  #define XEN_HAVE_COMPLEX_NUMBERS                 0
  #define XEN_COMPLEX_P(Arg)                       false
  #define XEN_TO_C_COMPLEX(a)                      0.0
  #define C_TO_XEN_COMPLEX(a)                      XEN_ZERO
#endif

#define XEN_HAVE_RATIOS                            1
#define XEN_NUMERATOR(Arg)                         s7_numerator(Arg)
#define XEN_DENOMINATOR(Arg)                       s7_denominator(Arg)
#define XEN_RATIONALIZE(Arg1, Arg2)                s7_rationalize(s7, XEN_TO_C_DOUBLE(Arg1), XEN_TO_C_DOUBLE(Arg2))
#define XEN_RATIO_P(Arg)                           s7_is_ratio(Arg)
#define XEN_MAKE_RATIO(Num, Den)                   s7_make_ratio(s7, XEN_TO_C_INT(Num), XEN_TO_C_INT(Den))

#define XEN_EVAL_C_STRING(Arg)                     s7_eval_c_string(s7, Arg)
#define XEN_TO_STRING(Obj)                         s7_object_to_string(s7, Obj, false)

#define XEN_SYMBOL_TO_C_STRING(Arg)                s7_symbol_name(Arg)
#define XEN_SYMBOL_P(Arg)                          s7_is_symbol(Arg)
#define C_STRING_TO_XEN_SYMBOL(Arg)                s7_make_symbol(s7, Arg)
#define XEN_DOCUMENTATION_SYMBOL                   C_STRING_TO_XEN_SYMBOL("documentation")
#define XEN_SET_DOCUMENTATION(Var, Doc)

#define XEN_VECTOR_P(Arg)                          s7_is_vector(Arg)
#define XEN_VECTOR_LENGTH(Arg)                     s7_vector_length(Arg)
#define XEN_VECTOR_REF(Vect, Num)                  s7_vector_ref(s7, Vect, Num)
#define XEN_VECTOR_SET(Vect, Num, Val)             s7_vector_set(s7, Vect, Num, Val)
#define XEN_MAKE_VECTOR(Num, Fill)                 s7_make_and_fill_vector(s7, Num, Fill)
#define XEN_VECTOR_TO_LIST(Vect)                   s7_vector_to_list(s7, Vect)
#define XEN_VECTOR_RANK(Vect)                      s7_vector_rank(Vect)
#define XEN_VECTOR_COPY(Vect)                      s7_vector_copy(s7, Vect)
#define XEN_VECTOR_ELEMENTS(Vect)                  s7_vector_elements(Vect)

#define XEN_CHAR_P(Arg)                            s7_is_character(Arg)
#define XEN_TO_C_CHAR(Arg)                         s7_character(Arg)
#define C_TO_XEN_CHAR(Arg)                         s7_make_character(s7, Arg)

#define XEN_KEYWORD_P(Obj)                         s7_is_keyword(Obj)
#define XEN_KEYWORD_EQ_P(k1, k2)                   ((k1) == (k2))
#define XEN_MAKE_KEYWORD(Arg)                      s7_make_keyword(s7, Arg)

#define XEN_PROCEDURE_P(Arg)                       s7_is_procedure(Arg)

#define XEN_LOAD_FILE(File)                        s7_load(s7, File)
#define XEN_LOAD_PATH                              s7_load_path(s7)
#define XEN_ADD_TO_LOAD_PATH(Path)                 s7_add_to_load_path(s7, Path)

#define XEN_ERROR_TYPE(Typ)                        C_STRING_TO_XEN_SYMBOL(Typ)
#define XEN_ERROR(Type, Info)                      s7_error(s7, Type, Info)
#define XEN_THROW(Type, Info)                      s7_error(s7, Type, Info)

#define XEN_PROVIDE(Feature)                       s7_provide(s7, Feature)
#define XEN_PROTECT_FROM_GC(Arg)                   s7_gc_protect(s7, Arg)

#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr) s7_wrong_type_arg_error(s7, Caller, ArgN, Arg, Descr)
#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr)   s7_out_of_range_error(s7, Caller, ArgN, Arg, Descr)

#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type) if (!(Assertion)) XEN_WRONG_TYPE_ARG_ERROR(Caller, Position, Arg, Correct_Type)

#define XEN_NARGIFY_0(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(InName());}
#define XEN_NARGIFY_1(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(InName(XEN_CAR(args)));}
#define XEN_NARGIFY_2(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_2(s7, args, InName));}
#define XEN_NARGIFY_3(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_3(s7, args, InName));}
#define XEN_NARGIFY_4(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_4(s7, args, InName));}
#define XEN_NARGIFY_5(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_5(s7, args, InName));}
#define XEN_NARGIFY_6(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_6(s7, args, InName));}
#define XEN_NARGIFY_7(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_7(s7, args, InName));}
#define XEN_NARGIFY_8(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_8(s7, args, InName));}
#define XEN_NARGIFY_9(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_9(s7, args, InName));}

#define XEN_ARGIFY_1(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_1(s7, args, InName));}
#define XEN_ARGIFY_2(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_2(s7, args, InName));}
#define XEN_ARGIFY_3(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_3(s7, args, InName));}
#define XEN_ARGIFY_4(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_4(s7, args, InName));}
#define XEN_ARGIFY_5(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_5(s7, args, InName));}
#define XEN_ARGIFY_6(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_6(s7, args, InName));}
#define XEN_ARGIFY_7(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_7(s7, args, InName));}
#define XEN_ARGIFY_8(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_8(s7, args, InName));}
#define XEN_ARGIFY_9(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(s7_apply_n_9(s7, args, InName));}
#define XEN_VARGIFY(OutName, InName) static s7_pointer OutName(s7_scheme *sc, s7_pointer args) {return(InName(args));}


#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) s7_define_function(s7, Name, Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_SAFE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) s7_define_safe_function(s7, Name, Func, ReqArg, OptArg, RstArg, Doc)
#define XEN_DEFINE_PROCEDURE_STAR(Name, Func, Args, Doc)              s7_define_function_star(s7, Name, Func, Args, Doc)

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  s7_dilambda(s7, Get_Name, Get_Func, Get_Req, Get_Opt, Set_Func, Set_Req, Set_Opt, Get_Help)

#define XEN_ARITY(Func)                                               s7_arity(s7, Func)
#define XEN_REQUIRED_ARGS(Func)                                       XEN_TO_C_INT(XEN_CAR(XEN_ARITY(Func)))
#define XEN_REQUIRED_ARGS_OK(Func, Args)                              s7_is_aritable(s7, Func, Args) /* (XEN_REQUIRED_ARGS(Func) == Args) */

#define XEN_CALL_0(Func, Caller)                                      s7_call_with_location(s7, Func, XEN_EMPTY_LIST, Caller, __FILE__, __LINE__) /* these need a catch */
#define XEN_CALL_1(Func, Arg1, Caller)                                s7_call_with_location(s7, Func, XEN_LIST_1(Arg1), Caller, __FILE__, __LINE__)
#define XEN_CALL_2(Func, Arg1, Arg2, Caller)                          s7_call_with_location(s7, Func, XEN_LIST_2(Arg1, Arg2), Caller, __FILE__, __LINE__)
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller)                    s7_call_with_location(s7, Func, XEN_LIST_3(Arg1, Arg2, Arg3), Caller, __FILE__, __LINE__)
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller)              s7_call_with_location(s7, Func, XEN_LIST_4(Arg1, Arg2, Arg3, Arg4), Caller, __FILE__, __LINE__)
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller)        s7_call_with_location(s7, Func, XEN_LIST_5(Arg1, Arg2, Arg3, Arg4, Arg5), Caller, __FILE__, __LINE__)
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller)  s7_call_with_location(s7, Func, XEN_LIST_6(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), Caller, __FILE__, __LINE__)
#define XEN_APPLY(Func, Args, Caller)                                 s7_call_with_location(s7, Func, Args, Caller, __FILE__, __LINE__)

#define XEN_CALL_0_NO_CATCH(Func)                                     s7_call_with_location(s7, Func, XEN_EMPTY_LIST, __func__, __FILE__, __LINE__)
#define XEN_CALL_1_NO_CATCH(Func, Arg1)                               s7_call_with_location(s7, Func, XEN_LIST_1(Arg1), __func__, __FILE__, __LINE__)
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2)                         s7_call_with_location(s7, Func, XEN_LIST_2(Arg1, Arg2), __func__, __FILE__, __LINE__)
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3)                   s7_call_with_location(s7, Func, XEN_LIST_3(Arg1, Arg2, Arg3), __func__, __FILE__, __LINE__)
#define XEN_APPLY_NO_CATCH(Func, Args)                                s7_call_with_location(s7, Func, Args, __func__, __FILE__, __LINE__)
typedef XEN (*XEN_CATCH_BODY_TYPE)                                    (void *data);

#define XEN_DEFINE_CONSTANT(Name, Value, Help)                        s7_define_constant_with_documentation(s7, Name, s7_make_integer(s7, Value), Help)
#define XEN_DEFINE(Name, Value)                                       s7_define_variable(s7, Name, Value)
#define XEN_DEFINED_P(Name)                                           s7_is_defined(s7, Name)

#define XEN_DEFINE_VARIABLE(Name, Var, Value)                         Var = s7_define_variable(s7, Name, Value)
#define XEN_VARIABLE_SET(Var, Val)                                    s7_symbol_set_value(s7, Var, Val)
#define XEN_VARIABLE_REF(Var)                                         s7_symbol_value(s7, Var)
#define XEN_NAME_AS_C_STRING_TO_VARIABLE(a)                           s7_make_symbol(s7, a)

#define XEN_MARK_OBJECT_TYPE                                          void
#define XEN_MAKE_OBJECT_TYPE(Name, Size) s7_make_c_type(s7, Name)

#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free) static void Wrapped_Free(void *obj) {Original_Free((Type *)obj);}
#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print) static char *Wrapped_Print(s7_scheme *sc, void *obj) {return(Original_Print((Type *)obj));}
#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, ig1, ig2)                return(s7_make_c_object(s7, Tag, Val))
#define XEN_MAKE_OBJECT(Tag, Val, ig1, ig2)                           s7_make_c_object(s7, Tag, Val)
#define XEN_OBJECT_REF(Arg)                                           s7_c_object_value(Arg)
#define XEN_OBJECT_TYPE                                               s7_int /* tag type */
#define XEN_OBJECT_TYPE_P(Obj, Tag)                                   (s7_c_object_type(Obj) == Tag)

#define XEN_HOOK_P(Arg)                                               ((Arg) != XEN_FALSE)
#define XEN_DEFINE_HOOK(Name, Descr, Arity, Help)                     s7_define_constant_with_documentation(s7, Name, s7_eval_c_string(s7, Descr), Help)
/* "simple hooks are for channel-local hooks (unnamed, accessed through the channel) */
#define XEN_DEFINE_SIMPLE_HOOK(Descr, Arity)                          s7_eval_c_string(s7, Descr)
#define XEN_HOOKED(Hook)                                              s7_is_pair(s7_hook_functions(s7, Hook))
#define XEN_CLEAR_HOOK(Hook)                                          s7_hook_set_functions(s7, Hook, s7_nil(s7))
#define XEN_HOOK_PROCEDURES(Hook)                                     s7_hook_functions(s7, Hook)
#define XEN_ADD_HOOK(Hook, Func, Name, Doc)                           s7_hook_set_functions(s7, Hook, s7_cons(s7, s7_make_function(s7, Name, Func, 1, 0, false, Doc), s7_hook_functions(s7, Hook)))

#ifdef __cplusplus
extern "C" {
#endif

s7_scheme *s7_xen_initialize(s7_scheme *sc);
void xen_s7_set_repl_prompt(const char *new_prompt);
XEN xen_set_assoc(s7_scheme *sc, s7_pointer key, s7_pointer val, s7_pointer alist);
XEN xen_assoc(s7_scheme *sc, XEN key, XEN alist);

#ifdef __cplusplus
}
#endif

#endif
/* end s7 */





/* ------------------------------ NO EXTENSION LANGUAGE ------------------------------ */

#ifndef XEN_OK

#define XEN int
#define XEN_FILE_EXTENSION  "txt"
#define XEN_LANGUAGE_NAME "What Language?"
#define XEN_COMMENT_STRING  ";"
#define XEN_FALSE 0
#define XEN_TRUE 1
#define XEN_TRUE_P(a) ((a) == XEN_TRUE)
#define XEN_FALSE_P(a) ((a) == XEN_FALSE)
#define XEN_BOOLEAN_P(Arg) 0
#define C_TO_XEN_BOOLEAN(a) 0
#define XEN_TO_C_BOOLEAN(a) 0
#define XEN_NULL_P(a) ((a) == XEN_EMPTY_LIST)
#define XEN_BOUND_P(Arg) 0
#define XEN_EMPTY_LIST 0
#define XEN_UNDEFINED 0
#define XEN_EQ_P(a, b) 0
#define XEN_CONS_P(Arg) 0
#define XEN_CONS(Arg1, Arg2) 0
#define XEN_CONS_2(Arg1, Arg2, Arg3) 0
#define XEN_PAIR_P(Arg) 0
#define XEN_CAR(a) 0
#define XEN_CADR(a) 0
#define XEN_CADDR(a) 0
#define XEN_CADDDR(a) 0
#define XEN_CDR(a) 0
#define XEN_CDDR(a) 0
#define XEN_CDDDR(a) 0
#define XEN_LIST_P(Arg) 0
#define XEN_LIST_P_WITH_LENGTH(Arg, Len) 0
#define XEN_LIST_LENGTH(Arg) 0
#define XEN_LIST_1(a) 0
#define XEN_LIST_2(a, b) 0
#define XEN_LIST_3(a, b, c) 0
#define XEN_LIST_4(a, b, c, d) 0
#define XEN_LIST_5(a, b, c, d, e) 0
#define XEN_LIST_6(a, b, c, d, e, f) 0
#define XEN_LIST_7(a, b, c, d, e, f, g) 0
#define XEN_LIST_8(a, b, c, d, e, f, g, h) 0
#define XEN_LIST_9(a, b, c, d, e, f, g, h, i) 0
#define XEN_LIST_REF(Lst, Num) 0
#define XEN_LIST_SET(Lst, Num, Val)
#define XEN_LIST_REVERSE(Lst) 0
#define XEN_COPY_ARG(Lst) Lst
#define XEN_APPEND(X, Y) 0
#define XEN_STRING_P(Arg) 0
#define XEN_NAME_AS_C_STRING_TO_VALUE(a) 0
#define XEN_TO_C_STRING(STR) "(not a string)"
#define C_TO_XEN_STRING(a) 0
#define C_TO_XEN_STRINGN(Str, Len) 0
#define C_STRING_TO_XEN_SYMBOL(a) 0
#define XEN_ZERO 0
#define XEN_NUMBER_P(Arg) 0
#define XEN_DOUBLE_P(Arg) 0
#define XEN_TO_C_DOUBLE(a) 0.0
#define C_TO_XEN_DOUBLE(a) 0
#define XEN_INTEGER_P(Arg) 0
#define C_TO_XEN_INT(a) a
#define XEN_TO_C_INT(a) 0
#define XEN_COMPLEX_P(Arg) 0
#define XEN_TO_C_COMPLEX(a) 0.0
#define C_TO_XEN_COMPLEX(a) a
#define XEN_ULONG_P(Arg) 0
#define XEN_TO_C_ULONG(a) 0
#define C_TO_XEN_ULONG(a) 0
#define C_TO_XEN_LONG_LONG(a) a
#define XEN_TO_C_LONG_LONG(a) a
#define XEN_ULONG_LONG_P(Arg) 0
#define XEN_TO_C_ULONG_LONG(Arg) 0
#define C_TO_XEN_ULONG_LONG(Arg) 0
#define XEN_WRAPPED_C_POINTER_P(Arg) 0
#define XEN_EVAL_C_STRING(Arg) 0
#define XEN_SYMBOL_TO_C_STRING(a) "(not a symbol)"
#define XEN_TO_STRING(Obj) "(unknown)"
#define XEN_PROCEDURE_P(Arg) 0

#define XEN_ARGIFY_1(OutName, InName) static int OutName(void) {return(-1);}
#define XEN_ARGIFY_2(OutName, InName) static int OutName(void) {return(-2);}
#define XEN_ARGIFY_3(OutName, InName) static int OutName(void) {return(-3);}
#define XEN_ARGIFY_4(OutName, InName) static int OutName(void) {return(-4);}
#define XEN_ARGIFY_5(OutName, InName) static int OutName(void) {return(-5);}
#define XEN_ARGIFY_6(OutName, InName) static int OutName(void) {return(-6);}
#define XEN_ARGIFY_7(OutName, InName) static int OutName(void) {return(-7);}
#define XEN_ARGIFY_8(OutName, InName) static int OutName(void) {return(-8);}
#define XEN_ARGIFY_9(OutName, InName) static int OutName(void) {return(-9);}

#define XEN_NARGIFY_0(OutName, InName) static int OutName(void) {return(0);}
#define XEN_NARGIFY_1(OutName, InName) static int OutName(void) {return(1);}
#define XEN_NARGIFY_2(OutName, InName) static int OutName(void) {return(2);}
#define XEN_NARGIFY_3(OutName, InName) static int OutName(void) {return(3);}
#define XEN_NARGIFY_4(OutName, InName) static int OutName(void) {return(4);}
#define XEN_NARGIFY_5(OutName, InName) static int OutName(void) {return(5);}
#define XEN_NARGIFY_6(OutName, InName) static int OutName(void) {return(6);}
#define XEN_NARGIFY_7(OutName, InName) static int OutName(void) {return(7);}
#define XEN_NARGIFY_8(OutName, InName) static int OutName(void) {return(8);}
#define XEN_NARGIFY_9(OutName, InName) static int OutName(void) {return(9);}

#define XEN_VARGIFY(OutName, InName) static int OutName(void) {return(-100);}

#define XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) \
  xen_no_ext_lang_check_args(Name, Func(), ReqArg, OptArg, RstArg)

#define XEN_DEFINE_PROCEDURE_WITH_SETTER(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt) \
  {xen_no_ext_lang_check_args(Get_Name, Get_Func(), Get_Req, Get_Opt, 0); xen_no_ext_lang_check_args(Set_Name, Set_Func(), Set_Req, Set_Opt, 0);}

#define XEN_DEFINE_SAFE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc) XEN_DEFINE_PROCEDURE(Name, Func, ReqArg, OptArg, RstArg, Doc)

#define XEN_ARITY(Func) 0
#define XEN_REQUIRED_ARGS(Func) 0
#define XEN_REQUIRED_ARGS_OK(Func, Args) false
#define XEN_CALL_0(Func, Caller) 0
#define XEN_CALL_1(Func, Arg1, Caller) 0
#define XEN_CALL_2(Func, Arg1, Arg2, Caller) 0
#define XEN_CALL_3(Func, Arg1, Arg2, Arg3, Caller) 0
#define XEN_CALL_4(Func, Arg1, Arg2, Arg3, Arg4, Caller) 0
#define XEN_CALL_5(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Caller) 0
#define XEN_CALL_6(Func, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Caller) 0
#define XEN_APPLY(Func, Args, Caller) 0
#define XEN_CALL_0_NO_CATCH(Func) 0
#define XEN_CALL_1_NO_CATCH(Func, Arg1) 0
#define XEN_CALL_2_NO_CATCH(Func, Arg1, Arg2) 0
#define XEN_CALL_3_NO_CATCH(Func, Arg1, Arg2, Arg3) 0
#define XEN_APPLY_NO_CATCH(Func, Args) 0
#define XEN_DEFINE_CONSTANT(a, b, c)
#define XEN_DEFINE_VARIABLE(a, b, c)
#define XEN_DEFINE(Name, Value)
#define XEN_VARIABLE_SET(a, b)
#define XEN_VARIABLE_REF(a) 0
#define XEN_MARK_OBJECT_TYPE         XEN
#define XEN_MAKE_OBJECT_TYPE(Typ, Siz) 0
#define XEN_MAKE_OBJECT_PRINT_PROCEDURE(Type, Wrapped_Print, Original_Print)
#define XEN_MAKE_OBJECT_FREE_PROCEDURE(Type, Wrapped_Free, Original_Free)
#define XEN_MAKE_AND_RETURN_OBJECT(Tag, Val, ig1, ig2) return(0)
#define XEN_MAKE_OBJECT(Tag, Val, ig1, ig2) 0
#define XEN_OBJECT_REF(a) 0
#define XEN_OBJECT_TYPE int
#define XEN_OBJECT_TYPE_P(OBJ, TAG) 0
#define XEN_SYMBOL_P(Arg) 0
#define XEN_HOOK_P(Arg) 0
#define XEN_HOOKED(a) 0
#define XEN_DEFINE_HOOK(Name, Descr, Arity, Help) 0
#define XEN_DEFINE_SIMPLE_HOOK(Descr, Arity) 0
#define XEN_CLEAR_HOOK(Arg)
#define XEN_HOOK_PROCEDURES(a) 0
#define XEN_ADD_HOOK(Hook, Func, Name, Doc)
#define XEN_VECTOR_P(Arg) 0
#define XEN_VECTOR_LENGTH(Arg) 0
#define XEN_VECTOR_REF(Vect, Num) 0
#define XEN_VECTOR_SET(a, b, c)
#define XEN_MAKE_VECTOR(Num, Fill) 0
#define XEN_VECTOR_TO_LIST(Vect) 0
#define XEN_ASSOC_REF(Sym, Lst) 0
#define XEN_ASSOC_SET(Sym, Val, Lst) 0
#define XEN_CHAR_P(Arg) 0
#define XEN_TO_C_CHAR(Arg) 0
#define C_TO_XEN_CHAR(Arg) 0
#define XEN_KEYWORD_P(Obj) 0
#define XEN_KEYWORD_EQ_P(k1, k2) 0
#define XEN_MAKE_KEYWORD(Arg) 0
#define XEN_PROVIDE(Feature)
#define XEN_DOCUMENTATION_SYMBOL 0
#define XEN_OBJECT_HELP(Name) 0
#define XEN_PROTECT_FROM_GC(a) 0
#define XEN_LOAD_FILE(a) 0
#define XEN_LOAD_PATH XEN_FALSE
#define XEN_ADD_TO_LOAD_PATH(Path) XEN_FALSE
#define XEN_ERROR_TYPE(Typ) XEN_FALSE
#define XEN_ERROR(Type, Info) fprintf(stderr, "error")
#define XEN_THROW(Type, Info) fprintf(stderr, "error")
#define XEN_ASSERT_TYPE(Assertion, Arg, Position, Caller, Correct_Type)
#define XEN_WRONG_TYPE_ARG_ERROR(Caller, ArgN, Arg, Descr)
#define XEN_OUT_OF_RANGE_ERROR(Caller, ArgN, Arg, Descr)
typedef XEN (*XEN_CATCH_BODY_TYPE) (void *data);
#define XEN_UNPROTECT_FROM_GC(Var) 0

#ifdef __cplusplus
extern "C" {
#endif

void xen_no_ext_lang_check_args(const char *name, int args, int req_args, int opt_args, int rst_args);

#ifdef __cplusplus
}
#endif

#endif
/* end NO EXTENSION LANGUAGE */



#define XEN_NOT_TRUE_P(a)    (!(XEN_TRUE_P(a)))
#define XEN_NOT_FALSE_P(a)   (!(XEN_FALSE_P(a)))
#define XEN_NOT_NULL_P(a)    (!(XEN_NULL_P(a)))
#define XEN_NOT_BOUND_P(Arg) (!(XEN_BOUND_P(Arg)))

#if defined(__GNUC__) && (!(defined(__cplusplus)))
  #define XEN_BOOLEAN_IF_BOUND_P(Arg)            ({ XEN _xen_h_14_ = Arg; ((XEN_BOOLEAN_P(_xen_h_14_))   || (XEN_NOT_BOUND_P(_xen_h_14_))); })
  #define XEN_INTEGER_IF_BOUND_P(Arg)            ({ XEN _xen_h_15_ = Arg; ((XEN_NOT_BOUND_P(_xen_h_15_)) || (XEN_INTEGER_P(_xen_h_15_))); })
  #define XEN_NUMBER_IF_BOUND_P(Arg)             ({ XEN _xen_h_16_ = Arg; ((XEN_NOT_BOUND_P(_xen_h_16_)) || (XEN_NUMBER_P(_xen_h_16_))); })
  #define XEN_STRING_IF_BOUND_P(Arg)             ({ XEN _xen_h_17_ = Arg; ((XEN_NOT_BOUND_P(_xen_h_17_)) || (XEN_STRING_P(_xen_h_17_))); })
  #define XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ({ XEN _xen_h_18_ = Arg; ((XEN_BOOLEAN_P(_xen_h_18_))   || (XEN_NOT_BOUND_P(_xen_h_18_)) || (XEN_INTEGER_P(_xen_h_18_))); })
  #define XEN_INTEGER_OR_BOOLEAN_P(Arg)          ({ XEN _xen_h_21_ = Arg; ((XEN_BOOLEAN_P(_xen_h_21_))   || (XEN_INTEGER_P(_xen_h_21_))); })
#else
  #define XEN_BOOLEAN_IF_BOUND_P(Arg)            ((XEN_BOOLEAN_P(Arg))   || (XEN_NOT_BOUND_P(Arg)))
  #define XEN_INTEGER_IF_BOUND_P(Arg)            ((XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
  #define XEN_NUMBER_IF_BOUND_P(Arg)             ((XEN_NOT_BOUND_P(Arg)) || (XEN_NUMBER_P(Arg)))
  #define XEN_STRING_IF_BOUND_P(Arg)             ((XEN_NOT_BOUND_P(Arg)) || (XEN_STRING_P(Arg)))
  #define XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg) ((XEN_BOOLEAN_P(Arg))   || (XEN_NOT_BOUND_P(Arg)) || (XEN_INTEGER_P(Arg)))
  #define XEN_INTEGER_OR_BOOLEAN_P(Arg)          ((XEN_BOOLEAN_P(Arg))   || (XEN_INTEGER_P(Arg)))
#endif

#if (!HAVE_FORTH)
#define XEN_LONG_LONG_P(Arg)            XEN_INTEGER_P(Arg)
#else
#define XEN_LONG_LONG_P(Arg)            FTH_LONG_LONG_P(Arg)
#endif
#define XEN_LONG_LONG_IF_BOUND_P(Arg)   ((XEN_NOT_BOUND_P(Arg)) || (XEN_LONG_LONG_P(Arg)))

#if (!HAVE_SCHEME)
  #define XEN_AS_STRING(form)           XEN_TO_C_STRING(XEN_TO_STRING(form))
  #define XEN_VECTOR_RANK(Vect)         1
#else
  #define XEN_AS_STRING(form)           s7_object_to_c_string(s7, form)
#endif


#define XEN_BAD_ARITY_ERROR(Caller, ArgN, Arg, Descr) \
  XEN_ERROR(XEN_ERROR_TYPE("bad-arity"), \
            XEN_LIST_3(C_TO_XEN_STRING(Caller), \
                       C_TO_XEN_STRING(Descr), \
                       Arg))

#ifndef XEN_HAVE_RATIOS
  #define XEN_NUMERATOR(Arg)          0
  #define XEN_DENOMINATOR(Arg)        1
  #define XEN_RATIONALIZE(Arg1, Arg2) 1
  #define XEN_RATIO_P(Arg)            false
  #define XEN_MAKE_RATIO(Num, Den)    1
#endif
#ifndef XEN_DEFINED_P
  #define XEN_DEFINED_P(Name) false
#endif

/* (need a way to pass an uninterpreted pointer from C to XEN then back to C) */
#if HAVE_SCHEME
  #define XEN_WRAP_C_POINTER(a)           s7_make_c_pointer(s7, (void *)(a))
  #define XEN_UNWRAP_C_POINTER(a)         s7_c_pointer(a)
#else
  #if (SIZEOF_VOID_P == 4)
    #define XEN_WRAP_C_POINTER(a)         ((XEN)(C_TO_XEN_ULONG((unsigned long)a)))
    #define XEN_UNWRAP_C_POINTER(a)       XEN_TO_C_ULONG(a)
  #else
    #define XEN_WRAP_C_POINTER(a)         C_TO_XEN_ULONG_LONG((uint64_t)(a))
    #define XEN_UNWRAP_C_POINTER(a)       XEN_TO_C_ULONG_LONG(a)
  #endif
#endif


/* Feb-14: the upper case macro names and the old-fashioned _p names are ugly and hard to read -- start replacing them
 */

#define Xen_is_number(Arg)               XEN_NUMBER_P(Arg)
#define Xen_is_integer(Arg)              XEN_INTEGER_P(Arg)
#define Xen_is_llong(Arg)                XEN_LONG_LONG_P(Arg)
#define Xen_is_keyword(Arg)              XEN_KEYWORD_P(Arg)
#define Xen_is_true(Arg)                 XEN_TRUE_P(Arg)
#define Xen_is_false(Arg)                XEN_FALSE_P(Arg)
#define Xen_is_bound(Arg)                XEN_BOUND_P(Arg)
#define Xen_is_boolean(Arg)              XEN_BOOLEAN_P(Arg)
#define Xen_is_null(Arg)                 XEN_NULL_P(Arg)
#define Xen_is_eq(Arg1, Arg2)            XEN_EQ_P(Arg1, Arg2)
#define Xen_is_cons(Arg)                 XEN_CONS_P(Arg)
#define Xen_is_pair(Arg)                 XEN_PAIR_P(Arg)
#define Xen_is_list(Arg)                 XEN_LIST_P(Arg)
#define Xen_is_string(Arg)               XEN_STRING_P(Arg)
#define Xen_is_double(Arg)               XEN_DOUBLE_P(Arg)
#define Xen_is_complex(Arg)              XEN_COMPLEX_P(Arg)
#define Xen_is_ulong(Arg)                XEN_ULONG_P(Arg)
#define Xen_is_ullong(Arg)               XEN_ULONG_LONG_P(Arg)
#define Xen_is_wrapped_c_pointer(Arg)    XEN_WRAPPED_C_POINTER_P(Arg)
#define Xen_is_procedure(Arg)            XEN_PROCEDURE_P(Arg)
#define Xen_c_object_is_type(Obj, Tag)   XEN_OBJECT_TYPE_P(Obj, Tag)
#define Xen_is_symbol(Arg)               XEN_SYMBOL_P(Arg)
#define Xen_is_hook(Arg)                 XEN_HOOK_P(Arg)
#define Xen_is_vector(Arg)               XEN_VECTOR_P(Arg)
#define Xen_is_char(Arg)                 XEN_CHAR_P(Arg)
#define Xen_keyword_is_eq(Arg1, Arg2)    XEN_KEYWORD_EQ_P(Arg1, Arg2)
#define Xen_is_defined(Arg)              XEN_DEFINED_P(Arg)
#define Xen_is_ratio(Arg)                XEN_RATIO_P(Arg)

#define Xen_is_llong_or_unbound(Arg)     XEN_LONG_LONG_IF_BOUND_P(Arg)
#define Xen_is_boolean_or_unbound(Arg)   XEN_BOOLEAN_IF_BOUND_P(Arg)
#define Xen_is_integer_or_unbound(Arg)   XEN_INTEGER_IF_BOUND_P(Arg)
#define Xen_is_number_or_unbound(Arg)    XEN_NUMBER_IF_BOUND_P(Arg)
#define Xen_is_string_or_unbound(Arg)    XEN_STRING_IF_BOUND_P(Arg)
#define Xen_is_integer_boolean_or_unbound(Arg) XEN_INTEGER_OR_BOOLEAN_IF_BOUND_P(Arg)
#define Xen_is_integer_or_boolean(Arg)   XEN_INTEGER_OR_BOOLEAN_P(Arg)

#define Xen_append(a, b)                 XEN_APPEND(a, b)
#define Xen_cadddr(a)                    XEN_CADDDR(a)
#define Xen_caddr(a)                     XEN_CADDR(a)
#define Xen_cadr(a)                      XEN_CADR(a)
#define Xen_car(a)                       XEN_CAR(a)
#define Xen_cddr(a)                      XEN_CDDR(a)
#define Xen_cdddr(a)                     XEN_CDDDR(a)
#define Xen_cdr(a)                       XEN_CDR(a)
#define Xen_cons(a, b)                   XEN_CONS(a, b)
#define Xen_cons_2(a, b, c)              XEN_CONS_2(a, b, c)
#define Xen_list_1(a)                    XEN_LIST_1(a)
#define Xen_list_2(a, b)                 XEN_LIST_2(a, b)
#define Xen_list_3(a, b, c)              XEN_LIST_3(a, b, c)
#define Xen_list_4(a, b, c, d)           XEN_LIST_4(a, b, c, d)
#define Xen_list_5(a, b, c, d, e)        XEN_LIST_5(a, b, c, d, e)
#define Xen_list_6(a, b, c, d, e, f)     XEN_LIST_6(a, b, c, d, e, f)
#define Xen_list_7(a, b, c, d, e, f, g)  XEN_LIST_7(a, b, c, d, e, f, g)
#define Xen_list_8(a, b, c, d, e, f, g, h)    XEN_LIST_8(a, b, c, d, e, f, g, h)
#define Xen_list_9(a, b, c, d, e, f, g, h, i) XEN_LIST_9(a, b, c, d, e, f, g, h, i)
#define Xen_list_length(a)               XEN_LIST_LENGTH(a)
#define Xen_list_ref(a, b)               XEN_LIST_REF(a, b)
#define Xen_list_reverse(a)              XEN_LIST_REVERSE(a)
#define Xen_list_set(a, b, c)            XEN_LIST_SET(a, b, c)
#define Xen_member(a, b)                 XEN_MEMBER(a, b)
#define Xen_make_keyword(a)              XEN_MAKE_KEYWORD(a)
#define Xen_make_vector(a, b)            XEN_MAKE_VECTOR(a, b)
#define Xen_throw(a)                     XEN_THROW(a)
#define Xen_vector_length(a)             XEN_VECTOR_LENGTH(a)
#define Xen_vector_ref(a, b)             XEN_VECTOR_REF(a, b)
#define Xen_vector_set(a, b, c)          XEN_VECTOR_SET(a, b, c)
#define Xen_vector_to_Xen_list(a)        XEN_VECTOR_TO_LIST(a)
#define C_bool_to_Xen_boolean(a)         C_TO_XEN_BOOLEAN(a)
#define C_char_to_Xen_char(a)            C_TO_XEN_CHAR(a)
#define C_complex_to_Xen_complex(a)      C_TO_XEN_COMPLEX(a)
#define C_double_to_Xen_real(a)          C_TO_XEN_DOUBLE(a)
#define C_int_to_Xen_integer(a)          C_TO_XEN_INT(a)
#define C_llong_to_Xen_llong(a)          C_TO_XEN_LONG_LONG(a)
#define C_string_to_Xen_string(a)        C_TO_XEN_STRING(a)
#define C_string_to_Xen_string_with_length(a, b) C_TO_XEN_STRINGN(a, b)
#define C_string_to_Xen_symbol(a)        C_STRING_TO_XEN_SYMBOL(a)
#define C_ulong_to_Xen_ulong(a)          C_TO_XEN_ULONG(a)
#define C_ullong_to_Xen_ullong(a)        C_TO_XEN_ULONG_LONG(a)
#define Xen_boolean_to_C_bool(a)         XEN_TO_C_BOOLEAN(a)
#define Xen_char_to_C_char(a)            XEN_TO_C_CHAR(a)
#define Xen_complex_to_C_complex(a)      XEN_TO_C_COMPLEX(a)
#define Xen_real_to_C_double(a)          XEN_TO_C_DOUBLE(a)
#define Xen_integer_to_C_int(a)          XEN_TO_C_INT(a)
#define Xen_llong_to_C_llong(a)          XEN_TO_C_LONG_LONG(a)
#define Xen_string_to_C_string(a)        XEN_TO_C_STRING(a)
#define Xen_symbol_to_C_string(a)        XEN_SYMBOL_TO_C_STRING(a)
#define C_string_to_Xen_value(a)         XEN_NAME_AS_C_STRING_TO_VALUE(a)
#define Xen_ulong_to_C_ulong(a)          XEN_TO_C_ULONG(a)
#define Xen_ullong_to_C_ullong(a)        XEN_TO_C_ULONG_LONG(a)
#define Xen_wrap_C_pointer(a)            XEN_WRAP_C_POINTER(a)
#define Xen_unwrap_C_pointer(a)          XEN_UNWRAP_C_POINTER(a)
#define Xen_numerator(a)                 XEN_NUMERATOR(a)
#define Xen_denominator(a)               XEN_DENOMINATOR(a)
#define Xen_rationalize(a, b)            XEN_RATIONALIZE(a, b)
#define Xen_make_ratio(a, b)             XEN_MAKE_RATIO(a, b)
#define Xen_load(a)                      XEN_LOAD_FILE(a)
#define Xen_documentation(a)             XEN_OBJECT_HELP(a)
#define Xen_vector_rank(a)               XEN_VECTOR_RANK(a)
#define Xen_wrap_no_args(a, b)           XEN_NARGIFY_0(a, b)
#define Xen_wrap_1_arg(a, b)             XEN_NARGIFY_1(a, b)
#define Xen_wrap_2_args(a, b)            XEN_NARGIFY_2(a, b)
#define Xen_wrap_3_args(a, b)            XEN_NARGIFY_3(a, b)
#define Xen_wrap_4_args(a, b)            XEN_NARGIFY_4(a, b)
#define Xen_wrap_5_args(a, b)            XEN_NARGIFY_5(a, b)
#define Xen_wrap_6_args(a, b)            XEN_NARGIFY_6(a, b)
#define Xen_wrap_7_args(a, b)            XEN_NARGIFY_7(a, b)
#define Xen_wrap_8_args(a, b)            XEN_NARGIFY_8(a, b)
#define Xen_wrap_9_args(a, b)            XEN_NARGIFY_9(a, b)
#define Xen_wrap_1_optional_arg(a, b)    XEN_ARGIFY_1(a, b)
#define Xen_wrap_2_optional_args(a, b)   XEN_ARGIFY_2(a, b)
#define Xen_wrap_3_optional_args(a, b)   XEN_ARGIFY_3(a, b)
#define Xen_wrap_4_optional_args(a, b)   XEN_ARGIFY_4(a, b)
#define Xen_wrap_5_optional_args(a, b)   XEN_ARGIFY_5(a, b)
#define Xen_wrap_6_optional_args(a, b)   XEN_ARGIFY_6(a, b)
#define Xen_wrap_7_optional_args(a, b)   XEN_ARGIFY_7(a, b)
#define Xen_wrap_8_optional_args(a, b)   XEN_ARGIFY_8(a, b)
#define Xen_wrap_9_optional_args(a, b)   XEN_ARGIFY_9(a, b)
#define Xen_wrap_any_args(a, b)          XEN_VARGIFY(a, b)
#define Xen_apply(a, b, c)               XEN_APPLY(a, b, c)
#define Xen_unprotected_apply(a, b)      XEN_APPLY_NO_CATCH(a, b)
#define Xen_eval_C_string(a)             XEN_EVAL_C_STRING(a)
#define Xen_error(a, b)                  XEN_ERROR(a, b)
#define Xen_call_with_no_args(a, b)                  XEN_CALL_0(a, b)
#define Xen_call_with_1_arg(a, b, c)                 XEN_CALL_1(a, b, c)
#define Xen_call_with_2_args(a, b, c, d)             XEN_CALL_2(a, b, c, d)
#define Xen_call_with_3_args(a, b, c, d, e)          XEN_CALL_3(a, b, c, d, e)
#define Xen_call_with_4_args(a, b, c, d, e, f)       XEN_CALL_4(a, b, c, d, e, f)
#define Xen_call_with_5_args(a, b, c, d, e, f, g)    XEN_CALL_5(a, b, c, d, e, f, g)
#define Xen_call_with_6_args(a, b, c, d, e, f, g, h) XEN_CALL_6(a, b, c, d, e, f, g, h)
#define Xen_unprotected_call_with_no_args(a)         XEN_CALL_0_NO_CATCH(a)
#define Xen_unprotected_call_with_1_arg(a, b)        XEN_CALL_1_NO_CATCH(a, b)
#define Xen_unprotected_call_with_2_args(a, b, c)    XEN_CALL_2_NO_CATCH(a, b, c)
#define Xen_unprotected_call_with_3_args(a, b, c, d) XEN_CALL_3_NO_CATCH(a, b, c, d)
#define Xen_define(a, b)                             XEN_DEFINE(a, b)
#define Xen_define_constant(a, b, c)                 XEN_DEFINE_CONSTANT(a, b, c)
#define Xen_define_hook(a, b, c, d)                  XEN_DEFINE_HOOK(a, b, c, d)
#define Xen_define_procedure(a, b, c, d, e, f)       XEN_DEFINE_PROCEDURE(a, b, c, d, e, f)
#define Xen_define_procedure_with_setter(a, b, c, d, e, f, g, h, i) XEN_DEFINE_PROCEDURE_WITH_SETTER(a, b, c, d, e, f, g, h, i)
#define Xen_define_dilambda(a, b, c, d, e, f, g, h, i) XEN_DEFINE_PROCEDURE_WITH_SETTER(a, b, c, d, e, f, g, h, i)
#define Xen_define_safe_procedure(a, b, c, d, e, f)  XEN_DEFINE_SAFE_PROCEDURE(a, b, c, d, e, f)

#define Xen_define_integer_procedure(a, b, c, d, e, f)  XEN_DEFINE_SAFE_PROCEDURE(a, b, c, d, e, f) /*obsolete */

#define Xen_define_simple_hook(a, b)                 XEN_DEFINE_SIMPLE_HOOK(a, b)
#define Xen_define_variable(a, b, c)                 XEN_DEFINE_VARIABLE(a, b, c)
#define Xen_out_of_range_error(a, b, c, d)           XEN_OUT_OF_RANGE_ERROR(a, b, c, d)
#define Xen_wrong_type_arg_error(a, b, c, d)         XEN_WRONG_TYPE_ARG_ERROR(a, b, c, d)
#define Xen_bad_arity_error(a, b, c, d)              XEN_BAD_ARITY_ERROR(a, b, c, d)
#define Xen_clear_hook_list(a)           XEN_CLEAR_HOOK(a)
#define Xen_hook_has_list(a)             XEN_HOOKED(a)
#define Xen_hook_list(a)                 XEN_HOOK_PROCEDURES(a)
#define Xen_add_to_hook_list(a, b, c, d) XEN_ADD_HOOK(a, b, c, d)
#define Xen_GC_protect(a)                XEN_PROTECT_FROM_GC(a)
#define Xen_GC_unprotect(a)              XEN_UNPROTECT_FROM_GC(a)
#define Xen_provide_feature(a)           XEN_PROVIDE(a)
#define Xen_arity(a)                     XEN_ARITY(a)
#define Xen_add_to_load_path(a)          XEN_ADD_TO_LOAD_PATH(a)
#define Xen_check_type(a, b, c, d, e)    XEN_ASSERT_TYPE(a, b, c, d, e)
#define Xen_make_object(a, b, c, d)      XEN_MAKE_OBJECT(a, b, c, d)
#define Xen_variable_ref(a)              XEN_VARIABLE_REF(a)
#define Xen_variable_set(a, b)           XEN_VARIABLE_SET(a, b)
#define Xen_object_ref(a)                XEN_OBJECT_REF(a)
#define Xen_copy_arg(a)                  XEN_COPY_ARG(a)
#define Xen_assoc(a, b)                  XEN_ASSOC(a, b)
#define Xen_assoc_ref(a, b)              XEN_ASSOC_REF(a, b)
#define Xen_assoc_set(a, b, c)           XEN_ASSOC_SET(a, b, c)
#define Xen_make_error_type(a)           XEN_ERROR_TYPE(a)
#define Xen_required_args(a)             XEN_REQUIRED_ARGS(a)
#define Xen_is_aritable(a, b)            XEN_REQUIRED_ARGS_OK(a, b)
#define Xen_object_to_C_string(a)        XEN_AS_STRING(a)
#define Xen_wrap_free(a, b, c)           XEN_MAKE_OBJECT_FREE_PROCEDURE(a, b, c)
#define Xen_wrap_print(a, b, c)          XEN_MAKE_OBJECT_PRINT_PROCEDURE(a, b, c)
#define Xen_make_object_type(a, b)       XEN_MAKE_OBJECT_TYPE(a, b)
#define Xen_object_mark_t                XEN_MARK_OBJECT_TYPE
#define Xen_object_type_t                XEN_OBJECT_TYPE
#define Xen_catch_t                      XEN_CATCH_BODY_TYPE
#define Xen_comment_mark                 XEN_COMMENT_STRING
#define Xen_documentation_symbol         XEN_DOCUMENTATION_SYMBOL
#define Xen_empty_list                   XEN_EMPTY_LIST
#define Xen_false                        XEN_FALSE
#define Xen_true                         XEN_TRUE
#define Xen_undefined                    XEN_UNDEFINED
#define Xen_integer_zero                 XEN_ZERO
#define Xen_file_extension               XEN_FILE_EXTENSION
#define Xen_language                     XEN_LANGUAGE_NAME
#define Xen_load_path                    XEN_LOAD_PATH
#define Xen_procedure_cast               XEN_PROCEDURE_CAST
#define Xen                              XEN

#if HAVE_SCHEME
#define Xen_define_typed_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc, Sig) s7_define_typed_function(s7, Name, Func, ReqArg, OptArg, RstArg, Doc, Sig)
#define Xen_define_unsafe_typed_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc, Sig) s7_define_unsafe_typed_function(s7, Name, Func, ReqArg, OptArg, RstArg, Doc, Sig)
#define Xen_define_typed_dilambda(Get_Name, Get_Func, Get_Help, Set_Name, Set_Func, Get_Req, Get_Opt, Set_Req, Set_Opt, Get_Sig, Set_Sig) \
  s7_typed_dilambda(s7, Get_Name, Get_Func, Get_Req, Get_Opt, Set_Func, Set_Req, Set_Opt, Get_Help, Get_Sig, Set_Sig)
#else
#define Xen_define_typed_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc, Sig) Xen_define_safe_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define Xen_define_unsafe_typed_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc, Sig) Xen_define_procedure(Name, Func, ReqArg, OptArg, RstArg, Doc)
#define Xen_define_typed_dilambda(a, b, c, d, e, f, g, h, i, j, k) XEN_DEFINE_PROCEDURE_WITH_SETTER(a, b, c, d, e, f, g, h, i)
#endif


#ifdef __cplusplus
extern "C" {
#endif

char *xen_strdup(const char *str);
char *xen_version(void);
void xen_repl(int argc, char **argv);
void xen_initialize(void);
void xen_gc_mark(XEN val);

#ifdef __cplusplus
}
#endif

#endif