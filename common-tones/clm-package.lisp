#+sbcl (shadow "SRC")

#-openmcl
(defpackage :clm 
  (:use :common-lisp #+(and clisp ansi-cl) :ext)
  #+clisp (:import-from "WALKER" "WALK-FORM")
  #+excl (:import-from :clos walk-form)
  #+cmu (:import-from :walker walk-form)
  #+cmu (:import-from :alien def-alien-routine)
  #+sbcl (:import-from :sb-walker walk-form)
  #+sbcl (:import-from :sb-ext quit)
  #+sbcl (:shadow sb-alien:double)
  #+lispworks (:import-from :walker walk-form)
  #+(and excl cltl2) (:import-from :excl exit)
  #+(and clisp (not ansi-cl)) (:import-from :lisp bye shell)
  #+(and clisp ansi-cl) (:import-from :ext bye shell)
  #+cmu (:import-from "EXTENSIONS" "QUIT" "LOAD-FOREIGN")
  )

#+openmcl
(defpackage :clm 
  (:use :common-lisp)
  (:import-from :walker walk-form)
  (:import-from :ccl quit %get-cstring))
