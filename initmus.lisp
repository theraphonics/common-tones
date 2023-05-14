;;; main defpackage is in clm-package.lisp. Export statement is in export.lisp.

(in-package :clm)

(defvar *clm* (find-package :clm) "clm synthesis package")
(defvar *clm-version* 5)
(defvar *clm-revision* 2)

(defvar *clm-source-directory* "")
(defvar *clm-binary-directory* "")
(defvar *clm-ins-directory* nil)
(defvar *clm-compiler-name* #-windoze "cc" #+windoze "cl")         
					;this is set in all.lisp via the envirionment variable "CC"


;;; this is reflected in cmus.h
(load "constants.lisp")
(load "generics.lisp")

(defvar *clm-instruments* nil)		;list of currently loaded instruments

(defvar *clm-linked* nil)


(defun double (x) (coerce x 'double-float))

(defmacro make-double-float-array (lim &key initial-contents initial-element)
  (let ((ic initial-contents)
	(ie initial-element))
    (if ic
	`(make-array ,lim :element-type 'double-float :initial-contents (map 'list #'(lambda (n) (double n)) ,ic)
		     #+lispworks :allocation #+lispworks :static)
	(if ie
	    `(make-array ,lim :element-type 'double-float :initial-element (double ,ie)
			 #+lispworks :allocation #+lispworks :static)
	    `(make-array ,lim :element-type 'double-float :initial-element (coerce 0.0 'double-float)
			 #+lispworks :allocation #+lispworks :static)
	    ))))
      
(defmacro make-double-array (lim &key initial-contents initial-element)
  (let ((ic initial-contents)
	(ie initial-element))
    (if ic
	`(make-array ,lim :element-type 'double-float :initial-contents (map 'list #'(lambda (n) (double n)) ,ic)
		     #+lispworks :allocation #+lispworks :static)
	(if ie
	    `(make-array ,lim :element-type 'double-float :initial-element (double ,ie) #+lispworks :allocation #+lispworks :static)
	    `(make-array ,lim :element-type 'double-float :initial-element (coerce 0.0 'double-float) #+lispworks :allocation #+lispworks :static)))))

(defmacro make-integer-array (len &key initial-contents initial-element) ; need the actual args because clisp thinks nil is an integer
  (let ((ic initial-contents)
	(ie initial-element)
	(type #-(or cmu sbcl openmcl acl-70 clisp lispworks) 'fixnum
              #+cmu '(unsigned-byte 32)
	      #+(or acl-70 openmcl clisp sbcl lispworks) '(signed-byte 32)))
    (if ic
	`(make-array ,len :element-type ',type :initial-contents ,ic
		     #+lispworks :allocation #+lispworks :static)
	(if ie
	    `(make-array ,len :element-type ',type :initial-element ,ie
			 #+lispworks :allocation #+lispworks :static)
	    `(make-array ,len :element-type ',type :initial-element 0
			 #+lispworks :allocation #+lispworks :static)))))

 
(defun print-hash (tab &optional (stream t)) (maphash #'(lambda (a b) (format stream "~A ~A~%" a b)) tab))

(defun clm-print (fstr &rest args) 
  ;; 30-Sep-96 allow file output(?)
  (if (stringp fstr)
      (princ (apply #'format nil fstr args))
    (apply #'format fstr (car args) (cdr args))))

(defun run-in-shell (prog args)
  (let ((str (format nil "~A ~A" prog args)))
    #+debug (progn (print str) (force-output))
    #+excl (excl:shell str)
    #+lispworks (sys::run-shell-command str)
    #+cmu (extensions:run-program "/bin/csh" (list "-fc" str) :output t)
    #+openmcl (ccl:run-program "/bin/csh" (list "-fc" str) :output t)
    #+sbcl (sb-ext:run-program "/bin/csh" (list "-fc" str) :output t)
    #+(and clisp (not ansi-cl)) (lisp::shell str)
    #+(and clisp ansi-cl) (ext::shell str)
    #-(or excl cmu sbcl openmcl clisp lispworks) (warn "can't run ~A in a shell" prog)
    ))

;;; take care of some minor differences in file names and so on

(defvar *clm-lisp-name* "lisp")

(defvar *clm-c-options* 
  #+sgi " -DMUS_SGI" 
  #+sun " -DMUS_SUN" 
  #+(and linux (not alsa)) " -DMUS_LINUX" #+(and linux alsa) " -DMUS_LINUX -DHAVE_ALSA"
  #+hpux " -DMUS_HPUX"
  #+windoze " -DMUS_WINDOZE"
  #-(or sgi openmcl sun linux hpux windoze) "")

(defun full-merge-pathnames (pathname &optional defaults)
  ;; default-version arg to merge-pathnames refers to the version number (i.e. :newest)
  (merge-pathnames pathname (or defaults ""))
  ;; can't use truename here because it complains about non-existent files!
  )

(defun ->pathname (arg) (pathname arg))                    ; who can remember what these dumb names mean?
(defun filename->string (arg) (namestring arg))
(defun expand-filename->pathname (arg) (truename arg))
(defun expand-filename->string (arg) (namestring (truename arg)))

(defvar *so-ext* nil)

;;; we also need restart-case in all lisps.  
;;;  In later ACL's it is built-in.
;;;  In CMU-CL it is broken.

;#+cmu (defmacro restart-case (expr &rest rest) (declare (ignore rest)) expr)

(defun clm-cerror (continue-control continue-default-value continue-test error-control &rest args)
  ;; like cerror, except provides a default continuation value, and if continue-test, prompts for new value
  (apply #'cerror continue-control error-control args)
  ;; if we get here, we've been told to go on
  (if continue-test
      (loop do 
	(progn
	  (princ (format nil "new value (return=~A):" continue-default-value))
	  (multiple-value-bind (new-str eof) (read-line)
	    (if (or eof (zerop (length new-str)))
		(return-from clm-cerror continue-default-value)
	      (let ((new-val (read-from-string new-str)))
		(if (funcall continue-test new-val)
		    (return-from clm-cerror new-val)
		  (print (format nil "~A is not a valid value in this context" new-val))))))))
    continue-default-value))

;;; The documentation lists the make-<gen> function arguments as &optional-key -- the
;;; meaning is that keyword names can be omitted, and the successive arguments are filled
;;; in order until a keyword is encountered, after which only keyword-arg pairs can occur.
;;; These can also have optional values (without the &optional in the declaration).

(defmacro def-optkey-fun (name (&rest args) &body body)
  (let ((keyed-name (intern (concatenate 'string (symbol-name name) "_-_1")))
	(argnames (loop for arg in args collect (intern (symbol-name (if (listp arg) (first arg) arg)) (find-package :keyword)))))
  `(progn
     (defun ,keyed-name (&key ,@args) ,@body)
     (defun ,name (&rest passed-args)
       (if (or (null passed-args) (keywordp (first passed-args)))
	   (apply #',keyed-name passed-args)
	 (let ((parglen (length passed-args)))
	   (if (or (= parglen 1) (and (> parglen 2) (keywordp (second passed-args))))
	       (apply #',keyed-name ,(first argnames) (first passed-args) (rest passed-args))
	     (if (or (= parglen 2) (and (> parglen 3) (keywordp (third passed-args))))
		 (apply #',keyed-name 
			,(first argnames) (first passed-args) 
			,(second argnames) (second passed-args) 
			(nthcdr 2 passed-args))
	       (let ((i 0))
		 (loop for arg in passed-args while (not (keywordp arg)) do (incf i))
		 (let ((unkeyed-args (loop for arg in passed-args and keyarg in ',argnames while (not (keywordp arg)) collect keyarg collect arg))
		       (keyed-args (nthcdr i passed-args)))
		   (apply #',keyed-name (append unkeyed-args keyed-args))))))))))))



(defun mus-header-type-ok (n)
  (and (> n mus-unsupported)
       (<= n mus-maui)))


(defun mus-data-format-ok (n)
  (and (> n mus-unknown)
       (<= n mus-ldoubleu)))


(defvar *output* nil)
(defvar *reverb* nil)

