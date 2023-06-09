;;; definstrument, various instrument debugging functions
;;;
;;; used to be in sound.lisp, but split out to make debugging the new versions simpler.
;;; Definstrument was straightforward until version 3 of cmus -- in its new incarnation
;;; it has to know how to write/compile/load c modules on each system.  And as of
;;; 12-Feb-97 it has to be able to live with any combination of lisp/machine output
;;; on the same directory (i.e. user here at ccrma starts clm, which automatically chooses
;;; whichever lisp/machine combination happens to work there, all such machines
;;; mounting the user's directories, so when he loads his instruments, clm has to
;;; find the right versions).

(in-package :common-tones)

(defvar *clm-snd-in-progress* nil)
(defvar *current-ins-args* nil)

(defmacro set-instrument-properties (name &optional file print-function)
  `(progn
     (setf (get ,name :ins-vars) ',variable-load-list)
     (setf (get ,name :ins-args) ',common-tones::*current-ins-args*)
     (setf (get ,name :c-proc) ,(symbol-name common-tones::*c-proc*))
     (setf (get ,name :print-function) ,print-function)
     (setf (get ,name :c-file-name) ,file)))


(defun clm-datai (&optional name) (get (or name *clm-ins*) :datai))
(defun clm-datar (&optional name) (get (or name *clm-ins*) :datar))

(defun ins-var (var-name &optional (ins-name *clm-ins*))
  (let ((insvars (get ins-name :ins-vars)))
    (if insvars
	(let ((var (find (symbol-name var-name) insvars :test #'string-equal :key #'first)))
	  (when var
	    (gen-unload (run-type->class (aref (clm-datai ins-name) (second var)))
			(second var)
			(third var)
			(clm-datai ins-name)
			(clm-datar ins-name)))))))

(defun describe-instrument (&optional ins-name varname (stream t))
  (let* ((insname (or ins-name *clm-ins*))
	 (vars (get insname :ins-vars)))
    (if vars
	(let ((print-function (get insname :print-function)))
	  (if print-function
	      (funcall print-function vars varname stream)
	    (let* ((datai (clm-datai insname))
		   (datar (and datai (clm-datar insname))))
	      (if (and datai datar)
		  (progn
		    (format stream "~A:~%" (or *clm-ins* insname))
		    (loop for var in vars do
		      (format stream "  ~A: ~A~%" (first var)
			      (if (eq (fourth var) :integer)
				  (aref datai (second var))
				(if (eq (fourth var) :real)
				    (aref datar (third var))
				  (if (= (aref datai (second var)) +string+)
				      (format nil "~S" (gen-unload "1" (second var) (third var) datai datar))
				    (gen-unload (run-type->class (aref datai (second var))) (second var) (third var) datai datar))))))))))))))

(defun di (&optional name) (describe-instrument name))

(defun noopfun (x y z)
  (declare (ignore y z))
  x)

(defvar *c-flags* nil)
(setf *c-flags*
  (concatenate 'string
	       #-(or clisp (and openmcl (not linux-target) (not linuxppc-target))) " -c"
	       " -g"
	       #-windoze " -O2" ;; this avoids a bizarre segfault in expsrc/Linux/ACL???
                " -I."   ;; [mus-config.h] clm.h cmus.h
 	       " -I"     ;;   needs also the actual source dir in case user is compiling elsewhere (mus-config.h also now)
 	       *clm-source-directory*
	       " -I"     ;; and mus-config.h...
	       *clm-binary-directory*
	       #+(and sgi (not acl-50)) " -Olimit 3000"
	       #+(and sgi acl-50) " -Olimit 3000 -n32 -w"
	       #+windoze " -O1"
	       #+lispworks-32bit " -m32"
	       #+(and mac-osx (not openmcl)) " -framework CoreAudio"
	       #+(and openmcl (not linux-target) (not linuxppc-target)) " -no-cpp-precomp"
	       #+(and excl macosx) " -dynamic -no-cpp-precomp"
	       #+(and excl linux debug) " -Wall"
	       #+(or clisp netbsd x86-64) " -fPIC"
	       #+(and excl freebsd) " -fPIC -DPIC"
	       #+(and excl linux) " -fPIC"
	       ))

(defvar *libclm-pathname* (format nil "~Alibclm.~A" *clm-binary-directory* *so-ext*))

(defvar *ins-file-loading* nil)

;;; *definstrument-hook* can be set to a function that returns a
;;; form to include in the definstrument expansion. If the hook
;;; is already set at macroexpansion time then its result will
;;; be expanded and compiled with the instrument definition.
;;; Otherwise, if the hook is set at load time then its result
;;; will be evaluated when the ins is loaded. Otherwise the hook
;;; is nil and it has no effect.

(defvar *definstrument-hook* nil)

(defmacro definstrument (ins-name (&rest args) &body body &environment env)
  (let* ((*header-info* nil)
	 (*c-file-name* nil)
	 (*c-compiler-options* *c-flags*)
	 (*c-print-function* nil)
	 (*with-reflection* nil)
	 (name
	  (if (listp ins-name)
	      (apply
	       #'(lambda
		   (nam &key c-file c-include-file
			(c-options *c-flags*)
			print-function
			&allow-other-keys)
		   (prog1 nam
		     (setf *c-file-name* c-file)
		     (setf *header-info* c-include-file)
		     (setf *c-compiler-options* c-options)
		     (setf *c-print-function* print-function)))
	       ins-name)
	    ins-name))
	 (silly-name (gentemp (string name)))
	 (dependent-file nil)
	 (antecedent-file nil)
         ;; if the hook exists at macroexpansion time then
         ;; include its form in the compilation. otherwise
         ;; include a form that checks at load time.
         (hookform (if *definstrument-hook*
                     (funcall *definstrument-hook* name args)
                     `(eval-when #-excl (:load-toplevel)
				 #+excl (load eval)
                        (if *definstrument-hook*
                          (eval (funcall *definstrument-hook*
                                         ',name ',args))))))
         )

    (setf *current-ins-args* args)
    (let* ((lsp-name (concatenate 'string "clm_" (string-downcase (lisp->c-name (symbol-name name)))))
	   ;; since *ins-file-loading* doesnt recompute cfile each time name must be reused.
	  (c-ff (if common-tones::*ins-file-loading* (intern lsp-name) (gentemp lsp-name)))
	  (c-ff (intern lsp-name)) ; recompilation (""can't find alien function ...") bugfix thanks to Todd Ingalls
	  (c-ff-name (symbol-name c-ff))
    (c-ff-cmu (gentemp lsp-name))
   	(ins-file-name (or #+(and excl cltl2) (truename (or excl:*source-pathname* *load-pathname*))
			      #+(and excl (not cltl2)) (truename excl:*source-pathname*)
			      #+(or clisp cmu sbcl lispworks) (or *load-pathname* *compile-file-truename*) ;this is the CLtL2 name
			      #+openmcl (or *compile-file-truename* *load-pathname*)
			      (error "oops -- I can't find ~A's lisp source file!" name)
			      ))
	   (c-file-name (or *c-file-name*
			    ;; try to find compile-time input file name so that the subsequent .c and .o files
			    ;; are writeten to the same directory.
			    (filename->string
			     (merge-pathnames
			      (concatenate 'string
					   "clm_"
					   (lisp->c-name #-openmcl (symbol-name name) #+openmcl (string-downcase name))
					   ;; changed 27-Feb-03 because instrument name with ">" or presumably "&" can confuse Linux
					   ".c")
			      ins-file-name))))
	   (l-file-name
	    (concatenate 'string (subseq c-file-name 0 (- (length c-file-name) 2)) #-windoze ".o" #+windoze ".obj")
	     )
	   (so-file-name (concatenate 'string (subseq c-file-name 0 (- (length c-file-name) 2))  "." *so-ext*))
	   (ins-code-file (if common-tones::*ins-file-loading* (concatenate 'string (subseq c-file-name 0 (- (length c-file-name) 2))  ".icl")))
	   ;; ^ this is for openmcl only, I think
	   )
	(format fil "(load-foreign ~S)~%" so-file-name)
	;; how to get the compiler's output filename?
	(format fil "(load ~S)~%"
		(concatenate 'string
			     (or *clm-ins-directory* (directory-namestring so-file-name))
			     (filename->string
			      (pathname-name (or *load-pathname* *compile-file-truename*)))
			     "."
			     *clm-fasl-name*)))
      (let ((ins-code nil))
	;; create C file unless *ins-file-loading* is true and there is
	;; already a c file and its write date is not later than the lisp file
	(if common-tones::*ins-file-loading*
	    (setf dependent-file c-file-name
		  antecedent-file #+openmcl *load-pathname* #-openmcl nil) ; *load-pathname* is not defined in older lisps, giving dumb warnings
            (setf dependent-file #-(or clisp openmcl) l-file-name #+(or clisp openmcl) so-file-name
                  antecedent-file c-file-name))
          (if (and common-tones::*ins-file-loading*
		   (probe-file dependent-file)
		   (probe-file antecedent-file)
		   (>= (file-write-date (truename dependent-file))
		       (file-write-date (truename antecedent-file))))
	      (with-open-file (fil ins-code-file :direction :input)
			      (setf ins-code (read fil)))
	    (unwind-protect
		(progn
		  (setf *c-file* (open c-file-name :direction :output :if-exists :supersede :if-does-not-exist :create))
		  (princ (format nil "; Writing ~S~%" c-file-name))

		  (format *c-file* "/* translate ~A in ~A to C~% *   written ~A by COMMON-TONES of ~A~% */~%~%"
			  (string-downcase name)
			  ins-file-name
			  (timestring)
			  *clm-date*)
		  (format *c-file* "#include <mus-config.h>~%~
                                    #include <stdio.h>~%~
                                    #include <stdlib.h>~%~
                                    #include <stdint.h>~%~
                                    #include <stdarg.h>~%~
                                    #include <math.h>~%")
		  (format *c-file* "#include <signal.h>~%")
		  #+sgi (format *c-file* "#include <sys/fpu.h>~%")
		  (format *c-file* "#include <cmus.h>~%")

		  (if *header-info* (format *c-file* "#include ~S~%" *header-info*))
		  (format *c-file* "~%")
		  (format *c-file* "static sig_atomic_t got_sigint = 0; /* catch C-C if hung */~%")
		  (format *c-file* "static void sig_err(int sig) {got_sigint = sig;}~%")
		  (format *c-file* "~%")

		  (setf *c-proc* c-ff)
		  (setf *lisp-proc* ins-name)
		  (setf ins-code (walk-form `(locally ,@body) env 'noopfun)))			;unwind-protect progn
	      (close *c-file*)
	      (setf *c-file* nil)
	      ))		;unwind-protect cleanup
	  `(progn
             ,hookform
             (eval-when #-excl (:compile-toplevel)
	       (when (or (not (probe-file ,dependent-file))
			 (not (probe-file ,antecedent-file))
 			 (> (file-write-date (truename ,antecedent-file)) (file-write-date (truename ,dependent-file)))
			 (and common-tones::*ins-file-loading*
			      (or (not (probe-file ,l-file-name))
				  (not (probe-file ,c-file-name))
				  (> (file-write-date (truename ,c-file-name)) (file-write-date (truename ,l-file-name))))))

		 ;;; ---------------------------------------- COMPILE ----------------------------------------

		 (princ (format nil "; Compiling ~S~%" ,c-file-name))
     (uiop:run-program (format nil "~A ~A -o ~A~%" ,*clm-compiler-name* ,*c-compiler-options* ,c-file-name ,l-file-name)
                  :output t)

		 ;;; ---------------------------------------- LOAD ----------------------------------------

			(princ (format nil "; Creating shared object file ~S~%" ,so-file-name))
      (uiop:run-program "gcc"
                  `("-shared" "-o" ,so-file-name ,l-file-name ,(uiop:pathname-directory-pathname *libclm-pathname*) "-lm"))))

	   (cffi:load-foreign-library ,so-file-name)

     (cffi:defcfun (,c-ff-name ,c-ff-cmu) :int
       (datar (* :double))
       (len :int) (datai (* :int)) (ilen :int))

	   (defun ,c-ff (c &optional d e f)
	     (,c-ff-cmu (array-data-address c) d (array-data-address e) f))

	   (pushnew ',name *clm-instruments*)
	   (defun ,name ,args
	     (setf *clm-ins* ',name)
	     (if (or (not *notehook*)
		     (not (eq (funcall *notehook*
				       (symbol-name ',name)
				       ,@(let ((defargs (set-difference args lambda-list-keywords)))
					      (nreverse (loop for arg in defargs collect (if (symbolp arg) arg (first arg))))))
			      :done)))
		 (if (zerop common-tones::*interrupted*)
		     (let ((val nil))
		       (tagbody
			  (restart-case
			      (setf val (progn ,ins-code))
			    (nil ()
			      :report "abort current note."
			      (go C-INSTRUMENT-DONE)))
			C-INSTRUMENT-DONE)
		       val))))
	   (defun ,silly-name ()
	     (pushnew ',name *clm-instruments*)
	     (set-instrument-properties ',name ,c-file-name ,*c-print-function*)
	     )
	   (,silly-name)))))

(defun clm-initialize-links ()
  (when (not *clm-linked*)
    (mus-sound-initialize)
    (initialize-cmus)
    )
  (setf *clm-linked* t))

(defvar clm-cleanup-functions nil)

(defun cleanup-clm ()
  (when clm-cleanup-functions
    (loop for func in clm-cleanup-functions do (funcall func))))

(defun quit () (uiop:quit))
(defun bye () (uiop:quit))
(defun exit () (uiop:quit))

(defun restart-clm ()
	(cffi:load-foreign-library *libclm-pathname*)
  (setf *clm-linked* nil)
  (reset-headers)
  (reset-audio)
  (reset-io)
  (clm-initialize-links))
