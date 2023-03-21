;;; main defpackage is in clm-package.lisp. Export statement is in export.lisp.

(in-package :clm)

(defvar *clm* (find-package :clm) "CLM synthesis package")
(defvar *clm-version* 5)
(defvar *clm-revision* 2)
(defvar *clm-date* "8-May-18")

(defvar *clm-news* 
  "
8-May-18: added (clm-initialize-links) at the end of all.lisp
31-Aug-17: %lld -> PRId64 changes
")

(defvar *clm-source-directory* "")
(defvar *clm-binary-directory* "")
(defvar *clm-ins-directory* nil)
(defvar *clm-compiler-name* #-windoze "cc" #+windoze "cl")         
					;this is set in all.lisp via the envirionment variable "CC"

(defconstant +clm-interrupted+ 0)
(defconstant +clm-beg+ 1) ; two words for bignum
(defconstant +clm-end+ 3) ; ditto
(defconstant +int-block-size+ 5)
(defconstant +float-block-size+ 0)

;;; this is reflected in cmus.h
(defconstant +no-type+ 0)
(defconstant +integer+ 1)
(defconstant +real+ 2)
(defconstant +oscil+ 3)
(defconstant +rand+ 5)
(defconstant +rand-interp+ 6)
(defconstant +table-lookup+ 7)
(defconstant +square-wave+ 8)
(defconstant +pulse-train+ 9)
(defconstant +sawtooth-wave+ 10)
(defconstant +triangle-wave+ 11)
(defconstant +asymmetric-fm+ 12)
(defconstant +wave-train+ 13)
(defconstant +one-pole+ 14)
(defconstant +two-pole+ 15)
(defconstant +one-zero+ 16)
(defconstant +two-zero+ 17)
(defconstant +delay+ 18)
(defconstant +tap+ 19)
(defconstant +comb+ 20)
(defconstant +notch+ 21)
(defconstant +all-pass+ 22)
(defconstant +filter+ 23)
(defconstant +fir-filter+ 24)
(defconstant +iir-filter+ 25)
(defconstant +array+ 26)
(defconstant +env+ 27)
(defconstant +locsig+ 28)
(defconstant +src+ 29)
(defconstant +granulate+ 30)
(defconstant +readin+ 31)
(defconstant +convolve+ 32)
(defconstant +formant+ 35)
(defconstant +real-array+ 36)
(defconstant +integer-array+ 37)
(defconstant +string+ 38)
;(defconstant +frame+ 39)
;(defconstant +mixer+ 40)
(defconstant +phase-vocoder+ 41)
(defconstant +bignum+ 42)
(defconstant +moving-average+ 43)
(defconstant +ssb-am+ 45)
(defconstant +file2sample+ 46)
(defconstant +file2frample+ 47)
(defconstant +sample2file+ 48)
(defconstant +frample2file+ 49)
(defconstant +polyshape+ 50)
(defconstant +filtered-comb+ 51)
(defconstant +move-sound+ 52)
(defconstant +ncos+ 53)
(defconstant +nsin+ 54)
(defconstant +nrxycos+ 55)
(defconstant +nrxysin+ 56)
(defconstant +polywave+ 57)
(defconstant +firmant+ 58)

(defgeneric mus-frequency (gen))   (defgeneric (setf mus-frequency) (val gen))
(defgeneric mus-phase (gen))       (defgeneric (setf mus-phase) (val gen))
(defgeneric mus-data (gen))        (defgeneric (setf mus-data) (val gen))
(defgeneric mus-offset (gen))      (defgeneric (setf mus-offset) (val gen))
(defgeneric mus-safety (gen))      (defgeneric (setf mus-safety) (val gen))
(defgeneric mus-scaler (gen))      (defgeneric (setf mus-scaler) (val gen))
(defgeneric mus-width (gen))       (defgeneric (setf mus-width) (val gen))
(defgeneric mus-length (gen))      (defgeneric (setf mus-length) (val gen))
(defgeneric mus-location (gen))    (defgeneric (setf mus-location) (val gen))
(defgeneric mus-increment (gen))   (defgeneric (setf mus-increment) (val gen))
(defgeneric mus-feedback (gen))    (defgeneric (setf mus-feedback) (val gen))
(defgeneric mus-feedforward (gen)) (defgeneric (setf mus-feedforward) (val gen))
(defgeneric mus-order (gen))     
(defgeneric mus-xcoeffs (gen))
(defgeneric mus-ycoeffs (gen))
(defgeneric mus-channels (gen))
(defgeneric mus-channel (gen))     (defgeneric (setf mus-channel) (val gen))
(defgeneric mus-hop (gen))         (defgeneric (setf mus-hop) (val gen))
(defgeneric mus-ramp (gen))        (defgeneric (setf mus-ramp) (val gen))
(defgeneric mus-interp-type (gen))
(defgeneric mus-xcoeff (gen index)) (defgeneric (setf mus-xcoeff) (val gen index))
(defgeneric mus-ycoeff (gen index)) (defgeneric (setf mus-ycoeff) (val gen index))
(defgeneric mus-describe (gen))
(defgeneric mus-file-name (gen))
(defgeneric mus-name (gen))        (defgeneric (setf mus-name) (val gen))
(defgeneric mus-reset (gen))

(defgeneric oscil? (gen))
(defgeneric table-lookup? (gen))
(defgeneric delay? (gen))
(defgeneric comb? (gen))
(defgeneric filtered-comb? (gen))
(defgeneric notch? (gen))
(defgeneric all-pass? (gen))
(defgeneric moving-average? (gen))
(defgeneric filter? (gen))
(defgeneric fir-filter? (gen))
(defgeneric iir-filter? (gen))
(defgeneric one-zero? (gen))
(defgeneric one-pole? (gen))
(defgeneric two-pole? (gen))
(defgeneric two-zero? (gen))
(defgeneric firmant? (gen))
(defgeneric formant? (gen))
(defgeneric rand? (gen))
(defgeneric rand-interp? (gen))
(defgeneric env? (gen))
(defgeneric triangle-wave? (gen))
(defgeneric square-wave? (gen))
(defgeneric sawtooth-wave? (gen))
(defgeneric pulse-train? (gen))
(defgeneric polyshape? (gen))
(defgeneric polywave? (gen))
(defgeneric ncos? (gen))
(defgeneric nsin? (gen))
(defgeneric nrxycos? (gen))
(defgeneric nrxysin? (gen))
(defgeneric asymmetric-fm? (gen))
(defgeneric locsig? (gen))
(defgeneric move-sound? (gen))
(defgeneric file->sample? (gen))
(defgeneric mus-input? (gen))
(defgeneric file->frample? (gen))
(defgeneric sample->file? (gen))
(defgeneric mus-output? (gen))
(defgeneric frample->file? (gen))
(defgeneric readin? (gen))
;(defgeneric frame? (gen))
;(defgeneric mixer? (gen))
(defgeneric wave-train? (gen))
(defgeneric src? (gen))
(defgeneric convolve? (gen))
(defgeneric granulate? (gen))
(defgeneric phase-vocoder? (gen))
(defgeneric ssb-am? (gen))

;(defgeneric sample->frame (gen s &optional outf))
;(defgeneric frame->sample (gen fin))
(defgeneric mus-run (gen &optional arg1 arg2))


(defvar *clm-instruments* nil)		;list of currently loaded instruments

(defvar *clm-linked* nil)

(defconstant two-pi (* pi 2))

(defun double (x) (coerce x 'double-float))
#+openmcl (defun double-float (x) (coerce x 'double-float)) ; backwards compatibility, I hope

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

(defconstant *clm-fasl-name* (pathname-type (compile-file-pathname "mus.lisp")))
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

#+clisp (defun full-lib-name () (expand-filename->string "libclm.so"))  ; also used in ffi.lisp, in clisp 2.41 can't be in sndlib2clm.lisp!

(defvar *so-ext* nil)

#+cltl2 (defmacro without-warnings (&body body)
	  `(handler-bind ((simple-warning 
			   #'(lambda (c) 
			       (declare (ignore c))
			       (when (find-restart 'muffle-warning) 
				 (invoke-restart 'muffle-warning)))))
	     ,@body))

#-cltl2 (defmacro without-warnings (&body body) `(progn ,.body))

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


(defconstant mus-unsupported 0)
(defconstant mus-next 1)
(defconstant mus-aifc 2)
(defconstant mus-riff 3)
(defconstant mus-rf64 4)
(defconstant mus-bicsf 5)
(defconstant mus-nist 6)
(defconstant mus-inrs 7)
(defconstant mus-esps 8)
(defconstant mus-svx 9)
(defconstant mus-voc 10)
(defconstant mus-sndt 11)
(defconstant mus-raw 12)
(defconstant mus-smp 13)
(defconstant mus-avr 14)
(defconstant mus-ircam 15)
(defconstant mus-sd1 16)
(defconstant mus-sppack 17)
(defconstant mus-mus10 18)
(defconstant mus-hcom 19)
(defconstant mus-psion 20)
(defconstant mus-maud 21)
(defconstant mus-ieee 22)
(defconstant mus-matlab 23)
(defconstant mus-adc 24)
(defconstant mus-midi 25)
(defconstant mus-soundfont 26)
(defconstant mus-gravis 27)
(defconstant mus-comdisco 28)
(defconstant mus-goldwave 29)
(defconstant mus-srfs 30)
(defconstant mus-midi-sample-dump 31)
(defconstant mus-diamondware 32)
(defconstant mus-adf 33)
(defconstant mus-sbstudioii 34)
(defconstant mus-delusion 35)
(defconstant mus-farandole 36)
(defconstant mus-sample-dump 37)
(defconstant mus-ultratracker 38)
(defconstant mus-yamaha-sy85 39)
(defconstant mus-yamaha-tx16 40)
(defconstant mus-digiplayer 41)
(defconstant mus-covox 42)
(defconstant mus-avi 43)
(defconstant mus-omf 44)
(defconstant mus-quicktime 45)
(defconstant mus-asf 46)
(defconstant mus-yamaha-sy99 47)
(defconstant mus-kurzweil-2000 48)
(defconstant mus-aiff 49)
(defconstant mus-paf 50)
(defconstant mus-csl 51)
(defconstant mus-file-samp 52)
(defconstant mus-pvf 53)
(defconstant mus-soundforge 54)
(defconstant mus-twinvq 55)
(defconstant mus-akai4 56)
(defconstant mus-impulsetracker 57)
(defconstant mus-korg 58)
(defconstant mus-nvf 59)
(defconstant mus-caff 60)
(defconstant mus-maui 61)
(defconstant mus-sdif 62)
(defconstant mus-ogg 63)
(defconstant mus-flac 64)
(defconstant mus-speex 65)
(defconstant mus-mpeg 66)
(defconstant mus-shorten 67)
(defconstant mus-tta 68)
(defconstant mus-wavpack 69)

(defun mus-header-type-ok (n)
  (and (> n mus-unsupported)
       (<= n mus-maui)))

(defconstant mus-unknown 0)
(defconstant mus-bshort 1)
(defconstant mus-mulaw 2)
(defconstant mus-byte 3)
(defconstant mus-bfloat 4)
(defconstant mus-bint 5)
(defconstant mus-alaw 6)
(defconstant mus-ubyte 7)
(defconstant mus-b24int 8)
(defconstant mus-bdouble 9)
(defconstant mus-lshort 10)
(defconstant mus-lint 11)
(defconstant mus-lfloat 12)
(defconstant mus-ldouble 13)
(defconstant mus-ubshort 14)
(defconstant mus-ulshort 15)
(defconstant mus-l24int 16)
(defconstant mus-bintn 17)
(defconstant mus-lintn 18)
(defconstant mus-blfoatu 19)
(defconstant mus-lfloatu 20)
(defconstant mus-bdoubleu 21)
(defconstant mus-ldoubleu 22)

(defun mus-data-format-ok (n)
  (and (> n mus-unknown)
       (<= n mus-ldoubleu)))

(defconstant mus-audio-default 0)

(defconstant mus-interp-none 0)
(defconstant mus-interp-linear 1)
(defconstant mus-interp-sinusoidal 2)
(defconstant mus-interp-all-pass 3)
(defconstant mus-interp-lagrange 4)
(defconstant mus-interp-bezier 5)
(defconstant mus-interp-hermite 6)

(defconstant mus-chebyshev-first-kind 1)
(defconstant mus-chebyshev-second-kind 2)

(defvar *output* nil)
(defvar *reverb* nil)

#+excl (defvar *use-chcon* nil)
