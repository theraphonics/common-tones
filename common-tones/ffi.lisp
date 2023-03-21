(in-package :clm)

;;; foreign function interfaces for ACL, CMUCL, SBCL, OpenMCL, Clisp, Lispworks
;;;   sndlib linkages are in sndlib2clm.lisp, packaged here into the old names
;;;   the rest of this file links to cmus.c and clm.c

(defun clm-close-output () (clm-close-output-1) (setf *output* nil))
(defun clm-close-reverb () (clm-close-reverb-1) (setf *reverb* nil))


#-(and sbcl x86-64)
(defun mus-sound-loop-info (name)
  (clm-initialize-links)
  (let ((err (mus-header-read name)))
    (if (= err 0)
	(let ((vals (make-integer-array 8)))
	  (setf (aref vals 0) (mus-header-loop-start 0))
	  (setf (aref vals 1) (mus-header-loop-end 0))
	  (setf (aref vals 6) (mus-header-loop-mode 0))
	  (setf (aref vals 2) (mus-header-loop-start 1))
	  (setf (aref vals 3) (mus-header-loop-end 1))
	  (setf (aref vals 7) (mus-header-loop-mode 1))
	  (setf (aref vals 4) (mus-header-base-note))
	  (setf (aref vals 5) (mus-header-base-detune))
	  vals)
      err)))
#+(and sbcl x86-64)
(defun mus-sound-loop-info (name)
  (warn "mus-sound-loop-info doesn't work in this system."))

(defun mus-sound-maxamp (filename)
  (let* ((chans (mus-sound-chans filename))
	 (maxamps (make-double-array chans))
	 (times (make-integer-array chans)))
    (sound-maxamp filename chans maxamps times)
    maxamps))


;;; I had originally planned to "deprecate" these in clm 4, but the filename expansion is handy,
;;;   and I'd just have to add them back under some other name, so I'll leave them in.

(defun fullstrname (n)
  #+(or windoze openmcl) (if (or (pathnamep n)
				 (stringp n))
			     n)
  #-(or windoze openmcl) (if (stringp n)
			     (filename->string
			      (full-merge-pathnames n)))
  )

(defun fullname (n caller)
  (if (mus-input? n)
      (mus-file-name n)
    (if (stringp n)
	(let ((str (fullstrname n))) 
	  (if (mus-file-probe str)
	      str
	    (let ((str (search-full-merge-pathnames n *clm-file-name*)))
	      (and str
		   (if (pathnamep str)
		       (setf str (namestring str)))
		   (if (mus-file-probe str)
		       str
		       (progn
			 (warn "~A: can't open ~S~%" caller n)
			 n))))))
      (warn "~A: file argument, ~A, should be a string or an input file pointer" caller n))))

(defun sound-chans (name) (mus-sound-chans (fullname name "sound-chans")))
(defun sound-duration (name) (mus-sound-duration (fullname name "sound-duration")))
(defun sound-data-format (name) (mus-sound-data-format (fullname name "sound-data-format")))
(defun sound-data-location (name) (mus-sound-data-location (fullname name "sound-data-location")))
(defun sound-datum-size (name) (mus-sound-datum-size (fullname name "sound-datum-size")))
(defun sound-header-type (name) (mus-sound-header-type (fullname name "sound-header-type")))
(defun sound-length (name) (mus-sound-length (fullname name "sound-length")))
(defun sound-samples (name) (mus-sound-samples (fullname name "sound-samples")))
(defun sound-framples (name) (mus-sound-framples (fullname name "sound-framples")))
(defun sound-srate (name) (mus-sound-srate (fullname name "sound-srate")))
(defun sound-comment (name) (mus-sound-comment (fullname name "sound-comment")))



;;; ---------------------------------------- ACL ----------------------------------------
#+(and excl acl-50)
  (progn
    ;; nearly all of these could include :call-direct t :arg-checking nil
    ;;   but my timing tests indicate that those arguments give less than a 2% speed-up (if any at all)

    (ff:def-foreign-call (sl-dac-1 "sl_dac")
			 ((name (* :char) #-acl-60 string) (dev :int))
			 #+acl-61 :strings-convert #+acl-61 t
			 :returning :int)
    (ff:def-foreign-call (initialize-cmus "initialize_cmus") (:void) :returning :void)
    (ff:def-foreign-call (sound-maxamp "clm_sound_maxamp")
					     ((ifile (* :char) #-acl-60 string)
					      (chans :int)
					      #-acl-70 (arr (* :double) (simple-array double-float (*)))
					      #+acl-70 (arr (:array :double))
					      #-acl-70 (times (* :int) array)
					      #+acl-70 (times (:array :int))
					      )
					     #+acl-61 :strings-convert #+acl-61 t 
					     :returning :int)

    #-acl-70
    (ff:def-foreign-call (autocorrelate "mus_autocorrelate")
			 ((rl1 (* :double) (simple-array double-float (*))) (n :int))
			 :returning :void)
    #+acl-70
    (ff:def-foreign-call (autocorrelate "mus_autocorrelate")
			 ((rl1 (:array :double)) (n :int))
			 :returning :void)

    #-acl-70
    (ff:def-foreign-call (correlate "mus_correlate")
			 ((rl1 (* :double) (simple-array double-float (*))) (rl2 (* :double) (simple-array double-float (*))) (n :int))
			 :returning :void)
    #+acl-70
    (ff:def-foreign-call (correlate "mus_correlate")
			 ((rl1 (:array :double)) (rl2 (:array :double)) (n :int))
			 :returning :void)

    #-acl-70
    (ff:def-foreign-call (convolution "mus_convolution")
			 ((rl1 (* :double) (simple-array double-float (*))) (rl2 (* :double) (simple-array double-float (*))) (n :int))
			 :returning :void)
    #+acl-70
    (ff:def-foreign-call (convolution "mus_convolution")
			 ((rl1 (:array :double)) (rl2 (:array :double)) (n :int))
			 :returning :void)

    #-acl-70
    (ff:def-foreign-call (clm-fft "mus_fft")
			 ((rl (* :double) (simple-array double-float (*))) (im (* :double) (simple-array double-float (*))) (n :int) (isign :int))
			 :returning :void)
    #+acl-70
    (ff:def-foreign-call (clm-fft "mus_fft")
			 ((rl (:array :double)) (im (:array :double)) (n :int) (isign :int))
			 :returning :void)
    (ff:def-foreign-call (mus-set-rand-seed "mus_set_rand_seed") ((seed :int)) :returning :void)
    (ff:def-foreign-call (clm-random "mus_frandom") ((amp :double)) :returning :double)
    #-acl-70
    (ff:def-foreign-call (array->file "clm_array2file")
			 ((file (* :char) #-acl-60 string) (data (* :double) (simple-array double-float (*))) (len :int) (srate :int) (chans :int))
			 #+acl-61 :strings-convert #+acl-61 t 
			 :returning :void)
    #+acl-70
    (ff:def-foreign-call (array->file "clm_array2file")
			 ((file (* :char) #-acl-60 string) (data (:array :double)) (len :int) (srate :int) (chans :int))
			 :strings-convert t 
			 :returning :void)
     (ff:def-foreign-call (clm-mix "clm_mix")
 			 ((outfile (* :char) #-acl-60 string) (infile (* :char) #-acl-60 string)
 			  (out_start :int) (out_framples :int) (in_start :int))
 			 #+acl-61 :strings-convert #+acl-61 t 
 			 :returning :void)
    (ff:def-foreign-call (clm-scale-file "clm_scale_file")
			 ((outfile (* :char) #-acl-60 string) (infile (* :char) #-acl-60 string) (scaler :double)
			  (frm :int) (hdr :int))
			 #+acl-61 :strings-convert #+acl-61 t 
			 :returning :int)
    #-acl-70
    (ff:def-foreign-call (file->array "clm_file2array") 
			 ((file (* :char) #-acl-60 string) (chan :int) (start :int) (samples :int)
			  (arr (* :double) (simple-array double-float (*))))
			 #+acl-61 :strings-convert #+acl-61 t 
			 :returning :int)
    #+acl-70
    (ff:def-foreign-call (file->array "clm_file2array") 
			 ((file (* :char) string) (chan :int) (start :int) (samples :int) (arr (:array :double)))
			 :strings-convert t 
			 :returning :int)
    (ff:def-foreign-call (clm-close-output-1 "clm_close_output") (:void) :returning :int)
    (ff:def-foreign-call (clm-close-reverb-1 "clm_close_reverb") (:void) :returning :int)
    (ff:def-foreign-call (clm-make-output "clm_make_output")
			 ((file (* :char) #-acl-60 string) (chans :int) (frm :int) (typ :int) (com (* :char) #-acl-60 string))
			 #+acl-61 :strings-convert #+acl-61 t 			 
			 :returning :int)
    (ff:def-foreign-call (clm-make-reverb "clm_make_reverb")
			 ((file (* :char) #-acl-60 string) (chans :int) (frm :int) (typ :int) (com (* :char) #-acl-60 string))
			 #+acl-61 :strings-convert #+acl-61 t 			 
			 :returning :int)
    (ff:def-foreign-call (clm-continue-output "clm_continue_output") ((file (* :char) #-acl-60 string))
			 #+acl-61 :strings-convert #+acl-61 t 			 
			 :returning :int)
    (ff:def-foreign-call (clm-continue-reverb "clm_continue_reverb") ((file (* :char) #-acl-60 string))
			 #+acl-61 :strings-convert #+acl-61 t 			 
			 :returning :int)
    (ff:def-foreign-call (mus-srate "mus_srate") (:void) :returning :double)
    (ff:def-foreign-call (mus-set-srate "mus_set_srate") ((srate :double)) :returning :double)
    (ff:def-foreign-call (mus-file-buffer-size "cl_clm_file_buffer_size") (:void) :returning :int)
    (ff:def-foreign-call (mus-set-file-buffer-size "cl_clm_set_file_buffer_size") ((clipped :int)) :returning :int)    
    (ff:def-foreign-call (clm-set-output-safety "clm_set_output_safety") ((safety :int)) :returning :void)
    (ff:def-foreign-call (clm-set-reverb-safety "clm_set_reverb_safety") ((safety :int)) :returning :void)
   )

#+(and excl (not acl-60) (not windoze)) (defun clm-receive-snd () (let ((ptr (clm-receive-snd-1))) (if (not (zerop ptr)) (ff:char*-to-string ptr))))


;;; ---------------------------------------- CMUCL ----------------------------------------
#+cmu (use-package "ALIEN")
;#+cmu (use-package "C-CALL")
;; from cmu docs (cmucl/doc/devenv/cmu-new/node242.html)
#+cmu (defun array-data-address (array)
       "Return the physical address of where the actual data of an array is
     stored.

     ARRAY must be a specialized array type in CMU Lisp.  This means ARRAY
     must be an array of one of the following types:

                       double-float
                       single-float
                       (unsigned-byte 32)
                       (unsigned-byte 16)
                       (unsigned-byte  8)
                       (signed-byte 32)
                       (signed-byte 16)
                       (signed-byte  8)
     "
       (declare (type (or #+signed-array (array (signed-byte 8))
                          #+signed-array (array (signed-byte 16))
                          #+signed-array (array (signed-byte 32))
                          (array (unsigned-byte 8))
                          (array (unsigned-byte 16))
                          (array (unsigned-byte 32))
                          (array single-float)
                          (array double-float))
                      array)
                (optimize (speed 3) (safety 0))
                (ext:optimize-interface (safety 3)))
       ;; with-array-data will get us to the actual data.  However, because
       ;; the array could have been displaced, we need to know where the
       ;; data starts.
       (lisp::with-array-data ((data array)
                               (start)
                               (end))
         (declare (ignore end))
         ;; DATA is a specialized simple-array.  Memory is laid out like this:
         ;;
         ;;   byte offset    Value
         ;;        0         type code (should be 70 for double-float vector)
         ;;        4         4 * number of elements in vector
         ;;        8         1st element of vector
         ;;      ...         ...
         ;;
         (let ((addr (+ 8 (logandc1 7 (kernel:get-lisp-obj-address data))))
               (type-size (let ((type (array-element-type data)))
                            (cond ((or (equal type '(signed-byte 8))
                                       (equal type '(unsigned-byte 8)))
                                   1)
                                  ((or (equal type '(signed-byte 16))
                                       (equal type '(unsigned-byte 16)))
                                   2)
                                  ((or (equal type '(signed-byte 32))
                                       (equal type '(unsigned-byte 32)))
                                   4)
                                  ((equal type 'single-float)
                                   4)
                                  ((equal type 'double-float)
                                   8)
                                  (t
                                   (error "Unknown specialized array element type"))))))
           (declare (type (unsigned-byte 32) addr)
                    (optimize (speed 3) (safety 0) (ext:inhibit-warnings 3)))
           (system:int-sap (the (unsigned-byte 32)
                             (+ addr (* type-size start)))))))
#+cmu
(progn

    (def-alien-routine ("sl_dac" sl-dac-1) c-call:int (name c-call:c-string) (dev c-call:int))
    (def-alien-routine ("initialize_cmus" initialize-cmus) c-call:void)
    (def-alien-routine ("clm_sound_maxamp" sound-maxamp-1) c-call:int (ifile c-call:c-string) (chans c-call:int) (vals (* double-float)) (times (* c-call:int)))
    (defun sound-maxamp (file chans arr times) (sound-maxamp-1 file chans (array-data-address arr) (array-data-address times)))

    (def-alien-routine ("mus_autocorrelate" autocorrelate-1) c-call:void (rl1 (* double-float)) (len c-call:int))
    (defun autocorrelate (rl1 len) (autocorrelate-1 (array-data-address rl1) len))
    (def-alien-routine ("mus_correlate" correlate-1) c-call:void (rl1 (* double-float)) (im1 (* double-float)) (len c-call:int))
    (defun correlate (rl1 im1 len) (correlate-1 (array-data-address rl1) (array-data-address im1) len))

    (def-alien-routine ("mus_convolution" convolution-1) c-call:void (rl1 (* double-float)) (im1 (* double-float)) (len c-call:int))
    (defun convolution (rl1 im1 len) (convolution-1 (array-data-address rl1) (array-data-address im1) len))

    (def-alien-routine ("mus_fft" clm-fft-1) c-call:void (rl (* double-float)) (im (* double-float)) (len c-call:int) (isign c-call:int))
    (defun clm-fft (rl im len isign) (clm-fft-1 (array-data-address rl) (array-data-address im) len isign))
    (def-alien-routine ("mus_set_rand_seed" mus-set-rand-seed) c-call:void (seed c-call:int))
    (def-alien-routine ("mus_frandom" clm-random-1) c-call:float (amp double-float))
    (def-alien-routine ("clm_array2file" array->file-1) c-call:void (file c-call:c-string) (data (* double-float)) (len c-call:int) (srate c-call:int) (chans c-call:int))
    (defun array->file (file data len srate chans) (array->file-1 file (array-data-address data) len srate chans))
    (def-alien-routine ("clm_mix" clm-mix) 
 	c-call:void
 	(outfile c-call:c-string) (infile c-call:c-string)  (out_start c-call:int) (out_framples c-call:int) (in_start c-call:int))
    (def-alien-routine ("clm_scale_file" clm-scale-file) c-call:int (outfile c-call:c-string) (infile c-call:c-string) (scaler double-float) (frm c-call:int) (hdr c-call:int))
    (def-alien-routine ("clm_file2array" file->array-1) 
	c-call:int 
	(file c-call:c-string) (chan c-call:int) (start c-call:int) (samples c-call:int)  (arr (* double-float)))
    (defun file->array (file chan start samples arr) (file->array-1 file chan start samples (array-data-address arr)))
    (def-alien-routine ("clm_close_output" clm-close-output-1) c-call:int)
    (def-alien-routine ("clm_close_reverb" clm-close-reverb-1) c-call:int)
    (def-alien-routine ("clm_make_output" clm-make-output) c-call:int
      (file c-call:c-string) (chans c-call:int) (frm c-call:int) (typ c-call:int) (com c-call:c-string))
    (def-alien-routine ("clm_make_reverb" clm-make-reverb) c-call:int
      (file c-call:c-string) (chans c-call:int) (frm c-call:int) (typ c-call:int) (com c-call:c-string))
    (def-alien-routine ("clm_continue_output" clm-continue-output) c-call:int (file c-call:c-string))
    (def-alien-routine ("clm_continue_reverb" clm-continue-reverb) c-call:int (file c-call:c-string))
    (def-alien-routine ("mus_srate" mus-srate) double-float)
    (def-alien-routine ("mus_set_srate" mus-set-srate-1) double-float (val double-float))
    (def-alien-routine ("cl_clm_file_buffer_size" mus-file-buffer-size) c-call:int)
    (def-alien-routine ("cl_clm_set_file_buffer_size" mus-set-file-buffer-size) c-call:int (val c-call:int))
    (def-alien-routine ("clm_set_output_safety" clm-set-output-safety) c-call:void (val c-call:int))
    (def-alien-routine ("clm_set_reverb_safety" clm-set-reverb-safety) c-call:void (val c-call:int))

    (defun clm-random (a) (clm-random-1 (coerce a 'double-float)))
    (defun mus-set-srate (a) (mus-set-srate-1 (coerce a 'double-float)))
    )


;;; ---------------------------------------- SBCL ----------------------------------------
;#+sbcl (use-package "SB-ALIEN")

;; from contrib/sb-grovel/array-data.lisp
#+sbcl (defun array-data-address (array)
	 "Return the physical address of where the actual data of an array is
stored.

ARRAY must be a specialized array type - an array of one of these types:

                  double-float
                  single-float
                  (unsigned-byte 32)
                  (unsigned-byte 16)
                  (unsigned-byte  8)
                  (signed-byte 32)
                  (signed-byte 16)
                  (signed-byte  8)
"
  (declare (type (or (array (signed-byte 8))
		     (array base-char)
		     simple-base-string
                     (array (signed-byte 16))
                     (array (signed-byte 32))
                     (array (unsigned-byte 8))
                     (array (unsigned-byte 16))
                     (array (unsigned-byte 32))
                     (array single-float)
                     (array double-float))
                 array)
           (optimize (speed 0) (debug 3) (safety 3)))
  ;; with-array-data will get us to the actual data.  However, because
  ;; the array could have been displaced, we need to know where the
  ;; data starts.

  (let* ((type (car (multiple-value-list (array-element-type array))))
	 (type-size
	  (cond ((or (equal type '(signed-byte 8))
		     (equal type 'cl::base-char)
		     (equal type '(unsigned-byte 8)))
		 1)
		((or (equal type '(signed-byte 16))
		     (equal type '(unsigned-byte 16)))
		 2)
		((or (equal type '(signed-byte 32))
		     (equal type '(unsigned-byte 32)))
		 4)
		((equal type 'single-float)
		 4)
		((equal type 'double-float)
		 8)
		(t (error "Unknown specialized array element type")))))
    (sb-kernel::with-array-data ((data array)
		      (start)
		      (end))
      (declare (ignore end))
      ;; DATA is a specialized simple-array.  Memory is laid out like this:
      ;;
      ;;   byte offset    Value
      ;;        0         type code (e.g. 70 for double-float vector)
      ;;        4         FIXNUMIZE(number of elements in vector)
      ;;        8         1st element of vector
      ;;      ...         ...
      ;;
      (let* ((addr (+ 8 (logandc1 7 (sb-kernel:get-lisp-obj-address data)))))
	(declare (type (unsigned-byte 32) addr)
		 (optimize (speed 3) (safety 0)))
	(sb-sys:int-sap (the (unsigned-byte 32)
			  (+ addr (* type-size start))))))))


#+sbcl
(progn
  
    (sb-alien:define-alien-routine ("sl_dac" sl-dac-1) sb-alien:int (name sb-alien:c-string) (dev sb-alien:int))
    (sb-alien:define-alien-routine ("initialize_cmus" initialize-cmus) sb-alien:void)
    (sb-alien:define-alien-routine ("clm_sound_maxamp" sound-maxamp-1) sb-alien:int
				   (ifile sb-alien:c-string) (chans sb-alien:int) (vals (* double-float)) (times (* sb-alien:int)))
    (defun sound-maxamp (file chans arr times) (sound-maxamp-1 file chans (array-data-address arr) (array-data-address times)))

    (sb-alien:define-alien-routine ("mus_autocorrelate" autocorrelate-1) sb-alien:void
				   (rl1 (* double-float)) (len sb-alien:int))
    (defun autocorrelate (rl1 len) (autocorrelate-1 (array-data-address rl1) len))
    (sb-alien:define-alien-routine ("mus_correlate" correlate-1) sb-alien:void
				   (rl1 (* double-float)) (im1 (* double-float)) (len sb-alien:int))
    (defun correlate (rl1 im1 len) (correlate-1 (array-data-address rl1) (array-data-address im1) len))

    (sb-alien:define-alien-routine ("mus_convolution" convolution-1) sb-alien:void
				   (rl1 (* double-float)) (im1 (* double-float)) (len sb-alien:int))
    (defun convolution (rl1 im1 len) (convolution-1 (array-data-address rl1) (array-data-address im1) len))

    (sb-alien:define-alien-routine ("mus_fft" clm-fft-1) sb-alien:void
				   (rl (* double-float)) (im (* double-float)) (len sb-alien:int) (isign sb-alien:int))
    (defun clm-fft (rl im len isign) (clm-fft-1 (array-data-address rl) (array-data-address im) len isign))
    (sb-alien:define-alien-routine ("mus_set_rand_seed" mus-set-rand-seed) sb-alien:void (seed sb-alien:int))
    (sb-alien:define-alien-routine ("mus_frandom" clm-random-1) sb-alien:float (amp double-float))
    (sb-alien:define-alien-routine ("clm_array2file" array->file-1) sb-alien:void
				   (file sb-alien:c-string) (data (* double-float)) (len sb-alien:int) (srate sb-alien:int) (chans sb-alien:int))
    (defun array->file (file data len srate chans) (array->file-1 file (array-data-address data) len srate chans))
    (sb-alien:define-alien-routine ("clm_mix" clm-mix) 
	sb-alien:void
 	(outfile sb-alien:c-string) (infile sb-alien:c-string)  (out_start sb-alien:int) (out_framples sb-alien:int) (in_start sb-alien:int))
    (sb-alien:define-alien-routine ("clm_scale_file" clm-scale-file) sb-alien:int
				   (outfile sb-alien:c-string) (infile sb-alien:c-string) (scaler double-float) (frm sb-alien:int) (hdr sb-alien:int))
    (sb-alien:define-alien-routine ("clm_file2array" file->array-1) 
	sb-alien:int 
	(file sb-alien:c-string) (chan sb-alien:int) (start sb-alien:int) (samples sb-alien:int)  (arr (* double-float)))
    (defun file->array (file chan start samples arr) (file->array-1 file chan start samples (array-data-address arr)))
    (sb-alien:define-alien-routine ("clm_close_output" clm-close-output-1) sb-alien:int)
    (sb-alien:define-alien-routine ("clm_close_reverb" clm-close-reverb-1) sb-alien:int)
    (sb-alien:define-alien-routine ("clm_make_output" clm-make-output) sb-alien:int
      (file sb-alien:c-string) (chans sb-alien:int) (frm sb-alien:int) (typ sb-alien:int) (com sb-alien:c-string))
    (sb-alien:define-alien-routine ("clm_make_reverb" clm-make-reverb) sb-alien:int
      (file sb-alien:c-string) (chans sb-alien:int) (frm sb-alien:int) (typ sb-alien:int) (com sb-alien:c-string))
    (sb-alien:define-alien-routine ("clm_continue_output" clm-continue-output) sb-alien:int (file sb-alien:c-string))
    (sb-alien:define-alien-routine ("clm_continue_reverb" clm-continue-reverb) sb-alien:int (file sb-alien:c-string))
    (sb-alien:define-alien-routine ("mus_srate" mus-srate) sb-alien:double-float)
    (sb-alien:define-alien-routine ("mus_set_srate" mus-set-srate-1) sb-alien:double-float (val sb-alien:double-float))
    (sb-alien:define-alien-routine ("cl_clm_file_buffer_size" mus-file-buffer-size) sb-alien:int)
    (sb-alien:define-alien-routine ("cl_clm_set_file_buffer_size" mus-set-file-buffer-size) sb-alien:int (val sb-alien:int))
    (sb-alien:define-alien-routine ("clm_set_output_safety" clm-set-output-safety) sb-alien:void (val sb-alien:int))
    (sb-alien:define-alien-routine ("clm_set_reverb_safety" clm-set-reverb-safety) sb-alien:void (val sb-alien:int))

    (defun clm-random (a) (clm-random-1 (coerce a 'double-float)))
    (defun mus-set-srate (a) (mus-set-srate-1 (coerce a 'double-float)))
)


;;; ---------------------------------------- OPENMCL ----------------------------------------

#+openmcl
(progn

  (defmacro with-c-filename ((p f) &body body)
    `(ccl:with-cstrs ((,p (filename->string (translate-logical-pathname ,f))))
		     ,@body))

  (defun heap-float* (hfarray &aux (p (ccl:%null-ptr)))
    (ccl::%vect-data-to-macptr hfarray p)
    p)

  (defun heap-int* (hiarray &aux (p (ccl:%null-ptr)))
    (ccl::%vect-data-to-macptr hiarray p)
    p)

  ;; (defun get-cstring (str) (when (not (ccl:%null-ptr-p str)) (%get-cstring str)))
  ;; this is in sndlib2clm.lisp

  (defun reset-audio () (ccl::external-call (clm_ffi_name "mus_reset_audio_c") :void))
  (defun reset-headers () (ccl::external-call (clm_ffi_name "mus_reset_headers_c") :void))
  (defun reset-io () (ccl::external-call (clm_ffi_name "mus_reset_io_c") :void))
  (defun initialize-cmus () (ccl::external-call (clm_ffi_name "initialize_cmus") :void))
  (defun mus-set-rand-seed (seed) (ccl::external-call (clm_ffi_name "mus_set_rand_seed") :signed seed :void))
  (defun sl-dac-1 (name dev)
    (warn "sl-dac doesn't work in openmcl"))
  (defun sound-maxamp (file chans arr times)
    (ccl:with-cstrs ((f file))
      (ccl::with-foreign-double-float-array-to-c-and-lisp (p arr) ; make sure values get back to lisp
        (ccl::external-call (clm_ffi_name "clm_sound_maxamp") :address f :signed chans :address p :address (heap-int* times) :signed))))
  (defun clm-random-1 (amp) (ccl::external-call (clm_ffi_name "mus_frandom") :double-float amp :double-float))
  (defun clm-random (a) (clm-random-1 (coerce a 'double-float)))
  (defun array->file (file data len srate chans)
    (ccl:with-cstrs ((f file))
      (ccl::with-foreign-double-float-array (p data)
        (ccl::external-call (clm_ffi_name "clm_array2file") :address f :address p :signed len :signed srate :signed chans :void))))
  (defun clm-scale-file (ofile ifile scaler frm hdr)
    (ccl:with-cstrs ((f1 ofile) (f2 ifile))
      (ccl::external-call (clm_ffi_name "clm_scale_file") :address f1 :address f2 :double-float scaler :signed frm :signed hdr :signed)))
  (defun clm-fft (rl im len isign)
    (ccl::with-foreign-double-float-array-to-c-and-lisp (arl rl)
      (ccl::with-foreign-double-float-array-to-c-and-lisp (aim im)
        (ccl::external-call (clm_ffi_name "mus_fft") :address arl :address aim :signed len :signed isign :void))))
  (defun autocorrelate (rl len)
    (ccl::with-foreign-double-float-array-to-c-and-lisp (arl rl)
      (ccl::external-call (clm_ffi_name "mus_autocorrelate") :address arl :signed len :void)))
  (defun correlate (rl im len)
    (ccl::with-foreign-double-float-array-to-c-and-lisp (arl rl)
      (ccl::with-foreign-double-float-array-to-c-and-lisp (aim im)
        (ccl::external-call (clm_ffi_name "mus_correlate") :address arl :address aim :signed len :void))))
  (defun convolution (rl im len)
    (ccl::with-foreign-double-float-array-to-c-and-lisp (arl rl)
      (ccl::with-foreign-double-float-array-to-c-and-lisp (aim im)
        (ccl::external-call (clm_ffi_name "mus_convolution") :address arl :address aim :signed len :void))))
  (defun file->array (file chan start samples arr)
    (ccl:with-cstrs ((f file))
      (ccl::with-foreign-double-float-array-to-lisp (p arr)
        (ccl::external-call (clm_ffi_name "clm_file2array") :address f :signed chan :signed start :signed samples :address p :signed))))
  (defun clm-mix (ofile ifile ostart oframples istart)
     (ccl:with-cstrs ((f1 ofile) (f2 ifile))
       (ccl::external-call (clm_ffi_name "clm_mix")
 			 :address f1 :address f2 :signed ostart :signed oframples :signed istart
 			 :void)))
  (defun clm-close-output-1 () (ccl::external-call (clm_ffi_name "clm_close_output") :signed))
  (defun clm-close-reverb-1 () (ccl::external-call (clm_ffi_name "clm_close_reverb") :signed))
  (defun clm-make-output (file chans frm typ com)
    (ccl:with-cstrs ((f1 file)
		     (c1 (or com "")))
      (ccl::external-call (clm_ffi_name "clm_make_output")
			 :address f1 :signed chans :signed frm :signed typ :address c1
			 :signed)))
  (defun clm-make-reverb (file chans frm typ com)
    (ccl:with-cstrs ((f1 file)
		     (c1 (or com "")))
      (ccl::external-call (clm_ffi_name "clm_make_reverb")
			 :address f1 :signed chans :signed frm :signed typ :address c1
			 :signed)))
  (defun clm-continue-output (file)
    (ccl:with-cstrs ((f1 file)) (ccl::external-call (clm_ffi_name "clm_continue_output") :address f1 :signed)))    
  (defun clm-continue-reverb (file)
    (ccl:with-cstrs ((f1 file)) (ccl::external-call (clm_ffi_name "clm_continue_reverb") :address f1 :signed)))    
  (defun mus-srate () (ccl::external-call (clm_ffi_name "mus_srate") :double-float))
  (defun mus-set-srate (val) (ccl::external-call (clm_ffi_name "mus_set_srate") :double-float val :double-float))
  (defun mus-file-buffer-size () (ccl::external-call (clm_ffi_name "cl_clm_file_buffer_size") :signed))
  (defun mus-set-file-buffer-size (val) (ccl::external-call (clm_ffi_name "cl_clm_set_file_buffer_size") :signed val :signed))
  (defun clm-set-output-safety (val) (ccl::external-call (clm_ffi_name "clm_set_output_safety") :signed val :void))
  (defun clm-set-reverb-safety (val) (ccl::external-call (clm_ffi_name "clm_set_reverb_safety") :signed val :void))
)


;;; ---------------------------------------- CLISP ----------------------------------------

#+clisp (use-package "FFI")
;; #+clisp (defun full-lib-name () (expand-filename->string "libclm.so"))
;; in sndlib2clm.lisp
#+clisp (ffi:default-foreign-language :stdc)
#+clisp
(progn
    (ffi:def-call-out sl-dac-1 (:name "sl_dac") (:library (full-lib-name)) (:return-type ffi:int) 
      (:arguments (name ffi:c-string) (dev ffi:int)))
    (ffi:def-call-out initialize-cmus (:name "initialize_cmus") (:library (full-lib-name)) 
      (:return-type ffi:nil))
    (ffi:def-call-out clm-little-endian (:name "clm_little_endian") (:library (full-lib-name)) (:return-type ffi:int))
    (ffi:def-call-out sound-maxamp-init (:name "clm_sound_maxamp_init") (:library (full-lib-name)) (:return-type ffi:int) 
      (:arguments (ifile ffi:c-string) (chans ffi:int)))
    (ffi:def-call-out sound-maxamp-time (:name "clm_sound_maxamp_time") (:library (full-lib-name)) (:return-type ffi:int)
      (:arguments (chan ffi:int)))	     
    (ffi:def-call-out sound-maxamp-amp (:name "clm_sound_maxamp_amp") (:library (full-lib-name)) (:return-type ffi:double-float)
      (:arguments (chan ffi:int)))
    (ffi:def-call-out mus-set-rand-seed (:name "mus_set_rand_seed") (:library (full-lib-name)) (:return-type ffi:nil) 
      (:arguments (seed ffi:int)))
    (ffi:def-call-out clm-random-1 (:name "mus_frandom") (:library (full-lib-name)) (:return-type ffi:double-float) 
      (:arguments (amp ffi:double-float)))
    (ffi:def-call-out clm-mix (:name "clm_mix") (:library (full-lib-name)) (:return-type ffi:nil)
      (:arguments (outfile ffi:c-string) (infile ffi:c-string)  (out_start ffi:int) (out_framples ffi:int) (in_start ffi:int)))
    (ffi:def-call-out clm-scale-file (:name "clm_scale_file") (:library (full-lib-name)) (:return-type ffi:int)
      (:arguments (outfile ffi:c-string) (infile ffi:c-string) (scaler ffi:double-float) (frm ffi:int) (hdr ffi:int)))
    (ffi:def-call-out clm-close-output-1 (:name "clm_close_output") (:library (full-lib-name)) (:return-type ffi:int))
    (ffi:def-call-out clm-close-reverb-1 (:name "clm_close_reverb") (:library (full-lib-name)) (:return-type ffi:int))
    (ffi:def-call-out clm-make-output (:name "clm_make_output") (:library (full-lib-name)) (:return-type ffi:int)
      (:arguments (file ffi:c-string) (chans ffi:int) (frm ffi:int) (typ ffi:int) (com ffi:c-string)))
    (ffi:def-call-out clm-make-reverb (:name "clm_make_reverb") (:library (full-lib-name)) (:return-type ffi:int) 
      (:arguments (file ffi:c-string) (chans ffi:int) (frm ffi:int) (typ ffi:int) (com ffi:c-string)))
    (ffi:def-call-out clm-continue-output (:name "clm_continue_output") (:library (full-lib-name)) (:return-type ffi:int) 
      (:arguments (file ffi:c-string)))
    (ffi:def-call-out clm-continue-reverb (:name "clm_continue_reverb") (:library (full-lib-name)) (:return-type ffi:int) 
      (:arguments (file ffi:c-string)))
    (ffi:def-call-out mus-srate (:name "mus_srate") (:library (full-lib-name)) (:return-type ffi:double-float))
    (ffi:def-call-out mus-set-srate-1 (:name "mus_set_srate") (:library (full-lib-name)) (:return-type ffi:double-float) 
      (:arguments (val ffi:double-float)))
    (ffi:def-call-out mus-file-buffer-size (:name "cl_clm_file_buffer_size") (:library (full-lib-name)) (:return-type ffi:int))
    (ffi:def-call-out mus-set-file-buffer-size (:name "cl_clm_set_file_buffer_size") (:library (full-lib-name)) (:return-type ffi:int) 
      (:arguments (val ffi:int)))
    (ffi:def-call-out clm-set-output-safety (:name "clm_set_output_safety") (:library (full-lib-name)) (:return-type ffi:nil) 
      (:arguments (val ffi:int)))
    (ffi:def-call-out clm-set-reverb-safety (:name "clm_set_reverb_safety") (:library (full-lib-name)) (:return-type ffi:nil) 
      (:arguments (val ffi:int)))

    (ffi:def-call-out array->file (:name "clm_array2file") (:library (full-lib-name)) (:return-type ffi:nil) 
      (:arguments (file ffi:c-string) (data (ffi:c-array-ptr ffi:double-float)) (len ffi:int) (srate ffi:int) (chans ffi:int)))

    (defun sound-maxamp (file chans arr times)
      (let ((end-time (sound-maxamp-init file chans)))
	(do ((i 0 (1+ i)))
	    ((= i chans))
	  (setf (aref arr i) (sound-maxamp-amp i))
	  (setf (aref times i) (sound-maxamp-time i)))
	end-time))

    (defun clm-random (a) (clm-random-1 (coerce a 'double-float)))
    (defun mus-set-srate (a) (mus-set-srate-1 (coerce a 'double-float)))

;    (ffi:def-call-out convolution-1 (:name "mus_convolution") (:library (full-lib-name)) (:return-type ffi:nil) 
;      (:arguments (rl1 (ffi:c-array-ptr ffi:double-float)) (im1 (ffi:c-array-ptr ffi:double-float)) (len ffi:int)))
;    (ffi:def-call-out clm-fft-1 (:name "mus_fft") (:library (full-lib-name)) (:return-type ffi:nil) 
;      (:arguments (rl (ffi:c-array-ptr ffi:double-float)) (im (ffi:c-array-ptr ffi:double-float)) (len ffi:int) (isign ffi:int)))

    (defun convolution (rl1 im1 len) (declare (ignore rl1 im1 len)) (warn "convolution doesn't work in clisp"))
    (defun autocorrelate (rl1 len) (declare (ignore rl1 len)) (warn "autocorrelate doesn't work in clisp"))
    (defun correlate (rl1 im1 len) (declare (ignore rl1 im1 len)) (warn "correlate doesn't work in clisp"))
    (defun clm-fft (rl im len isign) (declare (ignore rl im len isign)) (warn "clm-fft doesn't work in clisp"))

    (ffi:def-call-out clm-clisp-file->array-init (:name "clm_clisp_file2array_init") (:library (full-lib-name)) (:return-type ffi:int) 
      (:arguments (file ffi:c-string) (chan ffi:int) (start ffi:int) (samples ffi:int)))
    (ffi:def-call-out clm-clisp-file->array (:name "clm_clisp_file2array") (:library (full-lib-name)) (:return-type ffi:double-float) 
      (:arguments (i ffi:int)))

    (defun file->array (file chan start samples arr)
      (clm-clisp-file->array-init file chan start samples)
      (do ((i 0 (1+ i)))
	  ((= i samples))
	(setf (aref arr i) (clm-clisp-file->array i)))
      arr)

    )


;;; ---------------------------------------- LISPWORKS ----------------------------------------
#+lispworks
(progn
  (fli:define-foreign-function (sl-dac-1 "sl_dac")
      ((name (:reference-pass :ef-mb-string))
       (dev :int))
    :result-type :int)
  (fli:define-foreign-function (initialize-cmus "initialize_cmus") () :result-type :void)

  (fli:define-foreign-function (sound-maxamp "clm_sound_maxamp")
      ((ifile (:reference-pass :ef-mb-string)) (chans :int) (arr :lisp-array) (times :lisp-array))
    :result-type :int)

  (fli:define-foreign-function (autocorrelate "mus_autocorrelate")
      ((rl1 :lisp-array)
       (n :int))
    :result-type :void)

  (fli:define-foreign-function (correlate "mus_correlate")
      ((rl1 :lisp-array) (rl2 :lisp-array) (n :int))
    :result-type :void)

  (fli:define-foreign-function (convolution "mus_convolution")
      ((rl1 :lisp-array) (rl2 :lisp-array) (n :int))
    :result-type :void)

  (fli:define-foreign-function (clm-fft "mus_fft")
      ((rl :lisp-array) (im :lisp-array) (n :int) (isign :int))
    :result-type :void)
  (fli:define-foreign-function (mus-set-rand-seed "mus_set_rand_seed") ((seed :int)) :result-type :void)
  (fli:define-foreign-function (clm-random-1 "mus_frandom") ((amp :double)) :result-type :double)
  (defun clm-random (a) (clm-random-1 (coerce a 'double-float)))
  (fli:define-foreign-function (array->file "clm_array2file")
      ((file (:reference :ef-mb-string))
       (data :lisp-array)
       (len :int)
       (srate :int)
       (chans :int))
    :result-type :void)
  (fli:define-foreign-function (clm-mix "clm_mix")
      ((outfile (:reference-pass :ef-mb-string))
       (infile (:reference-pass :ef-mb-string))
       (out_start :int)
       (out_framples :int)
       (in_start :int))
    :result-type :void)
  (fli:define-foreign-function (clm-scale-file "clm_scale_file")
      ((outfile (:reference :ef-mb-string))
       (infile (:reference :ef-mb-string))
       (scaler :double)
       (frm :int)
       (hdr :int))
    :result-type :int)
  (fli:define-foreign-function (file->array "clm_file2array") 
      ((file (:reference :ef-mb-string))
       (chan :int)
       (start :int)
       (samples :int)
       (arr :lisp-array))
    :result-type :int)
  (fli:define-foreign-function (clm-close-output-1 "clm_close_output") () :result-type :int)
  (fli:define-foreign-function (clm-close-reverb-1 "clm_close_reverb") () :result-type :int)
  (fli:define-foreign-function (clm-make-output "clm_make_output")
      ((file (:reference-pass :ef-mb-string))
       (chans :int)
       (frm :int)
       (typ :int)
       (com (:reference-pass :ef-mb-string)))
    :result-type :int)
  (fli:define-foreign-function (clm-make-reverb "clm_make_reverb")
      ((file (:reference-pass :ef-mb-string))
       (chans :int)
       (frm :int)
       (typ :int)
       (com (:reference-pass :ef-mb-string)))
    :result-type :int)
  (fli:define-foreign-function (clm-continue-output "clm_continue_output")
      ((file (:reference-pass :ef-mb-string)))
    :result-type :int)
  (fli:define-foreign-function (clm-continue-reverb "clm_continue_reverb")
      ((file (:reference-pass :ef-mb-string)))
    :result-type :int)
  (fli:define-foreign-function (mus-srate "mus_srate") () :result-type :double)
  (fli:define-foreign-function (mus-set-srate "mus_set_srate") ((srate :double)) :result-type :double)
  (fli:define-foreign-function (mus-file-buffer-size "cl_clm_file_buffer_size") () :result-type :int)
  (fli:define-foreign-function (mus-set-file-buffer-size "cl_clm_set_file_buffer_size") ((clipped :int)) :result-type :int)    
  (fli:define-foreign-function (clm-set-output-safety "clm_set_output_safety") ((safety :int)) :result-type :void)
  (fli:define-foreign-function (clm-set-reverb-safety "clm_set_reverb_safety") ((safety :int)) :result-type :void)
  )
