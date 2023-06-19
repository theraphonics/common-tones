(in-package :common-tones)

;;; foreign function interfaces using portable cffi library.
;;; this file links to cmus.c and clm.c

(defun clm-close-output () (clm-close-output-1) (setf *output* nil))
(defun clm-close-reverb () (clm-close-reverb-1) (setf *reverb* nil))


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
;;; and I'd just have to add them back under some other name, so I'll leave them in.


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

(progn
    (cffi:defcfun ("sl_dac" sl-dac-1) :int (name :string) (dev :int))
    (cffi:defcfun ("initialize_cmus" initialize-cmus) :void)
    (cffi:defcfun ("clm_sound_maxamp" sound-maxamp-1) :int
        (ifile :string) (chans :int) (vals :pointer) (times :pointer))
    (defun sound-maxamp (file chans arr times) (sound-maxamp-1 file chans arr times))
    (cffi:defcfun ("mus_autocorrelate" autocorrelate-1) :void
        (rl1 :pointer) (len :int))
    (defun autocorrelate (rl1 len) (autocorrelate-1 rl1 len))
    (cffi:defcfun ("mus_correlate" correlate-1) :void
        (rl1 :pointer) (im1 :pointer) (len :int))
    (defun correlate (rl1 im1 len) (correlate-1 rl1 im1 len))
    (cffi:defcfun ("mus_convolution" convolution-1) :void
        (rl1 :pointer) (im1 :pointer) (len :int))
    (defun convolution (rl1 im1 len) (convolution-1 rl1 im1 len))
    (cffi:defcfun ("mus_fft" clm-fft-1) :void
        (rl :pointer) (im :pointer) (len :int) (isign :int))
    (defun clm-fft (rl im len isign) (clm-fft-1 rl im len isign))
    (cffi:defcfun ("mus_set_rand_seed" mus-set-rand-seed) :void (seed :int))
    (cffi:defcfun ("mus_frandom" clm-random-1) :float (amp :double))
    (cffi:defcfun ("clm_array2file" array->file-1) :void
        (file :string) (data :pointer) (len :int) (srate :int) (chans :int))
    (defun array->file (file data len srate chans) (array->file-1 file data len srate chans))
    (cffi:defcfun ("clm_mix" clm-mix)
        :void
        (outfile :string) (infile :string)  (out_start :int) (out_framples :int) (in_start :int))
    (cffi:defcfun ("clm_scale_file" clm-scale-file) :int
        (outfile :string) (infile :string) (scaler :double) (frm :int) (hdr :int))
    (cffi:defcfun ("clm_file2array" file->array-1)
        :int
        (file :string) (chan :int) (start :int) (samples :int)  (arr :pointer))
    (defun file->array (file chan start samples arr) (file->array-1 file chan start samples arr))
    (cffi:defcfun ("clm_close_output" clm-close-output-1) :int)
    (cffi:defcfun ("clm_close_reverb" clm-close-reverb-1) :int)
    (cffi:defcfun ("clm_make_output" clm-make-output) :int
      (file :string) (chans :int) (frm :int) (typ :int) (com :string))

    (cffi:defcfun ("clm_make_reverb" clm-make-reverb) :int
      (file :string) (chans :int) (frm :int) (typ :int) (com :string))

    (cffi:defcfun ("clm_continue_output" clm-continue-output) :int
      (file :string))

    (cffi:defcfun ("clm_continue_reverb" clm-continue-reverb) :int
      (file :string))

    (cffi:defcfun ("mus_srate" mus-srate) :double)

    (cffi:defcfun ("mus_set_srate" mus-set-srate-1) :double
      (val :double))

    (cffi:defcfun ("cl_clm_file_buffer_size" mus-file-buffer-size) :int)

    (cffi:defcfun ("cl_clm_set_file_buffer_size" mus-set-file-buffer-size) :int
      (val :int))

    (cffi:defcfun ("clm_set_output_safety" clm-set-output-safety) :void
      (val :int))

    (cffi:defcfun ("clm_set_reverb_safety" clm-set-reverb-safety) :void
      (val :int))

    (defun clm-random (a)
      (clm-random-1 (coerce a 'double-float)))

    (defun mus-set-srate (a)
      (mus-set-srate-1 (coerce a 'double-float))))
