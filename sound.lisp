(in-package :common-tones)

(defvar *open-input-explicit-output* nil)
(defvar *open-input-explicit-reverb* nil)
(defvar *open-input-verbose* nil)
(defvar *open-input-pathname* nil)
(defvar *open-input-truename* nil)
(defvar *clm-scaled-amp* nil)

(defvar last-dac-filename #-(or cmu sbcl openmcl) nil #+(or cmu sbcl openmcl) *clm-file-name*)
#+(or excl cmu sbcl) (defvar *dac-pid* nil)

(defvar *clm-dac-wait-default* nil)

#+acl-80 (defun acl-dac (name &optional (dev 0)) (sl-dac-1 name dev))
#+(and acl-80 (not mac-osx)) (setf *clm-player* #'acl-dac)

(defun play (&optional name-1 &key start end (wait *clm-dac-wait-default*))
  #-(or excl openmcl cmu sbcl) (declare (ignore wait))
  (clm-initialize-links)
  (let ((filename (if name-1
		      (filename->string #-excl (translate-logical-pathname (->pathname (filename->string name-1)))
				  #+excl (full-merge-pathnames name-1 *clm-file-name*) ; excl screws up in translate-logical-pathname
				  )
		    last-dac-filename)))
    (if (not filename)
	(warn "no previous file to play")
      (if (functionp *clm-player*) ;for example (setf *clm-player* #'sl-dac) -- sl-dac is in clm3.lisp (sl-dac-1 in ffi.lisp)
	  (funcall *clm-player* filename)
	(let* ((sndplay (or *clm-player*
			    (concatenate 'string *clm-binary-directory* "sndplay" #+windoze ".exe")))
	       (command-args
		;; this is a nightmare because every lisp handles external programs differently,
		;;   and we're trying to allow the user to stop the dac prematurely
		;;
		;; EXCL -- use vector
		#+(and excl (not windoze))
		(let ((args (list sndplay sndplay filename)))
		  (if (and (not *clm-player*)
			   (or start end))
		      (progn
			(if start (setf args (append args (list "-start" (format nil "~A" start)))))
			(if end (setf args (append args (list "-end" (format nil "~A" end)))))))
		  (apply #'vector args))
		;;
		;; CMU, SBCL, and OPENMCL -- use list
		#+(or cmu sbcl openmcl)
		(let ((args (list filename)))
		  (if (not *clm-player*)
		      (progn
			(if start
			    (setf args (append args (list "-start" (format nil "~A" start)))))
			(if end
			    (setf args (append args (list "-end" (format nil "~A" end)))))
			#+mac-osx (if (= *clm-output-properties-mutable* 0)
				      (setf args (append args (list "-mutable 0"))))
			))
		  args)
		;;
		;; ELSE (not always relevant)
		#-(or cmu sbcl openmcl (and excl (not windoze)))
		(format nil "~A~A~A"
			filename
			(if start (format nil " -start ~A" start) "")
			(if end (format nil " -end ~A" end) ""))
		))
	  (when (not (probe-file sndplay))
	    (setf sndplay "sndplay")) ; hope there's a system version, I guess
	  ;;
	  ;; SNDPLAY
	  (progn
	  ;; Wait for a previous play command
	  	(when *dac-pid*
	  		(uiop:wait-process *dac-pid*))
	  	(setf *dac-pid*
	  		(uiop:run-program "sndplay" command-args :wait wait)))
	  (if filename (setf last-dac-filename filename)))))
    last-dac-filename))

(defun stop-playing ()
  (when *dac-pid*
  	(let ((pid (uiop:process-info-pid *dac-pid*)))
  		(print pid)
  		(force-output)
  		(uiop:terminate-process pid :sigterm)
  		(uiop:wait-process pid))
  	(setf *dac-pid* nil)))

(defun dac (&optional name-1 &key
		      start end (wait *clm-dac-wait-default*))
  (play name-1 :start start :end end :wait wait))

(defun stop-dac () (stop-playing))


(defvar *force-recomputation* nil)

;;; in all that follows, we have nested with-sounds, so various globals like *offset*=
;;; need to be handled dynamically, but we also need to protect against two kinds of errors --
;;; exit from the debugger, wherein we have to leave CLM/Lisp in a clean state, and user-forgetfulness
;;; with regard to open-input (it's easy to leave files open accidentally).  Additionally, with-sound
;;; can be called within the debugger while in with-sound.  We used to call clm-cleanup all the time
;;; but that closed all open files, and cleared *offset* which was the wrong thing, especially
;;; if the user was handling mus_any structs globally across a with-mix call.  So, we try to see the normal
;;; exit via *clm-with-sound-depth*, and error exits via *clm-within-with-sound*.  clm-cleanup from
;;; io.lisp is still a complete wipe-the-slate function.
;;; An added complication is that mix can open files in C (hidden from clm's list of open files),
;;; an IO error can occur (disk full), then checked_write can exit back to lisp, whereupon the
;;; caller can :reset from the debugger, leaving files open with no way to free the space except
;;; exit from lisp!  So I added (22-Oct-98) mus_file_cleanup_descriptors which runs through
;;; the entire C array of file descriptors, tries to deduce which files are still open, and
;;; close them -- this means the error exit from with-sound will clobber any files global
;;; to that call.  Can't decide whether something fancier is needed.

(defvar *clm-with-sound-depth* 0)
(defvar *clm-within-with-sound* nil)
(defvar *clm-with-sound-body* nil)

(defun clm-reset ()			;complete cleanup -- return to blank slate
  (clm-cleanup)			        ;close all other files and reset various global variables
  (setf *clm-with-sound-depth* 0)
  (setf *clm-within-with-sound* nil))

(defun clm-check-for-reset ()		;this is a possibly-no-problem cleanup (i.e. just check for forgotten files)
  (when (or *clm-within-with-sound*	;oops -- reset somewhere
	    (zerop *clm-with-sound-depth*)) ;normal exit to top-level
    (clm-reset)))			;so cleanup completely

(defun cl-printf (s) (princ s) (force-output))
(defun clm-reports (str) (if *verbose* (cl-printf str)))

(defun scale-to-file (out-file scaled-to &optional data-format stats header-type)
  (let* ((old-out-file out-file)
	 (ovals (or stats (mus-sound-maxamp out-file))))
    (setf out-file (subseq out-file 0 (- (length out-file) 5)))
    (setf *offset* 0)
    (let ((oldamp (if stats
		      clm-max-stat-amp
		    (loop for i from 0 below (length ovals) maximize (aref ovals i)))))
      (if (= oldamp 0.0) (setf oldamp 1.0))
      (setf *clm-scaled-amp* (/ scaled-to oldamp))
      (clm-scale-file out-file old-out-file (double (/ scaled-to oldamp))
		      (or data-format *data-format*) (or header-type *header-type*)))
    (delete-file old-out-file)
    out-file))

(defun scale-by-file (out-file scaled-by &optional data-format stats header-type)
  (declare (ignore stats))
  (let* ((old-out-file out-file))
    (setf out-file (subseq out-file 0 (- (length out-file) 5)))
    (setf *offset* 0)
    (clm-scale-file out-file old-out-file (double scaled-by) (or data-format *data-format*) (or header-type *header-type*))
    (delete-file old-out-file)
    out-file))

(defun make-default-comment (comment info save-body)
  (concatenate 'string
	       (or info
		   (if comment
		       (if (> (length comment) 0)
			   (format nil "#| ~A |#" comment))
		     (if *open-input-explicit-output*
			 (concatenate 'string
				      (make-banner)
				      (format nil " (from ~A via open-input)"
					      (filename->string *open-input-truename*)))
		       (make-banner))))
	       (if save-body
		   (format nil " #| ~A |#" (write-to-string *clm-with-sound-body*))
		 "")))

(defun begin-with-sound (srate channels scaled-to out-file revf statistics continue-old-file
			 reverb-channels header-type data-format clipped scaled-by comment)
  (setf *clm-within-with-sound* t)
  (when (or scaled-to scaled-by) (setf out-file (concatenate 'string out-file ".temp")))
  (if statistics (initialize-statistics statistics out-file revf))

  (let ((our-format (if scaled-to *clm-tempfile-data-format* data-format))
	(our-type (if scaled-to *clm-tempfile-header-type* header-type)))
    (mus-set-srate (double srate))
    (mus-set-clipping clipped) ; translation to C handled by clm-mus-set-clipping in sndlib2clm.lisp
    (mus-set-file-buffer-size *clm-file-buffer-size*)
    (if continue-old-file
	(progn
	  (clm-continue-output out-file)
	  (setf *output* (make-frample->file out-file channels our-format our-type)) ; for lisp-side methods, not for output
	  (when revf
	    (clm-continue-reverb revf)
	    (setf *reverb* (make-frample->file revf reverb-channels our-format our-type)))
	  out-file)
      (if (mus-header-writable our-type our-format)
	;; returning from an error in the C code is problematic, but it's common to give
	;;   a bogus output file name, so we'll check for that error right away.

	(let ((f (open out-file :direction :output :if-exists :supersede)))
	  (if f
	      (progn
		(close f)
		(clm-make-output out-file channels our-format our-type (or comment ""))  ; cmus.c make_frample_to_file
		(setf *output* (make-frample->file out-file channels our-format our-type)) ; for lisp-side methods, not for output
		(when revf
		  (clm-make-reverb revf reverb-channels our-format our-type "temporary reverb stream")
		  (setf *reverb* (make-frample->file revf reverb-channels our-format our-type)))
		out-file)
	    nil))
	nil))))

(defun end-with-sound (scaled-to out-file revf statistics
		       header-type data-format reverb decay-time reverb-data
		       channels play scaled-by)
  (let ((rev-name (if *reverb* (mus-file-name *reverb*))))
    (when revf
      (clm-close-reverb)
      (when reverb
	(let ((*offset* 0))
	  (setf *reverb* (open-input rev-name :element-type :sound))
	  ;; *reverb* is now a file->sample generator
	  (apply reverb 0 (+ decay-time (sound-duration (mus-file-name *reverb*))) reverb-data)
	  (let ((full-reverb-name (mus-file-name *reverb*)))
	    (close-input *reverb*)
	    (if (and *clm-delete-reverb*
		     (probe-file full-reverb-name))
		(delete-file full-reverb-name))))))

    (clm-close-output)
    (if statistics
	(print-statistics (if scaled-to :scaled statistics) channels t scaled-to))
    (when scaled-to
      (setf out-file (scale-to-file out-file scaled-to data-format statistics header-type)))
    (when scaled-by
      (setf out-file (scale-by-file out-file scaled-by data-format statistics header-type)))
    (setf *clm-within-with-sound* nil)
    (when (and (not *open-input-explicit-output*) (= *clm-with-sound-depth* 1))
      (setf last-dac-filename out-file)
      (when play (dac out-file)))
    out-file))

(defmacro with-offset (val &body body) `(let ((*offset* (+ *offset* (round (* ,val *srate*))))) ,.body))

(defvar *ws-reverb-file* nil)

(defmacro with-sound ((&key (output *clm-file-name*)
			    continue-old-file
			    (channels *clm-channels*)
			    info
			    comment            ; set to :none to squelch the comment
			    (srate *clm-srate*)
			    reverb
			    reverb-data
			    (reverb-channels *clm-reverb-channels*)
			    revfile
			    (decay-time 1.0)
			    reverb-func
			    reverb-args
			    (play *clm-play*)
			    force-recomputation
			    (notehook *clm-notehook*)
			    (statistics *clm-statistics*)
			    (header-type *clm-header-type*)
			    (data-format *clm-data-format*)
			    save-body
			    (verbose *clm-verbose*)
			    scaled-to
			    (clipped *clm-clipped*)
			    scaled-by
			    sampling-rate
			    &allow-other-keys)
		      &body body)
  ;; it is possible to have revfile (i.e. reverb output stream) but no reverb function (with-mix)
  `(let ((*verbose* ,verbose)
	 (*offset* 0)
	 (common-tones::*interrupted* 0)
	 (common-tones::*statistics* nil)
	 (*clm-scaled-amp* nil)
	 (*notehook* ,notehook)
	 (*force-recomputation* ,force-recomputation)
	 (*clm-with-sound-body* (and ,save-body ',body))
	 (*channels* ,channels)
	 (*srate* ,(or sampling-rate srate))
	 (*header-type* ,header-type)
	 (*data-format* ,data-format)
	 (*clipped* ,clipped)
	 )
     (clm-initialize-links)
     (setf *clm-ins* nil)
     ;; close current computation, if any
     (let* ((old-output *output*)
	    (old-reverb *reverb*))
       (if old-output (clm-close-output))
       (if old-reverb (clm-close-reverb))
       ;(format t "old output: ~A~%" old-output)

       (unwind-protect
	   (let* ((out-file (or *open-input-explicit-output*
				(filename->string
				 (full-merge-pathnames
				  (translate-logical-pathname (->pathname (filename->string ,output)))
				  *clm-file-name*))))
		  (*clm-with-sound-depth* (1+ *clm-with-sound-depth*))
		  (our-srate ,(or sampling-rate srate))
		  (our-type ,header-type)
		  (our-format
		   (let ((first-guess ,data-format))
		     (if (and (= first-guess mus-bshort)
			      (= our-type mus-riff))
			 mus-lshort
		       (if (and (= first-guess mus-bint)
				(= our-type mus-riff))
			   mus-lint
			 first-guess))))
		  (our-reverb (or ,reverb-func ',reverb))
		  (our-reverb-data (or ,reverb-args ',reverb-data))
		  (reverb-filename (concatenate 'string (subseq out-file 0 (- (length out-file) (length (pathname-type out-file))))
						#-windoze "reverb"
						#+windoze "rev"))
		  (revf (if (or ,revfile our-reverb)
			    (or ,revfile *open-input-explicit-reverb* reverb-filename)))
		  (*ws-reverb-file* revf))
	     (if (and (frample->file? old-output)
		      (string= out-file (mus-file-name old-output)))
		 (warn "we're about to overwrite ~A..." out-file))
	     (setf out-file (begin-with-sound our-srate ,channels ,scaled-to out-file revf ,statistics
					      ,continue-old-file ,reverb-channels our-type
					      our-format ,clipped ,scaled-by
					      (and (not (eq ,comment :none))
						   (make-default-comment ,comment ,info ,save-body))))
	     (if out-file
		 ;; run with-sound body
		 (tagbody
		  (restart-case
		   (catch :FINISH
		     ,.body)	; mimic old style
		   (nil ()
			:report "close files and return to top-level."
			(progn (clm-reset) (go ALL-DONE)))
		   (nil ()
			:report "jump past remaining notes."
			(go NORMAL-FINISH)))
		  NORMAL-FINISH
		  (setf out-file (end-with-sound ,scaled-to out-file revf ,statistics our-type
						 our-format our-reverb ,decay-time our-reverb-data ,channels
						 ,play ,scaled-by
						 ))
		  ALL-DONE
		  )) ;end of tagbody

	     ;; reopen old computation, if any
	     (if old-output
		 (progn
		   (clm-continue-output (mus-file-name old-output))
		   (setf *output* old-output)))
	     (if old-reverb
		 (progn
		   (clm-continue-reverb (mus-file-name old-reverb))
		   (setf *reverb* old-reverb))
	       (setf *reverb* nil))
	     out-file)
       (progn
	 (setf *clm-ins* nil)
	 (setf *force-recomputation* nil)
	 (clm-check-for-reset))))))


(defvar *clm-threads* 4)

;SBCL version:
#+sb-thread
(defmacro with-threaded-sound ((&key (output *clm-file-name*)
			    continue-old-file
			    (channels *clm-channels*)
			    info
			    comment            ; set to :none to squelch the comment
			    (srate *clm-srate*)
			    reverb
			    reverb-data
			    (reverb-channels *clm-reverb-channels*)
			    revfile
			    (decay-time 1.0)
			    reverb-func
			    reverb-args
			    (play *clm-play*)
			    force-recomputation
			    (notehook *clm-notehook*)
			    (statistics *clm-statistics*)
			    (header-type *clm-header-type*)
			    (data-format *clm-data-format*)
			    save-body
			    (verbose *clm-verbose*)
			    scaled-to
			    (clipped *clm-clipped*)
			    scaled-by
			    sampling-rate
			    (output-safety 0)
			    &allow-other-keys)
		      &body body)
  ;; it is possible to have revfile (i.e. reverb output stream) but no reverb function (with-mix)
  `(let ((*verbose* ,verbose)
	 (*offset* 0)
	 (common-tones::*interrupted* 0)
	 (common-tones::*statistics* nil)
	 (*clm-scaled-amp* nil)
	 (*notehook* ,notehook)
	 (*force-recomputation* ,force-recomputation)
	 (*clm-with-sound-body* (and ,save-body ',body))
	 (*channels* ,channels)
	 (*srate* ,(or sampling-rate srate))
	 (*header-type* ,header-type)
	 (*data-format* ,data-format)
	 (*clipped* ,clipped)
	 )
     (clm-initialize-links)
     (setf *clm-ins* nil)
     ;; close current computation, if any
     (let* ((old-output *output*)
	    (old-reverb *reverb*))
       (if old-output (clm-close-output))
       (if old-reverb (clm-close-reverb))
       ;(format t "old output: ~A~%" old-output)

       (unwind-protect
	   (let* ((out-file (or *open-input-explicit-output*
				(filename->string
				 (full-merge-pathnames
				  (translate-logical-pathname (->pathname (filename->string ,output)))
				  *clm-file-name*))))
		  (*clm-with-sound-depth* (1+ *clm-with-sound-depth*))
		  (our-srate ,(or sampling-rate srate))
		  (our-type ,header-type)
		  (our-format
		   (let ((first-guess ,data-format))
		     (if (and (= first-guess mus-bshort)
			      (= our-type mus-riff))
			 mus-lshort
		       (if (and (= first-guess mus-bint)
				(= our-type mus-riff))
			   mus-lint
			 first-guess))))
		  (our-reverb (or ,reverb-func ',reverb))
		  (our-reverb-data (or ,reverb-args ',reverb-data))
		  (reverb-filename (concatenate 'string (subseq out-file 0 (- (length out-file) (length (pathname-type out-file))))
						#-windoze "reverb"
						#+windoze "rev"))
		  (revf (if (or ,revfile our-reverb)
			    (or ,revfile *open-input-explicit-reverb* reverb-filename)))
		  (*ws-reverb-file* revf))
	     (if (and (frample->file? old-output)
		      (string= out-file (mus-file-name old-output)))
		 (warn "we're about to overwrite ~A..." out-file))
	     (setf out-file (begin-with-sound our-srate ,channels ,scaled-to out-file revf ,statistics
					      ,continue-old-file ,reverb-channels our-type
					      our-format ,clipped ,scaled-by
					      (and (not (eq ,comment :none))
						   (make-default-comment ,comment ,info ,save-body))))

	     (clm-set-output-safety ,output-safety)
	     (if *reverb* (clm-set-reverb-safety ,output-safety))

	     (if out-file
		 ;; run with-sound body
		 (tagbody
		  (restart-case
		   (catch :FINISH

		     (let ((threads '()))
		       ,@(mapcar (lambda (expr)
				   `(let ((thread (sb-thread:make-thread (lambda ()
									   ,expr))))
				      (setf threads (cons thread threads))
				      (if (> (length threads) *clm-threads*)
					  (progn
					    (loop for thread in threads do
						  (sb-thread:join-thread thread))
					    (setf threads '())))))
				 body)
		       (loop for thread in threads do
			  (sb-thread:join-thread thread))))

		   (nil ()
			:report "close files and return to top-level."
			(progn (clm-reset) (go ALL-DONE)))
		   (nil ()
			:report "jump past remaining notes."
			(go NORMAL-FINISH)))
		  NORMAL-FINISH
		  (setf out-file (end-with-sound ,scaled-to out-file revf ,statistics our-type
						 our-format our-reverb ,decay-time our-reverb-data ,channels
						 ,play ,scaled-by
						 ))
		  ALL-DONE
		  )) ;end of tagbody

	     ;; reopen old computation, if any
	     (if old-output
		 (progn
		   (clm-continue-output (mus-file-name old-output))
		   (setf *output* old-output)))
	     (if old-reverb
		 (progn
		   (clm-continue-reverb (mus-file-name old-reverb))
		   (setf *reverb* old-reverb))
	       (setf *reverb* nil))
	     out-file)
       (progn
	 (setf *clm-ins* nil)
	 (setf *force-recomputation* nil)
	 (clm-check-for-reset))))))

;(with-threaded-sound () (fm-violin 0 1 440 .1) (fm-violin 0 1 660 .1))



(defun clm-load (pathname &key
			  (output *clm-file-name*) continue-old-file
			  (channels *clm-channels*) info comment
			  (srate *clm-srate*)
			  reverb reverb-data (reverb-channels 1) revfile (decay-time 1.0)
			  (play *clm-play*) force-recomputation
			  (notehook *clm-notehook*) statistics
			  (header-type *clm-header-type*) (data-format *clm-data-format*) save-body
			  (verbose *clm-verbose*) scaled-to (clipped *clm-clipped*) scaled-by
			  (load-package *package*)
			  sampling-rate
			  &allow-other-keys)
  ;; here reverb and reverb-data are quoted, or at least treated as one would normally expect, so
  ;; we use the "normal" style args in with-sound rather than reverb and reverb-data
  (with-sound (:output output :continue-old-file continue-old-file
	       :channels channels :comment comment :info info
	       :srate (or sampling-rate srate)
	       :reverb-func reverb :revfile revfile :reverb-args reverb-data :reverb-channels reverb-channels :decay-time decay-time
	       :play play :force-recomputation force-recomputation
	       :statistics statistics :notehook notehook
	       :header-type header-type :data-format data-format :verbose verbose :clipped clipped
	       :save-body save-body :scaled-to scaled-to :scaled-by scaled-by
	       )
    (let ((*package* (if (packagep load-package)
			 load-package
		       (find-package load-package))))
      (load pathname))))

(defmacro with-current-sound ((&key output comment scaled-to scaled-by) &body body)
  `(with-sound (:output ,output
		:channels *channels*
		:comment ,comment
		:srate *srate*
		:revfile *ws-reverb-file*
		:force-recomputation *force-recomputation*
		:notehook *notehook*
		:header-type *header-type*
		:data-format *data-format*
		:verbose *verbose*
		:clipped *clipped*
		:scaled-to ,scaled-to
		:scaled-by ,scaled-by)
	       ,.body))

(defmacro scaled-by (val &body body)
  `(let ((tempf "temp_clm_scaling.snd"))
     (with-current-sound (:output tempf :scaled-by ,val) ,.body)
     (mix tempf)
     (delete-file tempf)))

(defmacro scaled-to (val &body body)
  `(let ((tempf "temp_clm_scaling.snd"))
     (with-current-sound (:output tempf :scaled-to ,val) ,.body)
     (mix tempf)
     (delete-file tempf)))




;;; ---------------- OPEN-INPUT OPEN-OUTPUT ----------------

;;;

;;; for a "make" facility for sound file pieces.   Here we tie into the

;;; sound file headers etc.  mix is called if the file-to-be-merged

;;; is up-to-date.  We need the name of the output file, input file,

;;; sample number in the output to begin at, sample number in the input

;;; to start at, number of channel-independent samples to merge (i.e. seconds*srate).

;;; Output header may be changed.


(defun mix-wrapper (file output-sample input-file dur)
  (let ((framples (if dur (floor (* dur *srate*)) (sound-framples input-file))))
    (mix input-file :output (mus-file-name file) :output-frample output-sample :framples framples)
    (when *statistics*
      (setf clm-last-begin-time (max clm-last-begin-time (+ (floor *offset*) output-sample))))))

(defun make-typed-file-name (name ext)	;same name, directory, etc, but change "type" = extension
  (make-pathname :type ext :defaults name))


;;; this version is like lisp's load function to some extent in that

;;; if it gets an incomplete file name, or a cm/clm file name, it

;;; checks to see if the associated sound file is either not present

;;; or out of date and recomputes it if so.  In any case, open-input

;;; opens the sound file and returns an mus_any structure for it.  If it

;;; has to recompute the file, it must also close the current computation,

;;; open the new computation, run it to completion, then reopen the

;;; previous computation where it left off.


(defvar last-open-input-file-name nil)

(defun open-input (&optional name
		   &key (verbose nil verbose-p)
			(element-type nil element-p) ;can be :sound :clm :cm :lisp
			(if-does-not-exist :error)
			(mix-at nil) (mix-duration nil) (channel 0)
			(start 0) (force-recomputation *force-recomputation*))
  (let* ((fname (or name last-open-input-file-name *clm-file-name*))
	 (sound-file-extension (or (and element-p
					(eq element-type :sound)
					(pathname-type name))
				   (pathname-type *clm-file-name*)))
	 (file-extension (pathname-type fname))
	 (sound-file-name fname))
    (clm-initialize-links)
    (if (or (and element-p (not (eq element-type :sound)))
	    (and (not (string-equal file-extension sound-file-extension))
		 (not (find file-extension (list "snd" "aiff" "wav") :test #'string-equal))))
	;; not an obvious sound file, so start checking for nested computations
	;; mimic the Load function in Lisp
	(let* ((*open-input-verbose* (or (and verbose-p verbose) *open-input-verbose*))
	       (*open-input-pathname* name)
	       (cm-file (probe-file (make-typed-file-name fname "cm")))
	       (clm-file (probe-file (make-typed-file-name fname "clm")))
	       (snd-file (probe-file (make-typed-file-name fname sound-file-extension)))
	       (cm-date (and cm-file (file-write-date (truename cm-file))))
	       (clm-date (and clm-file (file-write-date (truename clm-file))))
	       (snd-date (and snd-file (file-write-date (truename snd-file)))))
	  (if (or force-recomputation
		  (and (not snd-file)
		       (or cm-file clm-file))
		  (and cm-file snd-file
		       (> cm-date snd-date))
		  (and clm-file snd-file
		       (> clm-date snd-date)))

	      ;; close current computation, if any
	      (let* ((old-output *output*)
		     (old-reverb *reverb*))
		(if (and *output*
			 (string-equal (filename->string (make-typed-file-name fname sound-file-extension))
				       (mus-file-name *output*)))
		    (warn "we're about to overwrite ~A..." (mus-file-name *output*)))
		(if old-output (clm-close-output))
		(if old-reverb (clm-close-reverb))

		(let* ((*open-input-explicit-output* (filename->string (make-typed-file-name fname sound-file-extension)))
		       (*clm-file-name* *open-input-explicit-output*)
		       ;; this is to turn off the directory fill-in in clm-open-input
		       ;; we can't use truename because it dies if it's passed a non-existent file
		       (*open-input-explicit-reverb* (filename->string
						      (make-pathname
						       :defaults fname
						       :name (concatenate 'string (pathname-name fname) #-openmcl ".rev" #+openmcl "-rev")
						       :type sound-file-extension)))
		       (*open-input-truename* (filename->string (if (and cm-file clm-file)
							      (if (> cm-date clm-date)
								  cm-file
								clm-file)
							    (or cm-file clm-file)))))
		  (if *open-input-verbose* (print (format nil "update ~A via ~A~% " *open-input-explicit-output* *open-input-truename*)))
		  (load *open-input-truename*)

		  (if (and cm-file (eq cm-file *open-input-truename*))
		      ;; now make sure cm actually made the new sound file
		      (let* ((clm-file (probe-file (make-typed-file-name fname "clm")))
			     (snd-file (probe-file (make-typed-file-name fname sound-file-extension)))
			     (clm-date (and clm-file (file-write-date (truename clm-file))))
			     (snd-date (and snd-file (file-write-date (truename snd-file)))))
			(if (or (and (not snd-file) clm-file)
				(and clm-file (> clm-date snd-date)))
			    (let ((*open-input-truename* (filename->string clm-file)))
			      (load *open-input-truename*)))))

		  ;; reopen old computation, if any

		  (if old-output
		      (progn
			(clm-continue-output (mus-file-name old-output))
			(setf *output* old-output)))
		  (if old-reverb
		      (progn
			(clm-continue-reverb (mus-file-name old-reverb))
			(setf *reverb* old-reverb))
		    (setf *reverb* nil))

		  (setf sound-file-name (truename *open-input-explicit-output*))))
	    (if snd-file (setf sound-file-name snd-file))))
      (let ((filename (search-full-merge-pathnames fname *clm-file-name* "test.snd")))
	(if filename (setf sound-file-name (filename->string filename))))) ;%#$@& namestring!

    (if (and sound-file-name
	     (probe-file sound-file-name))
	(progn
	  (setf last-open-input-file-name sound-file-name)
	  (if (not mix-at)
	      (progn
		(clm-open-input :file sound-file-name :start start :channel channel))
	    (let* ((beg (floor (* mix-at *srate*))))
	      (mix-wrapper *output*
			   beg
			   (if (pathnamep sound-file-name)
			       (filename->string sound-file-name)
			     (expand-filename->string sound-file-name))
			   mix-duration))))
      (if (eq if-does-not-exist :error)
	  (error "can't find ~A~A" name (if (not (eq name sound-file-name)) (format nil " (~A)" sound-file-name) ""))))))


;;; ---------------- WITH-MIX, SOUND-LET ----------------


(defvar *clm-mix-calls* nil)
(defvar *clm-mix-options* nil)

(defun get-mix-calls (f)
  (setf *clm-mix-calls* nil)
  (setf *clm-mix-options* nil)
  (let ((com (sound-comment f)))
    (if (and com (stringp com))
	(let ((len (length com))
	      (pos 0)
	      (form nil))
	  (loop while (< pos len) do
	    (multiple-value-setq (form pos)
	      (read-from-string com #-(or clisp openmcl) t #+(or clisp openmcl) nil :EOF :start pos))
	    (if (not (eq form :EOF))
		(eval form)
	      (setf pos (1+ len)))))))
  (write-to-string *clm-mix-calls*))

(defun get-mix-options ()
  (write-to-string *clm-mix-options*))

(defvar temp-sound-ctr 0)

(defmacro sound-let (sounds &body body)
  ;; the syntax of each local sound is like with-sound -- a list of options, then the body
  ;; here, if any of the needed internal options are omitted, we append them (especially the file name)
  ;; the result of the let variable form is to return the temp snd file name as the value of the variable
  ;; so that in the sound-let body a reference to that variable is a reference to the associated file.

  ;; because these sounds are viewed as temporary computations, there's no effort made to save and reuse
  ;; them as in mix and friends -- this might be a nice addition someday.

  `(let* ((old-output *output*)
	  (sound-file-list nil)
	  (old-recompute *force-recomputation*)
	  (*open-input-explicit-output* nil)
	  (*open-input-explicit-reverb* nil))
     (loop for (snd opts calls) in ',sounds do
       (let ((name-loc (position :output opts)))
	 (push (filename->string (full-merge-pathnames (if name-loc
						     (nth (1+ name-loc) opts)
						   (concatenate 'string "snd" (format nil "~D" (incf temp-sound-ctr))))
						 *clm-file-name*))
	       sound-file-list)))
     (let* (,@(loop for (snd opts calls) in sounds and i from 0 and all-calls in sounds
	       collect
	        (progn
	        `(,snd (progn
			 (with-sound (,@opts
				      ,@(if (not (find :output opts))
					    (list :output `(nth ,i sound-file-list)))
				      ,@(if (not (find :channels opts))
					    (list :channels `(mus-channels old-output)))
				      ;; the old way (no backquote, use of *output*) picked up
				      ;; the default with-sound keyword values, not those in effect at the
				      ;; point of the sound-let call.
				      ,@(if (not (find :srate opts))
					    (list :srate `(sound-srate (mus-file-name old-output))))
				      ,@(if (not (find :notehook opts))
					    (list :notehook nil))
				      :play nil)
			   ,@(cddr all-calls))
			 (nth ,i sound-file-list))))))

       (locally ,@body))
     (setf *force-recomputation* old-recompute)
     (setf *clm-ins* nil)
     ;; now clean up the temp output files
     (loop for snd in sound-file-list do (delete-file snd))
     ))

;;; WITH-MIX

;;;

;;; weird syntax = with-mix (with-sound-args) file-name start-in-output &body body

;;;

;;; (with-sound ()

;;; (with-mix () "section-1" 0 (fm-violin 0 1 440 .1)

;;; (fm-violin 1 2 660 .1))

;;; (with-mix (:reverb nrev) "section-2" ...)

;;; )


(defun mix-in (source-file begin-time &optional duration)
  (open-input source-file :mix-at begin-time :mix-duration duration))

(defun rev-mix-in (source-file begin-time reverb-file)
  (let ((res (open-input source-file :mix-at begin-time)))
    (when (and *reverb* reverb-file)
      (mix-wrapper *reverb* (floor (* begin-time *srate*)) reverb-file (sound-duration reverb-file)))
    res))

(defmacro with-mix (options ur-chkpt-file ur-beg &body body)
  `(let ((chkpt-file ,ur-chkpt-file)
	 (beg ,ur-beg)
	 (old-recompute *force-recomputation*))
     (if (not (listp ',options))
	 (error "with-mix options list (arg 1) is ~A?" ,options))
     (if (not (or (stringp chkpt-file)
		  (pathnamep chkpt-file)))
	 (error "with-mix file (arg 2) is ~A?" ,ur-chkpt-file))
     (if (not (numberp beg))
	 (setf beg (clm-cerror "use 0.0" 0.0 #'numberp "with-mix begin time (arg 3) for ~S = ~A?" chkpt-file beg)))
     (if (null ',body)
	 (mix-in chkpt-file beg)
       (let ((call-str (write-to-string ',body))
	     (option-str (write-to-string ',options))
	     (sndf (full-merge-pathnames (make-typed-file-name chkpt-file (pathname-type *clm-file-name*)) *clm-file-name*))
	     (errf (full-merge-pathnames (make-typed-file-name chkpt-file "error") *clm-file-name*))
	     (revf (full-merge-pathnames
		    (make-typed-file-name
		     chkpt-file
		     #+clisp "rev"
		     #-clisp (concatenate 'string #-sbcl "rev." #+sbcl "rev-" (pathname-type *clm-file-name*)))
		    *clm-file-name*)))
	 (if (and (not *force-recomputation*)
		  (probe-file sndf)
		  (or (not *reverb*)
		      (probe-file revf))
		  (not (probe-file errf))
		  (string-equal (get-mix-calls (filename->string sndf)) call-str)
		  (string-equal (get-mix-options) option-str))
	     (progn
	       (if *verbose* (cl-printf (format nil "; Mixing ~A " (filename->string sndf))))
	       (rev-mix-in (filename->string sndf) beg (and *reverb* (filename->string revf))))
	   (let ((finished-ok nil))
	     (unwind-protect
		 (let ((clmf (full-merge-pathnames (make-typed-file-name chkpt-file "clm") *clm-file-name*)))
		   (if *verbose*
		       (if (probe-file errf)
			   (cl-printf (format nil "; ~A was interrupted during previous computation -- will recompute it~%" (filename->string sndf)))
			 (cl-printf (format nil "; Computing ~A " (filename->string sndf)))))
		   (with-open-file (fil clmf :direction :output :if-does-not-exist :create :if-exists :supersede)
		     (format fil ";Temporary notelist for ~A~%~A~%" chkpt-file (make-banner))
		     (format fil "(with-sound (~{~S ~S ~}~%             ~
                                               :play nil~%            ~
                                               ~A~A~A ~%             ~
                                               :info ~S)~%~
                                               ~{  ~S~%~})"
			      ',options
			      (if (not (find :channels ',options))
				  (format nil " :channels ~D" (mus-channels *output*))
				  "")
			      (if (not (find :srate ',options))
				  (format nil " :srate ~D" *srate*)
				  "")
			      (if (and *reverb* (not (find :reverb ',options)))
				  (format nil " :revfile ~S" (filename->string revf))
				  "")
			      (format nil "~A~%  (setf *clm-mix-calls* '~A)~%  (setf *clm-mix-options* '~A)~%"
				      (make-banner) call-str option-str)
			      ',body))
		   (rev-mix-in clmf beg (and *reverb* (filename->string revf)))
		   (if (probe-file errf) (delete-file errf))
		   (setf finished-ok t))
	       (if (not finished-ok)
		   (let ((err (open errf :direction :output :if-exists :supersede)))
		     (close err))))))))
     (setf *force-recomputation* old-recompute)
     nil))

(def-optkey-fun mix (filename (input-frample 0) (output-frample 0) framples output)
  (if (and (null output) (null *output*)) (warn "mix called with no output file open?"))
  (let* ((outname (or output (mus-file-name *output*)))
	 (got-output (and *output* (string= outname (mus-file-name *output*))))
	 (got-reverb (and got-output *reverb*))
	 (old-output *output*)
	 (old-reverb *reverb*))
    (if got-output
	(clm-close-output)
      (if got-reverb
	  (clm-close-reverb)))
    (clm-mix outname
	     filename
	     (round (+ output-frample *offset*))
	     (or framples (sound-framples filename))
	     input-frample)
    (setf *clm-ins* nil)
    (if old-output
	(progn
	  (clm-continue-output (mus-file-name old-output))
	  (setf *output* old-output)))
    (if old-reverb
	(progn
	  (clm-continue-reverb (mus-file-name old-reverb))
	  (setf *reverb* old-reverb))
      (setf *reverb* nil))
    ))



;;; ---------------- COMMON MUSIC INTERFACE TO WITH-SOUND ----------------


(defstruct wsdat revfun revdat revdecay outtype play stats wait scaled-to format file channels scaled-by)

(defun init-with-sound (&key (output *clm-file-name*) sndfile
			     (channels *clm-channels*)
			     (srate *clm-srate*) continue-old-file
			     reverb reverb-data (reverb-channels *clm-reverb-channels*) revfile (decay-time 1.0)
			     (play *clm-play*)
			     (notehook *clm-notehook*)
			     (statistics *clm-statistics*)
			     type
			     (header-type *clm-header-type*)
			     (data-format *clm-data-format*)
			     scaled-to scaled-by
			     (clipped *clm-clipped*)
			     force-recomputation (verbose *clm-verbose*) comment
			     )
  (clm-initialize-links) ; buffer sizes need to be set
  (if srate (setf *srate* srate))
  (let* ((out-file (or *open-input-explicit-output*
		       (filename->string (full-merge-pathnames (or sndfile output) *clm-file-name*))))
	 (our-type (or type header-type))
	 (our-format data-format)
	 (revf (and reverb (or revfile *open-input-explicit-reverb*
			       (concatenate 'string (subseq out-file 0 (- (length out-file) (length (pathname-type out-file))))
					    #-windoze "reverb" #+windoze "rev")))))
    (setf *verbose* verbose)
    (setf *offset* 0)
    (setf *interrupted* 0)
    (setf *statistics* nil)
    (setf *notehook* notehook)
    (setf *force-recomputation* force-recomputation)
    (setf out-file (begin-with-sound srate channels scaled-to out-file revf statistics continue-old-file
				     reverb-channels our-type our-format clipped scaled-by comment))
    (make-wsdat :revfun reverb :revdat reverb-data :revdecay decay-time
		:outtype our-type :play play :stats statistics :channels channels
		:format our-format :scaled-to scaled-to :file out-file
		:scaled-by scaled-by
		)))

(defun finish-with-sound (wsd)
  (setf (wsdat-file wsd)
    (end-with-sound (wsdat-scaled-to wsd) (wsdat-file wsd)
		    (if *reverb* (mus-file-name *reverb*))
		    (wsdat-stats wsd)
		    (wsdat-outtype wsd)
		    (wsdat-format wsd) (wsdat-revfun wsd)
		    (wsdat-revdecay wsd) (wsdat-revdat wsd)
		    (wsdat-channels wsd)
		    (wsdat-play wsd)
		    (wsdat-scaled-by wsd)
		    ))
  (clm-reset))