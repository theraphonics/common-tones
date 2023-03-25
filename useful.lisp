;;; useful hacks for CLM:
;;;
;;;   hey:              add a comment to file just computed
;;;   gliss-dur:        true, post-src change, file duration (see also dur-gliss)
;;;                     how to run CLM from a shell script
;;;   sf and ssf:       info on sound files in directory
;;;   load-ins:         load all instruments in a directory
;;;   with-instruments: list instruments needed by subsequent note list for autoloading
;;;   add and cut:      easier access to *offset*
;;;   rmix:             mix with portion sent to reverb
;;;   remote-play:      play sound (e.g. with-sound output) on a remote host
;;;   fft-mag-and-phase: fft rectangular to polar coordinates
;;;   amp-to-dB et al:  macros for translating between dB and linear amps



(in-package :clm)
(export '(db-to-amp amp-to-db vol-to-amp adb-to-amp amp-to-adb rmix with-instruments load-ins))


;;; ------------------------------------------------------------------------
;;; often one wants to save and annotate the last sound, so...

(defun hey (new-name &optional comment old-name)
  (let ((nam (probe-file
	      (if (not old-name) 
		  (if (not last-dac-filename)
		      *clm-file-name*
		    last-dac-filename)
		(full-merge-pathnames old-name *clm-file-name*)))))
    (when (not nam)
      (setf nam (probe-file (full-merge-pathnames old-name "test.snd")))
      (when (not nam)
	(setf nam (probe-file (or old-name last-dac-filename *clm-file-name*)))))
    (if (not nam)
	(print (format nil "can't find ~A." (or old-name last-dac-filename *clm-file-name*)))
      (let* ((old-comment (sound-comment (filename->string nam)))
	     (new-comment (format nil "~A~%~A" old-comment (or comment ""))))
	(with-sound (:info new-comment 
		     :play nil 
		     :output new-name
		     :srate (sound-srate (filename->string nam)) 
		     :channels (sound-chans (filename->string nam))) 
	  (mix (filename->string nam)))))))


;;; ------------------------------------------------------------------------
;;; courtesy Michael Edwards (for use with src-change and such) -- returns 
;;;  duration after all the resampling has taken effect.

(defun gliss-dur (env initial-transp input-dur &key (scaler 1) (semitones nil))
  (flet ((semitone (n) (expt 2.0 (/ n 12.0))))
    (let* ((x-max (lastx env))
	   (src (if semitones (semitone initial-transp) initial-transp))
	   (scaled-env (loop for x in env by #'cddr and y in (cdr env) by #'cddr
			  collect x
                          collect (if semitones 
				      (- (semitone (* scaler y)) 1)
				    (* scaler y))))
	   (mean-y (loop for x1 in scaled-env by #'cddr
			 and y1 in (cdr scaled-env) by #'cddr
			 and x2 in (cddr scaled-env) by #'cddr
			 and y2 in (cdddr scaled-env) by #'cddr
			 sum (* (/ (- x2 x1) x-max)
				(+ (min y1 y2) (/ (- (max y1 y2) (min y1 y2)) 2.0))))))
      (/ input-dur (+ src mean-y)))))


;;;  ------------------------------------------------------------------------
;;; run CLM from a shell
;;;
;;; I think the following command will run CLM from the
;;; shell, load the CLM file batch.clm (just some arbitrary
;;; lisp code that includes a with-sound), and then
;;; auto-exit (back to the shell):
;;;
;;; /Lisp/next-m68k/acl/clm/clm -qq -batch < batch.clm


;;;  ------------------------------------------------------------------------
;;; get a listing of sound files in a given directory (courtesy Marco Trevisani)

(defun sf (path)
  (let ((dir (directory path)))
    (loop for fil in dir do
      (let ((typ (pathname-type fil))
	    (name (filename->string fil)))
	(when (or (string= typ "snd") (string= typ "aiff"))
	  (format t "~2&Filename:~S ~%  Sampling-rate:~D Channels:~D Size:~,3FMb ~&  Duration:~,3Fsec"
		  name
		  (sound-srate name)
		  (sound-chans name)
		  (float (/ (* (sound-samples name) (sound-datum-size name)) 1000000))
		  (sound-duration name)))))))

;;; short-sf
;;; just filename and duration
(defun ssf (path)
  (let ((dir (directory path)))
    (loop for fil in dir do
      (let ((typ (pathname-type fil))
	    (name (filename->string fil)))
	(when (or (string= typ "snd") (string= typ "aiff"))
	  (format t "~2&Filename:~S ~%DURATION:~,4Fsec"
		  name (sound-duration name)))))))

;;; example (sf "/user/marco/mrc/temp/")
;;; and     (ssf "/user/marco/mrc/temp/")



;;;  ------------------------------------------------------------------------
;;; load all instruments in a given directory (Marco Trevisani)

(defun load-ins  (&key (path "/user/marco/aura/pronti/") (extension "o"))
  (let ((dir (directory path)))
    (loop for file in dir do
      (if (string= extension (pathname-type file))
        (load file)))))

;;;  ------------------------------------------------------------------------
;;; another version of gliss-dur (Marco Trevisani)
;;;
;;; This one is very similar to Michael's (above) with the advantage that
;;; one need only provide the envelope.  The disadvantage is that the
;;; envelope x axis must match the precise duration in the input file (all
;;; or a portion).

(defun dur-gliss (x)
  (if (< (list-length x) 4)
      (error "Inside ~S~%You must use a list equal or larger than 4 elements~%" x)
    (if  (oddp (list-length x))
        (error "Inside ~S~%Careful oddp=T. An envelop must be evenp=T~%" x)
      (reduce #'+ (loop for a from 0 to (- (list-length x) 4) by 2
		   collect (* (/ (- (nth (+ 2 a) x) (nth a x)) 2.0)
			      (+ (/ 1.0 (nth (+ 3 a) x))
				 (/ 1.0 (nth (+ 1 a) x)))))))))


;;;  ------------------------------------------------------------------------
;;; with-instruments -- make sure needed instruments are loaded before going on.
;;;
;;; (with-instruments (FM-VIOLIN "v") (JC-REVERB "jcrev")) for example

(defmacro with-instruments (&rest ins)
  `(progn
     ,@(loop for i in ins collect 
       `(when (not (member ',(first i) *clm-instruments*)) (compile-and-load ,(second i))))))



;;;  ------------------------------------------------------------------------
;;; add and cut -- quick access to *clm-beg-offset*

(defun add (secs) (incf *offset* (floor (* *srate* secs))))
(defun cut (secs) (decf *offset* (floor (* *srate* secs))))


;;;  ------------------------------------------------------------------------
;;; mix with reverb
;;;
#|
 (with-sound (:reverb jc-reverb)
   (sound-let ((temp () (fm-violin 0 .1 660 .1)))
     (rmix temp :reverb .1)))
|#

(defun rmix (in-file &rest args &key (start-time 0.0) start reverb &allow-other-keys)
  (let* ((outname (mus-file-name *output*))
	 (revname (and *reverb* (mus-file-name *reverb*)))
	 (old-output *output*)
	 (old-reverb *reverb*)
	 (beg (or start (floor (* *srate* (or start-time 0.0))))))
    (clm-close-output)
    (if *reverb* (clm-close-reverb))
    (clm-mix outname in-file beg (sound-framples in-file) 0)
    (if (and reverb old-reverb)
	(if (not (= reverb 1.0))
	    (let ((tempname "mix-temp-reverb.snd"))
	      (clm-scale-file tempname in-file (double reverb) *clm-data-format* *clm-header-type*)
	      (clm-mix revname tempname beg (sound-framples in-file) 0)
	      (delete-file tempname))
	  (clm-mix revname in-file beg (sound-framples in-file) 0)))
    (when old-output
      (clm-continue-output (mus-file-name old-output))
      (setf *output* old-output))
    (when old-reverb
      (clm-continue-reverb (mus-file-name old-reverb))
      (setf *reverb* old-reverb))))


;;;  ------------------------------------------------------------------------
;;; remote play

(defun remote-play (host play-program sound-name)
  (run-in-shell "rsh" (format nil "~A ~A ~A" host play-program sound-name)))

;;; (remote-play "cmi1" "sfplay" (with-sound (:play nil) (fm-violin 0 1 440 .1)))


;;;  ------------------------------------------------------------------------
;;; fft-mag-and-phase: fft rectangular to polar coordinates at run time
;;; by Anders Vinjar

(defmacro fft-mag-and-phase (fdr fdi)
  `(let ((len (length ,fdr)))
     (dotimes (k len)
       (let ((datumr (aref ,fdr k))
	     (datumi (aref ,fdi k)))
	 (setf (aref ,fdr k) (sqrt (+ (* datumr datumr) (* datumi datumi))))
	 (setf (aref ,fdi k) (* -1 (if (zerop datumr)
				       (/ pi 2.0)
				     (atan (/ datumi datumr)))))))))

;;; ----------------------------------------------------------------
;;; amp in dB (Sam Heisz)

#|

Here are some macros that facilitate specification of amplitude in
either dB or as a linear volume.

Linear Volume
=============

(vol-to-amp vol &key (max 1000))

vol is a linear specification for amplitude from 0 to `max'.  `max'
indicates the point at which loudness is not changed, a vol of 2*max
indicates a doubling of perceived loudness. This is the most intuitive
form of amplitude specification because it approximates how we
perceive loudness. I design my instruments to take a vol parameter
which is then changed to an amplitude parameter:

(definstrument piano (start dur freq vol)
  (let* (...
	 ((amp (vol-to-amp vol))))
    .
    .
    (Run
     .
     .
     (out i (* amp signal)))))

You can also use it with existing instruments:

;; use v.ins
(with-sound ()
  (fmviolin 0 2 440 (vol-to-amp 500)
  (fmviolin 2 2 440 (vol-to-amp 250))))
    

(* signal (vol-to-amp 2000))		; signal is perceived as twice as loud
(* signal (vol-to-amp 1000))		; signal is not changed
(* signal (vol-to-amp 500))		; signal is perceived as 1/2 as loud
(* signal (vol-to-amp 250))		; signal is perceived as 1/4 as loud
(* signal (vol-to-amp 0))		; signal is set to 0
(* signal (vol-to-amp .25 :max 1))	; signal is perceived as 1/4 as loud

dB
==

(db-to-amp db)       
(amp-to-db db)

A dB is a relative value. A value of 10 represents a doubling of 
perceived loudness.

(* signal  (db-to-amp 10))		; signal is perceived as twice as loud
(* signal  (db-to-amp 0))		; signal is not changed
(* signal  (db-to-amp -10))		; signal is perceived as 1/2 as loud
(* signal  (db-to-amp -20))		; signal is perceived as 1/4 as loud


Absolute dB
===========

(adb-to-amp db &key (max 96.3296))
(amp-to-adb amp &key (max 96.3296))

Sometimes dB is used as an absolute value. In such a case, an amplitude
of 1.0 can be matched to a certain dB value. For these functions the
default dB value corresponding to an amplitude of 1.0 is 96.3296.


(* signal (adb-to-amp 106))		; signal is perceived as twice as loud
(* signal (adb-to-amp 96))		; signal is not changed
(* signal (adb-to-amp 86))		; signal is perceived as 1/2 as loud
(* signal (adb-to-amp 76))		; signal is perceived as 1/4 as loud

|#

;; Sam Heisz (samh@digicron.com) 
;;
;; dB = 20 log   (A1 / A2)                     db/20
;;            10                     A1/A2 = 10

(defmacro db-to-amp (db) 
  `(expt 10 (/ ,db 20)))

(defmacro amp-to-db (amp)
  `(* 20 (log ,amp 10)))

(defmacro vol-to-amp (v &key (max 1000))
  (let ((vol (gensym)))
    `(let ((,vol ,v))
       (if (<= ,vol 0) 0 (db-to-amp (* -10 (log (/ ,max ,vol) 2)))))))

;; the 96.3296 figure for max is from track-rms.ins
(defmacro adb-to-amp (adb &key (max 96.3296))
  (let ((db (gensym)))
    `(let ((,db ,adb))
       (if (<= ,db 0.0) 0.0 (db-to-amp (- (abs ,db) ,max))))))

(defmacro amp-to-adb (amp &key (max 96.3296))
  `(if (<= ,amp .00001526) 0.0 (+ ,max (amp-to-db ,amp))))
