

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
