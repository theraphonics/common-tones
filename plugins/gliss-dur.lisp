(in-package :common-tones/plugins)

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
