(in-package :common-tones/plugins)

;;; ------------------------------------------------------------------------

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