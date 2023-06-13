(in-package :common-tones)

;;; Comb filter (a delay line with a scaler on the feedback term)

;;;

;;; in filter parlance, y(n) <= x(n-D) + scaler * y(n-D)

;;; As a rule of thumb, the decay time of the feedback part is 7*(delay)/(1-scaler) samples,

;;; so to get a decay of DUR seconds, scaler <= 1-7*D/(DUR*Srate).  (D=delay length here).

;;; The peak gain is 1/(1-(abs scaler)).

;;;

;;; See Julius Smith's "An Introduction to Digital Filter Theory" in Strawn "Digital

;;; Audio Signal Processing"



(defclass comb (delay) ())

(def-optkey-fun make-comb (scaler size initial-contents initial-element max-size type)
  (let ((lsize (round (or max-size size))))
    (make-instance 'comb
		   :loc 0
		   :xscl scaler
		   :size (floor size)
		   :zsize lsize
		   :zdly max-size
		   :zloc (and max-size (- max-size size))
		   :line (if initial-contents
			     (make-double-array lsize :initial-contents initial-contents)
			   (if initial-element
			       (make-double-array lsize :initial-element (double initial-element))
			     (make-double-array lsize)))
		   :type (or type
			     (if max-size
				 mus-interp-linear
			       mus-interp-none)))))

(defmethod print-object ((d comb) stream)
  (format stream "#<(comb: size: ~A~A, loc: ~A~A, scaler: ~A, line: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-xscl d)
	  (prettified-array (dly-line d))))

(defun comb (d input &optional (pm 0.0))
  (delay d (+ input (* (dly-xscl d) (tap d pm)))))

(defmethod comb? ((g comb)) t)
(defmethod comb? ((g t)) nil)

(defmethod mus-feedback ((gen comb)) (dly-xscl gen))
(defmethod (setf mus-feedback) (val (gen comb)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen comb) &optional (arg1 0.0) (arg2 0.0)) (comb gen arg1 arg2))
(defmethod mus-interp-type ((gen comb)) (dly-type gen))