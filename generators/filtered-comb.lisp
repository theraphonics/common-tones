(in-package :common-tones)


;;; filtered-comb filter (a delay line with a scaler on the filtered feedback term)


(defclass filtered-comb (delay)
  ((filter :initform nil :initarg :filter :accessor dly-filter)))

(def-optkey-fun make-filtered-comb (scaler size initial-contents initial-element max-size type filter)
  (let ((lsize (round (or max-size size))))
    (make-instance 'filtered-comb
		   :loc 0
		   :xscl scaler
		   :size (floor size)
		   :zsize lsize
		   :zdly max-size
		   :zloc (and max-size (- max-size size))
		   :filter filter
		   :line (if initial-contents
			     (make-double-array lsize :initial-contents initial-contents)
			   (if initial-element
			       (make-double-array lsize :initial-element (double initial-element))
			     (make-double-array lsize)))
		   :type (or type
			     (if max-size
				 mus-interp-linear
			       mus-interp-none)))))

(defmethod print-object ((d filtered-comb) stream)
  (format stream "#<(filtered-comb: size: ~A~A, loc: ~A~A, scaler: ~A, line: ~A, filter: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-xscl d)
	  (prettified-array (dly-line d))
	  (dly-filter d)))

(defun filtered-comb (d input &optional (pm 0.0))
  (delay d (+ input (* (dly-xscl d) (mus-run (dly-filter d) (tap d pm))))))

(defmethod filtered-comb? ((g filtered-comb)) t)
(defmethod filtered-comb? ((g t)) nil)

(defmethod mus-feedback ((gen filtered-comb)) (dly-xscl gen))
(defmethod (setf mus-feedback) (val (gen filtered-comb)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen filtered-comb) &optional (arg1 0.0) (arg2 0.0)) (filtered-comb gen arg1 arg2))
(defmethod mus-interp-type ((gen filtered-comb)) (dly-type gen))



;;; Notch filter (a delay line with a feedforward term) -- also known as inverse comb

;;; see Julius Smith's "Music Applications of Digital Waveguides" for a brief discussion


(defclass notch (delay) ())

(def-optkey-fun make-notch (scaler size initial-contents initial-element max-size type)
  (let ((lsize (round (or max-size size))))
    (make-instance 'notch
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

(defmethod print-object ((d notch) stream)
  (format stream "#<(notch: size: ~A~A, loc: ~A~A, scaler: ~A, line: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-xscl d)
	  (prettified-array (dly-line d))))

(defun notch (d input &optional (pm 0.0))
  (+ (* input (dly-xscl d))
     (delay d input pm)))

(defmethod notch? ((g notch)) t)
(defmethod notch? ((g t)) nil)

(defmethod mus-feedforward ((gen notch)) (dly-xscl gen))
(defmethod (setf mus-feedforward) (val (gen notch)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen notch) &optional (arg1 0.0) (arg2 0.0)) (notch gen arg1 arg2))
(defmethod mus-interp-type ((gen notch)) (dly-type gen))