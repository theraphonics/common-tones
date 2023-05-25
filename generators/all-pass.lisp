(in-package :common-tones/generators)

/*!< All-pass or "moving moving-average comb" filter

/*!<

/*!< (if feedback scaler = 0, we get the moving moving-average comb)

/*!< (if both scale terms = 0, we get a pure delay line)

/*!< (if feedback = -feedforward, we get a Schroeder all-pass)

/*!< In filter parlance, y(n) <= feedforward*x(n) + x(n-D) + feedback*y(n-D)

/*!< see Peter Samson's article on the Samson box in Strawn, "Digital Audio Signal Processing" for a diagram,

/*!< This is the same as the C version in Ofranidis "Introduction to Signal Processing" p371, given that

/*!< I use tap and delay both as "sD" in his notation.



(defclass all-pass (delay) ())

(def-optkey-fun make-all-pass (feedback feedforward size initial-contents initial-element max-size type)
  (let ((lsize (round (or max-size size))))
    (make-instance 'all-pass
		   :loc 0
		   :yscl feedback
		   :xscl feedforward
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

(defmethod print-object ((d all-pass) stream)
  (format stream "#<(all-pass: size: ~A~A, loc: ~A~A, feedback: ~A, :feedforward: ~A, line: ~A>"
	  (dly-size d) (if (dly-zdly d) (format nil " (~A)" (dly-zsize d)) "")
	  (dly-loc d) (if (dly-zdly d) (format nil " (~A)" (dly-zloc d)) "")
	  (dly-yscl d) (dly-xscl d)
	  (prettified-array (dly-line d))))

(defun all-pass (d input &optional (pm 0.0))
  (let ((d-in (+ input (* (dly-yscl d) (tap d pm)))))
    (+ (delay d d-in pm)
       (* (dly-xscl d) d-in))))

(defmethod all-pass? ((g all-pass)) t)
(defmethod all-pass? ((g t)) nil)

(defmethod mus-feedback ((gen all-pass)) (dly-yscl gen))
(defmethod (setf mus-feedback) (val (gen all-pass)) (setf (dly-yscl gen) val))
(defmethod mus-feedforward ((gen all-pass)) (dly-xscl gen))
(defmethod (setf mus-feedforward) (val (gen all-pass)) (setf (dly-xscl gen) val))
(defmethod mus-run ((gen all-pass) &optional (arg1 0.0) (arg2 0.0)) (all-pass gen arg1 arg2))
(defmethod mus-interp-type ((gen all-pass)) (dly-type gen))