(in-package :common-tones)

(defclass moving-average (delay) ())

(def-optkey-fun make-moving-average (size initial-contents initial-element)
  (let ((lsize (floor size)))
    (make-instance 'moving-average
		   :loc 0
		   :yscl (/ 1.0 lsize)
		   :xscl (if initial-element
			     (* initial-element lsize)
			   (if initial-contents
			       (apply #'+ initial-contents)
			     0.0))
		   :size lsize
		   :zsize lsize
		   :zdly nil
		   :zloc 0
		   :line (if initial-contents
			     (make-double-array lsize :initial-contents initial-contents)
			   (if initial-element
			       (make-double-array lsize :initial-element (double initial-element))
			     (make-double-array lsize))))))

(defmethod print-object ((d moving-average) stream)
  (format stream "#<(moving-average: size: ~A, loc: ~A, line: ~A>"
	  (dly-size d) (dly-loc d) (prettified-array (dly-line d))))

(defun moving-average (d input)
  (let ((output (delay d input)))
    (incf (dly-xscl d) (- input output))
    (* (dly-xscl d) (dly-yscl d))))

(defmethod moving-average? ((g moving-average)) t)
(defmethod moving-average? ((g t)) nil)

(defmethod mus-run ((gen moving-average) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (moving-average gen arg1))