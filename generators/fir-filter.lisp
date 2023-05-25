(in-package :common-tones/generators)

(defclass fir-filter (filter) ())

(def-optkey-fun make-fir-filter (order x1coeffs coeffs)
  (let* ((xcoeffs (or x1coeffs coeffs))
	 (ord (or order (length xcoeffs))))
    (make-instance 'fir-filter
		   :xcoeffs (if xcoeffs (make-double-array ord :initial-contents xcoeffs) (make-double-array ord))
		   :state (make-double-array ord)
		   :order ord)))

(defmethod print-object ((d fir-filter) stream)
  (format stream "#<(fir-filter: order: ~A, xcoeffs: ~A, state: ~A>"
	  (flt-order d)
	  (prettified-array (flt-x d))
	  (prettified-array (flt-state d))))

(defmethod fir-filter? ((g fir-filter)) t)
(defmethod fir-filter? ((g t)) nil)

(defun fir-filter (fl inp)
  (let ((xout 0.0))
    (setf (aref (flt-state fl) 0) (double inp))
    (loop for j from (1- (flt-order fl)) downto 1 do
      (incf xout (* (aref (flt-state fl) j) (aref (flt-x fl) j)))
      (setf (aref (flt-state fl) j) (aref (flt-state fl) (1- j))))
    (+ xout (* (aref (flt-state fl) 0) (aref (flt-x fl) 0)))))

(defmethod mus-run ((gen fir-filter) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (fir-filter gen arg1))



(defclass iir-filter (filter) ())

(def-optkey-fun make-iir-filter (order y1coeffs coeffs)
  (let* ((ycoeffs (or y1coeffs coeffs))
	 (ord (or order (length ycoeffs))))
    (make-instance 'iir-filter
		   :ycoeffs (if ycoeffs (make-double-array ord :initial-contents ycoeffs) (make-double-array ord))
		   :state (make-double-array ord)
		   :order ord)))

(defmethod print-object ((d iir-filter) stream)
  (format stream "#<(iir-filter: order: ~A, ycoeffs: ~A, state: ~A>"
	  (flt-order d)
	  (prettified-array (flt-y d))
	  (prettified-array (flt-state d))))

(defmethod iir-filter? ((g iir-filter)) t)
(defmethod iir-filter? ((g t)) nil)

(defun iir-filter (fl inp)
  (setf (aref (flt-state fl) 0) (double inp))
  (loop for j from (1- (flt-order fl)) downto 1 do
    (decf (aref (flt-state fl) 0) (* (aref (flt-y fl) j) (aref (flt-state fl) j)))
    (setf (aref (flt-state fl) j) (aref (flt-state fl) (1- j))))
  (aref (flt-state fl) 0))

(defmethod mus-run ((gen iir-filter) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (iir-filter gen arg1))