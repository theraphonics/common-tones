(in-package :common-tones/generators)

(defclass filter ()
  ((order :initform nil :initarg :order :accessor flt-order)
   (x :initform nil :initarg :xcoeffs :accessor flt-x)
   (y :initform nil :initarg :ycoeffs :accessor flt-y)
   (state :initform nil :initarg :state :accessor flt-state)))

(defmethod print-object ((d filter) stream)
  (format stream "#<(filter: order: ~A, xcoeffs: ~A, ycoeffs: ~A, state: ~A>"
	  (flt-order d)
	  (prettified-array (flt-x d))
	  (prettified-array (flt-y d))
	  (prettified-array (flt-state d))))

(def-optkey-fun make-filter (order xcoeffs ycoeffs)
  (let ((len (or order (max (length xcoeffs) (length ycoeffs)))))
    (make-instance 'filter
		   :ycoeffs (if ycoeffs (make-double-array len :initial-contents ycoeffs) (make-double-array len))
		   :xcoeffs (if xcoeffs (make-double-array len :initial-contents xcoeffs) (make-double-array len))
		   :state (make-double-array len)
		   :order len)))

(defmethod filter? ((g filter)) t)
(defmethod filter? ((g t)) nil)

(defun filter (fl inp)
  (let ((xout 0.0))
    (if (flt-y fl)
	(if (flt-x fl)
	    (progn
	      (setf (aref (flt-state fl) 0) (double inp))
	      (loop for j from (1- (flt-order fl)) downto 1 do
		(incf xout (* (aref (flt-state fl) j) (aref (flt-x fl) j)))
		(decf (aref (flt-state fl) 0) (* (aref (flt-y fl) j) (aref (flt-state fl) j)))
		(setf (aref (flt-state fl) j) (aref (flt-state fl) (1- j))))
	      (+ xout (* (aref (flt-state fl) 0) (aref (flt-x fl) 0))))
	  (iir-filter fl inp))
      (fir-filter fl inp))))

(defmethod mus-xcoeffs ((gen filter)) (flt-x gen))
(defmethod mus-ycoeffs ((gen filter)) (flt-y gen))
(defmethod mus-xcoeff ((gen filter) index) (aref (flt-x gen) index))
(defmethod mus-ycoeff ((gen filter) index) (aref (flt-y gen) index))
(defmethod (setf mus-xcoeff) (val (gen filter) index) (setf (aref (flt-x gen) index) val))
(defmethod (setf mus-ycoeff) (val (gen filter) index) (setf (aref (flt-y gen) index) val))
(defmethod mus-order ((gen filter)) (flt-order gen))
(defmethod mus-data ((gen filter)) (flt-state gen))
(defmethod mus-length ((gen filter)) (flt-order gen))
(defmethod mus-run ((gen filter) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (filter gen arg1))
