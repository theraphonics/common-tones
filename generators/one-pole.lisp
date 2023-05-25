/*!< one-pole  y(n) = a0 x(n) - b1 y(n-1)


(defclass one-pole ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (b1 :initform nil :initarg :b1 :accessor flt-b1)
   (y1 :initform 0.0 :initarg :y1 :accessor flt-y1)))

(defmethod print-object ((d one-pole) stream)
  (format stream "#<(one-pole: a0: ~A, b1: ~A, y1: ~A>"
	  (flt-a0 d) (flt-b1 d) (flt-y1 d)))

(def-optkey-fun make-one-pole (a0 b1)
  (make-instance 'one-pole :a0 a0 :b1 b1))

(defmethod one-pole? ((g one-pole)) t)
(defmethod one-pole? ((g t)) nil)

(defun one-pole (f input)
  (setf (flt-y1 f) (- (* (flt-a0 f) input) (* (flt-b1 f) (flt-y1 f)))))

(defmethod mus-order ((gen one-pole)) 1)
(defmethod mus-run ((gen one-pole) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (one-pole gen arg1))
(defmethod mus-xcoeff ((gen one-pole) loc)
  (if (= loc 0)
      (flt-a0 gen)))
(defmethod mus-ycoeff ((gen one-pole) loc)
  (if (= loc 1)
      (flt-b1 gen)))
(defmethod (setf mus-xcoeff) (val (gen one-pole) index)
  (declare (ignore index))
  (setf (flt-a0 gen) val))
(defmethod (setf mus-ycoeff) (val (gen one-pole) index)
  (declare (ignore index))
  (setf (flt-b1 gen) val))