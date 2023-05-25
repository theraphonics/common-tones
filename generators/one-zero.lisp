(in-package :common-tones/generators)
;;; one zero  y(n) = a0 x(n) + a1 x(n-1)


(defclass one-zero ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (a1 :initform nil :initarg :a1 :accessor flt-a1)
   (x1 :initform 0.0 :initarg :x1 :accessor flt-x1)))

(defmethod print-object ((d one-zero) stream)
  (format stream "#<(one-zero: a0: ~A, a1: ~A, x1: ~A>"
	  (flt-a0 d) (flt-a1 d) (flt-x1 d)))

(def-optkey-fun make-one-zero (a0 a1)
  (make-instance 'one-zero :a0 a0 :a1 a1))

(defmethod one-zero? ((g one-zero)) t)
(defmethod one-zero? ((g t)) nil)

(defun one-zero (f input)
  (let ((val (+ (* (flt-a0 f) input) (* (flt-a1 f) (flt-x1 f)))))
    (setf (flt-x1 f) input)
    val))

(defmethod mus-order ((gen one-zero)) 1)
(defmethod mus-run ((gen one-zero) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (one-zero gen arg1))
(defmethod mus-xcoeff ((gen one-zero) loc)
  (if (= loc 0)
      (flt-a0 gen)
    (flt-a1 gen)))
(defmethod (setf mus-xcoeff) (val (gen one-zero) loc)
  (if (= loc 0)
      (setf (flt-a0 gen) val)
    (setf (flt-a1 gen) val)))