(in-package :common-tones/generators)

/*!< two-zero  y(n) = a0 x(n) + a1 x(n-1) + a2 x(n-2)


(defclass two-zero ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (a1 :initform nil :initarg :a1 :accessor flt-a1)
   (a2 :initform nil :initarg :a2 :accessor flt-a2)
   (x1 :initform 0.0 :initarg :x1 :accessor flt-x1)
   (x2 :initform 0.0 :initarg :x2 :accessor flt-x2)))

(defmethod print-object ((d two-zero) stream)
  (format stream "#<(two-zero: a0: ~A, a1: ~A, a2: ~A, x1: ~A, x2: ~A>"
	  (flt-a0 d) (flt-a1 d) (flt-a2 d) (flt-x1 d) (flt-x2 d)))

(defmethod two-zero? ((g two-zero)) t)
(defmethod two-zero? ((g t)) nil)

(def-optkey-fun make-two-zero (a0 a1 a2 frequency radius)
  (if (or radius frequency (and (not a2) (> a1 20.0)))
      (make-instance 'two-zero
		     :a0 1.0
		     :a1 (- (* 2.0 (or radius a0) (cos (hz->radians (or frequency a1)))))
		     :a2 (* (or radius a0) (or radius a0)))
    (make-instance 'two-zero :a0 a0 :a1 a1 :a2 a2)))

(defun two-zero (f input)
  (let ((y0 (+ (* (flt-a0 f) input)
	       (* (flt-a1 f) (flt-x1 f))
	       (* (flt-a2 f) (flt-x2 f)))))
    (setf (flt-x2 f) (flt-x1 f))
    (setf (flt-x1 f) input)
    y0))

(defmethod mus-order ((gen two-zero)) 2)

(defmethod mus-run ((gen two-zero) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (two-zero gen arg1))

(defmethod mus-xcoeff ((gen two-zero) loc)
  (if (= loc 0)
      (flt-a0 gen)
    (if (= loc 1)
	(flt-a1 gen)
      (flt-a2 gen))))

(defmethod (setf mus-xcoeff) (val (gen two-zero) loc)
  (if (= loc 0)
      (setf (flt-a0 gen) val)
    (if (= loc 1)
	(setf (flt-a1 gen) val)
      (setf (flt-a2 gen) val))))

(defmethod mus-scaler ((gen two-zero))
  (sqrt (flt-a2 gen)))

(defmethod (setf mus-scaler) (val (gen two-zero))
  (setf (flt-a1 gen) (* -2.0 val (cos (hz->radians (mus-frequency gen)))))
  (setf (flt-a2 gen) (* val val))
  val)

(defmethod mus-frequency ((gen two-zero))
  (radians->hz (acos (/ (flt-a1 gen) (* -2.0 (mus-scaler gen))))))

(defmethod (setf mus-frequency) (val (gen two-zero))
  (setf (flt-a1 gen) (* -2.0 (mus-scaler gen) (cos (hz->radians val))))
  val)