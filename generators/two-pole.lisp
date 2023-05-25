(in-package :common-tones/generators)

;;; two-pole  y(n) = a0 x(n) - b1 y(n-1) - b2 y(n-2)

(defclass two-pole ()
  ((a0 :initform nil :initarg :a0 :accessor flt-a0)
   (b1 :initform nil :initarg :b1 :accessor flt-b1)
   (b2 :initform nil :initarg :b2 :accessor flt-b2)
   (y1 :initform 0.0 :initarg :y1 :accessor flt-y1)
   (y2 :initform 0.0 :initarg :y2 :accessor flt-y2)))

(defmethod print-object ((d two-pole) stream)
  (format stream "#<(two-pole: a0: ~A, b1: ~A, b2: ~A, y1: ~A, y2: ~A>"
	  (flt-a0 d) (flt-b1 d) (flt-b2 d) (flt-y1 d) (flt-y2 d)))

(defmethod two-pole? ((g two-pole)) t)
(defmethod two-pole? ((g t)) nil)

(defun make-two-pole-base (a0 b1 b2)
  (if (>= (abs b1) 2.0)
      (format t "unstable two-pole filter, b1=~,3F ~A ~A" b1 (if (minusp b1) "<=" ">=") (if (minusp b1) "-2.0" "2.0"))
    (if (>= (abs b2) 1.0)
	(format t "unstable two-pole filter, b2=~,3F ~A ~A" b1 (if (minusp b2) "<=" ">=") (if (minusp b2) "-1.0" "1.0"))
      (if (and (>= (- (* b1 b1) (* b2 4.0)) 0.0)
	       (or (>= (+ b1 b2) 1.0)
		   (>= (- b2 b1) 1.0)))
	  (format t "unstable filter: b1=~,3F, b2=~,3F" b1 b2))))
  (make-instance 'two-pole :a0 a0 :b1 b1 :b2 b2))

(def-optkey-fun make-two-pole (a0 b1 b2 frequency radius)
  (if (or radius frequency (and (not b2) (>= b1 2.0)))
      (make-two-pole-base 1.0
			  (- (* 2.0 (or radius a0) (cos (hz->radians (or frequency b1)))))
			  (* (or radius a0) (or radius a0)))
    (make-two-pole-base a0 b1 b2)))

(defun two-pole (f input)
  (let ((y0 (- (* (flt-a0 f) input)
	       (* (flt-b1 f) (flt-y1 f))
	       (* (flt-b2 f) (flt-y2 f)))))
    (setf (flt-y2 f) (flt-y1 f))
    (setf (flt-y1 f) y0)
    y0))

(defmethod mus-order ((gen two-pole)) 2)

(defmethod mus-run ((gen two-pole) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (two-pole gen arg1))

(defmethod mus-xcoeff ((gen two-pole) loc)
  (if (= loc 0)
      (flt-a0 gen)))

(defmethod mus-ycoeff ((gen two-pole) loc)
  (if (= loc 1)
      (flt-b1 gen)
    (flt-b2 gen)))

(defmethod (setf mus-xcoeff) (val (gen two-pole) index)
  (declare (ignore index))
  (setf (flt-a0 gen) val))

(defmethod (setf mus-ycoeff) (val (gen two-pole) index)
  (if (= index 1)
      (setf (flt-b1 gen) val)
    (setf (flt-b2 gen) val)))

(defmethod mus-scaler ((gen two-pole))
  (sqrt (flt-b2 gen)))

(defmethod (setf mus-scaler) (val (gen two-pole))
  (setf (flt-b1 gen) (* -2.0 val (cos (hz->radians (mus-frequency gen)))))
  (setf (flt-b2 gen) (* val val))
  val)

(defmethod mus-frequency ((gen two-pole))
  (radians->hz (acos (/ (flt-b1 gen) (* -2.0 (mus-scaler gen))))))

(defmethod (setf mus-frequency) (val (gen two-pole))
  (setf (flt-b1 gen) (* -2.0 (mus-scaler gen) (cos (hz->radians val))))
  val)
