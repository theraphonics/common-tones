(in-package :common-tones/generators)

(defclass triangle-wave ()
  ((current-value :initform nil :initarg :current-value :accessor sw-current-value)
   (freq :initform nil :initarg :freq :accessor sw-freq)
   (phase :initform nil :initarg :phase :accessor sw-phase)
   (base :initform nil :initarg :base :accessor sw-base)))

(defmethod print-object ((d triangle-wave) stream)
  (format stream "#<triangle-wave: ~A, base: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))

(defun fix-up-phase (s)
  (if (plusp (sw-phase s))
      (loop while (>= (sw-phase s) two-pi) do (decf (sw-phase s) two-pi))
    (loop while (minusp (sw-phase s)) do (incf (sw-phase s) two-pi))))

(defun tri-val (amplitude phase)
  (* amplitude (if (< phase (/ pi 2.0)) phase
		 (if (< phase (/ (* 3.0 pi) 2.0))
		     (- pi phase)
		   (- phase two-pi)))))

(def-optkey-fun make-triangle-wave ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0))
  (make-instance 'triangle-wave
		 :current-value (/ (tri-val amplitude initial-phase) (/ pi 2.0))
		 :base (/ (* 2 amplitude) pi)
		 :phase initial-phase
		 :freq (hz->radians frequency)))

(defmethod triangle-wave? ((g triangle-wave)) t)
(defmethod triangle-wave? ((g t)) nil)

(defun triangle-wave (s &optional (fm 0.0))
  (prog1
      (sw-current-value s)
    (incf (sw-phase s) (+ (sw-freq s) fm))
    (if (or (minusp (sw-phase s))
	    (>= (sw-phase s) two-pi))
	(fix-up-phase s))
    (setf (sw-current-value s) (tri-val (sw-base s) (sw-phase s)))))

/*!< old method using increments tended to wander (and was off by a factor of two)


(defmethod mus-frequency ((gen triangle-wave)) (radians->hz (sw-freq gen)))
(defmethod (setf mus-frequency) (val (gen triangle-wave)) (setf (sw-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen triangle-wave)) (sw-freq gen))
(defmethod (setf mus-increment) (val (gen triangle-wave)) (setf (sw-freq gen) val) val)
(defmethod mus-phase ((gen triangle-wave)) (mod (sw-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen triangle-wave)) (setf (sw-phase gen) val) val)
(defmethod mus-scaler ((gen triangle-wave)) (/ (* pi (sw-base gen)) 2.0))
(defmethod (setf mus-scaler) (val (gen triangle-wave)) (setf (sw-base gen) (/ (* 2.0 val) pi)) val)
(defmethod mus-run ((gen triangle-wave) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (triangle-wave gen arg1))



(defclass square-wave (triangle-wave)
  ((width :initform pi :initarg :width :accessor sw-width)))

(def-optkey-fun make-square-wave ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase 0.0))
  (make-instance 'square-wave
		 :current-value (if (< initial-phase pi) 0.0 amplitude)
		 :base amplitude
		 :phase initial-phase
		 :width pi
		 :freq (hz->radians frequency)))

(defmethod print-object ((d square-wave) stream)
  (format stream "#<square-wave: ~A, base: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))

(defmethod square-wave? ((g square-wave)) t)
(defmethod square-wave? ((g t)) nil)

(defun square-wave (s &optional (fm 0.0))
  (prog1
      (sw-current-value s)
    (incf (sw-phase s) (+ (sw-freq s) fm))
    (if (or (minusp (sw-phase s))
	    (>= (sw-phase s) two-pi))
	(fix-up-phase s))
    (setf (sw-current-value s) (if (< (sw-phase s) (sw-width s)) (sw-base s) 0.0))))

(defmethod mus-scaler ((gen square-wave)) (sw-base gen))
(defmethod (setf mus-scaler) (val (gen square-wave)) (setf (sw-base gen) val) val)
(defmethod mus-run ((gen square-wave) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (square-wave gen arg1))
(defmethod mus-width ((gen square-wave)) (/ (sw-width gen) (* 2 pi)))
(defmethod (setf mus-width) (val (gen square-wave)) (setf (sw-width gen) (* 2 pi val)) val)


(defclass sawtooth-wave (triangle-wave) ())

(def-optkey-fun make-sawtooth-wave ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase pi))
  (make-instance 'sawtooth-wave
		 :current-value (* amplitude (/ (- initial-phase pi) pi))
		 :base (/ amplitude pi)
		 :phase initial-phase
		 :freq (hz->radians frequency)))

(defmethod print-object ((d sawtooth-wave) stream)
  (format stream "#<sawtooth-wave: ~A, base: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))

(defmethod sawtooth-wave? ((g sawtooth-wave)) t)
(defmethod sawtooth-wave? ((g t)) nil)

(defun sawtooth-wave (s &optional (fm 0.0))
  (prog1
      (sw-current-value s)
    (incf (sw-phase s) (+ (sw-freq s) fm))
    (if (or (minusp (sw-phase s))
	    (>= (sw-phase s) two-pi))
	(fix-up-phase s))
    (setf (sw-current-value s) (* (sw-base s) (- (sw-phase s) pi)))))

(defmethod mus-scaler ((gen sawtooth-wave)) (* pi (sw-base gen)))
(defmethod (setf mus-scaler) (val (gen sawtooth-wave)) (setf (sw-base gen) (/ val pi)) val)
(defmethod mus-run ((gen sawtooth-wave) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (sawtooth-wave gen arg1))