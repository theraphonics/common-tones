(in-package :common-tones/generators)

(defclass pulse-train (triangle-wave) ())

(def-optkey-fun make-pulse-train ((frequency *clm-default-frequency*) (amplitude 1.0) (initial-phase two-pi))
  (make-instance 'pulse-train
		 :current-value 0.0
		 :base amplitude		; another version alternates sign
		 :phase initial-phase		; this will give us an immediate pulse
		 :freq (hz->radians frequency)))

(defmethod print-object ((d pulse-train) stream)
  (format stream "#<pulse-train: ~A, amplitude: ~A, current-value: ~A>"
	  (prettified-freq (sw-freq d) (sw-phase d))
	  (prettified-float (sw-base d))
	  (prettified-float (sw-current-value d))))

(defmethod pulse-train? ((g pulse-train)) t)
(defmethod pulse-train? ((g t)) nil)

(defun pulse-train (s &optional (fm 0.0))
  (prog1
      (if (>= (abs (sw-phase s)) two-pi)
	  (progn
	    (fix-up-phase s)
	    (sw-base s))		;triggered upon overflow in a sense, so will jitter around if period not integer
					; use ncos for a better pulse
	0.0)
    (incf (sw-phase s) (+ (sw-freq s) fm))))

(defmethod mus-scaler ((gen pulse-train)) (sw-base gen))
(defmethod (setf mus-scaler) (val (gen pulse-train)) (setf (sw-base gen) val) val)
(defmethod mus-run ((gen pulse-train) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (pulse-train gen arg1))