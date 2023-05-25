(in-package :common-tones/generators)

(defclass oscil ()
  ((freq :initform nil :initarg :freq :accessor oscil-freq)
   (phase :initform nil :initarg :phase :accessor oscil-phase)))

(defmethod print-object ((gen oscil) stream)
  (format stream "#<oscil: ~A>" (prettified-freq (oscil-freq gen) (oscil-phase gen)))
  gen)

(def-optkey-fun make-oscil ((frequency *clm-default-frequency*) (initial-phase 0.0))
  (make-instance 'oscil
		 :freq (hz->radians frequency)
		 :phase initial-phase))

(defun oscil (gen &optional (fm-input 0.0) (pm-input 0.0))
  (prog1
      (sin (+ (oscil-phase gen) pm-input))
    (incf (oscil-phase gen) (+ (oscil-freq gen) fm-input))
    ;; if we were being extremely careful, we'd add the fm-input into the sin call at the start too.
    (when (or (> (oscil-phase gen) 100.0) (< (oscil-phase gen) -100.0))
      (setf (oscil-phase gen) (mod (oscil-phase gen) two-pi)))))

(defmethod oscil? ((g oscil)) t)
(defmethod oscil? ((g t)) nil)

(defmethod mus-frequency ((gen oscil)) (radians->hz (oscil-freq gen)))
(defmethod (setf mus-frequency) (val (gen oscil)) (setf (oscil-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen oscil)) (oscil-freq gen))
(defmethod (setf mus-increment) (val (gen oscil)) (setf (oscil-freq gen) val) val)
(defmethod mus-phase ((gen oscil)) (mod (oscil-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen oscil)) (setf (oscil-phase gen) val) val)
(defmethod mus-length ((gen oscil)) 1)
(defmethod mus-run ((gen oscil) &optional (arg1 0.0) (arg2 0.0)) (oscil gen arg1 arg2))
