(in-package :common-tones)

;;; Rand and Rand-Interp
;;;
;;; rand latches its output random number, getting a new number
;;; every srate/freq samples -- internally we pretend that our cycle is between 0 and
;;; two-pi so that the caller can use hz->radians without confusion.  This way,
;;; frequency calculations look the same between oscil and rand and so on.
;;; rand-interp interpolates between successive random numbers.


(defun ran (lo hi)			;returns random numbers between lo and hi
  (if (= hi lo)
      lo
    (+ lo (random (- hi lo)))
    ; #+(and excl cltl2) (+ lo (* (- hi lo) (random 1.0f0)))
    ))

(defun centered-random (n)          ;[-n .. n] not lisp's [0.0 .. n]
  (if (zerop n) n			;don't die just because n happens to touch 0!
    (- (random (* 2 n)) n)))

(defun mus-random (n) (centered-random n))

(defun inverse-integrate (dist &optional (data-size 512) (e-size 50))
  (let* ((e '())
	 (sum (cadr dist))
	 (first-sum sum)
	 (data (make-double-array data-size))
	 (x0 (car dist))
	 (x1 (nth (- (length dist) 2) dist))
	 (xincr (/ (- x1 x0) e-size)))
    (do ((i 0 (1+ i))
	 (x x0 (+ x xincr)))
	((> i e-size))
      (setf e (cons sum e))
      (setf e (cons x e))
      (setf sum (+ sum (envelope-interp x dist))))
    (let* ((incr (/ (- (cadr e) first-sum) (- data-size 1))))
      (setf e (reverse e))
      (do ((i 0 (1+ i))
	   (x first-sum (+ x incr)))
	  ((= i data-size))
	(setf (aref data i) (double (envelope-interp x e))))
      data)))

(defclass rand ()
  ((freq :initform nil :initarg :freq :accessor noi-freq)
   (base :initform nil :initarg :base :accessor noi-base)
   (phase :initform nil :initarg :phase :accessor noi-phase)
   (incr :initform nil :initarg :incr :accessor noi-incr)
   (output :initform nil :initarg :output :accessor noi-output)
   (distribution :initform nil :initarg :distribution :accessor noi-distribution)
   (distribution-size :initform nil :initarg :distribution-size :accessor noi-distribution-size)))

(defmethod print-object ((d rand) stream)
  (format stream "#<rand: ~A, amplitude: ~A~A>"
	  (prettified-freq (noi-freq d) (noi-phase d))
	  (prettified-float (noi-base d))
	  (if (noi-distribution d)
	      ", with distribution"
	    "")))

(def-optkey-fun make-rand ((frequency *clm-default-frequency*) (amplitude 1.0) (envelope nil) (distribution nil))
  (make-instance 'rand
		 :freq (hz->radians frequency)
		 :base amplitude
		 :phase 0.0
		 :incr 0.0
		 :output 0.0
		 :distribution (or (and envelope (inverse-integrate envelope))
				   distribution)
		 :distribution-size (if envelope 512
				      (if distribution (length distribution)
					0))))

(defmethod rand? ((g rand)) t)
(defmethod rand? ((g t)) nil)

(defun random-any (r)
  (if (noi-distribution r)
      (* (noi-base r)
	 (array-interp (noi-distribution r)
		       (random (float (noi-distribution-size r)))
		       (noi-distribution-size r)))
    (if (zerop (noi-base r))
	0.0
      (- (random (* 2.0 (noi-base r)))
	 (noi-base r)))))

(defun rand (r &optional (sweep 0.0))
  (progn
    (if (>= (noi-phase r) two-pi)
	(progn
	  (loop while (>= (noi-phase r) two-pi) do (decf (noi-phase r) two-pi))
	  (setf (noi-output r) (random-any r))))
    (incf (noi-phase r) (+ (noi-freq r) sweep))
    (loop while (minusp (noi-phase r)) do (incf (noi-phase r) two-pi))
    (noi-output r)))

(defmethod mus-frequency ((gen rand)) (radians->hz (noi-freq gen)))
(defmethod (setf mus-frequency) (val (gen rand)) (setf (noi-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen rand)) (noi-freq gen))
(defmethod (setf mus-increment) (val (gen rand)) (setf (noi-freq gen) val) val)
(defmethod mus-phase ((gen rand)) (mod (noi-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen rand)) (setf (noi-phase gen) val) val)
(defmethod mus-scaler ((gen rand)) (noi-base gen))
(defmethod (setf mus-scaler) (val (gen rand)) (setf (noi-base gen) val))
(defmethod mus-run ((gen rand) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (rand gen arg1))
(defmethod mus-length ((gen rand)) (noi-distribution-size gen))
(defmethod mus-data ((gen rand)) (noi-distribution gen))


(defclass rand-interp (rand) ())

(def-optkey-fun make-rand-interp ((frequency *clm-default-frequency*) (amplitude 1.0) (envelope nil) (distribution nil))
  (make-instance 'rand-interp
		 :freq (hz->radians frequency)
		 :base amplitude
		 :phase 0.0
		 :output 0.0
		 :incr (if (zerop amplitude)
			   0.0
			 (* (random amplitude) (/ frequency *srate*)))
		 :distribution (or (and envelope (inverse-integrate envelope))
				   distribution)
		 :distribution-size (if envelope 512
				      (if distribution (length distribution)
					0))))

(defmethod print-object ((d rand-interp) stream)
  (format stream "#<rand-interp: ~A, amplitude: ~A, increment: ~A~A>"
	  (prettified-freq (noi-freq d) (noi-phase d))
	  (prettified-float (noi-base d))
	  (prettified-float (noi-incr d))
	  (if (noi-distribution d)
	      ", with distribution"
	    "")))

(defmethod rand-interp? ((g rand-interp)) t)
(defmethod rand-interp? ((g t)) nil)

(defun rand-interp (r &optional (sweep 0.0))
  (prog1
      (incf (noi-output r) (noi-incr r))
    (when (>= (noi-phase r) two-pi)
      (loop while (>= (noi-phase r) two-pi) do (decf (noi-phase r) two-pi))
      (setf (noi-incr r) (* (- (random-any r)
			       (noi-output r))
			    (/ (+ (noi-freq r) sweep) two-pi))))
    ;; the (+ freq sweep) is obviously just a wild guess at the current "frequency"
    (incf (noi-phase r) (+ (noi-freq r) sweep))
    (loop while (minusp (noi-phase r)) do (incf (noi-phase r) two-pi))))

(defmethod mus-frequency ((gen rand-interp)) (radians->hz (noi-freq gen)))
(defmethod (setf mus-frequency) (val (gen rand-interp)) (setf (noi-freq gen) (hz->radians val)) val)
(defmethod mus-increment ((gen rand-interp)) (noi-freq gen))
(defmethod (setf mus-increment) (val (gen rand-interp)) (setf (noi-freq gen) val) val)
(defmethod mus-phase ((gen rand-interp)) (mod (noi-phase gen) two-pi))
(defmethod (setf mus-phase) (val (gen rand-interp)) (setf (noi-phase gen) val) val)
(defmethod mus-run ((gen rand-interp) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg2)) (rand-interp gen arg1))
(defmethod mus-length ((gen rand-interp)) (noi-distribution-size gen))
(defmethod mus-data ((gen rand-interp)) (noi-distribution gen))
