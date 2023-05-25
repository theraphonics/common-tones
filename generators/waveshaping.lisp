(in-package :common-tones/generators)

/*!< Waveshaping

/*!< see "Digital Waveshaping Synthesis" by Marc Le Brun in JAES 1979 April, vol 27, no 4, p250


(defun signify (harm-amps)		;taken very directly from MLB's Mus10 code.
					;Here we assume Harm-amps is ordered by partial number.
  (let ((lastpt (length harm-amps)))
    (do ((i 2 (+ i di))
	 (di 1 (- 4 di)))		;successively 1 and 3, getting the pattern + + - - + + ...
	((>= i lastpt) harm-amps)
      (setf (aref harm-amps i) (double (- (aref harm-amps i)))))))

; T(n+1) <= 2xT(n)-T(n-1) gives the Chebychev polynomials of the first kind
; (T0 = 1, T1 = X to get recursion going)

; we take the array of signified partial amplitudes (harm-amps) and use them to weight the
; associated Chebychev polynomial

/*!< assume we're using synth-data that looks like additive synthesis tables

/*!< (i.e. a list of partial-amp pairs).  That means we have to prepare it for

/*!< the preceding two procedures by loading it into an array.


(defun normalize-partials (partials)
  (let ((sum 0.0))
    (loop for i in (cdr partials) by #'cddr do (incf sum (abs i)))
    (if (zerop sum) (warn "all partials have 0.0 amplitude: ~A" partials))
    (setf sum (/ 1.0 sum))
    (do ((i 1 (+ i 2)))
	((>= i (length partials)) partials)
      (setf (nth i partials) (* (nth i partials) sum)))))

(defun highest-partial (data)
  (if (endp data) 0.0
    (max (car data) (highest-partial (cddr data)))))

(defun massage-partials (data)
  (do* ((i 0 (+ i 2))
	(lim (length data))
	(maxH (highest-partial data))
	(hamps (make-double-array (+ maxH 1))))
      ((>= i lim) hamps)
    (setf (aref hamps (nth i data)) (double (nth (+ i 1) data)))))



(defclass polyshape ()
  ((freq :initform nil :initarg :freq :accessor ws-freq)
   (phase :initform nil :initarg :phase :accessor ws-phase)
   (wave :initform nil :initarg :wave :accessor ws-wave)
   (offset :initform 0.0 :initarg :offset :accessor ws-offset)))

(defmethod print-object ((d polyshape) stream)
  (format stream "#<polyshape: ~A: coeffs[~A]: ~A>"
		       (prettified-freq (ws-freq d) (ws-phase d))
		       (length (ws-wave d))
		       (prettified-array (ws-wave d))))

(def-optkey-fun make-polyshape ((frequency *clm-default-frequency*) (initial-phase 0.0) (coeffs nil) (partials '(1 1)) (kind mus-chebyshev-first-kind))
  (make-instance 'polyshape
		 :wave (or coeffs (partials->polynomial (or partials (list 1.0 1.0)) kind))
		 :freq (hz->radians frequency)
		 :phase initial-phase))

(defun polyshape (w &optional (index 1.0) (fm 0.0))
  (let ((val (polynomial (ws-wave w)
			 (* index (cos (ws-phase w)))))) ; was sin, 31-May-08
    (incf (ws-phase w) (+ (ws-freq w) fm))
    (when (or (> (ws-phase w) 100.0) (< (ws-phase w) -100.0))
      (setf (ws-phase w) (mod (ws-phase w) two-pi)))
    val))

(defmethod polyshape? ((g polyshape)) t)
(defmethod polyshape? ((g t)) nil)


(defclass polywave (polyshape)
  ((index :initform 1.0 :initarg :index :accessor ws-index)
   (top :initform 0 :initarg :top :accessor ws-top)
   (type :initform mus-chebyshev-first-kind :initarg :type :accessor ws-type)))

(defmethod print-object ((d polywave) stream)
  (format stream "#<polywave: ~A: coeffs[~A]: ~A>"
		       (prettified-freq (ws-freq d) (ws-phase d))
		       (length (ws-wave d))
		       (prettified-array (ws-wave d))))

(defmethod mus-scaler ((gen polywave)) (ws-index gen))
(defmethod (setf mus-scaler) (val (gen polywave)) (setf (ws-index gen) val) val)

(def-optkey-fun make-polywave ((frequency *clm-default-frequency*) (partials '(1 1)) (type mus-chebyshev-first-kind))
  (let ((top (loop for partial in partials by #'cddr maximize partial)))
    (make-instance 'polywave
		   :top (1+ top)
		   :type type
		   :wave (let ((coeffs (make-double-array (1+ top)))
			       (len (/ (length partials) 2)))
			   (loop for i from 0 by 1 below len do
			     (setf (aref coeffs (nth (* i 2) partials)) (double (nth (+ (* i 2) 1) partials))))
			   coeffs)
		   :freq (hz->radians frequency)
		   :phase 0.0)))

(defun mus-chebyshev-t-sum (x index n tn)
  (let* ((b1 0.0)
	 (b2 0.0)
	 (cx (* index (cos x)))
	 (x2 (* 2.0 cx))
	 (b (aref tn (- n 1))))
    (loop for i from (- n 2) downto 0 do
      (setf b2 b1)
      (setf b1 b)
      (setf b (- (+ (* b1 x2) (aref tn i)) b2)))
    (- b (* cx b1))))

(defun mus-chebyshev-u-sum (x index n un)
  (let* ((b1 0.0)
	 (b2 0.0)
	 (cx (* index (cos x)))
	 (x2 (* 2.0 cx))
	 (b (aref un (- n 1))))
    (loop for i from (- n 2) downto 1 do
      (setf b2 b1)
      (setf b1 b)
      (setf b (- (+ (* b1 x2) (aref un i)) b2)))
    (* b (sin x))))

(defun polywave (w &optional (fm 0.0))
  (let ((result (if (not (= (ws-type w) mus-chebyshev-second-kind))
		    (mus-chebyshev-t-sum (ws-phase w) (ws-index w) (ws-top w) (ws-wave w))
		  (mus-chebyshev-u-sum (ws-phase w) (ws-index w) (ws-top w) (ws-wave w)))))
    (incf (ws-phase w) (+ (ws-freq w) fm))
    (when (or (> (ws-phase w) 100.0) (< (ws-phase w) -100.0))
      (setf (ws-phase w) (mod (ws-phase w) two-pi)))
    result))

(defmethod polywave? ((g polywave)) t)
(defmethod polywave? ((g t)) nil)



(defun partial-amp (n partials)
  (loop for i on partials by #'cddr do
    (if (= n (car i)) (return (cadr i)))))

(defun partials->polynomial (partials &optional (kind mus-chebyshev-first-kind))
  (let* ((top (floor (highest-partial partials)))
	 (size (+ top 1))
	 (T0 (make-array size :element-type 'integer :initial-element 0))
	 (T1 (make-array size :element-type 'integer :initial-element 0))
	 (Tn (make-array size :element-type 'integer :initial-element 0))
	 (Cc1 (make-array size :element-type 'float :initial-element 0.0))
	 (amp 0.0))
    (if (= kind mus-chebyshev-first-kind)
	(setf (aref T0 0) 1)
      (setf (aref T0 0) 0))
    (setf (aref T1 1) 1)		;initialize Tn recursion (0 in T0 is Un)
    (loop for i from 1 to top do	;linear combination of polynomials weighted by harmonic amplitude
      (setf amp (or (partial-amp i partials) 0.0))
      (when (/= 0.0 amp)
	(if (= kind mus-chebyshev-first-kind)
	    (loop for k from 0 to i do (incf (aref Cc1 k) (* amp (aref T1 k))))
	  (loop for k from 1 to i do (incf (aref Cc1 (- k 1)) (* amp (aref T1 k))))))
      (when (/= i top)
	(loop for k from (+ i 1) downto 1 do
	  (setf (aref Tn k) (- (* 2 (aref T1 (- k 1))) (aref T0 k))))
	(setf (aref Tn 0) (- (aref T0 0)))
	(loop for k from (+ i 1) downto 0 do
	  (setf (aref T0 k) (aref T1 k))
	  (setf (aref T1 k) (aref Tn k)))))
    (let ((cc (make-double-array size)))
      (loop for i from 0 below size do (setf (aref cc i) (double (aref Cc1 i))))
      cc)))