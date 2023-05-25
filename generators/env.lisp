(in-package :common-tones/generators)

/*!< Envelopes

/*!< magify-seg takes an envelope, a starting time in samples, the envelope duration in samples,

/*!< and a y scaler.  It returns another seg-like list (i.e. a list of time-value pairs),

/*!< where the times are pass numbers, and the values are increments to be added on each pass to

/*!< get to the next y-value.   For very large envelopes, (say more than 50 segments), we should

/*!< simply load the thing into an array and use table-lookup to read it out.


(defun magify-seg (envelope duration-in-samples scaler &optional (stepit nil) (offset 0.0))
  (let* ((lim (- (length envelope) 2))
	 (x0 0.0) (y0 0.0) (cur-x 0.0) (y-incr 0.0)
	 (x1 (car envelope))
	 (result nil)
	 (cur-pass 0)
	 (x-diff (- (nth lim envelope) x1))) ; x1 is really x0 here
    (if (zerop x-diff) (warn "envelope repeats x axis values: ~A" envelope))
    (let* ((x-mag (/ (1- duration-in-samples) x-diff)))
      (if (zerop x-mag)
	  (let ((dur (clm-cerror "use 1.0 for laughs" 1.0 #'plusp "envelope duration is 0.0: ~A" envelope)))
	    (setf x-mag (/ (1- (floor (* *srate* dur))) x-diff))))
      (let* ((inv-x-mag (/ 1.0 x-mag))
	     (y1 (cadr envelope)))
	(loop for i from 0 by 2 below lim and n2 in (cddr envelope) by #'cddr and n3 in (cdddr envelope) by #'cddr do
	  (setf x0 x1)
	  (setf x1 n2)
	  (setf y0 y1)
	  (setf y1 n3)
	  (setf cur-x (max 1 (round (* x-mag (- x1 x0)))))
	  (setf x1 (+ x0 (* inv-x-mag cur-x)))
	  (push cur-pass result)
	  (if (not stepit)
	      (if (= y0 y1)		;no change in y on this segment
		  (setf y-incr 0)
		(setf y-incr (* scaler (/ (- y1 y0) cur-x))))
	    (setf y-incr (+ offset (* scaler y0))))
	  (push y-incr result)
	  (incf cur-pass cur-x))
	(nreverse result)))))

(defun fix-up-exp-env (e off scl base)
  (if e
      (let* ((min-y (+ off (* scl (cadr e))))
	     (max-y min-y)
	     (val 0.0)
	     (tmp 0.0)
	     (nb (and base (not (zerop base)) (/= 1.0 base)))
	     (b (if nb (/ 1.0 (log base))))
	     (b-1 (if nb (- base 1.0)))
	     (result nil)
	     (flat nil)
	     (len (length e)))
	(loop for i from 1 below len by 2 and ni in (cdr e) by #'cddr and ni-1 in e by #'cddr do
	  (setf val (+ off (* scl ni)))
	  (setf min-y (min min-y val))
	  (setf max-y (max max-y val))
	  (push ni-1 result)
	  (push val result))
	(setf result (nreverse result))
	(setf flat (= min-y max-y))
	(if (not flat) (setf val (/ 1.0 (- max-y min-y))))
	(loop for i from 1 below len by 2 do
	  (if (not flat)
	      (setf tmp (* val (- (nth i result) min-y)))
	    (setf tmp 1.0))
	  ;; tmp is now a number between 0 and 1, we need the power that will give us that number given base...
	  (if nb
	      (setf (nth i result) (* (log (+ 1.0 (* tmp b-1))) b))
	    (setf (nth i result) tmp)))
	;; that is -- ((base^x)-1)/(base-1) solved for x in terms of base.
	(values result min-y max-y))
    (values nil 0)))

(defclass seg ()
  ((current-value :initform nil :initarg :current-value :accessor seg-current-value)
   (rate :initform nil :initarg :rate :accessor seg-rate)
   (data :initform nil :initarg :data :accessor seg-data)
   (pass :initform nil :initarg :pass :accessor seg-pass)
   (base :initform nil :initarg :base :accessor seg-base)
   (scaler :initform nil :initarg :scaler :accessor seg-scaler)
   (offset :initform nil :initarg :offset :accessor seg-offset)
   (original-scaler :initform nil :initarg :original-scaler :accessor seg-original-scaler)
   (original-offset :initform nil :initarg :original-offset :accessor seg-original-offset)
   (power :initform nil :initarg :power :accessor seg-power)
   (end :initform nil :initarg :end :accessor seg-end)
   (restart-data :initform nil :initarg :restart-data :accessor seg-restart-data)
   (restart-power :initform nil :initarg :restart-power :accessor seg-restart-power)
   (restart-y :initform nil :initarg :restart-y :accessor seg-restart-y)
   (type :initform nil :initarg :type :accessor seg-type)
   (original-data :initform nil :initarg :original-data :accessor seg-original-data)))

(defmethod print-object ((d seg) stream)
  (format stream "#<env: current-value: ~A, rate: ~A, data: ~A, offset: ~A, scaler: ~A, base: ~A, power: ~A, type: ~A, end: ~A, pass: ~A>"
	  (prettified-float (seg-current-value d))
	  (prettified-float (seg-rate d))
	  (prettified-array (seg-data d))
	  (prettified-float (seg-offset d))
	  (prettified-float (seg-scaler d))
	  (prettified-float (seg-base d))
	  (prettified-float (seg-power d))
	  (seg-type d) (seg-end d) (seg-pass d)))

(defmethod env? ((g seg)) t)
(defmethod env? ((g t)) nil)

(defmethod mus-location ((gen seg)) (seg-pass gen))
(defmethod mus-scaler ((gen seg)) (seg-original-scaler gen))
(defmethod mus-offset ((gen seg)) (seg-original-offset gen))
(defmethod mus-data ((gen seg)) (seg-original-data gen))
(defmethod mus-length ((gen seg)) (seg-end gen))

(defmethod (setf mus-location) (val (gen seg))
  ;; apparently named access-env in CLM-1
  (mus-reset gen)
  (let ((ctr 0))
    (setf (seg-pass gen) (min val (seg-end gen)))
    (loop while (and (seg-data gen) (< ctr val)) do
      (setf (seg-rate gen) (cadr (seg-data gen)))
      (setf (seg-data gen) (cddr (seg-data gen)))
      (let ((passes (- (min (or (car (seg-data gen)) (seg-end gen)) val) ctr)))
	(incf ctr passes)
	(if (eq (seg-type gen) :seg)
	    (if (or (null (seg-base gen))
		    (not (zerop (seg-base gen))))
		(incf (seg-current-value gen) (* passes (seg-rate gen)))
	      (setf (seg-current-value gen) (seg-rate gen)))
	  (progn			;type = :exp
	    (incf (seg-power gen) (* passes (seg-rate gen)))
	    (setf (seg-current-value gen)
	      (+ (seg-offset gen)
		 (* (seg-scaler gen)
		    (- (expt (seg-base gen) (seg-power gen)) 1.0))))))))))

(defun clm-flatten (L)			;borrowed from /dist/lisp/mac/Lisp-Utilities/extensions.lisp
  (cond ((null L) '())
	((atom L) L)
	((consp L)
	 (if (consp (car L))
	     (append (clm-flatten (car L)) (clm-flatten (cdr L)))
	   (cons (car L) (clm-flatten (cdr L)))))
	(t L)))

(def-optkey-fun make-env (envelope (scaler 1.0) duration (offset 0.0) base end length)
  (if (and envelope ; can be nil -> '(0 0 1 0)
	   (listp envelope)
	   (car envelope)
	   (listp (car envelope)))
      (setf envelope (clm-flatten envelope)))
  (if (and base (numberp base) (minusp base))
      (warn "make-env with :base ~,3F won't work -- the 'base' has to be 0.0 or greater.~
             If you're trying to get convex connecting segments, use a base between 0.0 and 1.0." base))
  (if (and (null duration) (null end) (null length))
      (error "make-env needs either :duration, :end, or :length"))
  (let ((dur-in-samples (or length
			    (and end (floor (1+ end)))
			    (floor (* (or duration 0.0) *srate*)))))
    (if (zerop dur-in-samples) (error "make-env with 0 duration?"))
    (let* ((checked-envelope (if (or (not envelope) (= (length envelope) 1))
				 (list 0 0 1 0)
			       (if (= (length envelope) 2)
				   (list (car envelope) (cadr envelope) (1+ (car envelope)) (cadr envelope))
				 envelope)))
	   (y0 (cadr checked-envelope))
	   (init-y (+ offset (* scaler y0))))

      (when #-(and sbcl (not little-endian)) (> *safety* 0)
	    #+(and sbcl (not little-endian)) t
	    (let ((x0 (first checked-envelope)))
	      (loop for x1 in (cddr checked-envelope) by #'cddr do
		    (if (< x1 x0)
			(error "X axis values out of order in: '~A going from ~A to ~A" envelope x0 x1))
		    (setf x0 x1))))

      (if (or (null base) (= base 1) (= base 0))
	  (let ((data (magify-seg checked-envelope dur-in-samples scaler (and (numberp base) (zerop base)) offset)))
	    (make-instance 'seg
			   :current-value init-y
			   :offset offset
			   :scaler scaler
			   :rate 0.0
			   :base (or base 1.0)
			   :power nil
			   :end (1- dur-in-samples)
			   :original-offset offset
			   :original-scaler scaler
			   :original-data (copy-list envelope)
			   :pass 0
			   :data data
			   :type :seg
				 :restart-y init-y
				 :restart-power 0.0
				 :restart-data data))
	(multiple-value-bind
	 (new-e min-y max-y)
	 (fix-up-exp-env checked-envelope offset scaler base)
	 (let ((data (magify-seg new-e dur-in-samples 1.0 nil 0.0)))
	   (make-instance 'seg
			  :current-value init-y
			  :power (cadr new-e)
			  :base base
			  :pass 0
			  :end (1- dur-in-samples)
			  :offset min-y
			  :scaler (/ (- max-y min-y) (- base 1.0))
			  :offset offset
			  :rate 0.0
			  :type :exp
				:data data
				:original-offset offset
				:original-scaler scaler
				:original-data (copy-list envelope)
				:restart-y init-y
				:restart-power (cadr new-e)
				:restart-data data)))))))

(defun restart-env (e)
  (mus-reset e))

(defun env-interp (x e &optional base)
  (if (eq (seg-type e) :seg)
      (+ (seg-offset e) (* (seg-scaler e) (envelope-interp x (seg-original-data e) (or base 1.0))))
    (envelope-interp x (seg-original-data e) (or base (seg-base e)))))


(defun env (e)
  (if (eq (seg-type e) :seg)
      (prog1
	  (seg-current-value e)
	(when (and (seg-data e)	;are there any segments queued up?
		   (>= (seg-pass e) (car (seg-data e))))
	  (setf (seg-rate e) (cadr (seg-data e)))
	  (setf (seg-data e) (cddr (seg-data e))))
	(incf (seg-pass e))
	(if (or (null (seg-base e))
		(not (zerop (seg-base e))))
	    (if (and (/= 0.0 (seg-rate e))
		     (<= (seg-pass e) (seg-end e)))
		(incf (seg-current-value e) (seg-rate e)))
	  (setf (seg-current-value e) (seg-rate e))))
    (if (eq (seg-type e) :exp)	;exponential interpolation between break-points
	(prog1
	    (seg-current-value e)
	  (when (and (seg-data e)
		     (>= (seg-pass e) (car (seg-data e))))
	    (setf (seg-rate e) (cadr (seg-data e)))
	    (setf (seg-data e) (cddr (seg-data e))))
	  (incf (seg-pass e))
	  (when (and (/= 0.0 (seg-rate e))
		     (<= (seg-pass e) (seg-end e)))
	    (incf (seg-power e) (seg-rate e))
	    (setf (seg-current-value e)
		  (+ (seg-offset e)
		     (* (seg-scaler e)
			(- (expt (seg-base e) (seg-power e)) 1.0))))))
      (error "unknown envelope type: ~A" (seg-type e)))))

(defmethod mus-run ((gen seg) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (env gen))
(defmethod mus-increment ((gen seg)) (seg-base gen))