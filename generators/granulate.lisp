(in-package :common-tones/generators)
/*!< Granulate was originally called SpeedFile (in Mixer.Sai)

/*!< Another version of SpeedFile alternated between forward and backward segments.


(defclass granulate ()
  ((rd :initform nil :initarg :rd :accessor spd-rd)
   (len :initform nil :initarg :len :accessor spd-len)
   (rmp :initform nil :initarg :rmp :accessor spd-rmp)
   (amp :initform nil :initarg :amp :accessor spd-amp)
   (input-hop :initform nil :initarg :input-hop :accessor spd-input-hop)
   (output-hop :initform nil :initarg :output-hop :accessor spd-output-hop)
   (cur-in :initform nil :initarg :cur-in :accessor spd-cur-in)
   (cur-out :initform 0 :initarg :cur-out :accessor spd-cur-out)
   (data :initform nil :initarg :b :accessor spd-data)
   (s20 :initform nil :initarg :s20 :accessor spd-s20)
   (s50 :initform nil :initarg :s50 :accessor spd-s50)
   (ctr :initform 0 :initarg :ctr :accessor spd-ctr)
   (block-len :initform nil :initarg :block-len :accessor spd-block-len)
   (in-data :initform nil :initarg :in-data :accessor spd-in-data)
   (in-data-start :initform 0 :accessor spd-in-data-start)
   (in-data-len :initform 0 :accessor spd-in-data-len)
   (grain :initform nil :accessor spd-grain)
   (edit :initform nil :initarg :edit :accessor spd-edit)

   ;; rest of fields for new run macro's benefit
   (expansion :initform nil :initarg :expansion :accessor spd-expansion)
   (length :initform nil :initarg :length :accessor spd-length)
   (hop :initform nil :initarg :hop :accessor spd-hop)
   (ramp :initform nil :initarg :ramp :accessor spd-ramp)
   (jitter :initform nil :initarg :jitter :accessor spd-jitter)
   (max-size :initform nil :initarg :max-size :accessor spd-max-size)))

(defmethod print-object ((d granulate) stream)
  (format stream "#<granulate: amp: ~A, len: ~A (~A), rmp: ~A, input-hop: ~A, output-hop: ~A, cur-in: ~A (~A), cur-out: ~A, s20: ~A, s50: ~A, ctr: ~A, rd: ~A, data: ~A, in-data: ~A>"
	  (prettified-float (spd-amp d))
	  (spd-len d) (spd-block-len d) (spd-rmp d) (spd-input-hop d) (spd-output-hop d)
	  (spd-cur-in d) (spd-in-data-start d) (spd-cur-out d) (spd-s20 d) (spd-s50 d) (spd-ctr d)
	  (spd-rd d)
	  (prettified-array (spd-data d))
	  (prettified-array (spd-in-data d))))

(def-optkey-fun make-granulate (input
				(expansion 1.0)
				(length .15)
				(scaler .6)
				(hop .05)
				(ramp .4)     ;amount of segment spent sloping up or down (envelope)
				(jitter 1.0)
				max-size
				edit)
   (let ((val (make-instance 'granulate
			    :cur-out 0
			    :rd (if (mus-input? input)
				    input
				  (make-file->sample (mus-file-name input)))
			    :cur-in 0 ;start
			    :len (ceiling (* length *srate*))
			    :rmp (floor (* ramp length *srate*))
			    :amp scaler
			    :input-hop (floor (/ (* hop *srate*)
					      (if (numberp expansion) expansion
						(if (null expansion) 1.0
						  (let* ((argtype (type-of expansion)))
						    (warn "the expansion argument to make-granulate: ~A, should be of type number, not ~A"
							  expansion argtype)
						    1.0)))))
			    :output-hop (floor (* hop *srate*))
			    :s20 (floor (* jitter (/ *srate* 20)))
			    :s50 (floor (* jitter (/ *srate* 50)))
			    :edit edit
			    :ctr 0
			    :expansion expansion
			    :length length
			    :hop hop
			    :ramp ramp
			    :jitter jitter
			    :max-size max-size)))
     (let ((block-length (ceiling (or max-size (* *srate* (+ hop length))))))
       (setf (spd-block-len val) block-length)
       (if (<= block-length 0) (warn "granulate block has ~D elements?" block-length))
       (if (readin? input)
         (setf (spd-cur-in val) (mus-location input))
         (progn
	  (setf (spd-in-data-len val) (+ block-length (spd-s20 val) 1))
	  (if (spd-edit val) (setf (spd-grain val) (make-double-array (spd-in-data-len val))))
	  (setf (spd-in-data-start val) (spd-in-data-len val))))
       val)))

(defmethod granulate? ((g granulate)) t)
(defmethod granulate? ((g t)) nil)

(defun granulate (e &optional input-function)
  (if (not (spd-data e)) (setf (spd-data e) (make-double-array (spd-block-len e))))
  (let ((cur-val (aref (spd-data e) (floor (spd-ctr e)))))
    (incf (spd-ctr e))

    ;; do we need the next grain?
    (when (>= (spd-ctr e) (spd-cur-out e))
      (let* ((start (floor (spd-cur-out e)))
	     (end (max 0 (- (spd-len e) start))))
	(if (> end 0)
	    (loop for i from 0 below end and j from start do
	      (setf (aref (spd-data e) i) (aref (spd-data e) j))))
	(loop for i from end below (spd-block-len e) do
	  (setf (aref (spd-data e) i) (double 0.0))))

      ;; we need unidirectional input from the input-function if it's not a file reader
      ;; so we save partial results in spd-in-data; this input has to be basically
      ;; regular (i.e. follow input-hop) with local (non-accumulating) jitter
      (when (not (readin? (spd-rd e)))
	(if (not (spd-in-data e)) (setf (spd-in-data e) (make-double-array (spd-in-data-len e))))
	(let ((start (spd-in-data-start e))
	      (len (spd-in-data-len e)))
	  (when (> start len)		; in hop is larger than buffer size
	    (let ((extra (- start len)))
	      (loop for i from 0 below extra do (funcall (or input-function (spd-rd e)) 1))
	      (setf start len)))
	  (if (< start len)
	      (loop for i from 0 and k from start below len do
		(setf (aref (spd-in-data e) i) (aref (spd-in-data e) k))))
	  (loop for i from (- len start) below len do
	    (setf (aref (spd-in-data e) i) (double (funcall (or input-function (spd-rd e)) 1))))
	  (setf (spd-in-data-start e) (spd-input-hop e))))

      (if (spd-edit e)
	  (loop for i from 0 below (spd-in-data-len e) do (setf (aref (spd-grain e) i) (double 0.0))))

      (let ((data (if (spd-edit e) (spd-grain e) (spd-data e))))
	;; add in enveloped grain
	(let* ((amp 0.0)
	       (incr (/ (spd-amp e) (spd-rmp e)))
	       (steady-end (- (spd-len e) (spd-rmp e))))
	  (if (readin? (spd-rd e))
	      (loop for i from 0 below (spd-len e) do
		(incf (aref data i) (double (* amp (readin (spd-rd e)))))
		(if (< i (spd-rmp e)) (incf amp incr) (if (> i steady-end) (decf amp incr))))
	    (let ((curstart (floor (random (spd-s20 e)))))
	      (loop for i from 0 below (spd-len e) and j from curstart do
		(incf (aref data i) (double (* amp (aref (spd-in-data e) j))))
		(if (< i (spd-rmp e)) (incf amp incr) (if (> i steady-end) (decf amp incr))))))))

      (if (spd-edit e)
	  (let ((new-len (min (funcall (spd-edit e) e) (spd-in-data-len e))))
	    (if (<= new-len 0)
		(setf new-len (spd-len e)))
	    (loop for i from 0 below new-len do
	      (incf (aref (spd-data e) i) (aref (spd-grain e) i)))))

      ;; set up counters to trigger next grain
      (decf (spd-ctr e) (spd-cur-out e))
      (setf (spd-cur-out e) (max 0 (+ (spd-output-hop e) (- (random (spd-s50 e)) (floor (spd-s50 e) 2)))))
      (when (readin? (spd-rd e))
	(setf (mus-location (spd-rd e)) (+ (spd-cur-in e) (random (spd-s20 e))))
	(incf (spd-cur-in e) (spd-input-hop e))))
    cur-val))

(defmethod mus-frequency ((gen granulate)) (double (/ (spd-output-hop gen) *srate*)))
(defmethod (setf mus-frequency) (val (gen granulate)) (setf (spd-output-hop gen) (round (* *srate* val))) val)
(defmethod mus-ramp ((gen granulate)) (spd-rmp gen))
(defmethod (setf mus-ramp) (val (gen granulate)) (setf (spd-rmp gen) val))
(defmethod mus-hop ((gen granulate)) (spd-output-hop gen))
(defmethod (setf mus-hop) (val (gen granulate)) (setf (spd-output-hop gen) val))
(defmethod mus-scaler ((gen granulate)) (spd-amp gen))
(defmethod (setf mus-scaler) (val (gen granulate)) (setf (spd-amp gen) val))
(defmethod mus-increment ((gen granulate)) (double (/ (spd-output-hop gen) (spd-input-hop gen))))
(defmethod (setf mus-increment) (val (gen granulate)) (setf (spd-input-hop gen) (floor (/ (spd-output-hop gen) val))) val)
(defmethod mus-length ((gen granulate)) (spd-len gen))
(defmethod (setf mus-length) (val (gen granulate)) (setf (spd-len gen) val))
(defmethod mus-run ((gen granulate) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (granulate gen))
(defmethod mus-data ((gen granulate)) (spd-grain gen))