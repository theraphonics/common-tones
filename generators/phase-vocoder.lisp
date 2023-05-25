(in-package :common-tones/generators)


(defclass phase-vocoder ()
  ((input :initform nil :initarg :input :accessor pv-input)
   (output :initform nil :initarg :output :accessor pv-output)
   (interp :initform nil :initarg :interp :accessor pv-interp)
   (filptr :initform 0 :initarg :filptr :accessor pv-filptr)
   (N :initform 512 :initarg :N :accessor pv-N)
   (win :initform nil :initarg :window :accessor pv-window)
   (in-data :initform nil :accessor pv-in-data)
   (D :initform nil :initarg :D :accessor pv-D)
   (amp-increments :initform nil :initarg :amp-increments :accessor pv-amp-increments)
   (amps :initform nil :initarg :amps :accessor pv-amps)
   (freqs :initform nil :initarg :freqs :accessor pv-freqs)
   (phases :initform nil :initarg :phases :accessor pv-phases)
   (phaseinc :initform nil :initarg :phaseinc :accessor pv-phase-increments)
   (lastphase :initform nil :initarg :lastphase :accessor pv-previous-phases)
   (analyze :initform nil :initarg :analyze :accessor pv-analyze)
   (synthesize :initform nil :initarg :synthesize :accessor pv-synthesize)
   (edit :initform nil :initarg :edit :accessor pv-edit)
   (pitch :initform 1.0 :initarg :pitch :accessor pv-pitch)
   (overlap :initform nil :initarg :overlap :accessor pv-overlap)
   ))

(defmethod print-object ((d phase-vocoder) stream)
  (format stream "#<phase-vocoder: N: ~A, D: ~A, interp: ~A, output: ~A>"
	  (pv-N d) (pv-D d) (pv-interp d) (pv-output d)))

(def-optkey-fun make-phase-vocoder (input
				    (fft-size 512)
				    (overlap 4)
				    (interp 256)
				    (pitch 1.0)
				    (analyze nil)
				    (edit nil)
				    (synthesize nil))
  (let ((N2 (floor fft-size 2))
	(D (/ fft-size overlap)))
    (make-instance 'phase-vocoder
		   :N fft-size
		   :interp interp
		   :D D
		   :pitch pitch
		   :output interp
		   :overlap overlap
		   :filptr 0
		   :window (let ((win (make-fft-window hamming-window fft-size))
				 (scl (/ 2.0 (* 0.54 fft-size))))
			     (dotimes (i fft-size)
			       (setf (aref win i) (* (aref win i) scl)))
			     win)
		   :amp-increments (make-double-array fft-size)
		   :freqs (make-double-array fft-size)
		   :amps (make-double-array N2)
		   :phases (make-double-array N2)
		   :lastphase (make-double-array N2)
		   :phaseinc (make-double-array N2)
		   :input (if (mus-input? input)
			      input
			    (make-file->sample (mus-file-name input)))
		   :analyze analyze
		   :edit edit
		   :synthesize synthesize)))

(defmethod phase-vocoder? ((g phase-vocoder)) t)
(defmethod phase-vocoder? ((g t)) nil)

(defun phase-vocoder (pv &optional input)
  (let ((N2 (floor (pv-N pv) 2)))
    (when (>= (pv-output pv) (pv-interp pv))
      ;; get next amp/phase data block
      (let* ((N (pv-N pv))
	     (D (pv-D pv))
	     (amps (pv-amp-increments pv))
	     (freqs (pv-freqs pv))
	     (filptr (pv-filptr pv)))

	(if (or (not (pv-analyze pv))
		(funcall (pv-analyze pv) pv input))
	    ;; if no analysis func, do:
	    (progn
	      (dotimes (i N) (setf (aref freqs i) (double 0.0)))
	      (setf (pv-output pv) 0)
	      (if (not (pv-in-data pv))
		  (progn
		    (setf (pv-in-data pv) (make-double-array N))
		    (dotimes (i N) (setf (aref (pv-in-data pv) i) (double (funcall (or input (pv-input pv)) 1)))))
		(let ((indat (pv-in-data pv)))
		  ;; extra loop here since I find the optimized case confusing (we could dispense with the data move)
		  (do ((i 0 (1+ i))
		       (j D (1+ j)))
		      ((= j N))
		    (setf (aref indat i) (double (aref indat j))))
		  (do ((i (- N D) (1+ i)))
		      ((= i N))
		    (setf (aref indat i) (double (funcall (or input (pv-input pv)) 1))))))
	      (let ((buf (mod filptr N)))
		(do ((k 0 (1+ k)))
		    ((= k N))
		  (setf (aref amps buf) (double (* (aref (pv-window pv) k) (aref (pv-in-data pv) k))))
		  (incf buf)
		  (if (= buf N) (setf buf 0))))
	      (incf (pv-filptr pv) D)
	      (fft amps freqs N 1)
	      (rectangular->polar amps freqs)))

	(if (or (not (pv-edit pv))
		(funcall (pv-edit pv) pv))
	    (progn
	      ;; if no editing func:
	      (do ((k 0 (1+ k))
		   (pscl (/ 1.0 D))
		   (kscl (/ (* 2.0 pi) N)))
		  ((= k (floor N 2)))
		(let ((phasediff (- (aref freqs k) (aref (pv-previous-phases pv) k))))
		  (setf (aref (pv-previous-phases pv) k) (double (aref freqs k)))
		  (if (> phasediff pi) (do () ((<= phasediff pi)) (setf phasediff (- phasediff (* 2.0 pi)))))
		  (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (setf phasediff (+ phasediff (* 2.0 pi)))))
		  (setf (aref freqs k) (double (* (pv-pitch pv) (+ (* pscl phasediff) (* k kscl)))))))))

	(let ((scl (/ 1.0 (pv-interp pv))))
	  (dotimes (i N2)
	    (setf (aref amps i) (double (* scl (- (aref amps i) (aref (pv-amps pv) i)))))
	    (setf (aref freqs i) (double (* scl (- (aref freqs i) (aref (pv-phase-increments pv) i)))))))))

    (incf (pv-output pv))
    (if (pv-synthesize pv)
	(funcall (pv-synthesize pv) pv)
      ;; if no synthesis func:
      ;; synthesize next sample
      (progn
	(dotimes (i N2)
	  (incf (aref (pv-amps pv) i) (double (aref (pv-amp-increments pv) i)))
	  (incf (aref (pv-phase-increments pv) i) (double (aref (pv-freqs pv) i)))
	  (incf (aref (pv-phases pv) i) (double (aref (pv-phase-increments pv) i))))
	(sine-bank (pv-amps pv) (pv-phases pv))))))

(defmethod mus-hop ((gen phase-vocoder)) (pv-D gen))
(defmethod (setf mus-hop) (val (gen phase-vocoder)) (setf (pv-D gen) val))
(defmethod mus-length ((gen phase-vocoder)) (pv-N gen))
(defmethod mus-increment ((gen phase-vocoder)) (pv-interp gen))
(defmethod (setf mus-increment) (val (gen phase-vocoder)) (setf (pv-interp gen) val))
;;; (defmethod mus-data ((gen phase-vocoder)) (pv-in-data gen))

(defmethod mus-frequency ((gen phase-vocoder)) (pv-pitch gen))
(defmethod (setf mus-frequency) (val (gen phase-vocoder)) (setf (pv-pitch gen) val))
(defmethod mus-run ((gen phase-vocoder) &optional (arg1 0.0) (arg2 0.0)) (declare (ignore arg1 arg2)) (phase-vocoder gen))



(defun mus-apply (&optional args) (apply #'mus-run args))