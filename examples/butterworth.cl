;; Sam Heisz, January 1998
;; inspired by some unit generators written for Csound by Paris Smaragdis
;; who based his work on formulas from 
;; Charles Doge, Computer music: synthesis, composition, and performance.

(defconstant root-2 (sqrt 2))

(def-clm-struct butterworth 
    type				; 1-highpass, 2-lowpass, 
					; 3-bandpass, 4-bandreject
    
    c d                                 ; values for figuring out c1..c5
    c1 c2 c3 c4 c5			; filter coefficients
    s1 s2)				; old samples

(defun make-empty-butterworth (type)
  (make-butterworth :type type
		    :c1 0.0 :c2 0.0 :c3 0.0 :c4 0.0 :c5 0.0 :s1 0.0 :s2 0.0))

(defmacro butter (b sig)
  `(let* ((_x! (- ,sig 
		  (* (butterworth-c4 ,b) (butterworth-s1 ,b))
		  (* (butterworth-c5 ,b) (butterworth-s2 ,b))))
	  (_out! (+ (* _x! (butterworth-c1 ,b))
		    (* (butterworth-s1 ,b) (butterworth-c2 ,b))
		    (* (butterworth-s2 ,b) (butterworth-c3 ,b)))))
		    
     (setf (butterworth-s2 ,b) (butterworth-s1 ,b))
     (setf (butterworth-s1 ,b) _x!)
     _out!))

(defmacro butterhp (b sig) `(butter ,b ,sig))
(defmacro butterlp (b sig) `(butter ,b ,sig))
(defmacro butterbp (b sig) `(butter ,b ,sig))
(defmacro butterbr (b sig) `(butter ,b ,sig))
  
;; HIGHPASS =============================================================

(defmacro set-butterhp (b fq)
  `(let* ((_c! (tan (/ (* pi ,fq) *srate*)))
	  (_c!! (* _c!  _c!)))
     
     (setf (butterworth-c1 ,b) (/ 1.0 (+ 1.0 (* _c! root-2) _c!!)))
     (setf (butterworth-c2 ,b) (* -2.0 (butterworth-c1 ,b)))
     (setf (butterworth-c3 ,b) (butterworth-c1 ,b))
     (setf (butterworth-c4 ,b) (* 2.0 (- _c!! 1.0) (butterworth-c1 ,b)))
     (setf (butterworth-c5 ,b) (* (+ (- 1.0 (* _c! root-2)) _c!!)
				  (butterworth-c1 ,b)))
     ,b))
  
(defun make-butterhp (frequency)
  (let ((b (make-empty-butterworth 1)))
    (set-butterhp b frequency)))

;; LOWPASS =============================================================

(defmacro set-butterlp (b fq)
  `(let* ((_c! (/ 1.0 (tan (/ (* pi ,fq) *srate*))))
	  (_c!! (* _c!  _c!)))
     
     (setf (butterworth-c1 ,b) (/ 1.0 (+ 1.0 (* _c! root-2) _c!!)))
     (setf (butterworth-c2 ,b) (* 2.0 (butterworth-c1 ,b)))
     (setf (butterworth-c3 ,b) (butterworth-c1 ,b))
     (setf (butterworth-c4 ,b) (* 2.0 (- 1.0 _c!!) (butterworth-c1 ,b)))
     (setf (butterworth-c5 ,b) (* (+ (- 1.0 (* _c! root-2)) _c!!)
				  (butterworth-c1 ,b)))
     ,b))
  
(defun make-butterlp (frequency)
  (let ((b (make-empty-butterworth 2)))
    (set-butterlp b frequency)))

;; BANDPASS =============================================================

(defmacro set-butterbp-bandwidth (b bw)
  `(progn
     (setf (butterworth-c ,b) (/ 1.0 (tan (/ (* pi ,bw) *srate*))))
     (setf (butterworth-c1 ,b) (/ 1.0 (+ 1.0 (butterworth-c ,b)))) 
     ;; c2 0.0
     (setf (butterworth-c3 ,b) (- (butterworth-c1 ,b)))
     (setf (butterworth-c4 ,b) (* (- (butterworth-c ,b))
				  (butterworth-d ,b)
				  (butterworth-c1 ,b)))     
     (setf (butterworth-c5 ,b) (* (- (butterworth-c ,b) 1.0) 
				  (butterworth-c1 ,b)))
     ,b))

(defmacro set-butterbp-frequency (b fq)
  `(progn
     (setf (butterworth-d ,b) (* 2.0 (cos (/ (* two-pi ,fq) *srate*))))
     (setf (butterworth-c4 ,b) (* (- (butterworth-c ,b))
				  (butterworth-d ,b)
				  (butterworth-c1 ,b)))
     ,b))

(defmacro set-butterbp (b fq bw)
  `(progn 
     (setf (butterworth-d ,b) (* 2.0 (cos (/ (* two-pi ,fq) *srate*))))
     (set-butterbp-bandwidth ,b ,bw)))

(defun make-butterbp (frequency bandwidth)
  (let ((b (make-empty-butterworth 3)))
    (set-butterbp b frequency bandwidth)))


;; BANDREJECT =============================================================

(defmacro set-butterbr-bandwidth (b bw)
  `(progn
     (setf (butterworth-c ,b) (tan (/ (* pi ,bw) *srate*)))

     (setf (butterworth-c1 ,b) (/ 1.0 (+ 1.0 (butterworth-c ,b)))) 
     (setf (butterworth-c2 ,b) (* (- (butterworth-d ,b)) (butterworth-c1 ,b)))
     (setf (butterworth-c3 ,b) (butterworth-c1 ,b))
     (setf (butterworth-c4 ,b) (butterworth-c2 ,b))
     (setf (butterworth-c5 ,b) (* (- 1.0 (butterworth-c ,b)) 
				  (butterworth-c1 ,b)))
     ,b))

(defmacro set-butterbr-frequency (b fq)
  `(progn
     (setf (butterworth-d ,b) (* 2.0 (cos (/ (* two-pi ,fq) *srate*))))
     (setf (butterworth-c2 ,b) (* (- (butterworth-d ,b)) (butterworth-c1 ,b)))
     (setf (butterworth-c4 ,b) (butterworth-c2 ,b))
     ,b))

(defmacro set-butterbr (b fq bw)
  `(progn 
     (setf (butterworth-d ,b) (* 2.0 (cos (/ (* two-pi ,fq) *srate*))))
     (set-butterbr-bandwidth ,b ,bw)))


(defun make-butterbr (frequency bandwidth)
  (let ((b (make-empty-butterworth 4)))
    (set-butterbr b frequency bandwidth)))

