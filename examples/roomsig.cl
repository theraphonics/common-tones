(in-package :common-tones)

;;; roomsig -- attempt to more accurately imitate the initial echos.  Here we assume a rectangular room.
;;; see "Extension of the Image Model to Arbitrary Polyhedra" by J. Borish  JASA 75(6) June 84.
;;;
;;; which form of the data you use probably depends on whether you're aiming at headphones or speakers.
;;;
;;; make-roomsig    room-width room-length  ;all distances in meters
;;;		    user-X user-Y           ;(from lower left corner)
;;;                 source-X source-Y
;;;                 seconds-of-echos-to-compute)
;;;
;;; returns a list of descriptions of the reflections in no particular order.
;;; To massage this list in to a list of lists ordered by distance:
;;;    reflection-path-distance-to-user apparent-angle-from-user
;;; call roomsig-locate-data:
;;;
;;; (roomsig-locate-data (make-roomsig 100 100 50 1 99 80 .75))
;;;
;;; which is asking for data given a room 100 meters square, listener sitting in the middle at the back,
;;; source almost on the right wall toward the back, reflections traced for .75 seconds
;;;
;;;    (92.96236 58.190605) 
;;;    (94.031906 57.154938) 
;;;    (94.66784 -58.828648) 
;;;    (95.71834 -57.804264) 
;;;    (128.69344 67.61986) 
;;;    (129.46814 66.80141) 
;;;    (130.54501 -67.95408) 
;;;    (131.30879 -67.14517) 
;;;    (168.64757 152.0674) 
;;;    (169.59363 -151.47041) 
;;;    (170.41713 152.38239) 
;;;    (171.35344 -151.78983) 
;;;    (190.68823 141.3871) 
;;;    (191.9427 -140.92062) 
;;;    (192.25504 141.75908) 
;;;    (193.49936 -141.29395) 


(defvar speed-of-sound 344)		;in air in meters per second (under normal conditions)
(defvar width-of-head .15)		;meters -- close to width of my head, I believe
(defvar half-head .075)
(defvar reflection-coefficient .95)
;(defvar water-speed-of-sound 1460)	;meters/sec at 20 C (goes up to around 1500 at 27 C)

(defvar sound-mag 0)

(defun set-sound-mag ()			;conversion factor for meters/sec to samples/meter
  (setf sound-mag (/ *srate* speed-of-sound)))

(defun in-samples (x)			;x = distance in meters, converts to samples
  (* x sound-mag))

(defun attenuation (distS distVS refl)	;distS=dist of source, distVS=dist of virtual source, refl=number of reflections
  (* (/ distS distVS) (expt reflection-coefficient refl)))

(defun distance (x y)			;pythagorean theorem
  (sqrt (+ (* x x) (* y y))))

(defun describe-VS (x y refl distS)	;(x,y)=position relative to listener's nose
  (let ((left-ear-dist (distance (if (minusp x) (- (abs x) half-head) (+ x half-head)) y))
	(right-ear-dist (distance (if (minusp x) (+ (abs x) half-head) (- x half-head)) y)))
    (values (in-samples left-ear-dist)
	    (in-samples right-ear-dist)
	    (attenuation distS left-ear-dist refl)
	    (attenuation distS right-ear-dist refl)
	    (if (minusp x)
		(if (minusp y)
		    "LB" "LF")          ;left back, left front
	      (if (minusp y)
		  "RB" "RF"))
	    x y)))

(defun max-room (time W L)		;given room size and time of last desired echo, get max of virtual rooms
  (let* ((radius (* time speed-of-sound))
	 (maxW (floor radius W))
	 (maxL (floor radius L)))
    (values maxW maxL radius)))

(defun get-all-VS-blocks (rm Nw Nl)	;march through all the virtual rooms
  (loop for i from 0 to Nw do
    (loop for j from 0 to Nl do
      (get-one-VS-block rm i j))))

(defun get-one-VS-block (rm w l)
  (get-VS rm w l)
  (if (/= 0 w) (get-VS rm (- w) l))
  (if (/= 0 l) (get-VS rm w (- l)))
  (if (and (/= 0 w) (/= 0 l)) (get-VS rm (- w) (- l))))

(defstruct rmloc W L uW uL rdiff ldiff fdiff bdiff data distS sxdiff sydiff minR)

(defun get-VS (rm w l)			;add info for this VS to the data array in rm
  (flet ((get-x-s (rm w)
	   (if (zerop w) 0
	     (if (oddp w)
		 (if (minusp w)
		     (- (+ (* 2 (rmloc-ldiff rm)) (* (rmloc-W rm) (1- (abs w)))))
		   (+ (* 2 (rmloc-rdiff rm)) (* (rmloc-W rm) (1- w))))
	       (* w (rmloc-W rm)))))
	 (get-y-s (rm l)
	   (if (zerop l) 0
	     (if (oddp l)
		 (if (minusp l)
		     (- (+ (* 2 (rmloc-bdiff rm)) (* (rmloc-L rm) (1- (abs l)))))
		   (+ (* 2 (rmloc-fdiff rm)) (* (rmloc-L rm) (1- l))))
	       (* l (rmloc-L rm))))))
    (flet ((get-xx (rm w) (+ (get-x-s rm w) (rmloc-sxdiff rm)))
	   (get-yy (rm l) (+ (get-y-s rm l) (rmloc-sydiff rm)))
	   (get-refl (rm w l) (declare (ignore rm)) (+ (abs w) (abs l)))
	   (add-VS (rm l r vl vr quad x y)
	     (if (>= (max vl vr) (rmloc-minR rm))
		 (push (list l r vl vr quad x y) (rmloc-data rm)))))
	  ;; so each element of the rmloc-data array is a list 
	  ;; left-delay right-delay left-amp right-amp string-desc (i.e. "LF"=left front etc) apparent-x apparent-y (from listener)
      (multiple-value-bind
	  (l r vl vr quad x y) 
	  (describe-VS (get-xx rm w) 
		       (get-yy rm l) 
		       (get-refl rm w l) 
		       (rmloc-distS rm))
	(add-VS rm l r vl vr quad x y)))))

;; 2*diff+W*(w-1) or W*w (ldiff rdiff fdiff bdiff)

(defun make-roomsig (W L uW uL sw sL time) ;width of room, length of room, 
					;user-X user-Y (from lower left corner), 
					;source-X source-Y, seconds of echos to compute

  (if (or (minusp W) (minusp L)) (error "impossible room dimensions")
    (if (or (> uW W) (> uL L)) (error "listener is outside the room")
      (if (or (minusp uW) (minusp uL)) (error "impossible listener position")
	(if (or (> sw W) (> sL L)) (error "source is outside room")
	  (if (or (minusp sw) (minusp sL)) (error "impossible source position")
	    (if (and (= sw uW) (= sL uL)) (error "source and listener cannot be at same location")))))))

  (set-sound-mag)
  (multiple-value-bind 
      (mW mL rad) (max-room time W L)
    (let ((rm (make-rmloc :W W
			  :L L
			  :uW uW
			  :uL uL
			  :rdiff (- W sw)    ;direct distance from source to right wall
			  :ldiff sw          ;left
			  :fdiff (- L sL)    ;front
			  :bdiff sL          ;back
			  :distS (distance (- sw uW) (- sL uL)) ;direct distance to listener
			  :sxdiff (- sw uW)  ;x/y direct to listener
			  :sydiff (- sL uL)
			  :data nil)))
      (setf (rmloc-minR rm) (min 0.95 (/ (rmloc-distS rm) rad)))
      (get-all-VS-blocks rm mW mL)
      (setf (rmloc-data rm) (nreverse (rmloc-data rm)))
      rm)))

(defun roomsig-locate-data (rm)
  ;; take data returned by make-roomsig, and massage it into a locsig-like form:
  ;; a list ordered by apparent distance giving (dist degree)
  ;; (roomsig-locate-data (make-roomsig 100 100 50 1 99 80 .75)) for example
  (sort (loop for ping in (rmloc-data rm) 
	 collect (let ((x (sixth ping))
		       (y (seventh ping)))
		   (list (distance x y) (double (* (/ 180.0 pi) (atan y x))))))
	#'< :key #'first))

#|
;;; arbitrary shapes are slightly harder.  The following code provides the support for handling them (in 2 dimensions)

(defun intersection-of-line-with-perpendicular-from-point (x0 y0 x1 y1 sx sy)
  ;; find the point of intersection with the line between (x0 y0) and (x1 y1) and a perpendicular to it dropped from (sx sy)
  ;; this gives the first part of the calculation of the virtual source (i.e. reflection) given any current source (sx sy) and any line
  ;; the line equation is y = (d-b)/(c-a) x + (bc-ad)/(c-a) 
  ;; the perpendicular to it that passes through (e f) is y = f + (c-a)/(d-b) e - (c-a)/(d-b) x
  ;; we can solve these two equations for x, then plug in that value to get y (to get the point of intersection)
  ;; x = (f + (c-a)/(d-b) e - (bc-ad)/(c-a)) / ((d-b)/(c-a) + (c-a)/(d-b))
  ;; all this algebra is just an explicit solution of the vector equations given in Borish's article.
  (if (= x0 x1)
      (values x0 sy)
    (if (= y0 y1)
	(values sx y0)
      (let* ((c-a (- x1 x0))
	     (d-b (- y1 y0))
	     (s1 (/ c-a d-b))
	     (s2 (/ d-b c-a))
	     (bc (* y0 x1))
	     (ad (* x0 y1))
	     (offset (/ (- bc ad) c-a))
	     (inX (/ (+ (* s1 sx) sy (- offset)) (+ s1 s2)))
	     (inY (+ (* inX s2) offset)))
	(values inX inY)))))

(defun reflection-of-point-through-line (x0 y0 x1 y1 sx sy)
  (multiple-value-bind
      (inX inY) (intersection-of-line-with-perpendicular-from-point x0 y0 x1 y1 sx sy)
    (values (- (* 2 inX) sx)
	    (- (* 2 inY) sy))))

;;; this function can also be used to get the new virtual room vertices -- just reflect each original vertex through the current side.
|#
