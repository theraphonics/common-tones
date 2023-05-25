;;; Moog style four pole lowpass filter clm unit generator
;;;   low pass, 24db/Oct, variable resonance, warm, analog sound ;-)
;;;   [all this digital wizardry and we're back where we started!]
;;;
;;; original C instrument by Tim Stilson
;;; translation into clm and tuning by 
;;;   Fernando Lopez-Lezcano, nando@ccrma.stanford.edu
;;;   http://ccrma.stanford.edu/~nando/clm/moog
;;; Michael Edwards added the do-saturate argument, 23-Nov-10

;;; bil says: if you get "setf: unknown accessor: MOOG-Q in (MOOG-Q F)"
;;;   or something like that from this code, trying compiling this file
;;;   before loading it.


(defmacro saturate(x)
  (let* ((xs (gensym)))
    `(let* ((,xs ,x))
       (if (> ,xs 0.95) 0.95
	 (if (< ,xs -0.95) -0.95
	   ,xs)))))

(defparameter moog-gaincoeffs
    '(0.999969 0.990082 0.980347 0.970764 0.961304 0.951996 0.94281 0.933777 0.924866 0.916077 
      0.90741 0.898865 0.890442 0.882141  0.873962 0.865906 0.857941 0.850067 0.842346 0.834686
      0.827148 0.819733 0.812378 0.805145 0.798004 0.790955 0.783997 0.77713 0.770355 0.763672
      0.75708  0.75058 0.744141 0.737793 0.731537 0.725342 0.719238 0.713196 0.707245 0.701355
      0.695557 0.689819 0.684174 0.678558 0.673035 0.667572 0.66217 0.65686 0.651581 0.646393
      0.641235 0.636169 0.631134 0.62619 0.621277 0.616425 0.611633 0.606903 0.602234 0.597626
      0.593048 0.588531 0.584045 0.579651 0.575287  0.570953 0.566681 0.562469 0.558289 0.554169
      0.550079 0.546051 0.542053 0.538116 0.53421 0.530334 0.52652 0.522736 0.518982 0.515289
      0.511627 0.507996  0.504425 0.500885 0.497375 0.493896 0.490448 0.487061 0.483704 0.480377
      0.477081 0.473816 0.470581 0.467377 0.464203 0.46109 0.457977 0.454926 0.451874 0.448883
      0.445892 0.442932 0.440033 0.437134 0.434265 0.431427 0.428619 0.425842 0.423096 0.42038
      0.417664 0.415009 0.412354 0.409729 0.407135 0.404572 0.402008 0.399506 0.397003 0.394501
      0.392059 0.389618 0.387207 0.384827 0.382477 0.380127 0.377808 0.375488 0.37323 0.370972
      0.368713 0.366516 0.364319 0.362122 0.359985 0.357849 0.355713 0.353607 0.351532 0.349457
      0.347412 0.345398 0.343384 0.34137 0.339417 0.337463 0.33551 0.333588 0.331665 0.329773
      0.327911 0.32605 0.324188 0.322357 0.320557 0.318756 0.316986 0.315216 0.313446 0.311707
      0.309998 0.308289 0.30658 0.304901 0.303223 0.301575 0.299927 0.298309 0.296692 0.295074
      0.293488 0.291931 0.290375 0.288818 0.287262 0.285736 0.284241 0.282715 0.28125 0.279755
      0.27829 0.276825 0.275391 0.273956 0.272552 0.271118 0.269745 0.268341 0.266968 0.265594
      0.264252 0.262909 0.261566 0.260223 0.258911 0.257599 0.256317 0.255035 0.25375))

(defparameter moog-gaintable
    (make-double-array (length moog-gaincoeffs) 
		       :initial-contents (mapcar #'double moog-gaincoeffs)))

(defparameter moog-rawdata 
    '((1.00 730.1)
      (2.00 1423.8)
      (3.00 2117.4)
      (4.00 2811.0)
      (4.99 3541.1)
      (5.99 4234.8)
      (6.99 5001.4)
      (7.99 5768.0)
      (8.99 6571.2)
      (9.99 7337.8)
      (10.99 8177.5)
      (11.99 9017.1)
      (12.99 10002.8)
      (14.01 11025.0)
      (15.01 12047.2)
      (16.00 13105.9)
      (17.00 14456.6)
      (18.00 15916.9)
      (19.00 17851.7)
      (19.50 19385.0)
      (19.85 21904.0)))

(defun create-moog-freqtable (&optional (data moog-rawdata))
  (loop for set in data 
      collect (/ (second set) 22050) 
      collect (if (< (first set) 10)
		  (- (/ (first set) 10) 1)
		(/ (- (first set) 10) 10))))

(defparameter moog-freqtable-list
    '(0 -1
      0.03311111 -0.9
      0.06457143 -0.8
      0.0960272 -0.7
      0.127483 -0.6
      0.1605941 -0.5
      0.1920544 -0.4
      0.22682086 -0.3
      0.2615873 -0.2
      0.29801363 -0.1
      0.33278003 -0.0
      0.37086168 0.1
      0.40893877 0.2
      0.4536417 0.3
      0.5 0.4
      0.5463583 0.5
      0.5943719 0.6
      0.6556281 0.7
      0.72185487 0.8
      0.8096009 0.9
      0.87913835 0.95
      0.9933787 1
      1 1))

(defvar freqsize 100)
(defvar moog-freqtable
  (make-double-array 100
		     :initial-contents (loop for i from 0 below 100 and j from 0.0 by .01 collect (envelope-interp j moog-freqtable-list))))

(def-clm-struct moog
  s
  (A 0.0)
  (frequency 440.0)
  (Q 0.9))

;;; Create a ug structure. 
;;;   freq: cutoff frequency in Hertz
;;;   Q: resonance, 0->no resonance, 1->oscilates at freq
;;;
;;; Note: the relation between freq and the actual cutoff is not exactly linear but
;;;       I prefered to translate Hz into the internal parameter rather than controlling
;;;       the cutoff frequency in terms of a number that goes between -1 and 1. 

(clm::def-optkey-fun make-moog-filter ((frequency 440)
				       (Q 0.9))
  (make-moog :frequency frequency
	     :Q Q
	     :s (make-array 4 :initial-contents '(0 0 0 0))
	     :A 0.0))

;;; Macro to do the filtering
#|
(defmacro moog-filter (m sig)
  ;; generate internal symbols so there's no potential name collision
  (let* ((st (gensym))
	 (cell (gensym))
	 (fc (gensym))
	 (ix (gensym))
	 (ixint (gensym))
	 (ixfrac (gensym))
	 (A (gensym))
	 (out (gensym)))
    ;; and now the real macro
    `(let* ((,st 0.0)
	    (,fc 0.0)
	    (,ix 0)
	    (,ixint 0.0)
	    (,ixfrac 0.0)
	    (,A (moog-A ,m))
	    (moog-x (* freqsize (/ (moog-frequency ,m) (* *srate* 0.5)))))
       (setf ,fc (array-interp moog-freqtable moog-x freqsize))
       (setf ,A (* 0.25 (- ,sig ,A)))
       (loop for ,cell from 0 below 4 do
	     (setf ,st (aref (moog-s ,m) ,cell)
		   ,A (saturate (+ ,A (* ,fc (- ,A ,st))))
		   (aref (moog-s ,m) ,cell) ,A
		   ,A (saturate (+ ,A ,st))))
       (let* ((,out ,A))
	 (setf ,ix (* ,fc 99.0)
	       ,ixint (floor ,ix)
	       ,ixfrac (- ,ix ,ixint)
	       ,A (* ,A (moog-Q ,m) 
		     (+ (* (- 1 ,ixfrac)(aref moog-gaintable (+ ,ixint 99)))
			(* ,ixfrac (aref moog-gaintable (+ ,ixint 100)))))
	       (moog-A ,m) ,A)
	 ,out))))
|#

(defmacro moog-filter (m sig &optional (do-saturate t))
  ;; generate internal symbols so there's no potential name collision
  (let* ((st (gensym))
         (cell (gensym))
         (fc (gensym))
         (ix (gensym))
         (ixint (gensym))
         (ixfrac (gensym))
         (A (gensym))
         ;; ME 22.11.10
         (Atmp (gensym))
         (out (gensym)))
    ;; and now the real macro
    `(let* ((,st 0.0)
            (,fc 0.0)
            (,ix 0)
            (,ixint 0.0)
            (,ixfrac 0.0)
            (,Atmp 0.0)
            (,A (moog-A ,m))
            (moog-x (* freqsize (/ (moog-frequency ,m) (* *srate* 0.5)))))
       (setf ,fc (array-interp moog-freqtable moog-x freqsize))
       (setf ,A (* 0.25 (- ,sig ,A)))
       (loop for ,cell from 0 below 4 do
             (setf ,st (aref (moog-s ,m) ,cell)
                   ,Atmp (+ ,A (* ,fc (- ,A ,st)))
                   ;; ME 22.11.10
                   ,A (if ,do-saturate (saturate ,Atmp) ,Atmp)
                   (aref (moog-s ,m) ,cell) ,A
                   ;; ME 22.11.10
                   ,Atmp (+ ,A ,st)
                   ,A (if ,do-saturate (saturate ,Atmp) ,Atmp)))
       (let* ((,out ,A))
         (setf ,ix (* ,fc 99.0)
               ,ixint (floor ,ix)
               ,ixfrac (- ,ix ,ixint)
               ,A (* ,A (moog-Q ,m) 
                     (+ (* (- 1 ,ixfrac)(aref moog-gaintable (+ ,ixint 99)))
                        (* ,ixfrac (aref moog-gaintable (+ ,ixint 100)))))
               (moog-A ,m) ,A)
         ,out))))
 