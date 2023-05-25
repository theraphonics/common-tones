(in-package :common-tones/plugins)

;;; ----------------------------------------------------------------

;;; amp in dB (Sam Heisz)


#|

Here are some macros that facilitate specification of amplitude in
either dB or as a linear volume.

Linear Volume
=============

(vol-to-amp vol &key (max 1000))

vol is a linear specification for amplitude from 0 to `max'.  `max'
indicates the point at which loudness is not changed, a vol of 2*max
indicates a doubling of perceived loudness. This is the most intuitive
form of amplitude specification because it approximates how we
perceive loudness. I design my instruments to take a vol parameter
which is then changed to an amplitude parameter:

(definstrument piano (start dur freq vol)
  (let* (...
	 ((amp (vol-to-amp vol))))
    .
    .
    (Run
     .
     .
     (out i (* amp signal)))))

You can also use it with existing instruments:

;; use v.ins
(with-sound ()
  (fmviolin 0 2 440 (vol-to-amp 500)
  (fmviolin 2 2 440 (vol-to-amp 250))))


(* signal (vol-to-amp 2000))		; signal is perceived as twice as loud
(* signal (vol-to-amp 1000))		; signal is not changed
(* signal (vol-to-amp 500))		; signal is perceived as 1/2 as loud
(* signal (vol-to-amp 250))		; signal is perceived as 1/4 as loud
(* signal (vol-to-amp 0))		; signal is set to 0
(* signal (vol-to-amp .25 :max 1))	; signal is perceived as 1/4 as loud

dB
==

(db-to-amp db)
(amp-to-db db)

A dB is a relative value. A value of 10 represents a doubling of
perceived loudness.

(* signal  (db-to-amp 10))		; signal is perceived as twice as loud
(* signal  (db-to-amp 0))		; signal is not changed
(* signal  (db-to-amp -10))		; signal is perceived as 1/2 as loud
(* signal  (db-to-amp -20))		; signal is perceived as 1/4 as loud


Absolute dB
===========

(adb-to-amp db &key (max 96.3296))
(amp-to-adb amp &key (max 96.3296))

Sometimes dB is used as an absolute value. In such a case, an amplitude
of 1.0 can be matched to a certain dB value. For these functions the
default dB value corresponding to an amplitude of 1.0 is 96.3296.


(* signal (adb-to-amp 106))		; signal is perceived as twice as loud
(* signal (adb-to-amp 96))		; signal is not changed
(* signal (adb-to-amp 86))		; signal is perceived as 1/2 as loud
(* signal (adb-to-amp 76))		; signal is perceived as 1/4 as loud

|#

;; Sam Heisz (samh@digicron.com)
;;
;; dB = 20 log   (A1 / A2)                     db/20
;;            10                     A1/A2 = 10

(defmacro db-to-amp (db)
  `(expt 10 (/ ,db 20)))

(defmacro amp-to-db (amp)
  `(* 20 (log ,amp 10)))

(defmacro vol-to-amp (v &key (max 1000))
  (let ((vol (gensym)))
    `(let ((,vol ,v))
       (if (<= ,vol 0) 0 (db-to-amp (* -10 (log (/ ,max ,vol) 2)))))))

;; the 96.3296 figure for max is from track-rms.ins
(defmacro adb-to-amp (adb &key (max 96.3296))
  (let ((db (gensym)))
    `(let ((,db ,adb))
       (if (<= ,db 0.0) 0.0 (db-to-amp (- (abs ,db) ,max))))))

(defmacro amp-to-adb (amp &key (max 96.3296))
  `(if (<= ,amp .00001526) 0.0 (+ ,max (amp-to-db ,amp))))