
#+cltl2 (in-package :common-music)
#-cltl2 (in-package "COMMON-MUSIC")

/*!< rather than typing

/*!< type clm-scorefile

;; all the time, you can
/*!< (setf *default-score-file-class* 'clm-scorefile)



(defscorefile (pathname "pluck.clm" type clm-scorefile after '(load :channels 2))
  (with-part pluck  (
		     events 12
		     time 0
		     amp .1
		     weighting .3
		     lossfact .96
		     decaytime 0
		     attacktime 0
		     loudness 0
		     duration 2
		     )
    (setf freq (item (pitches f4 a c5 f c4 e g c4 g5 b d4 f)))
    (setf rhythm (item (rhythms e e q s)))
    ))


/*!< start up CM (/dist/lisp/cm/cm)

/*!< load the part definitions (:ld /dist/lisp/cm/clm/clm-parts)

/*!< and :ld /dist/lisp/cm/clm/examples/ex2



/*!< To get reverb, use an "after" option something like:

/*!< after '(load :reverb nrev :decay-time 3.0)

/*!< and so on.


#|
/*!< here is an example init.lisp:


(setf *default-scorefile-pathname* "/user/bil/cl/test.clm")

(setf *default-scorefile-class* 'clm-scorefile)

(setf *default-clm-load-options* '(:reverb jc-reverb))

/*!< here's an example of how dbs and I got lists passed:


(defscorefile (pathname "test" type CLM-SCOREFILE)
  (with-part reson (start-time 0
		    duration 1.0
		    pitch 440
		    amp .1
		    numformants 2
		    indxfun ''(0 0 100 1)
		    skewFun ''(0 0 100 1)
		    pcSkew .1
		    skewat .1
		    skewdc .1
		    vibfreq 5
		    vibpc .01
		    ranvibfreq 5
		    ranvibpc .01
		    degree 0
		    distance 1.0
		    reverb-amount 0.01
		    data '('((0 0 50 1 100 0) 1200 .5 .1 .1 0 1.0 .1 .1)
			   '((0 0 50 1 100 0) 2400 .5 .1 .1 0 1.0 .1 .1)))
    (setf pitch (item (pitches a4 as b) :kill t))
    (setf rhythm (item (rhythms q w e s)))))

/*!< the CM manual says the slot values are evaluated on every note, so to get a quoted

/*!< list into the note list (as an envelope for the clm instrument), we quote it twice.


|#