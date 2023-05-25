/*!< Oversampled state-variable filter

/*!< By David Lowenfels 2/2003

/*!< Implemented from Andy Simper's pseudocode at musicdsp.org

/*!< AKA Chamberlin filter after Hal Chamberlin's filter introduced

/*!< in "Musical Applications of Microprocessors"

/*!<

/*!< Cutoff: between 0 and Nyquist (fs/2)

/*!< Resonance and Overdrive: between 0 and 1

/*!<

/*!< This filter is oversampled to improve the accuracy of the cutoff at high frequencies.

/*!< 2x oversampling is enough to make it well behaved. The moving average filter used

/*!< in the downsampling gives a sinc frequency-response, which doesn't completely

/*!< remove imaging. In addition, the duplication of the input sample gives another sinc response,

/*!< which removes more imaging, but adds more high-frequency rolloff. In practice, this should

/*!< be acceptable in musical situations.

/*!<

/*!< NOTE: Take care to avoid clipping, as the resonance will increase the gain of your signal!

/*!<

/*!< Important methods:

/*!< (set-svf-resonance svf res)

/*!< (set-svf-cutoff svf freq)

/*!< (svf-bandpass svf sig)

/*!< (svf-highpass svf sig)

/*!< (svf-lowpass  svf sig)

/*!< (svf-notch svf sig)


(def-clm-struct svf
  (overdrive 0.0)
  (damp 0.0)
  (omega 0.0)
  (low 0.0)
  (high 0.0)
  (band 0.0)
  (_notch 0.0)
  (oversample 2)
)


(defmacro svf-calc-omega (freq oversample)
    `(* 2.0 (sin (* pi ,freq (/ (* ,oversample *srate*)))))
  )

(defmacro svf-calc-damp (res)
  `(* 2.0 (cos (* (expt ,res 0.1) pi 0.5)))
  )

/*!< Create a ug structure.


(clm::def-optkey-fun make-svf-filter ((frequency 440)
				      (resonance 0.8)
				      (overdrive 0.0)
				      (oversample 2)
				      )

  (make-svf :omega (svf-calc-omega frequency oversample)
	    :damp (svf-calc-damp resonance)
	    :oversample oversample
	    :overdrive overdrive
	    :_notch 0.0
	    :low 0.0
	    :high 0.0
	    :band 0.0
  )
)

/*!< Macro to do the filtering


(defmacro svf-filter (svf sig)
  ;; generate internal symbols so there's no potential name collision
  (let* (
	 (band (gensym))
	 (input (gensym))
	 (overdrive (gensym))
	 (damp (gensym))
	 (freq (gensym))
	 (qnorm (gensym))
	 )
    ;; and now the real macro
    `(let* ((,damp (svf-damp ,svf))
	    (,freq (svf-omega ,svf))
	    (,overdrive (/ (svf-overdrive ,svf) 10))
	    (,qnorm (sqrt (+ (/ ,damp 2) 0.01)))
	    (,input ,sig)
;;	    (,input (* ,sig ,qnorm))
	    ,band
	    )
	 ;;calculate a round of samples
	 (setf ,band (svf-band ,svf))
	 (setf (svf-_notch ,svf) (- ,input (* ,damp ,band)))
	 (setf (svf-low ,svf) (+ (svf-low ,svf) (* ,freq ,band )))
	 (setf (svf-high ,svf) (- (svf-_notch ,svf) (svf-low ,svf)))
	 (setf (svf-band ,svF) (+ (* ,freq (svf-high ,svf)) ,band (- (* ,overdrive ,band ,band ,band))))
     ))
  )

(defmacro set-svf-cutoff (s freq)
  `(setf (svf-omega ,s) (svf-calc-omega ,freq (svf-oversample ,s)))
)

(defmacro set-svf-resonance (s res)
  `(setf (svf-damp ,s) (svf-calc-damp ,res))
  )

(defmacro svf-bandpass (svf in)
  (let* (
	 (input (gensym))
	 (oversample (gensym))
	 (output (gensym))
	 )
    `(let* ((,input ,in)
	    (,oversample (svf-oversample ,svf))
	    (,output 0)
       )
       (loop repeat ,oversample do
	 (svf-filter ,svf ,input)
	 (setf ,output (+ ,output (svf-band ,svf))) ;lowpass/decimate the desired stage
;	 (setf ,input 0) ;for proper upsampling
	 )
       ,output
  )))

(defmacro svf-highpass (svf in)
  (let* (
	 (input (gensym))
	 (oversample (gensym))
	 (output (gensym))
	 )
    `(let* ((,input ,in)
	    (,oversample (svf-oversample ,svf))
	    (,output 0)
       )
       (loop repeat ,oversample do
	 (svf-filter ,svf ,input)
	 (setf ,output (+ ,output (svf-high ,svf))) ;lowpass/decimate the desired stage
;	 (setf ,input 0) ;for proper upsampling
	 )
       ,output
  )))

(defmacro svf-lowpass (svf in)
  (let* (
	 (input (gensym))
	 (oversample (gensym))
	 (output (gensym))
	 )
    `(let* ((,input ,in)
	    (,oversample (svf-oversample ,svf))
	    (,output 0)
       )
       (loop repeat ,oversample do
	 (svf-filter ,svf ,input)
	 (setf ,output (+ ,output (svf-low ,svf))) ;lowpass/decimate the desired stage
;	 (setf ,input 0) ;for proper upsampling
	 )
       ,output
  )))

(defmacro svf-notch (svf in)
  (let* (
	 (input (gensym))
	 (oversample (gensym))
	 (output (gensym))
	 )
    `(let* ((,input ,in)
	    (,oversample (svf-oversample ,svf))
	    (,output 0)
       )
       (loop repeat ,oversample do
	 (svf-filter ,svf ,input)
	 (setf ,output (+ ,output (svf-_notch ,svf))) ;lowpass/decimate the desired stage
;	 (setf ,input 0) ;for proper upsampling
	 )
       ,output
  )))