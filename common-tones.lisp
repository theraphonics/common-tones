(defpackage :common-tones
	(:use :cl-user :cffi :bordeaux-threads)
	(:export
		#:hz->radians #:radians->hz #:in-hz #:times->samples #:seconds->samples #:samples->seconds #:degrees->radians #:radians->degrees #:two-pi #:db->linear #:linear->db
	  #:oscil #:make-oscil #:oscil?
	  #:make-table-lookup #:table-lookup #:table-lookup? #:array-interp #:partials->wave #:phase-partials->wave #:normalize-partials
	  #:ring-modulate #:amplitude-modulate #:dot-product #:clear-array #:polynomial #:contrast-enhancement #:mus-interpolate
	  #:make-delay #:delay #:tap #:delay? #:delay-tick
	  #:make-comb #:comb #:comb? #:make-filtered-comb #:filtered-comb #:filtered-comb? #:notch #:make-notch #:notch?
	  #:make-all-pass #:all-pass #:all-pass? #:make-moving-average #:moving-average #:moving-average?
	  #:make-filter #:filter #:filter? #:make-fir-filter #:fir-filter #:fir-filter? #:make-iir-filter #:iir-filter #:iir-filter?
	  #:one-pole #:one-pole? #:one-zero #:one-zero? #:two-pole #:two-pole? #:two-zero #:two-zero?
	  #:make-one-pole #:make-one-zero #:make-two-pole #:make-two-zero
	  #:make-formant #:formant #:formant? #:formant-bank
	  #:make-firmant #:firmant #:firmant?
	  #:make-rand #:rand #:rand? #:make-rand-interp #:rand-interp #:rand-interp? #:clm-random #:mus-set-rand-seed #:centered-random #:mus-random
	  #:inverse-integrate
	  #:env #:make-env #:env? #:restart-env #:env-interp
	  #:make-triangle-wave #:triangle-wave #:triangle-wave? #:make-square-wave #:square-wave #:square-wave?
	  #:make-sawtooth-wave #:sawtooth-wave #:sawtooth-wave? #:make-pulse-train #:pulse-train #:pulse-train?
	  #:ncos #:ncos? #:make-ncos
	  #:nsin #:nsin? #:make-nsin
	  #:nrxycos #:nrxycos? #:make-nrxycos
	  #:nrxysin #:nrxysin? #:make-nrxysin
	  #:ssb-am #:ssb-am? #:make-ssb-am
	  #:asymmetric-fm #:asymmetric-fm? #:make-asymmetric-fm
	  #:locsig #:make-locsig #:locsig? #:locsig-ref #:locsig-set! #:locsig-reverb-ref #:locsig-reverb-set! #:locsig-type #:move-locsig
	  #:move-sound #:make-move-sound #:move-sound?
	  #:make-wave-train #:wave-train #:wave-train?
	  #:convolve #:make-convolve #:convolve? #:convolve-files
	  #:make-granulate #:granulate #:granulate?
	  #:make-polyshape #:polyshape #:polyshape? #:make-polywave #:polywave #:polywave? #:partials->polynomial
	  #:make-src #:src #:src?

	  #:make-frame #:frame? #:make-empty-frame #:frame+ #:frame* #:sample->frame #:frame->sample
	  #:mixer? #:frame->frame #:frame->list #:make-mixer #:make-empty-mixer #:make-identity-mixer #:mixer+ #:mixer* #:mixer-ref #:mixer-set! #:frame-ref #:frame-set!
	  #:make-scalar-mixer #:mixer-scale #:frame->file #:make-frame->file #:frame->file?
	  #:file->frame #:make-file->frame #:file->frame?

	  #:sample->file #:make-sample->file #:sample->file?
	  #:file->sample #:make-file->sample #:file->sample?
	  #:frample->frample #:file->frample #:make-file->frample #:file->frample?
	  #:frample->file #:frample->file? #:make-frample->file
	  #:file->array #:array->file

	  #:mus-output? #:mus-input?
	  #:readin #:make-readin #:readin?
	  #:outa #:outb #:outc #:outd #:ina #:inb #:out-any #:in-any
	  #:sine-bank #:phase-vocoder #:make-phase-vocoder #:phase-vocoder? #:phase-vocoder-amps
	  #:phase-vocoder-amp-increments #:phase-vocoder-freqs #:phase-vocoder-phases #:phase-vocoder-phase-increments
	  #:run-with-fm-and-pm

	  #:with-sound #:clm-load #:dac #:close-input #:definstrument #:open-input #:open-input*
	  #:stop-dac #:play #:stop-playing
	  #:def-optkey-fun

	  #:with-threaded-sound
	  #:*clm-threads*
	  #:clm-set-output-safety #:clm-set-reverb-safety

	  #:describe-instrument
	  #:double #:make-double-float-array #:make-double-array #:make-integer-array
	  #:fft #:make-fft-window #:rectangular->polar #:spectrum #:convolution #:polar->rectangular #:rectangular->magnitudes
	  #:correlate #:autocorrelate

	  #:mus-data #:mus-length #:mus-frequency #:mus-phase #:mus-scaler #:mus-ramp #:mus-channels
	  #:mus-a0 #:mus-a1 #:mus-a2 #:mus-b1 #:mus-b2 #:mus-interp-type #:mus-describe
	  #:mus-location #:mus-increment #:mus-order #:mus-channel #:mus-name #:mus-file-name
	  #:mus-xcoeff #:mus-xcoeffs #:mus-ycoeff #:mus-ycoeffs #:mus-feedback #:mus-feedforward
	  #:mus-hop #:clm-print #:mus-run #:mus-apply #:mus-close
	  #:mus-offset #:mus-width #:mus-reset #:mus-safety

	  #:sound-duration #:sound-chans #:sound-comment #:sound-data-format #:sound-data-location #:sound-datum-size #:sound-maxamp
	  #:sound-length #:sound-samples #:sound-framples #:sound-srate #:sound-header-type #:sound-loop-info

	  #:*definstrument-hook*

	  #:*clm*
	  #:*clm-version*
	  #:*clm-news*
	  #:*clm-date*
	  #:*clm-revision*
	  #:*clm-linked*
	  #:*clm-srate* #:*srate*
	  #:*clm-file-buffer-size*
	  #:*clm-header-type*
	  #:*clm-play*
	  #:*clm-data-format*
	  #:*clm-channels*
	  #:*clm-file-name* #:*output* #:*reverb*
	  #:*clm-compiler-name*
	  #:*clm-verbose*
	  #:*clm-player*
	  #:*clm-table-size*
	  #:*clm-safety* #:*safety*
	  #:*clm-debug* #:*debug*
	  #:*clm-array-print-length*
	  #:*clm-init*
	  #:*clm-source-directory*
	  #:*clm-binary-directory*
	  #:*clm-ins-directory*
	  #:*clm-ins*
	  #:*clm-search-list*
	  #:*clm-notehook*
	  #:*clm-instruments*
	  #:*clm-mix-options* #:*clm-mix-calls*
	  #:*clm-clipped*
	  #:*clm-src-width*
	  #:*clm-delete-reverb*
	  #:*clm-locsig-type*
	  #:*clm-default-frequency*

	  #:mus-linear #:mus-sinusoidal #:; #:backwards #:compatibility
	  #:mus-chebyshev-first-kind #:mus-chebyshev-second-kind

	  #:rectangular-window #:hanning-window #:hann-window #:welch-window #:parzen-window #:bartlett-window #:gaussian-window
	  #:hamming-window #:blackman2-window #:blackman3-window #:blackman4-window #:blackman5-window
	  #:blackman6-window #:blackman7-window #:blackman8-window #:blackman9-window #:blackman10-window
	  #:exponential-window #:kaiser-window #:cauchy-window #:poisson-window #:riemann-window #:rv2-window #:rv3-window #:rv4-window
	  #:tukey-window #:dolph-chebyshev-window #:hann-poisson-window #:connes-window #:samaraki-window #:ultraspherical-window
	  #:bartlett-hann-window #:bohman-window #:flat-top-window

	  #:with-offset #:scaled-to #:scaled-by #:with-current-sound
	  #:mix #:run #:run* #:sound-let #:with-mix

	  #:print-hash #:without-warnings

	  #:envelope-length #:envelope-reverse #:envelope-concatenate #:envelope+ #:envelope* #:envelope-max #:envelope-repeat #:envelope-exp
	  #:envelope-funcall #:envelope-apply #:envelope-map #:map-across-envelopes
	  #:add-or-edit-breakpoint #:remove-breakpoint #:envelope-simplify #:fft-envelope-simplify
	  #:reduce-amplitude-quantization-noise #:meld-envelopes
	  #:exp-envelope #:make-power-env #:power-env
	  #:exp-envelope #:dB-envelope #:make-dB-env #:semitones-envelope #:make-semitones-env #:octaves-envelope #:make-octaves-env
	  #:window-envelope #:stretch-envelope #:x-norm
	  #:max-envelope #:scale-envelope #:normalize-envelope #:min-envelope
	  #:envelope-interp
	  #:envelope->coeffs
	  #:clm-last-begin-time #:clm-last-end-time

	  #:sound-files-in-directory
	  #:def-clm-fun #:def-clm-struct #:def-clm-float-struct #:clm-datai #:clm-datar

	  #:full-merge-pathnames #:search-full-merge-pathnames #:clm-cerror #:clm-cleanup

	  #:set-instrument-properties #:ins-var

	  #:clm-reset
	  #:*open-input-verbose* #:*open-input-truename* #:*open-input-pathname*
	  #:*open-input-explicit-output* #:*open-input-explicit-reverb*

	  #:init-with-sound #:finish-with-sound
	  #:clm-initialize-links

	  #:mus-unsupported
	  #:mus-next #:mus-aifc #:mus-riff #:mus-bicsf #:mus-nist #:mus-inrs #:mus-esps #:mus-svx #:mus-voc #:mus-sndt #:mus-raw
	  #:mus-smp #:mus-sd2 #:mus-avr #:mus-ircam #:mus-sd1 #:mus-sppack #:mus-mus10 #:mus-hcom #:mus-psion #:mus-maud
	  #:mus-ieee #:mus-matlab #:mus-adc #:mus-sound-edit #:mus-sound-edit-16
	  #:mus-dvsm #:mus-midi #:mus-esignal #:mus-soundfont #:mus-gravis #:mus-comdisco #:mus-goldwave #:mus-srfs
	  #:mus-midi-sample-dump #:mus-diamondware #:mus-realaudio #:mus-adf #:mus-sbstudioii #:mus-delusion
	  #:mus-farandole #:mus-sample-dump #:mus-ultratracker #:mus-yamaha-sy85 #:mus-yamaha-tx16 #:mus-digiplayer
	  #:mus-covox #:mus-avi #:mus-omf #:mus-quicktime #:mus-asf #:mus-yamaha-sy99 #:mus-kurzweil-2000
	  #:mus-aiff #:mus-paf #:mus-csl #:mus-file-samp #:mus-pvf

	  #:mus-header-type-ok

	  #:mus-unknown #:mus-bshort #:mus-mulaw #:mus-byte #:mus-bfloat #:mus-bint #:mus-alaw #:mus-ubyte #:mus-b24int
	  #:mus-bdouble #:mus-lshort #:mus-lint #:mus-lfloat #:mus-ldouble #:mus-ubshort #:mus-ulshort #:mus-l24int
	  #:mus-bintn #:mus-lintn

	  #:mus-data-format-ok
	  #:mus-audio-default
	  #:mus-error-type->string
	  #:mus-sound-samples
	  #:mus-sound-framples
	  #:mus-sound-datum-size
	  #:mus-sound-data-location
	  #:mus-sound-chans
	  #:mus-sound-srate
	  #:mus-sound-header-type
	  #:mus-sound-data-format
	  #:mus-sound-original-format
	  #:mus-sound-comment-start
	  #:mus-sound-comment-end
	  #:mus-sound-length
	  #:mus-sound-write-date
	  #:mus-sound-type-specifier
	  #:mus-sound-bits-per-sample
	  #:mus-header-type-name
	  #:mus-data-format-name
	  #:mus-sound-comment
	  #:mus-data-format-to-bytes-per-sample
	  #:mus-sound-duration
	  #:mus-sound-initialize
	  #:mus-sound-override-header
	  #:mus-sound-forget
	  #:mus-sound-print-cache
	  #:mus-sound-loop-info
	  #:mus-sound-maxamp
	  #:mus-audio-initialize
	  #:mus-file-probe
	  #:mus-header-samples
	  #:mus-header-data-location
	  #:mus-header-chans
	  #:mus-header-srate
	  #:mus-header-type
	  #:mus-header-format
	  #:mus-header-comment-start
	  #:mus-header-comment-end
	  #:mus-header-type-specifier
	  #:mus-header-bits-per-sample
	  #:mus-header-loop-mode
	  #:mus-header-loop-start
	  #:mus-header-loop-end
	  #:mus-header-mark-position
	  #:mus-header-base-note
	  #:mus-header-base-detune
	  #:mus-header-set-raw-defaults
	  #:mus-header-true-length
	  #:mus-header-original-format
	  #:mus-bytes-per-sample
	  #:mus-samples-to-bytes
	  #:mus-bytes-to-samples
	  #:mus-header-read
	  #:mus-header-write
	  #:mus-header-aux-comment-start
	  #:mus-header-aux-comment-end
	  #:mus-header-update-comment
	  #:mus-header-initialize
	  #:mus-header-writable
	  #:mus-header-sf2-entries
	  #:mus-header-sf2-name
	  #:mus-header-sf2-start
	  #:mus-header-sf2-end
	  #:mus-header-sf2-loop-start
	  #:mus-header-sf2-loop-end
	  #:mus-header-original-format-name

	  #:mus-clipping
	  #:mus-set-clipping
	  #:mus-file-buffer-size
	  #:mus-set-file-buffer-size
	  #:mus-srate
	  #:mus-set-srate
	  #:initialize-cmus
	  #:snd-memo

	  #:mus-interp-none #:mus-interp-linear #:mus-interp-sinusoidal #:mus-interp-all-pass #:mus-interp-lagrange #:mus-interp-bezier #:mus-interp-hermite
		))