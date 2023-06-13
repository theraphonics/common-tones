;;; ASDF system definition file for common-tones
(in-package :asdf)

(defsystem "common-tones"
    :depends-on ("cffi" "bordeaux-threads")
    :description "common-tones synthesis library"
    :version "0.1"
    :author "Josh Armenta"
    :licence "BSD"
    :components
    ((:file "common-tones")
     (:file "defaults" :depends-on ("common-tones"))
     (:file "ffi" :depends-on ("common-tones" "generators"))
     (:file "mus" :depends-on ("common-tones"))
     (:file "run" :depends-on ("common-tones" "generators"))
     (:file "sound" :depends-on ("common-tones"))
     (:file "defins" :depends-on ("common-tones"))
     (:file "env" :depends-on ("common-tones"))
     (:file "clm1" :depends-on ("common-tones"))
     (:module "plugins" :depends-on ("common-tones")
       :components
         ((:file "plugins")
           (:file "add-and-cut" :depends-on ("plugins"))
           (:file "db-to-linear" :depends-on ("plugins"))
           (:file "dur-gliss" :depends-on ("plugins"))
           (:file "fft-mag-and-phase" :depends-on ("plugins"))
           (:file "gliss-dur" :depends-on ("plugins"))
           (:file "hey" :depends-on ("plugins"))
           (:file "load-ins" :depends-on ("plugins"))
           (:file "remote-play" :depends-on ("plugins"))
           (:file "rmix" :depends-on ("plugins"))
           (:file "sf-ssf" :depends-on ("plugins"))
           (:file "with-instruments" :depends-on ("plugins"))))
     (:module "generators" :depends-on ("common-tones" "defaults")
       :components
       ((:file "all-pass")
        (:file "comb-filter")
        (:file "convolve")
        (:file "delay")
        (:file "env")
        (:file "file-to-frample")
        (:file "file-to-sample")
        (:file "filter")
        (:file "filtered-comb")
        (:file "fir-filter")
        (:file "formant")
        (:file "frample-to-file")
        (:file "granulate")
        (:file "locsig")
        (:file "modulation")
        (:file "move-sound")
        (:file "moving-average")
        (:file "ncos")
        (:file "nrxycos")
        (:file "nrxysin")
        (:file "nsin")
        (:file "one-pole")
        (:file "one-zero")
        (:file "oscil")
        (:file "phase-vocoder")
        (:file "pulse-train")
        (:file "rand")
        (:file "readin")
        (:file "sample-to-file")
        (:file "sr-conversion")
        (:file "ssb-am")
        (:file "table-lookup")
        (:file "two-pole")
        (:file "two-zero")
        (:file "wave-train")
        (:file "waves")
        (:file "waveshaping")
        ))
     ))
