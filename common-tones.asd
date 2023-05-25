;;; ASDF system definition file for common-tones
(in-package :asdf)

(defsystem "common-tones"
    :depends-on ("cffi" "bordeaux-threads")
    :description "common-tones synthesis library (forked from CLM-V)"
    :version "0.1"
    :author "Josh Armenta"
    :licence "BSD"
    :serial t
    :components
    ((:file "common-tones")
     (:file "constants")
     (:file "generics")
     (:file "initmus")
     (:file "all")
     (:file "ffi")
     (:file "mus")
     (:file "run")
     (:file "sound")
     (:file "defins")
     (:file "env")
     (:file "export")
     (:file "clm1")
     (:file "temp-init")))

(defsystem "common-tones/plugins"
  :depends-on ("common-tones")
  :pathname "t/" ;; specify the subdirectory
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

(defsystem "common-tones/generators"
  :depends-on ("common-tones")
  :pathname "t/" ;; specify the subdirectory
  :components
  ((:file "generators")
   (:file "all-pass" :depends-on ("generators"))
   (:file "comb-filter" :depends-on ("generators"))
   (:file "convolve" :depends-on ("generators"))
   (:file "delay" :depends-on ("generators"))
   (:file "env" :depends-on ("generators"))
   (:file "file-to-frample" :depends-on ("generators"))
   (:file "file-to-sample" :depends-on ("generators"))
   (:file "filter" :depends-on ("generators"))
   (:file "filtered-comb" :depends-on ("generators"))
   (:file "fir-filter" :depends-on ("generators"))
   (:file "formant" :depends-on ("generators"))
   (:file "frample-to-file" :depends-on ("generators"))
   (:file "granulate" :depends-on ("generators"))
   (:file "locsig" :depends-on ("generators"))
   (:file "modulation" :depends-on ("generators"))
   (:file "move-sound" :depends-on ("generators"))
   (:file "moving-average" :depends-on ("generators"))
   (:file "ncos" :depends-on ("generators"))
   (:file "nrxycos" :depends-on ("generators"))
   (:file "nrxysin" :depends-on ("generators"))
   (:file "nsin" :depends-on ("generators"))
   (:file "one-pole" :depends-on ("generators"))
   (:file "one-zero" :depends-on ("generators"))
   (:file "oscil" :depends-on ("generators"))
   (:file "phase-vocoder" :depends-on ("generators"))
   (:file "pulse-train" :depends-on ("generators"))
   (:file "rand" :depends-on ("generators"))
   (:file "readin" :depends-on ("generators"))
   (:file "sample-to-file" :depends-on ("generators"))
   (:file "sr-conversion" :depends-on ("generators"))
   (:file "ssb-am" :depends-on ("generators"))
   (:file "table-lookup" :depends-on ("generators"))
   (:file "two-pole" :depends-on ("generators"))
   (:file "two-zero" :depends-on ("generators"))
   (:file "wave-train" :depends-on ("generators"))
   (:file "waves" :depends-on ("generators"))
   (:file "waveshaping" :depends-on ("generators"))))
