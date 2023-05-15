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
