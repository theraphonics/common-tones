;;; ASDF system definition file for CLM

(asdf:defsystem "clm"
    :depends-on ("cffi")
    :description "common-tones synthesis library (forked from CLM-V)"
    :version "1.0"
    :author "Josh Armenta"
    :licence "BSD or whatever"
    :serial t
    :components
    ((:file "ffi")
     (:file "mus")
     (:file "run")
     (:file "sound")
     (:file "defins")
     (:file "env")
     (:file "export")
     (:file "clm1")
     (:file "temp-init")))
