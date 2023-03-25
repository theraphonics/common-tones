;;; ASDF system definition file for CLM
;;; For information on ASDF see: http://www.cliki.net/asdf
;;;
;;; To load CLM from a non-standard install location:
;;;
;;; (require :asdf)
;;; (push "/path/to/clm-3/" asdf:*central-registry*)
;;; (asdf:operate 'asdf:load-source-op :clm)
;;;
;;; To download/install/load CLM from its archive:
;;;
;;; (require :asdf)
;;; (progn (push "/path/to/asdf-install/" asdf:*central-registry*)
;;;        (asdf:operate 'asdf:load-op 'asdf-install))
;;; (asdf-install:install 'clm)
;;; (asdf:operate 'asdf:load-source-op 'clm)

;; from Rick Taube in 2006:
; (asdf:defsystem "clm"
;   :description "Common Lisp Music"
;   :version "3"
;   :author "William Schottstaedt <bil (at) ccrma (dot) stanford (dot) edu>"
;   :licence "BSD or whatever"
;   :components ((:file "all" )))

(in-package :asdf-user)

;; from Tito Latini 14-Dec-16:
(asdf:defsystem "clm"
   :description "Common Lisp Music"
   :version "5"
   :author "William Schottstaedt <bil (at) ccrma (dot) stanford (dot) edu>"
   :licence "BSD or whatever"
   :perform (compile-op (o c)
	      (load (system-relative-pathname "clm" "all.lisp"))))


