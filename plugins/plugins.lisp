(defpackage :common-tones/plugins
  (:use :cl)
  (:export
    #:add
    #:cut
    #:db-to-amp
    #:amp-to-db
    #:vol-to-amp
    #:adb-to-amp
    #:amp-to-adb
    #:rmix
    #:with-instruments
    #:load-ins))

(in-package :common-tones/plugins)
