;;; -*- mode: Common-Lisp ; Syntax: ANSI-Common-Lisp ; coding: utf-8 -*-
;;; LOADING COMMON LISP MUSIC IN LISPWORKS
;;; Frank Zalkow, 2014
;;; All intellectual property rights to this code are relinquished and
;;; permission is given to anyone to use, duplicate, modify and distribute it.

; define a function COMPILE-AND-LOAD that doesn't exist in LispWorks by default
(defun compile-and-load (file &optional (verbose t))
  (let ((lisp-file 
         (if (pathname-type file) file (concatenate 'string (namestring file)
						    ".lisp"))))
    (let ((fasl-file (compile-file-pathname file)))
      (when (or 
             (not (probe-file fasl-file))
             (not (file-write-date lisp-file))
             (not (file-write-date fasl-file))
             (> (file-write-date lisp-file) (file-write-date fasl-file)))
        (compile-file file :verbose verbose))))
  (load file :verbose verbose))

(let ((directory-before (get-working-directory))
      (clm-directory (directory-namestring *load-truename*)))
  ; change to the directory of CLM
  (change-directory clm-directory)
  ; load "all.lisp"
  (load (make-pathname :directory (pathname-directory *load-truename*)
                       :name "all.lisp"))
  ; go back to working-directory
  (change-directory directory-before))
