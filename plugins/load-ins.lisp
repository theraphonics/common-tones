(in-package :common-tones/plugins)

;;;  ------------------------------------------------------------------------
;;; load all instruments in a given directory (Marco Trevisani)

(defun load-ins  (&key (path "/user/marco/aura/pronti/") (extension "o"))
  (let ((dir (directory path)))
    (loop for file in dir do
      (if (string= extension (pathname-type file))
        (load file)))))
