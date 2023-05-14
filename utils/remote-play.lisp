;;;  ------------------------------------------------------------------------
;;; remote play

(defun remote-play (host play-program sound-name)
  (run-in-shell "rsh" (format nil "~A ~A ~A" host play-program sound-name)))

;;; (remote-play "cmi1" "sfplay" (with-sound (:play nil) (fm-violin 0 1 440 .1)))
