/*!< clm3.lisp

/*!<

/*!< backwards compatibility stuff for clm4


(in-package :common-tones)
(export '(
	  #+linux mus-audio-set-oss-buffers
	  sound-format-name
	  sound-type-name
	  mus-set-raw-header-defaults
	  describe-audio
	  sl-dac
	  mus-formant-radius
	  ))

/*!< these are from clm1 or clm2


(defun sound-format-name (a) (mus-data-format-name a))
(defun sound-type-name (a) (clm-initialize-links) (mus-header-type-name a))

(defun mus-set-raw-header-defaults (a b c) (clm-initialize-links) (mus-header-set-raw-defaults a b c))
(defun describe-audio () nil)
(defun sl-dac (name &optional (dev 0)) (sl-dac-1 name dev))
(defun format->bytes (a) (mus-bytes-per-sample a))

(defun sound-loop-info (name vals)
  (let ((info (mus-sound-loop-info (fullname name))))
    (loop for i from 0 to 7 do
      (setf (aref vals i) (aref info i)))))

(defun initialize-sndlib () (mus-sound-initialize))
(defun mus-data-format-to-bytes-per-sample (a) (mus-bytes-per-sample a))
#+linux (defun set-oss-buffers (a b) (mus-oss-set-buffers a b))
#+linux (defun mus-audio-set-oss-buffers (a b) (mus-oss-set-buffers a b))

(defun mus-formant-radius (gen) (mus-scaler gen))
(defun set-mus-formant-radius (gen val) (setf (mus-scaler gen) val))
(defsetf mus-formant-radius set-mus-formant-radius)