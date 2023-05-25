(in-package :common-tones/generators)
;;; -------- FILE->SAMPLE --------


(defclass file->sample ()
  ((fil :initform nil :initarg :fil :accessor f2s-fil)
   ;; here and in file->frample the loc/chn fields and start/channel args are purely for
   ;;   backwards compatibility -- they provide a path between open-input (which calls
   ;;   make-file->sample), and readin (which may get its start/channel info from
   ;;   open-input in CL/CLM -- this is an obsolete way to do things, but many
   ;;   instruments still use it.  (see simple-rd-start in ug2.ins)
   (loc :initform 0 :initarg :loc :accessor f2s-loc)
   (chn :initform 0 :initarg :chn :accessor f2s-chn)
   (size :initform nil :initarg :size :accessor f2s-size)))

(defmethod print-object ((d file->sample) stream)
  (format stream "#<file->sample: fil: ~A, chan: ~A, start: ~A>" (f2s-fil d) (f2s-chn d) (f2s-loc d)))

(def-optkey-fun make-file->sample (file (start 0) (channel 0) (size *clm-file-buffer-size*))
  (if file
      (make-instance 'file->sample :fil file :chn channel :loc (floor start) :size size)
    nil))

(defmethod file->sample? ((g file->sample)) t)
(defmethod file->sample? ((g t)) nil)
(defmethod mus-input? ((obj file->sample)) t)
(defmethod mus-input? ((obj t)) nil)
(defmethod mus-file-name ((gen file->sample)) (f2s-fil gen))

(defmethod mus-channel ((gen file->sample)) (f2s-chn gen))
(defmethod mus-location ((gen file->sample)) (f2s-loc gen))
(defmethod mus-length ((gen file->sample)) (sound-framples (f2s-fil gen)))
(defmethod mus-channels ((gen file->sample)) (mus-sound-chans (mus-file-name gen)))

(defun file->sample (obj samp &optional (chn 0))
  (declare (ignore obj samp chn))
  (warn "file->sample is a no-op outside 'run'"))

(defmethod mus-increment ((g file->sample)) 1)
(defmethod (setf mus-increment) (val (g file->sample)) val)
