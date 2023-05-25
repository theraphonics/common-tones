/*!< -------- FILE->FRAMPLE --------


(defclass file->frample ()
  ((fil :initform nil :initarg :fil :accessor f2f-fil)
   (loc :initform 0 :initarg :loc :accessor f2f-loc)
   (chn :initform 0 :initarg :chn :accessor f2f-chn)
   (size :initform nil :initarg :size :accessor f2f-size)))

(defmethod print-object ((d file->frample) stream)
  (format stream "#<file->frample: fil: ~A>" (f2f-fil d)))

(def-optkey-fun make-file->frample (file (start 0) (channel 0) (size *clm-file-buffer-size*))
  (if file
      (make-instance 'file->frample :fil file :chn channel :loc start :size size)
    nil))

(defmethod file->frample? ((g file->frample)) t)
(defmethod file->frample? ((g t)) nil)
(defmethod mus-input? ((obj file->frample)) t)
(defmethod mus-file-name ((gen file->frample)) (f2f-fil gen))

(defmethod mus-channel ((gen file->frample)) (f2f-chn gen))
(defmethod mus-location ((gen file->frample)) (f2f-loc gen))
(defmethod mus-length ((gen file->frample)) (sound-framples (f2s-fil gen)))
(defmethod mus-channels ((gen file->frample)) (mus-sound-chans (mus-file-name gen)))

(defun file->frample (obj samp frm)
  (declare (ignore obj samp frm))
  (warn "file->frample is a no-op outside 'run'"))

(defmethod mus-increment ((g file->frample)) 1)
(defmethod (setf mus-increment) (val (g file->frample)) val)