(in-package :common-tones/generators)

;;; -------- FRAMPLE->FILE --------


(defclass frample->file ()
  ((loc :initform 0 :initarg :start :accessor f2f-loc)
   (chns :initform 1 :initarg :chans :accessor f2f-chns)
   (file :initform nil :initarg :file :accessor f2f-file)
   (frm :initform nil :initarg :format :accessor f2f-frm)
   (typ :initform nil :initarg :type :accessor f2f-typ)
   (com :initform nil :initarg :comment :accessor f2f-com)
   (safety :initform 0 :initarg :safety :accessor f2f-safety)))

(defmethod print-object ((d frample->file) stream)
  (format stream "#<frample->file: file: ~A, chans: ~A, loc: ~A>" (f2f-file d) (f2f-chns d) (f2f-loc d)))

(defun make-frample->file (name &optional (chans 1) (format *clm-data-format*) (type *clm-header-type*) (comment nil))
  (make-instance 'frample->file
		 :file name
		 :chans chans
		 :format format
		 :type type
		 :safety 0
		 :comment comment))

(defmethod frample->file? ((g frample->file)) t)
(defmethod frample->file? ((g t)) nil)

(defmethod mus-output? ((obj frample->file)) t)
(defmethod mus-file-name ((gen frample->file)) (f2f-file gen))
(defmethod mus-channels ((gen frample->file)) (f2f-chns gen))
(defmethod mus-location ((gen frample->file)) (f2f-loc gen))

(defmethod mus-channel ((gen frample->file)) 0)

(defun frample->file (obj samp val)
  (declare (ignore obj samp val))
  (warn "Lisp interpreted frample->file is a no-op"))

(defmethod mus-increment ((g frample->file)) 1)
(defmethod (setf mus-increment) (val (g frample->file)) val)

(defmethod mus-safety ((gen frample->file))
  (f2f-safety gen))

(defmethod (setf mus-safety) (val (gen frample->file))
  (setf (f2f-safety gen) val)
  val)