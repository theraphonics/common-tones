(in-package :common-tones)
;;; -------- SAMPLE->FILE --------


(defclass sample->file ()
  ((loc :initform 0 :initarg :start :accessor s2f-loc)
   (chns :initform 1 :initarg :chans :accessor s2f-chns)
   (file :initform nil :initarg :file :accessor s2f-file)
   (frm :initform nil :initarg :format :accessor s2f-frm)
   (typ :initform nil :initarg :type :accessor s2f-typ)
   (com :initform nil :initarg :comment :accessor s2f-com)
   (safety :initform 0 :initarg :safety :accessor s2f-safety)))

(defmethod print-object ((d sample->file) stream)
  (format stream "#<sample->file: file: ~A, chans: ~A, loc: ~A>" (s2f-file d) (s2f-chns d) (s2f-loc d)))

(defun make-sample->file (name &optional (chans 1) (format *clm-data-format*) (type *clm-header-type*) (comment nil))
  (make-instance 'sample->file
		 :file name
		 :chans chans
		 :format format
		 :type type
		 :safety 0
		 :comment comment))

(defmethod sample->file? ((g sample->file)) t)
(defmethod sample->file? ((g t)) nil)

(defmethod mus-output? ((obj sample->file)) t)
(defmethod mus-file-name ((gen sample->file)) (s2f-file gen))
(defmethod mus-channels ((gen sample->file)) (s2f-chns gen))
(defmethod mus-location ((gen sample->file)) (s2f-loc gen))

(defmethod mus-channel ((gen sample->file)) 0)

(defun sample->file (obj samp chan val)
  (declare (ignore obj samp chan val))
  (warn "Lisp interpreted sample->file is a no-op"))

(defmethod mus-increment ((g sample->file)) 1)
(defmethod (setf mus-increment) (val (g sample->file)) val)


(defmethod mus-safety ((gen sample->file))
  (s2f-safety gen))

(defmethod (setf mus-safety) (val (gen sample->file))
  (setf (s2f-safety gen) val)
  val)