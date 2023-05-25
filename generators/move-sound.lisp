(in-package :common-tones/generators)
;;; -------- move-sound (dlocsig) --------


(defclass move-sound ()
  ((outf :initform nil :initarg :outf :accessor dlocs-outf)
   (revf :initform nil :initarg :revf :accessor dlocs-revf)
   (data :initform nil :initarg :data :accessor dlocs-data)))

(defmethod move-sound? ((g move-sound)) t)
(defmethod move-sound? ((g t)) nil)

(defun make-move-sound (dlocs-list &optional output revout)
  (make-instance 'move-sound
		 :data dlocs-list
		 :outf (or output *output*)
		 :revf (or revout *reverb*)))

(defun move-sound (l i in-sig)
  (declare (ignore l i))
  (warn "Lisp interpreted move-sound is a no-op")
  in-sig)

(defmethod print-object ((d move-sound) stream)
  (format stream "#<move-sound: data: ~A>"
	  (dlocs-data d)))