(in-package :common-tones)

(export '(clm-seek-bytes clm-seek-floats
          clm-read-floats clm-write-floats
          clm-read-ints clm-write-ints
          c-open-input-file c-open-output-file c-create-file c-close c-seek
          clm-swap-ints clm-swap-doubles
          ))

/*!< ---------------- lseek ----------------


(cffi:defcfun ("lseek" c-seek) :void
  (fd :int)
  (loc :int)
  (type :int))

/*!< ---------------- c-seek-bytes ----------------


(defun clm-seek-bytes (fd n)
  "Seek the file descriptor FD by N bytes from the start of the file."
  `(cffi:foreign-funcall "lseek" :void ,@fd ,@n 0))

/*!< ---------------- c-seek-floats ----------------


(defun clm-seek-floats (fd n)
  "Seek the file descriptor FD by N floats from the start of the file."
  `(cffi:foreign-funcall "lseek" :void ,@fd (* ,@n 8) 0)) ; SEEK_SET=0, 8=sizeof(double)

/*!< ---------------- clm-read-floats ----------------


(cffi:defcfun ("read" c-read-floats-1) :int
  (fd :int)
  (buf :pointer)
  (bytes :int))

(defun c-read-floats (fd arr n)
  "Read N floats from the file descriptor FD into the array ARR."
  `(cffi:with-foreign-pointer (ptr @,arr :double)
    (/ (cffi:foreign-funcall "read" :int @,fd ptr (* @,n (foreign-type-size :double))) 8)))

(defun clm-read-floats (fd arr n)
  "Read N floats from the file descriptor FD into the array ARR."
  (let ((result (c-read-floats fd arr n)))
    (values result arr)))

/*!< ---------------- clm-read-ints ----------------


(cffi:defcfun ("read" c-read-ints-1) :int
  (fd :int)
  (buf :pointer)
  (bytes :int))

(defun c-read-ints (fd arr n)
  "Read N ints from the file descriptor FD into the array ARR."
  `(cffi:with-foreign-pointer (ptr @,arr)
    (cffi:foreign-funcall "read" :int @,fd ptr (* @,n (foreign-type-size :int)))))

(defun clm-read-ints (fd arr n)
  "Read N ints from the file descriptor FD into the array ARR."
  (let ((result (c-read-ints fd arr n)))
    (values result arr)))

/*!< ---------------- clm-write-floats ----------------


(cffi:defcfun ("write" c-write-floats-1) :int
  (fd :int) (buf :pointer) (bytes :int))

(defun c-write-floats (fd arr n)
  "Write N floats from the array ARR to the file descriptor FD."
  `(cffi:with-foreign-pointer (buf @,arr)
    (/ (cffi:foreign-funcall "write" :int @,fd buf (* @,n 8)) 8)))

(defun clm-write-floats (fd arr n)
  "Write N floats from the array ARR to the file descriptor FD."
  (let ((result (c-write-floats fd arr n)))
    (values result arr)))

/*!< ---------------- clm-write-ints ----------------


(defmacro <clm-write-ints> (result fd arr n)
  (format *c-file* "  ~A = write(~A, (char *)(~A), ~A * sizeof(int)) / sizeof(int);~%"
	  (lc (second result))
	  (lc-num-ref fd :integer)
	  (lc-arr-ref arr)
	  (lc-num-ref n :integer))
  nil)

(cffi:defcfun ("write" c-write-ints-1) :int
  (fd :int) (buf :pointer) (bytes :int))

(defun c-write-ints (fd arr n)
  (cffi:with-foreign-pointer (buf arr)
    (c-write-ints-1 fd buf n)))

(defun clm-write-ints (fd arr n) (/ (c-write-ints fd arr (* n (cffi:foreign-type-size :int))) (cffi:foreign-type-size :int)))

/*!< ---------------- c-open-input-file ----------------


(cffi:defcfun ("open" c-open-input-file-1) :int
  (pathname :string)
  (flags :int))

(defun c-open-input-file (pathname)
  (c-open-input-file-1 pathname 0))

/*!< ---------------- c-open-output-file ----------------


(cffi:defcfun ("open" c-open-output-file-1) :int
  (pathname :string)
  (flags :int)
  (mode :int))

(defun c-open-output-file (pathname)
  (c-open-output-file-1 pathname (logior 1 64) 438))

/*!< ---------------- c-create-file ----------------


(cffi:defcfun ("open" c-create-file-1) :int
  (pathname :string)
  (flags :int)
  (mode :int))

(defun c-create-file (pathname)
  (c-create-file-1 pathname (logior 2 64) 438))

/*!< ---------------- c-close ----------------


(cffi:defcfun ("close" c-close) :int
  (fd :int))

/*!< ---------------- clm-swap-ints ----------------


(defun clm-swap-ints (data n)
  (declare (type (simple-array (unsigned-byte 32) (*)) data))
  (let ((i 0))
    (declare (type fixnum i))
    (loop until (= i n) do
      (setf (aref data i) (ldb (byte 32 0) (dpb (ldb (byte 8 0) (aref data i)) (byte 8 24) 0)))
      (incf i))))

/*!< ---------------- clm-swap-doubles ----------------


(defun clm-swap-doubles (data n)
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((i 0))
    (declare (type fixnum i))
    (loop until (= i n) do
      (let ((temp (aref data i)))
	(setf (aref data i) (aref data (+ i 7)))
	(setf (aref data (+ i 7)) temp))
      (incf i 8))))