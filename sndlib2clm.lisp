(in-package :common-tones)

;;; import some sndlib names to CLM
;;;
;;; file name is no longer accurate (this is now CL/CLM-specific, but aims at sndlib stuff mostly)

(cffi:defcfun ("mus_error_type_to_string" mus-error-type->string) :cstring
  (err :int))

(cffi:defcfun ("clm_sound_samples" mus-sound-samples) :int
  (arg :cstring))

(cffi:defcfun ("clm_sound_framples" mus-sound-framples) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_datum_size" mus-sound-datum-size) :int
  (arg :cstring))

(cffi:defcfun ("clm_sound_data_location" mus-sound-data-location) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_chans" mus-sound-chans) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_srate" mus-sound-srate) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_header_type" mus-sound-header-type) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_sample_type" mus-sound-data-format) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_original_sample_type" mus-sound-original-format) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_write_date" mus-sound-write-date) :int
  (arg :cstring))

(cffi:defcfun ("clm_sound_comment_start" mus-sound-comment-start) :int
  (arg :cstring))

(cffi:defcfun ("clm_sound_comment_end" mus-sound-comment-end) :int
  (arg :cstring))

(cffi:defcfun ("clm_sound_length" mus-sound-length) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_type_specifier" mus-sound-type-specifier) :int
  (arg :cstring))

(cffi:defcfun ("mus_sound_bits_per_sample" mus-sound-bits-per-sample) :int
  (arg :cstring))

(cffi:defcfun ("mus_header_type_name" mus-header-type-name) :cstring
  (type :int))

(cffi:defcfun ("mus_sample_type_name" mus-data-format-name) :cstring
  (format :int))

(cffi:defcfun ("mus_sound_comment" mus-sound-comment) :cstring
  (name :cstring))

(cffi:defcfun ("mus_sound_duration" mus-sound-duration) :float
  (arg :cstring))

(cffi:defcfun ("mus_sound_initialize" mus-sound-initialize) :int)

(cffi:defcfun ("mus_sound_forget" mus-sound-forget) :int
  (name :cstring))

(cffi:defcfun ("mus_audio_initialize" mus-audio-initialize) :int)

(cffi:defcfun ("clm_mus_file_probe" clm-mus-file-probe) :int
  (arg :cstring))

(cffi:defcfun ("clm_mus_set_clipping" clm-mus-set-clipping) :int
  (clipped :int))

(cffi:defcfun ("clm_mus_clipping" clm-mus-clipping) :int)

(cffi:defcfun ("clm_header_samples" mus-header-samples) :int)

(cffi:defcfun ("clm_header_data_location" mus-header-data-location) :int)
(cffi:defcvar ("mus_header_chans" mus-header-chans) :int)
(cffi:defcvar ("mus_header_srate" mus-header-srate) :int)
(cffi:defcvar ("mus_header_type" mus-header-type) :int)
(cffi:defcvar ("mus_header_sample_type" mus-header-format) :int)
(cffi:defcvar ("clm_header_comment_start" mus-header-comment-start) :int)
(cffi:defcvar ("clm_header_comment_end" mus-header-comment-end) :int)
(cffi:defcvar ("mus_header_type_specifier" mus-header-type-specifier) :int)
(cffi:defcvar ("mus_header_bits_per_sample" mus-header-bits-per-sample) :int)
(cffi:defcfun ("mus_header_loop_mode" mus-header-loop-mode) :int (which :int))
(cffi:defcfun ("mus_header_loop_start" mus-header-loop-start) :int (which :int))
(cffi:defcfun ("mus_header_loop_end" mus-header-loop-end) :int (which :int))
(cffi:defcfun ("mus_header_mark_position" mus-header-mark-position) :int (id :int))
(cffi:defcvar ("mus_header_base_note" mus-header-base-note) :int)
(cffi:defcvar ("mus_header_base_detune" mus-header-base-detune) :int)
(cffi:defcfun ("mus_header_set_raw_defaults" mus-header-set-raw-defaults) :void
  (sr :int) (chn :int) (frm :int))
(cffi:defcvar ("clm_header_true_length" mus-header-true-length) :int)
(cffi:defcvar ("mus_header_original_sample_type" mus-header-original-format) :int)
(cffi:defcfun ("clm_samples_to_bytes" mus-samples-to-bytes) :int
  (format :int) (size :int))
(cffi:defcfun ("clm_bytes_to_samples" mus-bytes-to-samples) :int
  (format :int) (size :int))
(cffi:defcfun ("mus_header_read" mus-header-read) :int (name :string))
(cffi:defcfun ("clm_header_write" mus-header-write) :int
  (name :string) (type :int) (srate :int) (chans :int)
  (loc :int) (size :int) (format :int) (comment :string)
  (len :int))
(cffi:defcfun ("clm_header_aux_comment_start" mus-header-aux-comment-start) :int
  (n :int))
(cffi:defcfun ("clm_header_aux_comment_end" mus-header-aux-comment-end) :int
  (n :int))
(cffi:defcfun ("mus_header_initialize" mus-header-initialize) :int)
(cffi:defcfun ("clm_mus_header_writable" clm-mus-header-writable) :int
  (type :int) (format :int))
(cffi:defcvar ("mus_header_sf2_entries" mus-header-sf2-entries) :int)
(cffi:defcfun ("mus_header_sf2_name" mus-header-sf2-name) :string
  (n :int))
(cffi:defcfun ("mus_header_sf2_start" mus-header-sf2-start) :int
  (n :int))
(cffi:defcfun ("mus_header_sf2_end" mus-header-sf2-end) :int
  (n :int))

(cffi:defcfun ("mus_header_sf2_loop_start" mus-header-sf2-loop-start) :int
  (n :int))

(cffi:defcfun ("mus_header_sf2_loop_end" mus-header-sf2-loop-end) :int
  (n :int))

(cffi:defcfun ("mus_header_original_sample_type_name" mus-header-original-format-name) :string
  (format :int)
  (type :int))

(cffi:defcfun ("mus_bytes_per_sample" mus-bytes-per-sample) :int
  (format :int))

#+(and sbcl linux)
(cffi:defcfun ("mus_oss_set_buffers" mus-oss-set-buffers) :void
  (num :int)
  (size :int))

(cffi:defcfun ("mus_reset_audio_c" reset-audio) :void)

(cffi:defcfun ("mus_reset_headers_c" reset-headers) :void)

(cffi:defcfun ("mus_reset_io_c" reset-io) :void)

#+(and mac-osx sbcl)
(cffi:defcfun ("mus_audio_output_properties_mutable" mus-audio-output-properties-mutable-1) :int
  (n :int))

(defvar *clm-output-properties-mutable* 1)

(defun mus-audio-output-properties-mutable (mutable)
  (let ((arg (if (numberp mutable)
                 mutable
               (if mutable
                   1 0))))
    (setf *clm-output-properties-mutable* arg)
    #+mac-osx (mus-audio-output-properties-mutable-1 arg)
    )) ; this has no effect since Lisp calls sndplay

(defun mus-file-probe (name)
  (not (zerop (clm-mus-file-probe name))))

(defun mus-set-clipping (val)
  (not (zerop (clm-mus-set-clipping (if val 1 0)))))

(defun mus-clipping ()
  (not (zerop (clm-mus-clipping))))

(defun mus-header-writable (typ frm)
  (not (zerop (clm-mus-header-writable typ frm))))
