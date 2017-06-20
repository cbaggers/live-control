(in-package #:live-remote)

(defun read-uint32 (stream)
  (binary-types:read-binary 'binary-types:u32 stream))

(defun read-float32 (stream)
  (ieee-floats:decode-float32 (read-uint32 stream)))

(defun read-vec4 (stream)
  (let ((arr (make-array 4 :element-type 'single-float)))
    (setf (aref arr 0) (read-float32 stream)
          (aref arr 1) (read-float32 stream)
          (aref arr 2) (read-float32 stream)
          (aref arr 3) (read-float32 stream))
    arr))
