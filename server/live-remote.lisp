(in-package #:live-remote)

;;----------------------------------------------------------------------

(defconstant +announce-source-id+ #.(- (expt 2 32) 1))
(defconstant +time-sync-id+ #.(- (expt 2 32) 2))

;;----------------------------------------------------------------------

(defvar *source-metadata* (make-hash-table))
(defvar *servers* nil)

(defstruct (server (:constructor %make-server))
  (usocket nil :type t)
  (channel (error "chanl must be provided")
           :type chanl:unbounded-channel))

(defun make-server (&optional (port 1234))
  (let* ((chanl (make-instance 'chanl:unbounded-channel))
         (uss (usocket:socket-server
               "127.0.0.1" port #'recieve-thread-loop (list chanl)
               :element-type '(unsigned-byte 8)
               :in-new-thread t
               :reuse-address t
               :multi-threading t))
         (server (%make-server :usocket uss :channel chanl)))
    (push server  *servers*)
    server))

(defun recieve-thread-loop (stream chanl)
  (handler-case
      (let ((binary-types:*endian* :little-endian))
        (loop :do (chanl:send chanl (read-message stream))))
    (end-of-file (err)
      (format t "Client disconnected~%args:~a~%" err))))

(defun read-message (stream)
  (let ((source-name (read-uint32 stream)))
    (cond
      ((= source-name +announce-source-id+) (handle-announce-source stream))
      ((= source-name +time-sync-id+) (handle-time-sync stream))
      (t (handle-data-event source-name stream)))))

(defun handle-data-event (source-id stream)
  (let ((group-id (read-uint32 stream))
        (data (read-vec4 stream)))
    (list source-id group-id data)))


(defun handle-announce-source (stream)
  (let* ((new-source-id (read-uint32 stream))
         (name-len (read-uint32 stream))
         (name-char-codes (loop for i below name-len collect (read-uint32 stream)))
         (name (format nil "~s ~{~a~}" name-char-codes (mapcar #'code-char name-char-codes))))
    (setf (gethash new-source-id *source-metadata*) (list name))
    (list :new-source-id new-source-id :name name)))

(defun handle-time-sync (stream)
  (let ((client-time (binary-types:read-binary 'binary-types:u64 stream)))
    (format t "time sync ~s" client-time)))


;;----------------------------------------------------------------------
;; REPL side

(defun read-all-remote-messages (server)
  (loop :for message = (chanl:recv (server-channel server)
                                   :blockp nil)
     :until (null message) :collect message))

(defun test ()
  (loop :do
     (loop :for server :in *servers* :do
        (continuable
          (update-repl-link)
          (let ((messages (read-all-remote-messages server)))
            (when messages
              (print messages)))))))

;;----------------------------------------------------------------------
