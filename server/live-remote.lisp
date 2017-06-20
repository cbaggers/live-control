(in-package #:live-remote)

;;----------------------------------------------------------------------

(defconstant +announce-source-id+ #.(- (expt 2 32) 1))
(defconstant +time-sync-id+ #.(- (expt 2 32) 2))

;;----------------------------------------------------------------------

(defstruct tool
  (id 0 :type (unsigned-byte 32))
  (group-id 0 :type (unsigned-byte 32)))

;;----------------------------------------------------------------------

(defvar *source-metadata* (make-hash-table))
(defvar *servers* nil)

(defstruct (server (:constructor %make-server))
  (usocket nil :type t)
  (chanl-from-thread (error "chanl must be provided")
                     :type chanl:unbounded-channel)
  (chanl-to-thread (error "chanl must be provided")
                   :type chanl:unbounded-channel))

(defun make-server (&optional (port 1234))
  (let* ((chanl-from (make-instance 'chanl:unbounded-channel))
         (chanl-to (make-instance 'chanl:unbounded-channel))
         (uss (usocket:socket-server
               "127.0.0.1" port
               #'recieve-thread-loop (list chanl-from chanl-to)
               :element-type '(unsigned-byte 8)
               :in-new-thread t
               :reuse-address t
               :multi-threading t
               :name "livecontrol-server-thread"))
         (server (%make-server :usocket uss
                               :chanl-from-thread chanl-from
                               :chanl-to-thread chanl-to)))
    (push server  *servers*)
    server))

(defun kill-server (server)
  (chanl:send (server-chanl-to-thread server) :kill :blockp t))

(defun recieve-thread-loop (stream chanl-from-thread chanl-to-thread)
  (handler-case
      (let ((binary-types:*endian* :little-endian)
            (running t))
        (loop :while running :do
           ;; blocking here can make it blind to being closed -- ↓↓↓↓
           (chanl:send chanl-from-thread (read-message stream) :blockp t)
           (when (chanl:recv chanl-to-thread :blockp nil)
             (format t "Server asked to shut down")
             (setf running nil))))
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
         (new-source-group-id (read-uint32 stream))
         (name-len (read-uint32 stream))
         (name-char-codes (loop for i below name-len collect (read-uint32 stream)))
         (name (format nil "~s ~{~a~}" name-char-codes (mapcar #'code-char name-char-codes))))
    (setf (gethash new-source-id *source-metadata*) (list name new-source-group-id))
    (list :new-source-id new-source-id :name name)))

(defun handle-time-sync (stream)
  (let ((client-time (binary-types:read-binary 'binary-types:u64 stream)))
    (format t "time sync ~s" client-time)))


;;----------------------------------------------------------------------
;; REPL side

(defun read-all-remote-messages ()
  (loop :for server :in *servers* :append
     (loop :for message = (chanl:recv (server-chanl-from-thread server)
                                      :blockp nil)
        :until (null message) :collect message)))

(defmacro with-live-remote ((&optional (port 1234)) &body body)
  (let ((server (gensym "SERVER")))
    `(let ((,server (make-server ,port)))
       (unwind-protect
            (progn ,@body)
         (kill-server ,server)))))

(defun test (&optional (port 1234))
  (print "ok, let's start")
  (with-live-remote (port)
    (format t "Started")
    (loop :do
       (continuable
         (update-repl-link)
         (let ((messages (read-all-remote-messages)))
           (when messages
             (print messages)))))))

;;----------------------------------------------------------------------
