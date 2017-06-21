;;;; package.lisp

(uiop:define-package #:live-remote
    (:use #:cl #:livesupport)
  (:export :make-server
           :kill-server
           :read-all-remote-messages
           :hard-kill-all-servers))
