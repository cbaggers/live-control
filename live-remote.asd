;;;; live-remote.asd

(asdf:defsystem #:live-remote
  :description "Describe live-remote here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:usocket #:usocket-server
               #:bordeaux-threads #:ieee-floats #:binary-types
               #:chanl #:trivial-garbage #:livesupport)
  :components ((:file "server/package")
               (:file "server/read-funcs")
               (:file "server/live-remote")))
