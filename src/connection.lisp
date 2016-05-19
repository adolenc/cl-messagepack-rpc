(in-package #:messagepack-rpc.event-loop)


(defmacro with-event-loop-bindings ((event-base) &body body)
  "Make cl-async think the event loop was running all along, because it
otherwise sometimes refuses to work."
  `(let ((as::*event-base* ,event-base)
         (as::*data-registry* (as::event-base-data-registry ,event-base))
         (as::*function-registry* (as::event-base-function-registry ,event-base)))
     ,@body))

(defun on-connection-close (ev)
  (declare (ignore ev))
  (clean as::*event-base* "Connection closed!"))


(defun connect-pipe (event-base callback &key path)
  (with-event-loop-bindings (event-base)
    (as:pipe-connect file #'(lambda (s d) (funcall callback d)) :event-cb #'on-connection-close)))

(defun connect-tcp (event-base callback &key host port)
  (with-event-loop-bindings (event-base)
    (as:tcp-connect host port #'(lambda (s d) (funcall callback d)) :event-cb #'on-connection-close)))


(defun send (event-base socket msg)
  "Send msg using event-loop via the provided socket."
  (with-event-loop-bindings (event-base)
    (as:write-socket-data socket msg :force t)))
