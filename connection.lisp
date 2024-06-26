(defpackage #:jsonrpc/connection
  (:use #:cl)
  (:import-from #:jsonrpc/request-response
                #:request
                #:response
                #:response-id)
  (:import-from #:bordeaux-threads
                #:make-condition-variable
                #:make-recursive-lock
                #:with-recursive-lock-held
                #:condition-wait
                #:condition-notify
                #:*default-special-bindings*)
  (:import-from #:dissect
                #:present)
  (:import-from #:chanl)
  (:import-from #:vom)
  (:export #:connection
           #:*connection*
           #:wait-for-ready
           #:connection-stream
           #:connection-request-callback
           #:add-message-to-queue
           #:add-message-to-outbox
           #:process-request
           #:connection-request-queue
           #:connection-outbox)
  (:documentation "jsonrpc/connection provides a class `connection' for holding data of each connections, like inbox and outbox."))
(in-package #:jsonrpc/connection)

(defvar *connection*)

(defclass process-wait ()
  ((condvar :initform (bt:make-condition-variable)
            :reader process-wait-condvar)
   (condlock :initform (bt:make-recursive-lock)
             :reader process-wait-condlock)))

(defgeneric wait-for-ready (process-wait)
  (:method ((process-wait process-wait))
    (bt:with-recursive-lock-held ((process-wait-condlock process-wait))
      (bt:condition-wait (process-wait-condvar process-wait)
                         (process-wait-condlock process-wait)))))

(defgeneric notify-ready (process-wait)
  (:method ((process-wait process-wait))
    (bt:with-recursive-lock-held ((process-wait-condlock process-wait))
      (bt:condition-notify (process-wait-condvar process-wait)))))

(defclass connection (process-wait)
  ((stream :initarg :stream
           :accessor connection-stream)
   (request-callback :initarg :request-callback
                     :accessor connection-request-callback)

   (request-queue :initform (make-instance 'chanl:unbounded-channel)
                  :accessor connection-request-queue)

   (response-map :initform (make-hash-table :test 'equal)
                 :reader connection-response-map)
   (response-lock :initform (bt:make-recursive-lock "jsonrpc/connection response-lock")
                  :reader connection-response-lock)
   (response-callback :initform (make-hash-table :test 'equal)
                      :reader connection-response-callback)

   (outbox :initform (make-instance 'chanl:unbounded-channel)
           :accessor connection-outbox)))

(defgeneric add-message-to-queue (connection message)
  ;; batch
  (:method ((connection connection) (messages list))
    (if (typep (first messages) 'request)
        (progn
          (chanl:send (connection-request-queue connection) messages)
          (notify-ready connection))
        (dolist (response messages)
          (add-message-to-queue connection response)))
    (values))

  (:method ((connection connection) (message request))
    (chanl:send (connection-request-queue connection) message)
    (notify-ready connection)
    (values))

  (:method ((connection connection) (message response))
    (let ((id (response-id message)))
      (unless id
        (warn "Unexpected response which has no id. Ignored.")
        (return-from add-message-to-queue))

      (let ((response-map (connection-response-map connection))
            (response-lock (connection-response-lock connection))
            (response-callback (connection-response-callback connection)))
        (bt:with-recursive-lock-held (response-lock)
          (let ((callback (gethash id response-callback)))
            (if callback
                (progn
                  (handler-case
                      (funcall callback message)
                    (error (e)
                      (vom:error "~A in a JSON-RPC response callback: ~A"
                                 (type-of e)
                                 e)))
                  (remhash id response-callback))
                (setf (gethash id response-map) message))))))

    (values)))

(defun add-message-to-outbox (connection message)
  (chanl:send (connection-outbox connection) message)
  (notify-ready connection))

(defun set-callback-for-id (connection id callback)
  (let ((response-map (connection-response-map connection))
        (response-lock (connection-response-lock connection))
        (response-callback (connection-response-callback connection)))
    (bt:with-recursive-lock-held (response-lock)
      (multiple-value-bind (response existsp)
          (gethash id response-map)
        (if existsp
            (progn
              (funcall callback response)
              (remhash id response-map))
            (setf (gethash id response-callback) callback))))
    (values)))

(defgeneric process-request (connection request)
  ;; batch request
  (:method ((connection connection) (requests list))
    (mapcar (lambda (request)
              (process-request connection request))
            requests))

  (:method ((connection connection) (request request))
    (let ((*connection* connection)
          (bt:*default-special-bindings* (append `((*connection* . ,connection))
                                                 bt:*default-special-bindings*)))
      (funcall (connection-request-callback connection) request))))
