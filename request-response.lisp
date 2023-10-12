(in-package #:cl-user)
(defpackage #:jsonrpc/request-response
  (:use #:cl
        #:jsonrpc/errors)
  (:import-from #:yason
                #:with-output
                #:parse
                #:encode
                #:with-object
                #:encode-object-element)
  (:import-from #:alexandria
                #:hash-table-keys
		#:when-let
                #:xor)
  (:export #:request
           #:response
           #:make-request
           #:make-response
           #:make-error-response
           #:request-method
           #:request-params
           #:request-id
           #:response-error
           #:response-error-message
           #:response-error-code
           #:response-result
           #:response-id
           #:parse-message))
(in-package #:jsonrpc/request-response)

(defstruct request
  jsonrpc
  method
  params
  id)

(defstruct response
  jsonrpc
  error
  result
  id)

(defun make-error-response (&key id code message (data nil data-specified-p))
  (let ((hash (make-hash-table :test 'equal)))
    (setf (gethash "code" hash) code
          (gethash "message" hash) message)
    (when data-specified-p
      (setf (gethash "data" hash) data))
    (make-response :error hash :id id)))

(defun response-error-message (response)
  (let ((error (response-error response)))
    (when error
      (gethash "message" error))))

(defun response-error-code (response)
  (let ((error (response-error response)))
    (when error
      (gethash "code" error))))

(declaim (ftype (function (hash-table &key (:rpc-version (or null number)))
			  (values boolean &optional))
		valid-request-p))
(defun valid-request-p (request &key (rpc-version 2.0))
  (let ((id (gethash "id" request)))
    (and (stringp (gethash "method" request))
	 (typep id
		'(or string number null))
	 (ecase rpc-version
	   (2.0
	    (and (equal (gethash "jsonrpc" request) "2.0")
		 (typep (gethash "params" request)
			'(or hash-table list))
		 (every (lambda (key)
			  (find key '("jsonrpc" "method" "params" "id") :test #'string=))
			(hash-table-keys request))))
	   (1.0
	    (consp (gethash "params" request))
	    (or id (eql id 'null))
	    (every (lambda (key)
		     (find key '("method" "params" "id") :test #'string=))
		   (hash-table-keys request)))))))

(declaim (ftype (function (hash-table &key (:rpc-version number))
			  (values boolean &optional))
		valid-response-p))
(defun valid-response-p (response &key (rpc-version 2.0))
  "Predicate function which returns t, if response is valid and nil if not.
Default rpc-version is 2.0, alternatively 1.0 can be supplied."
  (let ((jsonrpc (gethash "jsonrpc" response))
        (id (gethash "id" response)))
    (when (ecase rpc-version
	    (2.0
	     (and (string= jsonrpc "2.0")
		  (typep id '(or string number null))
		  (typep (gethash "error" response) '(or null hash-table))
		  (xor (nth-value 1 (gethash "error" response))
		       (nth-value 1 (gethash "result" response)))
		  (every (lambda (key)
			   (find key '("jsonrpc" "result" "error" "id") :test #'string=))
			 (hash-table-keys response))))
	    (1.0
	     (and id
		  (let ((error (gethash "error" response)))
		    (or (eq error 'null)
			(typep error '(or null hash-table))))
		  (gethash "result" response)
		  (every (lambda (key)
			   (find key '("result" "error" "id") :test #'string=))
			 (hash-table-keys response)))))
      t)))

(declaim (ftype (function ((or string stream) &key (:rpc-version number))
			  t)
		parse-message))
(defun parse-message (input &key (rpc-version 2.0))
  "Parse a message which can be either a request or a response."
  (when (or (and (typep input 'string)
                 (< 0 (length input)))
            (typep input 'stream))
    (let ((message (handler-case (yason:parse input)
                     (error () (error 'jsonrpc-parse-error)))))
      (flet ((make-message (hash)
               (if (gethash "method" hash)
                   (progn
                     (unless (valid-request-p hash :rpc-version rpc-version)
                       (error 'jsonrpc-invalid-request))
                     (make-request :jsonrpc (gethash "jsonrpc" hash)
				   :method (gethash "method" hash)
                                   :params (gethash "params" hash)
                                   :id (gethash "id" hash)))
                   (progn
                     (unless (valid-response-p hash :rpc-version rpc-version)
                       (error 'jsonrpc-invalid-response))
                     (make-response :jsonrpc (gethash "jsonrpc" hash)
				    :result (gethash "result" hash)
                                    :error (gethash "error" hash)
                                    :id (gethash "id" hash))))))
        (etypecase message
          (list
           (mapcar #'make-message message))
          (hash-table
           (make-message message)))))))

(defmethod yason:encode ((request request) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (when-let (jsonrpc (request-jsonrpc request))
	(yason:encode-object-element "jsonrpc" jsonrpc))
      (yason:encode-object-element "method" (request-method request))
      (yason:encode-object-element "params" (request-params request))
      (when (request-id request)
        (yason:encode-object-element "id" (request-id request))))))

(defmethod yason:encode ((response response) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (when-let (jsonrpc (response-jsonrpc response))
	(yason:encode-object-element "jsonrpc" jsonrpc))
      (if (response-error response)
          (yason:encode-object-element "error" (response-error response))
          (yason:encode-object-element "result" (response-result response)))
      (yason:encode-object-element "id" (response-id response)))))
