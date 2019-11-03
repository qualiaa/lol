(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16
                :junk-allowed t)))
    (if code
      (code-char code)
      default)))

(defun decode-param (s)
  (labels ((f (lst)
              (when lst
                (case (car lst)
                  (#\% (cons (http-char (cadr lst) (caddr lst)) (f (cdddr lst))))
                  (#\+ (cons #\space (f (cdr lst))))
                  (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (query (position #\? url)))
    (if query
      (cons (subseq url 0 query) (parse-params (subseq url (1+ query))))
      (list url))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2 )))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(defun serve (request-handler &optional (port 8080))
  (let ((socket (socket-server port)))
    (unwind-protect
      (loop (with-open-stream (stream (socket-accept socket))
              (let* ((url    (parse-url (read-line stream)))
                     (path   (car url))
                     (header (get-header stream))
                     (params (append (cdr url)
                                     (get-content-params stream header)))
                     (*standard-output* stream))
                (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun html-wrapper (body)
  (format nil
"<!DOCTYPE html>
<html>
    <head>
        <meta charset=\"utf-8\">
    </head>
    <body>
        ~a
    </body>
</html>" body))

(defun hello-handler (path header params)
  (princ (http-response (html-wrapper
           (if (equal path "greetings")
             (let ((name (assoc 'name params)))
               (if name
                 (format nil "<p>Nice to meet you, ~a!</p>" (cdr name))
                 "<form>What is your name?<input name='name'/></form>"))
             "<p>Sorry, I don't know that</p>")))))


(defparameter *reason-phrases* '(
    (100  . "Continue")
    (101  . "Switching Protocols")
    (200  . "OK")
    (201  . "Created")
    (202  . "Accepted")
    (203  . "Non-Authoritative Information")
    (204  . "No Content")
    (205  . "Reset Content")
    (206  . "Partial Content")
    (300  . "Multiple Choices")
    (301  . "Moved Permanently")
    (302  . "Found")
    (303  . "See Other")
    (304  . "Not Modified")
    (305  . "Use Proxy")
    (307  . "Temporary Redirect")
    (400  . "Bad Request")
    (401  . "Unauthorized")
    (402  . "Payment Required")
    (403  . "Forbidden")
    (404  . "Not Found")
    (405  . "Method Not Allowed")
    (406  . "Not Acceptable")
    (407  . "Proxy Authentication Required")
    (408  . "Request Time-out")
    (409  . "Conflict")
    (410  . "Gone")
    (411  . "Length Required")
    (412  . "Precondition Failed")
    (413  . "Request Entity Too Large")
    (414  . "Request-URI Too Large")
    (415  . "Unsupported Media Type")
    (416  . "Requested range not satisfiable")
    (417  . "Expectation Failed")
    (500  . "Internal Server Error")
    (501  . "Not Implemented")
    (502  . "Bad Gateway")
    (503  . "Service Unavailable")
    (504  . "Gateway Time-out")
    (505  . "HTTP Version not supported")
))

(defun http-status-line (status)
  (format nil "HTTP/1.1 ~D ~a" status (cdr (assoc status *reason-phrases*))))

(defparameter *crlf* (concatenate 'string '(#\return #\linefeed)))


(defun http-response (&optional (body nil) (status 200))
  (concatenate 'string
    (http-status-line status) *crlf*
    (response-headers body)
    body))

(defun response-headers (&optional body)
  (concatenate 'string
    "Content-Type: text/html; charset=UTF-8" *crlf*
    "Connection: close" *crlf*
    "Server: jamiehttp" *crlf*
    (when body
      (format nil "Content-Length: ~D~a" (length body) *crlf*))))
