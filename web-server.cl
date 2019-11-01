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

(defun hello-handler (path header params)
  (if (equal path "greetings")
    (let ((name (assoc 'name params)))
      (if name
        (format t "<html><p>Nice to meet you, ~a!</p></html>" (cdr name))
        (princ "<html><form>What is your name?<input name='name'/></form></html>")))
    (princ "<html><p><Sorry, I don't know that</p></html>")))
