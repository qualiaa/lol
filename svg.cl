(load "macros")

(defun print-tag (tag &key attributes closing)
  (format t "<~:[~a~{ ~a=\"~a\"~}~;/~a~]>" closing (string-downcase tag) attributes))

(defmacro tag (name attributes &body body)
  `(progn (print-tag ',name
                     :attributes (append ,@(mapcar (lambda (x)
                                                     `(list ,(string-downcase (car x))
                                                            ,(cdr x)))
                                                   (pairs attributes))))
          ,@body
          (print-tag ',name :closing t)))

(defparameter *simple-tags* '(html body head))

(defmacro def-simple-tag (tag-name)
  `(defmacro ,tag-name (&body encl)
     (let1 name ',tag-name
       `(tag ,name () ,@encl))))

(defmacro def-simple-tags (tags)
  `(progn ,@(mapcar (lambda (x) `(def-simple-tag ,x)) (symbol-value tags))))

(def-simple-tags *simple-tags*)

;(defmacro html (&body encl)
  ;`(tag html () ,@encl))

;(defmacro body (&body encl)
  ;`(tag body () ,@encl))

(defmacro svg (width height &body encl)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www,w3.org/1999/xlink"
                   height ,height
                   width ,width)
        ,@encl))

(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                style (svg-style color))))


(defun random-walk (value length)
  (unless (zerop length)
    (cons value (random-walk (if (zerop (random 2))
                               (1- value)
                               (1+ value))
                             (1- length)))))

(defun draw-circles ()
  (with-open-file (*standard-output* "circles.svg"
                   :direction :output 
                   :if-exists :supersede)
    (svg 250 250
         (circle '(50 . 50)   50 '(255 0 0))
         (circle '(100 . 100) 50 '(0 0 255)))))
    :
                    
(defun draw-graphs ()
  (with-open-file (*standard-output* "random_walk.svg"
                   :direction :output 
                   :if-exists :supersede)
    (svg 400 200
         (loop repeat 10
               do (polygon (append '((0 . 200))
                                   (loop for x from 0
                                         for y in (random-walk 100 400)
                                         collect (cons x y))
                                   '((400 . 200)))
                           (loop repeat 3 collect (random 256)))))))
    :
