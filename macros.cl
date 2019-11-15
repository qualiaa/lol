(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro split (input yes no)
  (let1 val (gensym)
  `(let1 ,val ,input
     (if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)
    (if lst
      (let1 pair (cons (car lst) (cadr lst))
        (f (cddr lst) (cons pair acc)))
      (reverse acc))))
    (f lst nil)))

(defmacro recurse (args &body body)
  (let* ((arg-pairs (pairs args))
         (params (mapcar #'car arg-pairs))
         (inits (mapcar #'cdr arg-pairs)))
    `(labels ((f (,@params) ,@body))
       (f ,@inits))))

(defun test (lst)
  (recurse (lst lst
            acc 0)
    (split lst
      (f tail (1+ acc))
      acc)))
