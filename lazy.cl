(defmacro lazy (&body body)
  (let ((forced (gensym))
        (val    (gensym)))
    `(let ((,forced nil)
           (,val    nil))
       (lambda ()
         (unless ,forced
           (setf ,val (progn ,@body))
           (setf ,forced t))
         ,val))))

(defun force (thunk)
  (funcall thunk))

; LoL defines this as a macro - not clear why?
(defun lazy-cons (a d)
  (lazy (cons a d)))

(defun lazy-car (lst)
  (car (force lst)))

(defun lazy-cdr (lst)
  (cdr (force lst)))

; LoL defines this as a function
(defconstant lazy-nil (lambda () nil))

(defun lazy-null (lst)
  (null (force lst)))

(defun lazy-find-if (p lst)
  (unless (lazy-null lst)
    (if (p (lazy-car lst))
      (lazy-car lst)
      (lazy-find-if p (lazy-cdr lst)))))

(defun lazy-nth (n lst)
  (unless (lazy-null lst)
    (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst)))))

(defun lazy-mapcar (f lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall f (lazy-car lst))
                (lazy-mapcar f (lazy-cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(defun make-lazy (lst)
  (reduce (lambda (a x)
            (lazy-cons x a))
          (reverse lst)
          :initial-value lazy-nil))

(defun lazy-mapcan (f lst)
  (labels ((g (current-items rest)
              (if current-items
                (cons (car current-items) (lazy (g (cdr current-items) rest)))
                (unless (lazy-null rest)
                  (g (funcall f (lazy-car rest)) (lazy-cdr rest))))))
    (lazy (g (funcall f (lazy-car lst)) (lazy-cdr lst)))))

(defun test-fn (x)
  (loop for i to x collect i))
