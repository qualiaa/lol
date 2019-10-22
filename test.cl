#!/usr/bin/env clisp

(defparameter *items* '(bottle axe frog chain))
(defparameter *item-locations* '((bottle . living-room)
                                 (axe    . living-room)
                                 (frog   . garden)))

(defparameter *edges* '((living-room  (garden east door)
                                      (attic upstairs ladder))
                        (garden       (living-room west door))
                        (attic        (living-room downstairs ladder))))

(defparameter *locations* '((living-room . (You are in a dingy living room.
                                                There is a wizard snoring loudly
                                                on a chair.))
                            (garden      . (you are in a lovely garden. a bed of
                                                pagonias waves gently in the
                                                breeze.))
                            (attic       . (you are in a dingy attic))))
(defparameter *location* 'living-room)

;;; Description generators
(defun items-at (items item-locations location)
  (labels ((at-loc-p (item)
             (eq (cdr (assoc item item-locations)) location)))
    (remove-if (complement #'at-loc-p) items)))

(defun describe-path (path)
  `(there is a ,(caddr path) going ,(cadr path) from here.))

(defun describe-paths (edges location)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defun describe-items (items item-locations location)
  (apply #'append
         (mapcar
           (lambda (item)
             `(there is a ,item on the ground.))
           (items-at items item-locations location))))

(defun describe-location (locations location)
  (cdr (assoc location locations)))

(defun describe-room (location)
  (append (describe-location *locations* location)
          (describe-items *items* *item-locations* location)
          (describe-paths *edges* location)))

;;; Game functions
(defun look () (describe-room *location*))
(defun walk (direction)
  (let* ((edges (cdr (assoc *location* *edges*)))
         (next (find direction edges :key #'cadr)))
  (if next
    (progn (setf *location* (car next))
           (look))
    '(You cannot go that way))))

(defun pickup (item)
  (let ((items (items-at *items* *item-locations* *location*)))
    (if (member item items)
      (progn (push `(,item body) *item-locations*)
             `(You pick up the ,item))
      '(You cannot pick that up))))



;(defun game-eval (input)
;  (let ((allowed-commands '(look pickup walk)))
;    (if (member input allowed-commands)


(defun game-read ()
  (concatenate 'string "(" (read) ")"))

;; (print (describe-exits 'living-room *edges*))
(princ (look))
(print (walk 'east))
(print (walk 'west))
(print (walk 'upstairs))
(print (walk 'downstairs))
(print (pickup 'axe))
(print (pickup 'dildo))
(print (look))
