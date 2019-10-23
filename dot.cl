(defparameter *edges* '((living-room  (garden east door)
                                      (attic upstairs ladder))
                        (garden       (living-room west door))
                        (attic        (living-room downstairs ladder))))

(defparameter *locations* '((living-room (You are in a dingy living room.
                                              There is a wizard snoring loudly
                                              on a chair.))
                            (garden      (you are in a lovely garden. a bed of
                                              pagonias waves gently in the
                                              breeze.))
                            (attic       (you are in a dingy attic.))))

(defparameter *max-label-length* 20)

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
        (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (cdr edge))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (fresh-line)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (fresh-line)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda () (graph->dot nodes edges))))

(graph->png "wizard.dot" *locations* *edges*)
