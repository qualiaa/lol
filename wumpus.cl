(load "graph-util")

(defparameter *congestion-city-nodes* ())
(defparameter *congestion-city-edges* ())
(defparameter *visited-nodes* ())
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    `((,a . ,b) (,b . ,a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
                        collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
                   (eql (car x) node))
                 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                (unless (member node visited)
                  (push node visited)
                  (mapc #'traverse (mapcar #'cdr (direct-edges node edge-list))))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
                (let* ((connected    (get-connected (car nodes) edge-list))
                       (disconnected (set-difference nodes connected)))
                   (push connected islands)
                   (when disconnected
                     (find-island disconnected)))))
      (find-island nodes))
    islands))

(defun join-islands (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
            (join-islands (cdr islands)))))

; exists as remove-duplicates
;(defun existing-nodes (edge-list)
;  (let ((result nil))
;      (labels ((uniq (nodes)
;                 (unless (member (car nodes) result)
;                   (push (car nodes) result))
;                 (when (cdr nodes) (uniq (cdr nodes)))))
;        (uniq (join-pairs edge-list)))
;      result))

;(defun existing-nodes-functional (edge-list)
;  (labels ((uniq (nodes built)
;             (cond ((not nodes) built)
;                   ((member (car nodes) built) (uniq (cdr nodes) built))
;                   (t (uniq (cdr nodes) (cons (car nodes) built))))))
;    (uniq (join-pairs edge-list) nil)))

(defun join-pairs (l)
  (let ((x  (car l))
        (xs (cdr l)))
    (cons (car x) (cons (cdr x) (when xs (join-pairs xs))))))

(defun join-all-islands (nodes edge-list)
  (append (join-islands (find-islands nodes edge-list)) edge-list))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node)
            (cons node
                  (list (remove-duplicates (mapcar #'cdr
                                             (direct-edges node edge-list))
                                           :test #'equal))))
    (remove-duplicates (mapcar #'car edge-list))))
