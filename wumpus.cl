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
                  (mapcar #'list (remove-duplicates (mapcar #'cdr
                                             (direct-edges node edge-list))
                                           :test #'equal))))
    (remove-duplicates (mapcar #'car edge-list))))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
                      collect i))
         (edge-list (join-all-islands nodes (make-edge-list)))
         (cops (remove-if-not (lambda (x)
                                (zerop (random *cop-odds*)))
                              edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
            (let ((node1 (car x))
                  (node1-edges (cdr x)))
              (cons node1
                    (mapcar (lambda (edge)
                              (let ((node2 (car edge)))
                                (if (intersection (edge-pair node1 node2)
                                                  edges-with-cops
                                                  :test #'equal)
                                  (list node2 'cops)
                                  edge)))
                            node1-edges))))
          edge-alist))

(defun neighbours (a edge-alist)
  (mapcar #'car (cdr (assoc a edge-alist))))

(defun one-away (a b edge-alist)
  (member b (neighbours a edge-alist)))

(defun two-away (a b edge-alist)
  (or (one-away a b edge-alist)
      (some (lambda (c)
              (one-away b c edge-alist))
            (neighbours a edge-alist))))

(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
        (glow-worms (loop for i below *worm-num* collect (random-node))))
    (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond ((eq n wumpus) '(wumpus))
                                ((two-away n wumpus edge-alist) '(blood!)))
                          (cond ((member n glow-worms) '(glow-worms!))
                                ((some (lambda (worm-node)
                                         (one-away worm-node n edge-alist))
                                       glow-worms) '(lights!)))
                          (when (some #'cdr (cdr (assoc n edge-alist)))
                            '(sirens!))))))

(defun new-game ()
  (setf *edges* (make-city-edges))
  (setf *nodes* (make-city-nodes *edges*))
  (setf *player-pos* (find-empty-node))
  (if *player-pos*
    (progn (setf *visited-nodes* (list *player-pos*))
           (draw-city)
           (draw-known-city)
           (launch-feh "known-city.dot.png"))
    (new-game)))

(defun find-empty-node ()
  (let ((result (find 'nil *nodes* :key #'cdr)))
    (when result (car result))))

(defun draw-city ()
  (ugraph->png "congestion-city.dot" *nodes* *edges*))

(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
              (let ((n (assoc node *nodes*)))
                (if (eql node *player-pos*)
                  (append n '(*))
                  n))
              (list node '?)))
          (remove-duplicates
            (append *visited-nodes*
                    (mapcan (lambda (node)
                              (mapcar #'car
                                      (cdr (assoc node *edges*))))
                            *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                  (if (member (car x) *visited-nodes*)
                                    x
                                    (list (car x))))
                                (cdr (assoc node *edges*)))))
           *visited-nodes*))

(defun draw-known-city ()
  (ugraph->png "known-city.dot" (known-city-nodes) (known-city-edges)))

(defun launch-feh (file)
  (ext:run-shell-command (concatenate 'string "feh -. -R1 " file) :wait nil))

(defun walk (pos)
  (handle-direction pos nil))
(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)
  (let ((edge (assoc pos
                     (cdr (assoc *player-pos* *edges*)))))
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *nodes*))
         (has-worm (and (member 'glow-worm node)
                   (not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops. Game Over."))
          ((member 'wumpus node) (if charging
                                   (princ "You found the Wumpus!")
                                   (princ "You ran into the Wumpus")))
          (charging (princ "You wasted your last bullet. Game Over"))
          (has-worm (let ((new-pos (random-node)))
                      (princ "You ran into a Glow Worm Gang! You're now at")
                      (princ new-pos)
                      (handle-new-place nil new-pos nil))))))

