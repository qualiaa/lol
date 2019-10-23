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


(load "graph-util")


(graph->png "wizard-directed.dot" *locations* *edges*)
(ugraph->png "wizard-undirected.dot" *locations* *edges*)
