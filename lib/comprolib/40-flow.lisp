(in-package :comprolib)

#+(or :cpl-all :cpl-flow)
(progn

  ;; --- Dinic (最大流) ---
  (defstruct (mf-graph (:constructor %make-mf (n g)))
    n g)

  (defstruct mf-edge to rev cap)

  (defun make-mf-graph (n)
    (%make-mf n (make-array n :initial-element nil)))

  (defun mf-add-edge (g from to cap)
    (let ((gg (mf-graph-g g))
          (e1 (make-mf-edge :to to :rev nil :cap cap))
          (e2 (make-mf-edge :to from :rev nil :cap 0)))
      (setf (mf-edge-rev e1) e2)
      (setf (mf-edge-rev e2) e1)
      (push e1 (aref gg from))
      (push e2 (aref gg to))
      g))

  (defun mf-bfs (g s dist)
    (let ((gg (mf-graph-g g))
          (q (make-array 0 :adjustable t :fill-pointer 0)))
      (fill dist -1)
      (setf (aref dist s) 0)
      (vector-push-extend s q)
      (let ((qi 0))
        (loop while (< qi (fill-pointer q))
              for v = (aref q qi)
              do (incf qi)
                 (dolist (e (aref gg v))
                   (when (and (> (mf-edge-cap e) 0)
                              (= (aref dist (mf-edge-to e)) -1))
                     (setf (aref dist (mf-edge-to e)) (1+ (aref dist v)))
                     (vector-push-extend (mf-edge-to e) q)))))))

  (defun mf-dfs (g v tg dist iters)
    "増加パスをDFSで1本流す (iters: 各頂点の次探索辺index)。返り値: 流量 or NIL。"
    (let ((gg (mf-graph-g g)))
      (labels ((rec (v up)
                 (if (= v tg)
                     up
                     (loop with lim = (length (aref gg v))
                           while (< (aref iters v) lim)
                           for e = (nth (aref iters v) (aref gg v))
                           do (if (and (> (mf-edge-cap e) 0)
                                       (= (aref dist (mf-edge-to e)) (1+ (aref dist v))))
                                  ;; ACL流: 上限 (min up cap) を下流に伝播し、
                                  ;; 子が返す流量は必ずこの辺のcap以下になる。
                                  ;; そうしないと中間辺が実際より多く減算される。
                                  (let ((d (rec (mf-edge-to e) (min up (mf-edge-cap e)))))
                                    (if (and d (> d 0))
                                        (progn
                                          (decf (mf-edge-cap e) d)
                                          (incf (mf-edge-cap (mf-edge-rev e)) d)
                                          (return-from rec d))
                                        (incf (aref iters v))))
                                  (incf (aref iters v)))))))
        (rec v most-positive-fixnum))))

  (defun mf-flow (g s tg)
    "最大流 (Dinic)。"
    (let ((flow 0)
          (dist (make-array (mf-graph-n g)))
          (iters (make-array (mf-graph-n g) :element-type 'integer)))
      (loop
        (mf-bfs g s dist)
        (when (= (aref dist tg) -1) (return))
        (fill iters 0)
        (loop for f = (mf-dfs g s tg dist iters)
              while f
              do (incf flow f)))
      flow))

  ;; --- 最小費用流 (SPFA) ---
  (defstruct (mcf-graph (:constructor %make-mcf (n g)))
    n g)

  (defstruct mcf-edge to rev cap cost)

  (defun make-mcf-graph (n)
    (%make-mcf n (make-array n :initial-element nil)))

  (defun mcf-add-edge (g from to cap cost)
    (let ((gg (mcf-graph-g g))
          (e1 (make-mcf-edge :to to :rev nil :cap cap :cost cost))
          (e2 (make-mcf-edge :to from :rev nil :cap 0 :cost (- cost))))
      (setf (mcf-edge-rev e1) e2)
      (setf (mcf-edge-rev e2) e1)
      (push e1 (aref gg from))
      (push e2 (aref gg to))
      g))

(defun mcf-flow (g s tg &optional (limitf most-positive-fixnum))
  "最小費用流。多値 (flow cost)。"
  (let ((n (mcf-graph-n g)) (gg (mcf-graph-g g))
        (flow 0) (cost 0))
    (block done
      (loop while (< flow limitf)
            do (let ((dist (make-array n :element-type 'integer
                                        :initial-element most-positive-fixnum))
                     (pv (make-array n :initial-element -1))
                     (pe (make-array n :initial-element nil))
                     (inq (make-array n :element-type 'bit :initial-element 0)))
                 (setf (aref dist s) 0)
                 (let ((q (make-array 0 :adjustable t :fill-pointer 0)) (qi 0))
                   (vector-push-extend s q)
                   (setf (aref inq s) 1)
                   (loop while (< qi (fill-pointer q))
                         for v = (aref q qi)
                         do (incf qi)
                            (setf (aref inq v) 0)
                            (dolist (e (aref gg v))
                              (when (and (> (mcf-edge-cap e) 0)
                                         (< (+ (aref dist v) (mcf-edge-cost e))
                                            (aref dist (mcf-edge-to e))))
                                (setf (aref dist (mcf-edge-to e))
                                      (+ (aref dist v) (mcf-edge-cost e)))
                                (setf (aref pv (mcf-edge-to e)) v)
                                (setf (aref pe (mcf-edge-to e)) e)
                                (when (zerop (aref inq (mcf-edge-to e)))
                                  (vector-push-extend (mcf-edge-to e) q)
                                  (setf (aref inq (mcf-edge-to e)) 1))))))
                 (when (= (aref dist tg) most-positive-fixnum)
                   (return-from done))
                 (let ((d (- limitf flow)))
                   (loop for v = tg then (aref pv v)
                         while (/= v s)
                         do (setf d (min d (mcf-edge-cap (aref pe v)))))
                   (loop for v = tg then (aref pv v)
                         while (/= v s)
                         for e = (aref pe v)
                         do (decf (mcf-edge-cap e) d)
                            (incf (mcf-edge-cap (mcf-edge-rev e)) d))
                   (incf flow d)
                   (incf cost (* d (aref dist tg)))))))
    (values flow cost)))
) ; end section

;;; ============ :cpl-2sat ============
