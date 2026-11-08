(in-package :comprolib)

#+(or :cpl-all :cpl-graph)
(progn

  ;; --- 二分ヒープ (dijkstra 用) ---
  (defstruct (pheap (:constructor %new-pheap (vec)))
    vec)

  (defun make-pheap ()
    (%new-pheap (make-array 0 :adjustable t :fill-pointer 0)))

  (defun pheap-empty (h)
    (zerop (fill-pointer (pheap-vec h))))

  (defun pheap-push (h key val)
    (let ((v (pheap-vec h)))
      (vector-push-extend (cons key val) v)
      (let ((i (- (fill-pointer v) 1)))
        (loop
          (when (zerop i)
            (return))
          (let ((p (ash (- i 1) -1)))
            (if (< (car (aref v i)) (car (aref v p)))
                (progn (rotatef (aref v i) (aref v p))
                       (setf i p))
                (return)))))))

  (defun pheap-peek (h)
    "最小要素の (key . val)。空ならNIL。"
    (unless (pheap-empty h)
      (aref (pheap-vec h) 0)))

  (defun pheap-pop (h)
    (let* ((v (pheap-vec h))
           (top (aref v 0)))
      (setf (aref v 0) (aref v (- (fill-pointer v) 1)))
      (decf (fill-pointer v))
      (let ((n (fill-pointer v))
            (i 0))
        (loop
          (let* ((l (+ 1 (* 2 i)))
                 (r (+ 2 (* 2 i)))
                 (smallest i))
            (when (and (< l n) (< (car (aref v l)) (car (aref v smallest))))
              (setf smallest l))
            (when (and (< r n) (< (car (aref v r)) (car (aref v smallest))))
              (setf smallest r))
            (if (= smallest i)
                (return)
                (progn (rotatef (aref v i) (aref v smallest))
                       (setf i smallest))))))
      top))

  ;; --- dijkstra ---
  (defun dijkstra (g src)
    "単一始点最短経路。g: 隣接リスト (vector of list of (dst . weight))。
返り値: 距離の simple-vector。到達不能は most-positive-fixnum。"
    (let* ((n (length g))
           (dist (make-array n :element-type 'integer
                                  :initial-element most-positive-fixnum))
           (h (make-pheap)))
      (setf (aref dist src) 0)
      (pheap-push h 0 src)
      (loop until (pheap-empty h)
            do (let* ((top (pheap-pop h))
                      (dv (car top))
                      (v (cdr top)))
                 (when (= dv (aref dist v))
                   (dolist (e (aref g v))
                     (let* ((to (car e))
                            (nd (+ dv (cdr e))))
                       (when (< nd (aref dist to))
                         (setf (aref dist to) nd)
                         (pheap-push h nd to)))))))
      dist))

  ;; --- LCA (ダブリング) ---
  (defstruct (lca-table (:constructor %make-lca (up dep log)))
    up dep log)

  (defun make-lca (g &optional (root 0))
    "構築 O(n log n)。g: 無向木の隣接リスト (vector of list)。"
    (let* ((n (length g))
           (log 1))
      (loop while (< (ash 1 log) n)
            do (incf log))
      (let ((up (make-array log))
            (dep (make-array n :element-type 'fixnum :initial-element -1)))
        (dotimes (k log)
          (setf (aref up k)
                (make-array n :element-type 'fixnum :initial-element -1)))
        ;; BFS で親と深さ
        (let ((queue (make-array 0 :adjustable t :fill-pointer 0)))
          (vector-push-extend root queue)
          (setf (aref (aref up 0) root) root)
          (setf (aref dep root) 0)
          (loop for qi from 0
                while (< qi (fill-pointer queue))
                for v = (aref queue qi)
                do (dolist (to (aref g v))
                     (unless (>= (aref dep to) 0)
                       (setf (aref dep to) (+ 1 (aref dep v))
                             (aref (aref up 0) to) v)
                       (vector-push-extend to queue)))))
        (loop for k from 0 below (- log 1)
              do (loop for v below n
                       do (setf (aref (aref up (+ k 1)) v)
                                (aref (aref up k) (aref (aref up k) v)))))
        (%make-lca up dep log))))

  (defun lca (tab u v)
    "u, v の最小共通祖先。"
    (let* ((up (lca-table-up tab))
           (dep (lca-table-dep tab))
           (log (lca-table-log tab)))
      (when (> (aref dep u) (aref dep v))
        (rotatef u v))
      (let ((d (- (aref dep v) (aref dep u))))
        (dotimes (k log)
          (when (logbitp k d)
            (setf v (aref (aref up k) v)))))
      (if (= u v)
          u
          (progn
            (loop for k from (- log 1) downto 0
                  do (when (/= (aref (aref up k) u) (aref (aref up k) v))
                       (setf u (aref (aref up k) u)
                             v (aref (aref up k) v))))
            (aref (aref up 0) u)))))

  (defun lca-dist (tab u v)
    "u, v 間の辺数。"
    (+ (aref (lca-table-dep tab) u)
       (aref (lca-table-dep tab) v)
       (* -2 (aref (lca-table-dep tab) (lca tab u v)))))

  ;; --- SCC (Kosaraji, 反復) ---
  (defun scc (g)
    "強連結成分分解。g: 隣接リスト (vector of list)。
返り値: 成分ごとの頂点リスト。辺は常に先頭側の成分から後続側の成分へ
(縮約グラフのトポロジカル順序)。"
    (let* ((n (length g))
           (rg (make-array n :initial-element nil)))
      (loop for v below n
            do (dolist (w (aref g v))
                 (push v (aref rg w))))
      ;; 1パス目: 帰りがけ順 (反復 DFS)
      (let ((order (make-array 0 :adjustable t :fill-pointer 0))
            (seen (make-array n :element-type 'bit :initial-element 0)))
        (loop for s below n
              when (zerop (aref seen s))
                do (let ((stack (list (cons s (aref g s)))))
                     (setf (aref seen s) 1)
                     (loop while stack
                           do (let* ((top (car stack))
                                     (rest (cdr top)))
                                (if (null rest)
                                    (progn
                                      (vector-push-extend (car top) order)
                                      (pop stack))
                                    (let ((w (car rest)))
                                      (setf (cdr top) (cdr rest))
                                      (when (zerop (aref seen w))
                                        (setf (aref seen w) 1)
                                        (push (cons w (aref g w)) stack))))))))
        ;; 2パス目: 逆グラフで到達順に成分として切り出す
        (let ((comp (make-array n :element-type 'fixnum :initial-element -1))
              (nc 0)
              (comps '()))
          (loop for idx from (- n 1) downto 0
                for s = (aref order idx)
                when (= (aref comp s) -1)
                  do (let ((stack (list s))
                           (members '()))
                       (setf (aref comp s) nc)
                       (loop while stack
                             for v = (pop stack)
                             do (push v members)
                                (dolist (w (aref rg v))
                                  (when (= (aref comp w) -1)
                                    (setf (aref comp w) nc)
                                    (push w stack))))
                       (push (nreverse members) comps)
                       (incf nc)))
          (nreverse comps)))))

  ;; --- BFS (グラフ: 隣接リスト vector of list) ---
  (defun bfs (g s)
    "単一始点の最短ホップ数。到達不能は most-positive-fixnum。"
    (let* ((n (length g))
           (dist (make-array n :element-type 'integer :initial-element most-positive-fixnum))
           (q (make-array 0 :adjustable t :fill-pointer 0)))
      (setf (aref dist s) 0)
      (vector-push-extend s q)
      (let ((qi 0))
        (loop while (< qi (fill-pointer q))
              for v = (aref q qi)
              do (incf qi)
                 (dolist (to (aref g v))
                   (when (= (aref dist to) most-positive-fixnum)
                     (setf (aref dist to) (1+ (aref dist v)))
                     (vector-push-extend to q)))))
      dist))

  ;; --- BFS (グリッド): 壁wall-p、移動は上下左右 ---
  (defun bfs-grid (grid sr sc &key (wallp (lambda (c) (char= c #\#))))
    "grid: 2次元配列。返り値: 距離の2次元配列 (到達不能 most-positive-fixnum)。"
    (let* ((h (array-dimension grid 0))
           (w (array-dimension grid 1))
           (dist (make-array (list h w) :element-type 'integer
                             :initial-element most-positive-fixnum))
           (q (make-array 0 :adjustable t :fill-pointer 0)))
      (setf (aref dist sr sc) 0)
      (vector-push-extend (cons sr sc) q)
      (let ((qi 0))
        (loop while (< qi (fill-pointer q))
              for (r . c) = (aref q qi)
              do (incf qi)
                 (dolist (d '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
                   (let ((nr (+ r (car d))) (nc (+ c (cdr d))))
                     (when (and (<= 0 nr (- h 1)) (<= 0 nc (- w 1))
                                (= (aref dist nr nc) most-positive-fixnum)
                                (not (funcall wallp (aref grid nr nc))))
                       (setf (aref dist nr nc) (1+ (aref dist r c)))
                       (vector-push-extend (cons nr nc) q))))))
      dist))

  ;; --- Floyd-Warshall ---
  (defun floyd-warshall (g)
    "g: 隣接行列 (2次元配列)。破壊的に最短距離へ。到達不能は most-positive-fixnum。"
    (let ((n (array-dimension g 0)))
      (loop for k below n
            do (loop for i below n
                     do (loop for j below n
                              when (and (/= (aref g i k) most-positive-fixnum)
                                        (/= (aref g k j) most-positive-fixnum))
                                do (let ((nd (+ (aref g i k) (aref g k j))))
                                     (when (< nd (aref g i j))
                                       (setf (aref g i j) nd))))))
      g))
) ; end section

  ;; --- 中国剰余定理: 2元 ---
  (defun crt (r1 m1 r2 m2)
    "crt2 の alias (export名と実体名を一致させる)。多値 (r m)。"
    (crt2 r1 m1 r2 m2))

  (defun crt2 (r1 m1 r2 m2)
  "x = r1 (mod m1), x = r2 (mod m2) を解く。多値 (r m)。解なしは (NIL NIL)。"
  (multiple-value-bind (g x) (extgcd m1 m2)
    ;; extgcd: m1*x + m2*y = g の係数xを返す (多値2つ目)
    (if (zerop (mod (- r2 r1) g))
        (let* ((m2g (floor m2 g))
               (lcm (* m1 m2g))
               (t0 (mod (* (floor (- r2 r1) g) (mod x m2g)) m2g)))
          ;; x = r1 + m1*t0 (mod lcm)
          (values (mod (+ r1 (* m1 t0)) lcm) lcm))
        (values nil nil))))

  (defun floor-sum (n m a b)
  "sum_{i=0}^{n-1} floor((a*i+b)/m)。ACL準拠。n,m正整数, a,b任意整数。"
  (labels ((fsu (n m a b)
             (loop with ans of-type integer = 0
                   do (when (>= a m)
                        (incf ans (* (/ (* n (1- n)) 2) (floor a m)))
                        (setf a (mod a m)))
                      (when (>= b m)
                        (incf ans (* n (floor b m)))
                        (setf b (mod b m)))
                      (let ((y-max (+ (* a n) b)))
                        (if (< y-max m)
                            (return ans)
                            (setf n (floor y-max m)
                                  b (mod y-max m)
                                  m (prog1 a (setf a m))))))))
    (cond ((and (>= a 0) (>= b 0))
           (fsu n m a b))
          (t
           ;; 商の線形性: a = m*qa + ra, b = m*qb + rb (ra,rb∈[0,m)) のとき
           ;; floor((a*i+b)/m) = qa*i + qb + floor((ra*i+rb)/m) (qa,qb整数)
           (let ((qa (floor a m)) (qb (floor b m))
                 (ra (mod a m)) (rb (mod b m)))
             (+ (* qa (/ (* n (1- n)) 2))
                (* n qb)
                (fsu n m ra rb)))))))

;;; ============ :cpl-io ============
