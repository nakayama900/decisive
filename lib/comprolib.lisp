;;; comprolib.lisp — 競技プログラミング用 Common Lisp ライブラリ (SBCL 想定)
;;;
;;; (decisive 同梱版: system 経由で読む場合は全 feature を自動有効化)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :cpl-all *features*))
;;;
;;; 使い方: 使う機能の feature を *features* に push してから
;;; compile-file / load する。
;;;
;;;   (pushnew :cpl-dsu *features*)          ; DSU だけ欲しい時
;;;   (pushnew :cpl-all *features*)          ; 全部欲しい時
;;;   (compile-file "comprolib.lisp")        ; -> comprolib.fasl
;;;   (load "comprolib.fasl")
;;;
;;; 指定していない機能は #+ リーダー条件付きで skip されるため、
;;; 読み込まれもコンパイルされもしない (= FASL に載らない)。
;;; 機能間に依存はないのでどの組み合わせでも単体で使える。
;;;
;;; feature 一覧:
;;;   :cpl-math    extgcd / pow-mod / inv-mod / prime-p / factorize / sieve
;;;   :cpl-combi   make-combi / comb-nck   (nCk mod 素数, 前計算 O(n))
;;;   :cpl-dsu     Union-Find (経路圧縮 + union by rank)
;;;   :cpl-fenwick Fenwick 木 (BIT)
;;;   :cpl-segtree 点更新・区間取得セグ木 (op / e を関数で渡す)
;;;   :cpl-string  z-algorithm / ローリングハッシュ (2重)
;;;   :cpl-graph   dijkstra (二分ヒープ) / LCA (ダブリング) / SCC (Kosaraju)
;;;   :cpl-all     上記すべて
;;;
;;; 常時有効 (ごく小さいのでゲート外): chmax / chmin マクロ

(defpackage :comprolib
  (:use #:cl)
  (:export #:chmax #:chmin
           ;; math
           #:extgcd #:pow-mod #:inv-mod #:prime-p #:factorize #:sieve
           #:crt #:crt2 #:floor-sum
           ;; combi
           #:make-combi #:comb-nck
           ;; dsu
           #:make-dsu #:dsu-find #:dsu-same #:dsu-union #:dsu-size #:dsu-groups
           ;; fenwick
           #:make-fenwick #:fw-add #:fw-prefix-sum #:fw-sum
           ;; segtree
           #:make-segtree #:seg-set #:seg-get #:seg-prod #:seg-all-prod
           #:make-lazysegtree #:lseg-set #:lseg-get #:lseg-prod #:lseg-apply #:lseg-all-prod
           ;; string
           #:z-algorithm #:make-rhash #:rh-get #:rh-equal
           #:suffix-array #:lcp-array
           #:make-trie #:trie-insert #:trie-find #:trie-prefix-count
           ;; graph
           #:dijkstra #:make-lca #:lca #:lca-dist #:scc #:make-pheap #:pheap-empty #:pheap-push #:pheap-pop #:pheap-peek
           #:bfs #:bfs-grid #:floyd-warshall
           #:make-mf-graph #:mf-add-edge #:mf-flow
           #:make-mcf-graph #:mcf-add-edge #:mcf-flow
           ;; io
           #:input* #:def-input-reader #:print-yes-no #:read-int-line #:read-lines
           ;; container (queues/trees/pileupラッパ)
           #:make-queue #:queue-push #:queue-pop #:queue-peek #:queue-size #:queue-empty-p
           #:make-deque #:deq-push-front #:deq-push-back #:deq-pop-front #:deq-pop-back #:deq-peek-front #:deq-peek-back #:deq-empty-p #:deq-size
           #:make-ordered-set #:oset-insert #:oset-find #:oset-delete #:oset-size #:oset-min #:oset-max #:oset-lower-bound #:oset-upper-bound
           #:make-multiset #:ms-insert #:ms-delete #:ms-size #:ms-min #:ms-pop-min
           #:make-lib-heap #:lheap-push #:lheap-pop #:lheap-top #:lheap-empty-p
           ;; util
           #:iota #:argmax #:argmin #:run-length-encode #:next-permutation
           ;; search
           #:binary-search #:lower-bound #:upper-bound
           ;; cumulative
           #:make-cumsum #:cumsum-range #:make-cumsum2 #:cumsum2-range
           ;; modint
           #:define-modint #:mint-val
           ;; convolution
           #:convolution-mod #:convolution-fft))

(in-package :comprolib)

;;; ============ 共通 (常に有効・ごく小さい) ============

(defmacro chmax (place val)
  "place < val なら place を val に更新して T, そうでなければ NIL。"
  (let ((v (gensym "V")))
    `(let ((,v ,val))
       (when (> ,v ,place)
         (setf ,place ,v)
         t))))

(defmacro chmin (place val)
  "place > val なら place を val に更新して T, そうでなければ NIL。"
  (let ((v (gensym "V")))
    `(let ((,v ,val))
       (when (< ,v ,place)
         (setf ,place ,v)
         t))))

;;; ============ :cpl-math ============
#+(or :cpl-all :cpl-math)
(progn

  (defun extgcd (a b)
    "多値 (g x y): a*x + b*y = g = gcd(a,b) を満たすもの。"
    (if (zerop b)
        (values a 1 0)
        (multiple-value-bind (g x y) (extgcd b (mod a b))
          (values g y (- x (* (floor a b) y))))))

  (defun pow-mod (a n mod)
    "a^n mod mod (n >= 0)。"
    (declare (type integer a n mod))
    (loop with r = 1
          for b = (mod a mod) then (mod (* b b) mod)
          for e = n then (ash e -1)
          until (zerop e)
          when (logtest e 1)
            do (setf r (mod (* r b) mod))
          finally (return r)))

  (defun inv-mod (a mod)
    "a の mod における逆元 (gcd(a,mod)=1 必須)。mod が素数なら pow-mod でも可。"
    (multiple-value-bind (g x) (extgcd a mod)
      (assert (= g 1) (a mod) "inv-mod: gcd(~A, ~A) = ~A ≠ 1" a mod g)
      (mod x mod)))

  (defun prime-p (n)
    "決定的 Miller-Rabin (64bit 範囲で正確)。"
    (if (< n 2)
        nil
        (let ((bases '(2 3 5 7 11 13 17 19 23 29 31 37)))
          (cond ((member n bases) t)
                ((some (lambda (p) (zerop (mod n p))) bases) nil)
                (t
                 (let ((d (- n 1)) (s 0))
                   (loop while (evenp d)
                         do (setf d (ash d -1))
                            (incf s))
                   (loop for a in bases
                         always (let ((x (pow-mod a d n)))
                                  (or (= x 1)
                                      (= x (- n 1))
                                      (loop for k below (- s 1)
                                            do (setf x (mod (* x x) n))
                                            thereis (= x (- n 1))))))))))))

  (defun factorize (n)
    "素因数分解 (試し割り)。昇順の (素因数 . 指数) の alist。"
    (let ((res '()))
      (flet ((push-p (p)
               (let ((cell (assoc p res)))
                 (if cell
                     (incf (cdr cell))
                     (push (cons p 1) res)))))
        (loop for p = 2 then (if (= p 2) 3 (+ p 2))
              while (<= (* p p) n)
              do (loop while (zerop (mod n p))
                       do (push-p p)
                          (setf n (floor n p))))
        (when (> n 1)
          (push-p n))
        (nreverse res))))

  (defun sieve (n)
    "n 以下の素数のリスト (エラトステネス)。"
    (let ((flags (make-array (+ n 1) :element-type 'bit :initial-element 0))
          (res '()))
      (loop for i from 2 to n
            when (zerop (aref flags i))
              do (push i res)
                 (loop for j from (* i i) to n by i
                       do (setf (aref flags j) 1)))
      (nreverse res))))


;;; ============ :cpl-combi ============
#+(or :cpl-all :cpl-combi)
(progn

  (defstruct (combi (:constructor %make-combi (fact inv-fact mod)))
    fact inv-fact mod)

  (defun make-combi (n mod)
    "nCk mod mod を O(1) で返すための前計算 (O(n))。mod は素数前提。"
    (let ((fact (make-array (+ n 1) :element-type 'integer))
          (inv  (make-array (+ n 1) :element-type 'integer)))
      (setf (aref fact 0) 1)
      (loop for i from 1 to n
            do (setf (aref fact i) (mod (* (aref fact (- i 1)) i) mod)))
      ;; 内部 extgcd (math セクション非依存にするためここに置く)
      (flet ((inv% (a)
               (loop with old-r = a and r = mod
                     with old-s = 1 and s = 0
                     until (zerop r)
                     for q = (floor old-r r)
                     do (psetq old-r r r (- old-r (* q r))
                               old-s s s (- old-s (* q s)))
                     finally (return (mod old-s mod)))))
        (setf (aref inv n) (inv% (aref fact n)))
        (loop for i from n downto 1
              do (setf (aref inv (- i 1))
                       (mod (* (aref inv i) i) mod))))
      (%make-combi fact inv mod)))

  (defun comb-nck (c n k)
    "nCk mod mod (前計算済みなら O(1))。範囲外は 0。"
    (let ((lim (- (length (combi-fact c)) 1)))
      (assert (and (<= 0 n lim)) (n) "comb-nck: n=~A は前計算範囲外" n))
    (if (or (< k 0) (< n k))
        0
        (mod (* (aref (combi-fact c) n)
                (aref (combi-inv-fact c) k)
                (aref (combi-inv-fact c) (- n k)))
             (combi-mod c))))
) ; end section


;;; ============ :cpl-dsu ============
#+(or :cpl-all :cpl-dsu)
(progn

  (defstruct (dsu (:constructor %make-dsu (par rnk sz)))
    par rnk sz)

  (defun make-dsu (n)
    "n 頂点の Union-Find。"
    (let ((par (make-array n :element-type 'fixnum))
          (sz  (make-array n :element-type 'fixnum :initial-element 1)))
      (loop for i below n
            do (setf (aref par i) i))
      (%make-dsu par
                 (make-array n :element-type 'fixnum :initial-element 0)
                 sz)))

  (defun dsu-find (d x)
    "x の属するグループの代表元。経路圧縮 (path halving)。"
    (let ((par (dsu-par d)))
      (loop while (/= (aref par x) x)
            do (setf (aref par x) (aref par (aref par x))
                     x           (aref par x)))
      x))

  (defun dsu-same (d a b)
    (= (dsu-find d a) (dsu-find d b)))

  (defun dsu-union (d a b)
    "a と b を併合。実際に併合したら T、元々同グループなら NIL。"
    (let* ((par (dsu-par d))
           (rnk (dsu-rnk d))
           (sz  (dsu-sz d))
           (ra (dsu-find d a))
           (rb (dsu-find d b)))
      (when (= ra rb)
        (return-from dsu-union nil))
      (when (< (aref rnk ra) (aref rnk rb))
        (rotatef ra rb))
      (setf (aref par rb) ra)
      (incf (aref sz ra) (aref sz rb))
      (when (= (aref rnk ra) (aref rnk rb))
        (incf (aref rnk ra)))
      t))

  (defun dsu-size (d x)
    (aref (dsu-sz d) (dsu-find d x)))

  (defun dsu-groups (d)
    "グループごとの頂点リスト (各グループ内は昇順、グループは先頭要素昇順)。"
    (let* ((par (dsu-par d))
           (n (length par))
           (tmp (make-hash-table)))
      (loop for i below n
            do (push i (gethash (dsu-find d i) tmp)))
      (sort (loop for v being each hash-value of tmp
                  collect (nreverse v))
            #'< :key #'car)))
) ; end section


;;; ============ :cpl-fenwick ============
#+(or :cpl-all :cpl-fenwick)
(progn

  (defstruct (fwt (:constructor %make-fwt (bit)))
    bit)

  (defun make-fenwick (n &optional init-vec)
    "長さ n の Fenwick 木。init-vec (simple-vector/list) を渡すと初期値を積む。"
    (let ((bit (make-array (+ n 1) :element-type 'integer :initial-element 0)))
      (when init-vec
        (loop for x across init-vec
              for i from 0
              do (loop for j = (+ i 1) then (+ j (logand j (- j)))
                       while (<= j n)
                       do (incf (aref bit j) x))))
      (%make-fwt bit)))

  (defun fw-add (f i x)
    "a[i] += x (0-indexed)。"
    (let* ((bit (fwt-bit f))
           (n (- (length bit) 1)))
      (loop for j = (+ i 1) then (+ j (logand j (- j)))
            while (<= j n)
            do (incf (aref bit j) x))))

  (defun fw-prefix-sum (f r)
    "a[0..r) の和。"
    (loop with bit = (fwt-bit f)
          for j = r then (- j (logand j (- j)))
          while (> j 0)
          sum (aref bit j)))

  (defun fw-sum (f l r)
    "a[l..r) の和。"
    (- (fw-prefix-sum f r) (fw-prefix-sum f l)))
) ; end section


;;; ============ :cpl-segtree ============
#+(or :cpl-all :cpl-segtree)
(progn

  (defstruct (segtree (:constructor %make-segtree (n size log d op e)))
    n size log d op e)

  (defun make-segtree (vec op e)
    "点更新・区間取得セグ木。vec: 初期値の vector, op: (S S)->S, e: 単位元。"
    (let* ((n (length vec))
           (log 1))
      (assert (plusp n) (vec) "make-segtree: 空ベクトルは不可")
      (loop while (< (ash 1 log) n)
            do (incf log))
      (let* ((size (ash 1 log))
             (d (make-array (* 2 size))))
        (loop for i below size
              do (setf (aref d (+ size i)) e))
        (loop for i below n
              do (setf (aref d (+ size i)) (aref vec i)))
        (loop for i from (- size 1) downto 1
              do (setf (aref d i)
                       (funcall op (aref d (* 2 i)) (aref d (1+ (* 2 i))))))
        (%make-segtree n size log d op e))))

  (defun seg-set (s p x)
    "a[p] = x。"
    (let* ((d (segtree-d s))
           (op (segtree-op s))
           (i (+ (segtree-size s) p)))
      (setf (aref d i) x)
      (loop for k = (ash i -1) then (ash k -1)
            until (zerop k)
            do (setf (aref d k)
                     (funcall op (aref d (* 2 k)) (aref d (1+ (* 2 k))))))))

  (defun seg-get (s p)
    (aref (segtree-d s) (+ (segtree-size s) p)))

  (defun seg-all-prod (s)
    (aref (segtree-d s) 1))

  (defun seg-prod (s l r)
    "a[l..r) の総積 (op を適用したもの)。"
    (let* ((d (segtree-d s))
           (op (segtree-op s))
           (e (segtree-e s))
           (size (segtree-size s)))
      (loop with a = (+ size l)
            and b = (+ size r)
            and sml = e
            and smr = e
            while (< a b)
            when (logtest a 1)
              do (setf sml (funcall op sml (aref d a))
                       a (+ a 1))
            when (logtest b 1)
              do (setf b (- b 1)
                       smr (funcall op (aref d b) smr))
            do (setf a (ash a -1)
                     b (ash b -1))
            finally (return (funcall op sml smr)))))
) ; end section


;;; ============ :cpl-string ============
#+(or :cpl-all :cpl-string)
(progn

  (defun z-algorithm (s)
    "z[i] = s と s[i..] の最長共通接頭辞長 (z[0] = n)。文字列か vector。"
    (let* ((n (length s))
           (z (make-array n :element-type 'fixnum)))
      (unless (zerop n)
        (setf (aref z 0) n)
        (let ((i 1) (j 0))
          (loop while (< i n)
                do (loop while (and (< (+ i j) n)
                                     (char= (char s j) (char s (+ i j))))
                         do (incf j))
                   (setf (aref z i) j)
                   (cond ((zerop j)
                          (incf i))
                         (t
                          (let ((k 1))
                            (loop while (and (< (+ i k) n)
                                              (< (+ k (aref z k)) j))
                                  do (setf (aref z (+ i k)) (aref z k))
                                     (incf k))
                            (incf i k)
                            (decf j k))))))
        z)))

  (defconstant +rh-m1+ 1000000007)
  (defconstant +rh-m2+ 998244353)
  (defconstant +rh-base+ 1000003)

  (defstruct (rhash (:constructor %make-rhash (n h1 h2 p1 p2)))
    n h1 h2 p1 p2)

  (defun make-rhash (s)
    "2重ローリングハッシュ。rh-equal で部分文字列一致判定。"
    (let* ((n (length s))
           (h1 (make-array (+ n 1) :element-type 'integer :initial-element 0))
           (h2 (make-array (+ n 1) :element-type 'integer :initial-element 0))
           (p1 (make-array (+ n 1) :element-type 'integer :initial-element 1))
           (p2 (make-array (+ n 1) :element-type 'integer :initial-element 1)))
      (loop for c across s
            for i from 0
            do (setf (aref h1 (+ i 1)) (mod (+ (* (aref h1 i) +rh-base+)
                                              (char-code c))
                                            +rh-m1+)
                     (aref h2 (+ i 1)) (mod (+ (* (aref h2 i) +rh-base+)
                                              (char-code c))
                                            +rh-m2+)
                     (aref p1 (+ i 1)) (mod (* (aref p1 i) +rh-base+) +rh-m1+)
                     (aref p2 (+ i 1)) (mod (* (aref p2 i) +rh-base+) +rh-m2+)))
      (%make-rhash n h1 h2 p1 p2)))

  (defun rh-get (rh l r)
    "s[l..r) のハッシュ。多値 (h1 h2)。"
    (let ((k (- r l)))
      (values (mod (- (aref (rhash-h1 rh) r)
                      (* (aref (rhash-h1 rh) l) (aref (rhash-p1 rh) k)))
                   +rh-m1+)
              (mod (- (aref (rhash-h2 rh) r)
                      (* (aref (rhash-h2 rh) l) (aref (rhash-p2 rh) k)))
                   +rh-m2+))))

  (defun rh-equal (rh l1 r1 l2 r2)
    "s[l1..r1) = s[l2..r2) か。"
    (multiple-value-bind (a1 b1) (rh-get rh l1 r1)
      (multiple-value-bind (a2 b2) (rh-get rh l2 r2)
        (and (= a1 a2) (= b1 b2)))))
) ; end section


;;; ============ :cpl-graph ============
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
#+(or :cpl-all :cpl-io)
(progn

  ;; Yes/No 出力
  (defun print-yes-no (x &optional (stream *standard-output*))
    (write-line (if x "Yes" "No") stream)
    (values))

  ;; 1行を読んで空白区切り整数のベクトル
  (defun read-int-line (&optional (stream *standard-input*))
    (let ((parts (str:split " " (string-trim '(#\Space #\Return) (read-line stream)))))
      (map 'vector
           (lambda (s) (parse-integer s :junk-allowed t))
           (remove "" parts :test #'string=))))

  ;; N行読み込み
  (defun read-lines (n &optional (stream *standard-input*))
    (loop repeat n collect (read-line stream)))

  ;; 型付き入力マクロ: (input* ((n fixnum) (a (vector fixnum n))) body...)
  (defvar *input-reader-table* (make-hash-table :test #'eq))

  (defun set-input-reader (marker reader)
    (setf (gethash marker *input-reader-table*) reader))

  (defun get-input-reader (marker) (gethash marker *input-reader-table*))

  (defmacro def-input-reader (marker (specvar) &body body)
    `(set-input-reader ',marker (lambda (,specvar) ,@body)))

  (defun input-expand (forms)
    (mapcar (lambda (form)
              (list (car form)
                    (funcall (or (get-input-reader
                                  (if (listp (cadr form)) (car (cadr form)) (cadr form)))
                                 (lambda (s) (error "Unknown input typespec: ~S" s)))
                             (cadr form))))
            forms))

  (def-input-reader fixnum (s) '(read))
  (def-input-reader integer (s) '(read))
  (def-input-reader string (s) '(read-line))
  (def-input-reader list (s) `(loop repeat ,(caddr s) collect (read)))
  (def-input-reader vector (s) `(make-array ,(caddr s) :initial-contents
                                            (loop repeat ,(caddr s) collect (read))))
  (def-input-reader array (s)
    (destructuring-bind (elem dims) (cdr s)
      (if (listp dims)
          `(make-array ',dims :initial-contents
                       (loop for i below ,(first dims)
                             collect (loop repeat ,(second dims) collect (read))))
          `(make-array ,dims :initial-contents (loop repeat ,dims collect (read))))))
  (def-input-reader line (s) `(read-line))

  (defmacro input* (forms &body body)
    "競プロ向け入力束縛: (input* ((n fixnum) (a (vector fixnum n))) ...)"
    (let ((vars (mapcar #'car forms)))
      `(let* ,(input-expand forms)
         (declare (ignorable ,@vars))
         ,@body)))
) ; end section

;;; ============ :cpl-container ============
#+(or :cpl-all :cpl-container)
(progn

  ;; --- queue: queuesラッパ (FIFO) ---
  (defun make-queue ()
    (queues:make-queue :simple-queue))
  (defun queue-push (q x) (queues:qpush q x) x)
  (defun queue-pop (q) (queues:qpop q))
  (defun queue-peek (q) (queues:qtop q))
  (defun queue-size (q) (queues:qsize q))
  (defun queue-empty-p (q) (zerop (queues:qsize q)))

  ;; --- deque: ring buffer ---
  (defstruct (deq (:constructor %make-deq (buf front back size)))
    buf front back size)
  (defun make-deque (&optional (cap 16))
    (%make-deq (make-array cap) 0 0 0))
  (defun deq-grow (d)
    (let* ((old (deq-buf d)) (n (length old))
           (new (make-array (* 2 n))))
      (loop for i below (deq-size d)
            do (setf (aref new i)
                     (aref old (mod (+ (deq-front d) i) n))))
      (setf (deq-buf d) new (deq-front d) 0 (deq-back d) (deq-size d))))
  (defun deq-push-back (d x)
    (when (= (deq-size d) (length (deq-buf d))) (deq-grow d))
    (setf (aref (deq-buf d) (deq-back d)) x
          (deq-back d) (mod (1+ (deq-back d)) (length (deq-buf d)))
          (deq-size d) (1+ (deq-size d)))
    x)
  (defun deq-push-front (d x)
    (when (= (deq-size d) (length (deq-buf d))) (deq-grow d))
    (setf (deq-front d) (mod (1- (deq-front d)) (length (deq-buf d)))
          (aref (deq-buf d) (deq-front d)) x
          (deq-size d) (1+ (deq-size d)))
    x)
  (defun deq-pop-front (d)
    (prog1 (aref (deq-buf d) (deq-front d))
      (setf (deq-front d) (mod (1+ (deq-front d)) (length (deq-buf d)))
            (deq-size d) (1- (deq-size d)))))
  (defun deq-pop-back (d)
    (setf (deq-back d) (mod (1- (deq-back d)) (length (deq-buf d))))
    (prog1 (aref (deq-buf d) (deq-back d))
      (setf (deq-size d) (1- (deq-size d)))))
  (defun deq-peek-front (d) (aref (deq-buf d) (deq-front d)))
  (defun deq-peek-back (d)
    (aref (deq-buf d) (mod (1- (deq-back d)) (length (deq-buf d)))))
  (defun deq-empty-p (d) (zerop (deq-size d)))

  ;; --- ordered-set / multiset: trees (AVL) ラッパ ---
  (defstruct (oset (:constructor %make-oset (tree)))
    tree)
  (defun make-ordered-set (&key (order #'<))
    (%make-oset (trees:make-binary-tree :avl order :key #'identity :test #'eql)))
  (defun oset-insert (s x) (trees:insert x (oset-tree s)) x)
  (defun oset-find (s x) (trees:find x (oset-tree s)))
  (defun oset-delete (s x) (trees:delete x (oset-tree s)) x)
  (defun oset-size (s) (trees:size (oset-tree s)))
  (defun oset-min (s) (trees:minimum (oset-tree s)))
  (defun oset-max (s) (trees:maximum (oset-tree s)))
  (defun oset-lower-bound (s x) (trees:lower-bound x (oset-tree s)))
  (defun oset-upper-bound (s x) (trees:upper-bound x (oset-tree s)))

  (defstruct (mset (:constructor %make-mset (tree cnts)))
    tree cnts)
  (defun make-multiset ()
    ;; trees(AVL)でキー管理 + hashで同キー個数
    (%make-mset (make-ordered-set)
                (make-hash-table :test #'eql)))
  (defun ms-insert (m x)
    (let ((c (gethash x (mset-cnts m) 0)))
      (when (zerop c) (oset-insert (mset-tree m) x))
      (setf (gethash x (mset-cnts m)) (1+ c))
      x))
  (defun ms-delete (m x)
    "x を1個削除 (存在時)。返り値: 削除したら T"
    (let ((c (gethash x (mset-cnts m) 0)))
      (when (plusp c)
        (if (= c 1)
            (progn (remhash x (mset-cnts m))
                   (oset-delete (mset-tree m) x))
            (setf (gethash x (mset-cnts m)) (1- c)))
        t)))
  (defun ms-size (m)
    (loop for v being the hash-values of (mset-cnts m) sum v))
  (defun ms-min (m) (oset-min (mset-tree m)))
  (defun ms-pop-min (m)
    (let ((x (oset-min (mset-tree m))))
      (ms-delete m x)
      x))
;; --- heap: pileup ラッパ ---
  (defun make-lib-heap (&key (order #'<))
    (pileup:make-heap order))
  (defun lheap-push (h x) (pileup:heap-insert x h) x)
  (defun lheap-pop (h) (pileup:heap-pop h))
  (defun lheap-top (h) (pileup:heap-top h))
  (defun lheap-empty-p (h) (pileup:heap-empty-p h))
) ; end section

;;; ============ :cpl-util ============
#+(or :cpl-all :cpl-util)
(progn

  (defun iota (n &optional (start 0) (step 1))
    (loop for i below n for v = start then (+ v step) collect v))

  (defun argmax (fn seq)
    "fn最大の要素。複数は最初。多値 (要素 値)。"
    (loop with best with best-val = most-negative-double-float
          for x across (coerce seq 'vector)
          for v = (funcall fn x)
          when (> v best-val) do (setf best x best-val v)
          finally (return (values best best-val))))

  (defun argmin (fn seq)
    (loop with best with best-val = most-positive-double-float
          for x across (coerce seq 'vector)
          for v = (funcall fn x)
          when (< v best-val) do (setf best x best-val v)
          finally (return (values best best-val))))

  (defun run-length-encode (seq)
    "((要素 . 連長) ...) のリスト"
    (loop with res = nil
          for x across (coerce seq 'vector)
          if (and res (eql (caar res) x))
            do (incf (cdar res))
          else
            do (push (cons x 1) res)
          finally (return (nreverse res))))

  (defun next-permutation (vec)
    "vec を破壊的に次の順列へ。最後なら NIL。"
    (let ((n (length vec)) (i (- (length vec) 2)))
      (loop while (and (>= i 0) (>= (aref vec i) (aref vec (1+ i)))) do (decf i))
      (when (< i 0) (return-from next-permutation nil))
      (let ((j (1- n)))
        (loop while (<= (aref vec j) (aref vec i)) do (decf j))
        (rotatef (aref vec i) (aref vec j))
        (do ((l (1+ i)) (r (1- n)))
            ((>= l r))
          (rotatef (aref vec l) (aref vec r))
          (incf l) (decf r))
        t)))
) ; end section

;;; ============ :cpl-search ============
#+(or :cpl-all :cpl-search)
(progn

  (defun binary-search (ok ng pred)
    "meguru法: pred(ok)=T, pred(ng)=NIL の前提で境界を探す。返り値 ok。"
    (loop while (> (abs (- ok ng)) 1)
          do (let ((mid (floor (+ ok ng) 2)))
               (if (funcall pred mid)
                   (setf ok mid)
                   (setf ng mid))))
    ok)

  ;; ソート済みベクトル上の二分
  (defun lower-bound (vec x &key (order #'<))
    "x以上の最初のindex"
    (let ((lo 0) (hi (length vec)))
      (loop while (< lo hi)
            for mid = (floor (+ lo hi) 2)
            if (funcall order (aref vec mid) x)
              do (setf lo (1+ mid))
            else
              do (setf hi mid))
      lo))

  (defun upper-bound (vec x &key (order #'<))
    "xより大きい最初のindex"
    (let ((lo 0) (hi (length vec)))
      (loop while (< lo hi)
            for mid = (floor (+ lo hi) 2)
            if (funcall order x (aref vec mid))
              do (setf hi mid)
            else
              do (setf lo (1+ mid)))
      lo))
) ; end section

;;; ============ :cpl-cum ============
#+(or :cpl-all :cpl-cum)
(progn

  (defun make-cumsum (vec)
    (let* ((n (length vec))
           (acc (make-array (1+ n) :element-type 'integer :initial-element 0)))
      (loop for i below n do (setf (aref acc (1+ i)) (+ (aref acc i) (aref vec i))))
      acc))

  (defun cumsum-range (acc l r)   ; [l, r) 0-indexed
    (- (aref acc r) (aref acc l)))

  (defun make-cumsum2 (grid h w)
    "2次元累積和 (h+1)×(w+1)"
    (let ((acc (make-array (list (1+ h) (1+ w)) :element-type 'integer :initial-element 0)))
      (loop for i below h
            do (loop for j below w
                     do (setf (aref acc (1+ i) (1+ j))
                              (+ (aref grid i j)
                                 (aref acc i (1+ j))
                                 (aref acc (1+ i) j)
                                 (- (aref acc i j))))))
      acc))

  (defun cumsum2-range (acc r1 c1 r2 c2) ; [r1,r2)×[c1,c2)
    (- (+ (aref acc r2 c2) (aref acc r1 c1))
       (aref acc r1 c2) (aref acc r2 c1)))
) ; end section

;;; ============ :cpl-ordered ============
; ordered-set/multiset は :cpl-container 側へ統合 (treesラッパ)
; このセクションは共通の再exportのみ
#+(or :cpl-all :cpl-ordered)
(progn
  (defun %cpl-ordered-placeholder () t)
) ; end section

;;; ============ :cpl-modint ============
#+(or :cpl-all :cpl-modint)
(progn

  ;; 環境: 各mod用の型をマクロで生成
  (defmacro define-modint (name mod)
  "NAME mod型を定義。 (define-modint mint 998244353) → make-mint / mint-v / mint-+ など。
演算は関数: mint-add mint-sub mint-mul mint-div mint-pow (NAME-接頭辞)"
  (let* ((pkg (find-package :comprolib))
         (acc (intern (format nil "~A-V" name) pkg))
         (maker (intern (format nil "MAKE-~A" name) pkg))
         (add (intern (format nil "~A-ADD" name) pkg))
         (sub (intern (format nil "~A-SUB" name) pkg))
         (mul (intern (format nil "~A-MUL" name) pkg))
         (div (intern (format nil "~A-DIV" name) pkg))
         (powf (intern (format nil "~A-POW" name) pkg)))
    `(progn
       (defstruct (,name (:constructor ,(intern (format nil "%MAKE-~A" name) pkg) (v)))
         (v 0 :type integer))
       (defun ,maker (x) (,(intern (format nil "%MAKE-~A" name) pkg) (mod x ,mod)))
       (defun ,add (a b) (,maker (mod (+ (,acc a) (,acc b)) ,mod)))
       (defun ,sub (a b) (,maker (mod (- (,acc a) (,acc b)) ,mod)))
       (defun ,mul (a b) (,maker (mod (* (,acc a) (,acc b)) ,mod)))
       (defun ,div (a b) (,maker (mod (* (,acc a) (pow-mod (,acc b) (- ,mod 2) ,mod)) ,mod)))
       (defun ,powf (a n) (,maker (pow-mod (,acc a) n ,mod))))))

(defun mint-val (m)
    "modint構造体から生の整数値を取り出す (共通accessor)"
    (slot-value m 'v))
) ; end section

;;; ============ :cpl-lazysegtree ============
#+(or :cpl-all :cpl-lazysegtree)
(progn

  ;; 点更新+区間取得+区間作用。S=値, F=作用素。
  ;; make-lazysegtree n op e mapping composition id f
  (defstruct (lseg (:constructor %make-lseg
                     (&key n size log d lz op e mapping composition idf)))
    n size log d lz op e mapping composition idf)

  (defun make-lazysegtree (vec op e mapping composition id)
    (let* ((n (length vec))
           (log (max 1 (ceiling (log (max 1 n) 2))))
           (size (ash 1 log))
           (d (make-array (* 2 size)))
           (lz (make-array size)))
      (loop for i below n
            do (setf (aref d (+ size i)) (aref vec i)))
      (loop for i from (- size 1) downto 1
            do (setf (aref d i) (funcall op (aref d (* 2 i)) (aref d (1+ (* 2 i))))))
      (loop for i below size
            do (setf (aref lz i) id))
      (%make-lseg :n n :size size :log log :d d :lz lz
                  :op op :e e :mapping mapping :composition composition :idf id))))

  (defun lseg-all-apply (s k f)
    (setf (aref (lseg-d s) k)
          (funcall (lseg-mapping s) f (aref (lseg-d s) k)))
    (when (< k (lseg-size s))
      (setf (aref (lseg-lz s) k)
            (funcall (lseg-composition s) f (aref (lseg-lz s) k)))))

  (defun lseg-push (s k)
    (lseg-all-apply s (* 2 k) (aref (lseg-lz s) k))
    (lseg-all-apply s (1+ (* 2 k)) (aref (lseg-lz s) k))
    (setf (aref (lseg-lz s) k) (lseg-idf s)))

  (defun lseg-set (s p x)
    (incf p (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (lseg-push s (ash p (- i))))
    (setf (aref (lseg-d s) p) x)
    (loop for i from 1 to (lseg-log s)
          do (setf (aref (lseg-d s) (ash p (- i)))
                   (funcall (lseg-op s)
                            (aref (lseg-d s) (* 2 (ash p (- i))))
                            (aref (lseg-d s) (1+ (* 2 (ash p (- i)))))))))

  (defun lseg-get (s p)
    (incf p (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (lseg-push s (ash p (- i))))
    (aref (lseg-d s) p))

  (defun lseg-prod (s l r)
    (when (= l r) (return-from lseg-prod (lseg-e s)))
    (incf l (lseg-size s)) (incf r (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (progn
               (when (/= 0 (logand l (1- (ash 1 i))))
                     (lseg-push s (ash l (- i))))
               (when (/= 0 (logand r (1- (ash 1 i))))
                     (lseg-push s (ash (1- r) (- i))))))
    (let ((sml (lseg-e s)) (smr (lseg-e s)))
      (loop while (< l r)
            do (progn
                 (when (logtest l 1)
                   (setf sml (funcall (lseg-op s) sml (aref (lseg-d s) l)))
                   (incf l))
                 (when (logtest r 1)
                   (decf r)
                   (setf smr (funcall (lseg-op s) (aref (lseg-d s) r) smr)))
                 (setf l (ash l -1) r (ash r -1))))
      (funcall (lseg-op s) sml smr)))

  (defun lseg-all-prod (s) (aref (lseg-d s) 1))

  (defun lseg-apply (s l r f)
    (when (= l r) (return-from lseg-apply))
    (incf l (lseg-size s)) (incf r (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (progn
               (when (/= 0 (logand l (1- (ash 1 i))))
                     (lseg-push s (ash l (- i))))
               (when (/= 0 (logand r (1- (ash 1 i))))
                     (lseg-push s (ash (1- r) (- i))))))
    (let ((l2 l) (r2 r))
      (loop while (< l2 r2)
            do (progn
                 (when (logtest l2 1)
                   (lseg-all-apply s l2 f) (incf l2))
                 (when (logtest r2 1)
                   (decf r2) (lseg-all-apply s r2 f))
                 (setf l2 (ash l2 -1) r2 (ash r2 -1)))))
    (loop for i from 1 to (lseg-log s)
          do (progn
               (when (/= 0 (logand l (1- (ash 1 i))))
                     (setf (aref (lseg-d s) (ash l (- i)))
                           (funcall (lseg-op s)
                                    (aref (lseg-d s) (* 2 (ash l (- i))))
                                    (aref (lseg-d s) (1+ (* 2 (ash l (- i))))))))
               (when (/= 0 (logand r (1- (ash 1 i))))
                     (setf (aref (lseg-d s) (ash (1- r) (- i)))
                           (funcall (lseg-op s)
                                    (aref (lseg-d s) (* 2 (ash (1- r) (- i))))
                                    (aref (lseg-d s) (1+ (* 2 (ash (1- r) (- i))))))))))
) ; end section

;;; ============ :cpl-convolution ============
#+(or :cpl-all :cpl-convolution)
(progn

  (defparameter *ntt-mod* 998244353)
  (defparameter *ntt-g* 3)

  (defun ntt (a invert-p)
    "a をNTT (破壊的)。mod = *ntt-mod*。"
    (let ((n (length a)) (m *ntt-mod*))
      ;; bit reversal
      (loop for i from 0 below n
            with j = 0
            do (let ((jj 0) (k i))
                 (loop for b = (ash n -1) then (ash b -1)
                       while (> b 0)
                       do (setf jj (ash jj 1)
                                jj (logior jj (logand k 1))
                                k (ash k -1)))
                 (when (< i jj)
                   (rotatef (aref a i) (aref a jj)))))
      (loop for len = 2 then (* len 2)
            while (<= len n)
            do (let ((wlen (pow-mod *ntt-g*
                                    (let ((e (floor (- m 1) len)))
                                      (if invert-p (- (- m 1) e) e))
                                    m)))
                 (loop for i from 0 below n by len
                       do (loop for j below (ash len -1)
                                with w = 1
                                do (let ((u (aref a (+ i j)))
                                         (v (mod (* w (aref a (+ i j (ash len -1)))) m)))
                                     (setf (aref a (+ i j)) (mod (+ u v) m)
                                           (aref a (+ i j (ash len -1))) (mod (- u v) m)
                                           w (mod (* w wlen) m)))))))
      (when invert-p
        (let ((ninv (inv-mod n m)))
          (loop for i below n
                do (setf (aref a i) (mod (* (aref a i) ninv) m)))))
      a))

  (defun convolution-mod (a b)
    "整数ベクトル2つの畳み込み mod 998244353 (NTT)。"
    (let* ((na (length a)) (nb (length b)))
      (when (or (zerop na) (zerop nb))
        (return-from convolution-mod (make-array 0)))
      (let* ((res-n (+ na nb -1))
             (n (ash 1 (ceiling (log res-n 2))))
             (fa (make-array n :initial-element 0))
             (fb (make-array n :initial-element 0)))
        (loop for i below na do (setf (aref fa i) (mod (aref a i) *ntt-mod*)))
        (loop for i below nb do (setf (aref fb i) (mod (aref b i) *ntt-mod*)))
        (ntt fa nil)
        (ntt fb nil)
        (loop for i below n do (setf (aref fa i) (mod (* (aref fa i) (aref fb i)) *ntt-mod*)))
        (ntt fa t)
        (make-array res-n
                    :initial-contents
                    (loop for i below res-n collect (aref fa i))))))

  ;; gsll/grid (GSL) が必要な FFT 畳み込みは :cpl-gsll が有効な時のみ定義。
  ;; decisive 同梱版では gsll を optional にしているため、無い環境では
  ;; convolution-fft は未定義 (convolution-mod は NTT で常時使える)。
  #+(and :cpl-gsll (or :cpl-all :cpl-convolution))
  (defun convolution-fft (a b)
  "実数ベクトル2つの畳み込み (倍精度・誤差あり)。gsllのFFT利用。整数に丸めて返す。"
  (let* ((na (length a))
         (nb (length b)))
    (when (or (zerop na) (zerop nb))
      (return-from convolution-fft (make-array 0)))
    (let* ((res-n (+ na nb -1))
           (n (ash 1 (ceiling (log res-n 2))))
           (fa (grid:make-foreign-array '(complex double-float)
                                        :initial-contents
                                        (loop for i below n
                                              collect (complex
                                                       (if (< i na)
                                                           (coerce (aref a i) 'double-float)
                                                           0d0)
                                                       (if (< i nb)
                                                           (coerce (aref b i) 'double-float)
                                                           0d0))))))
      (let ((fft (gsll:forward-discrete-fourier-transform fa)))
        (let ((inv-arr
                (grid:make-foreign-array
                  '(complex double-float)
                  :initial-contents
                  (loop for i below n
                        for x = (grid:gref fft i)
                        for xj = (grid:gref fft (mod (- n i) n))
                        collect (* (/ #c(-0.0d0 -0.5d0) (* 2 n))
                                   (+ x (conjugate xj))
                                   (- x (conjugate xj)))))))
          (let ((res (gsll:backward-discrete-fourier-transform inv-arr)))
            (make-array res-n
                        :initial-contents
                        (loop for i below res-n
                              collect (round (realpart (grid:gref res i))))))))))))


;;; ============ :cpl-flow ============
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
#+(or :cpl-all :cpl-2sat)
(progn

  (defstruct (two-sat (:constructor %make-2sat (n g ans)))
    n g ans)
  (defun make-two-sat (n)
    (%make-2sat n (make-array (* 2 n) :initial-element nil)
                (make-array n :element-type 'bit :initial-element 0)))

  

  ;; ↑の複雑さを避け、直接実装:
  (defun 2sat-add (ts i xi j xj)
    "節 (xi ∨ xj) を追加。i,j は変数index、xi/xj は真理値(その変数が真である含意)。"
    (let ((g (two-sat-g ts)) (n (two-sat-n ts)))
      ;; ¬xi → xj, ¬xj → xi  (ノードk=x_k真, ノードn+k=x_k偽)
      ;; 出発はリテラルの否定ノード、到着はリテラルのノードindexを直接push
      (push (+ (if xj 0 n) j) (aref g (+ (if xi n 0) i)))
      (push (+ (if xi 0 n) i) (aref g (+ (if xj n 0) j)))
      ts))

  (defun 2sat-solve (ts)
    "充足可能なら T して答えを two-sat-ans に格納。不可なら NIL。"
    (let* ((n (two-sat-n ts)) (g (two-sat-g ts))
           (comp (make-array (* 2 n) :element-type 'integer :initial-element -1))
           (order (make-array 0 :adjustable t :fill-pointer 0))
           (low (make-array (* 2 n) :element-type 'integer))
           (num (make-array (* 2 n) :element-type 'integer :initial-element 0))
           (idx 0) (nc 0)
           (stack nil))
      (declare (ignorable order))
      (labels ((dfs (v)
                 (setf (aref low v) (incf idx) (aref num v) idx)
                 (push v stack)
                 (dolist (e (aref g v))
                   (let ((to e))  ; 辺は到着ノードindex (整数) を直接格納
                     (cond ((zerop (aref num to))
                            (dfs to)
                            (setf (aref low v) (min (aref low v) (aref low to))))
                           ((= (aref comp to) -1)
                            (setf (aref low v) (min (aref low v) (aref num to)))))))
                 (when (= (aref low v) (aref num v))
                   (loop for u = (pop stack)
                         do (setf (aref comp u) nc)
                         until (= u v))
                   (incf nc))))
        (loop for v below (* 2 n)
              when (zerop (aref num v)) do (dfs v)))
      ;; TarjanのSCC番号は逆トポ順 (シンク側=トポ順で後が先に確定し小さい番号)。
      ;; ACL規約「トポ順で後のSCCが真」より comp[i] < comp[n+i] なら x_i = T。
      (loop for i below n
            when (= (aref comp i) (aref comp (+ i n)))
              do (return-from 2sat-solve nil))
      (loop for i below n
            do (setf (aref (two-sat-ans ts) i)
                     (if (< (aref comp i) (aref comp (+ i n))) 1 0)))
      t))
) ; end section

;;; ============ :cpl-suffix ============
#+(or :cpl-all :cpl-suffix)
(progn

  ;; suffix array (O(n log n) doubling)
  (defun suffix-array (s)
    "s: 文字列 or ベクトル。suffix array (開始位置の昇順ベクトル)"
    (let* ((n (length s))
           (sa (make-array n :initial-contents (iota n)))
           (rank (make-array n :initial-contents
                             (map 'vector #'char-code (coerce s 'vector))))
           (tmp (make-array n :element-type 'integer)))
      (loop for k = 1 then (* k 2)
            while (< k n)
            do (let ((cmp (lambda (a b)
                            (or (< (aref rank a) (aref rank b))
                                (and (= (aref rank a) (aref rank b))
                                     (< (if (< (+ a k) n) (aref rank (+ a k)) -1)
                                        (if (< (+ b k) n) (aref rank (+ b k)) -1)))))))
                 (setf sa (sort sa cmp))
                 (let ((new (make-array n :element-type 'integer)))
                   (setf (aref new (aref sa 0)) 0)
                   (loop for i from 1 below n
                         do (setf (aref new (aref sa i))
                                  (+ (aref new (aref sa (1- i)))
                                     (if (funcall cmp (aref sa (1- i)) (aref sa i)) 1 0))))
                   (setf rank new))))
      sa))

  ;; Kasai's algorithm
  (defun lcp-array (s sa)
    (let* ((n (length s))
           (rank (make-array n :element-type 'integer))
           (lcp (make-array (max 0 (1- n)) :element-type 'integer :initial-element 0))
           (h 0))
      (loop for i below n do (setf (aref rank (aref sa i)) i))
      (loop for i below n
            when (> (aref rank i) 0)
              do (let ((j (aref sa (1- (aref rank i)))))
                   (loop while (and (< (+ i h) n) (< (+ j h) n)
                                     (eql (aref s (+ i h)) (aref s (+ j h))))
                         do (incf h))
                   (setf (aref lcp (1- (aref rank i))) h)
                   (when (> h 0) (decf h))))
      lcp))
) ; end section

;;; ============ :cpl-trie ============
#+(or :cpl-all :cpl-trie)
(progn

  (defstruct (trie-node (:constructor make-trie-node (children terminal count)))
    children terminal count)
  (defstruct (trie (:constructor %make-trie (root)))
    root)

  (defun make-trie ()
    (%make-trie (make-trie-node (make-hash-table) nil 0)))

  (defun trie-insert (tr s)
    (let ((node (trie-root tr)))
      (incf (trie-node-count node))
      (loop for ch across s
            do (let ((next (or (gethash ch (trie-node-children node))
                               (let ((nn (make-trie-node (make-hash-table) nil 0)))
                                 (setf (gethash ch (trie-node-children node)) nn)
                                 nn))))
                 (setf node next)
                 (incf (trie-node-count node))))
      (setf (trie-node-terminal node) t)
      tr))

  (defun trie-find (tr s)
    "s が登録済み単語なら T"
    (let ((node (trie-root tr)))
      (loop for ch across s
            for next = (gethash ch (trie-node-children node))
            while next
            do (setf node next)
            finally (return (and (trie-node-terminal node) t)))))

  (defun trie-prefix-count (tr s)
    "s を接頭辞とする登録単語の数 (経路count)"
    (let ((node (trie-root tr)))
      (loop for ch across s
            for next = (gethash ch (trie-node-children node))
            while next
            do (setf node next)
            finally (return (trie-node-count node)))))
) ; end section
