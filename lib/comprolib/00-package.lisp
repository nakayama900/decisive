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

