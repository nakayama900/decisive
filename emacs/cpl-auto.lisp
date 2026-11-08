;;; cpl-auto.lisp — comprolib 依存自動解決
;;;
;;; ユーザファイル内の (comprolib:xxx ...) 呼び出しを走査し、
;;; 必要最小限の feature を自動算出して pushnew + ライブラリロードまで行う。
;;; 手書きの (pushnew :cpl-dsu *features*) は不要になる。
;;;
;;; 使い方 (solve.lisp 冒頭):
;;;   (load "~/.hermes/workspace/comprolib-lisp/cpl-auto.lisp")
;;;   (cpl-auto:setup)          ; *load-truename* (このファイル自身) を走査
;;;
;;; または明示的に:
;;;   (cpl-auto:setup-file "path/to/solve.lisp")
;;;   (cpl-auto:features-for-file "path/to/solve.lisp")  ; feature列挙のみ (pushしない)

(defpackage :cpl-auto
  (:use #:cl)
  (:export #:setup #:setup-file #:features-for-file #:*library-path*))
(in-package :cpl-auto)

(defparameter *library-path*
  (or (probe-file #P"~/.hermes/workspace/comprolib-lisp/comprolib.lisp")
      #P"comprolib.lisp"))

;;; シンボル → feature 対応表 (comprolib.lisp の :export とセクション構成より)
;;; chmax/chmin は共通部 (常に有効) なので feature 不要。

(defparameter *symbol->feature*
  '(("extgcd" . :cpl-math) ("pow-mod" . :cpl-math) ("inv-mod" . :cpl-math)
    ("prime-p" . :cpl-math) ("factorize" . :cpl-math) ("sieve" . :cpl-math)
    ("make-combi" . :cpl-combi) ("comb-nck" . :cpl-combi)
    ("make-dsu" . :cpl-dsu) ("dsu-find" . :cpl-dsu) ("dsu-same" . :cpl-dsu)
    ("dsu-union" . :cpl-dsu) ("dsu-size" . :cpl-dsu) ("dsu-groups" . :cpl-dsu)
    ("make-fenwick" . :cpl-fenwick) ("fw-add" . :cpl-fenwick)
    ("fw-prefix-sum" . :cpl-fenwick) ("fw-sum" . :cpl-fenwick)
    ("make-segtree" . :cpl-segtree) ("seg-set" . :cpl-segtree)
    ("seg-get" . :cpl-segtree) ("seg-prod" . :cpl-segtree)
    ("seg-all-prod" . :cpl-segtree)
    ("z-algorithm" . :cpl-string) ("make-rhash" . :cpl-string)
    ("rh-get" . :cpl-string) ("rh-equal" . :cpl-string)
    ("dijkstra" . :cpl-graph) ("make-lca" . :cpl-graph) ("lca" . :cpl-graph)
    ("lca-dist" . :cpl-graph) ("scc" . :cpl-graph)
    ("make-pheap" . :cpl-graph) ("pheap-empty" . :cpl-graph) ("pheap-push" . :cpl-graph) ("pheap-pop" . :cpl-graph) ("pheap-peek" . :cpl-graph)))

(defun features-for-file (path)
  "PATH のソースを走査し、使われている comprolib シンボルから
必要な feature のリストを返す (重複なし・push しない)。
検出条件: `comprolib:SYM` 修飾呼び出し。共通部 (chmax/chmin) は常に有効。"
  (let ((text (with-open-file (in path) ; read整個はコメント含めテキストで
                (let ((buf (make-string (file-length in))))
                  (read-sequence buf in)
                  buf)))
        (feats))
    (loop for (sym . feat) in *symbol->feature*
          when (search (concatenate 'string "comprolib:" sym) text
                       :test #'char-equal)
          do (pushnew feat feats))
    (nreverse feats)))

(defun features-for-string (text)
  "features-for-file の文字列版。"
  (let ((feats))
    (loop for (sym . feat) in *symbol->feature*
          when (search (concatenate 'string "comprolib:" sym) text
                       :test #'char-equal)
          do (pushnew feat feats))
    (nreverse feats)))

(defun setup-file (path &key (verbose t))
  "PATH を走査して feature を pushnew し、comprolib.lisp をロードする。
戻り値: 有効化した feature のリスト。"
  (let ((feats (features-for-file path)))
    (dolist (f feats) (pushnew f *features*))
    (when verbose
      (format *error-output* "~&; cpl-auto: ~A → features ~{~A~^ ~}~%" path feats))
    (load *library-path* :verbose nil)
    feats))

(defun setup (&key (verbose t))
  "現在ロード中のファイル (*load-truename*) を走査して setup-file。
REPLから直接呼ぶ場合は *default-pathname-defaults* の *.lisp を対象にしないので
明示的に setup-file を使うこと。"
  (if *load-truename*
      (setup-file *load-truename* :verbose verbose)
      (error "cpl-auto:setup はファイルロード中に呼んでください。REPLなら setup-file を。")))
