;;; cpl-check.lisp — comprolib 用 SBCL チェッカーバックエンド
;;; 標準入力から: 対象lispファイルのパス 1行 + featureキーワード群 (空白区切り)
;;; 標準出力に (line severity message) をタブ区切りで出力する。
;;; severity: W / S (style-warning) / E (error)
;;;
;;; 使い方:
;;;   echo "/path/to/solve.lisp cpl-dsu cpl-math" | sbcl --script cpl-check.lisp

(defun split-ws (s)
  (let ((res nil) (i 0) (n (length s)))
    (loop while (< i n)
          do (let ((j (position-if (lambda (c) (not (member c '(#\space #\tab #\newline #\return)))) s :start i)))
               (if (null j)
                   (return)
                   (let ((k (position-if (lambda (c) (member c '(#\space #\tab #\newline #\return))) s :start j)))
                     (push (subseq s j (or k n)) res)
                     (setf i (or k n))))))
    (nreverse res)))

(defun parse-args ()
  "環境変数 CPL_TARGET (ファイル) + CPL_FEATURES (カンマ区切り)。
CPL_AUTO=1 なら CPL_FEATURES を無視して対象ファイルから自動算出。
未設定なら旧stdin形式 (1行: パス + 空白区切りfeatures)。"
  (let ((env-path (sb-ext:posix-getenv "CPL_TARGET"))
        (env-feats (sb-ext:posix-getenv "CPL_FEATURES"))
        (auto (sb-ext:posix-getenv "CPL_AUTO")))
    (if (and env-path (plusp (length env-path)))
        (if (and auto (string= auto "1"))
            ;; 自動解決: ファイル内の comprolib:SYM 呼び出しから feature 算出
            (values env-path (auto-features env-path))
            (values env-path
                    (and env-feats (split-ws (substitute #\space #\, env-feats)))))
        (let ((line (read-line *standard-input* nil "")))
          (let ((parts (split-ws line)))
            (values (first parts) (rest parts)))))))

;;; ---- 依存自動解決 (cpl-auto.lisp と同じ表) ----

(defvar *symbol->feature*
  '(("extgcd" . cpl-math) ("pow-mod" . cpl-math) ("inv-mod" . cpl-math)
    ("prime-p" . cpl-math) ("factorize" . cpl-math) ("sieve" . cpl-math)
    ("make-combi" . cpl-combi) ("comb-nck" . cpl-combi)
    ("make-dsu" . cpl-dsu) ("dsu-find" . cpl-dsu) ("dsu-same" . cpl-dsu)
    ("dsu-union" . cpl-dsu) ("dsu-size" . cpl-dsu) ("dsu-groups" . cpl-dsu)
    ("make-fenwick" . cpl-fenwick) ("fw-add" . cpl-fenwick)
    ("fw-prefix-sum" . cpl-fenwick) ("fw-sum" . cpl-fenwick)
    ("make-segtree" . cpl-segtree) ("seg-set" . cpl-segtree)
    ("seg-get" . cpl-segtree) ("seg-prod" . cpl-segtree)
    ("seg-all-prod" . cpl-segtree)
    ("z-algorithm" . cpl-string) ("make-rhash" . cpl-string)
    ("rh-get" . cpl-string) ("rh-equal" . cpl-string)
    ("dijkstra" . cpl-graph) ("make-lca" . cpl-graph) ("lca" . cpl-graph)
    ("lca-dist" . cpl-graph) ("scc" . cpl-graph)))

(defun auto-features (path)
  "PATH 内の comprolib:SYM 呼び出しから必要 feature 名 (文字列) を算出。"
  (let ((text (with-open-file (in path)
                (let ((buf (make-string (file-length in))))
                  (read-sequence buf in)
                  buf)))
        (feats))
    (loop for (sym . feat) in *symbol->feature*
          when (search (concatenate 'string "comprolib:" sym) text
                       :test #'char-equal)
          do (pushnew (string feat) feats :test #'string-equal))
    (nreverse feats)))

(defun symbol-line (sym file-lines)
  "file-lines (vector of strings) から sym が出現する最初の行番号 (1-based)。"
  (loop for i from 0 below (length file-lines)
        for l = (aref file-lines i)
        when (search sym l :test #'char-equal)
          return (1+ i)))

(defun main ()
  (multiple-value-bind (path feats) (parse-args)
    (unless (and path (plusp (length path)))
      (format t "0~%") (sb-ext:exit :code 0))
    (dolist (f feats)
      (pushnew (intern (string-upcase f) :keyword) *features*))
    ;; cpl-auto をロード (対象ファイルが (cpl-auto:setup) を参照するため READ error防止)
    (let ((auto (or (probe-file #P"~/.hermes/workspace/comprolib-lisp/cpl-auto.lisp")
                    (probe-file "cpl-auto.lisp"))))
      (when auto (load auto :verbose nil)))
    ;; comprolib をロード (ライブラリが見つからなければ cpl-dir から)
    (let ((lib (or (probe-file #P"~/.hermes/workspace/comprolib-lisp/comprolib.lisp")
                   (probe-file "comprolib.lisp"))))
      (when lib (load lib :verbose nil)))
    (let* ((lines (with-open-file (in path)
                    (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
                      (loop for l = (read-line in nil nil)
                            while l do (vector-push-extend l v))
                      v)))
           (out *standard-output*)
           (count 0))
      (flet ((emit (sev line msg)
               (format out "~a~a~a~a~a~a" sev #\tab line #\tab (substitute #\space #\tab msg) #\newline)
               (incf count)))
        (let ((chunks nil))          ; ((context-form first-line) ...) verbose出力から
          (declare (ignorable chunks))
          ;; compile-file の diagnostics を condition 経由で収集。
          ;; 行番号は取れないので symbol-line でファイル内を探索する。
          (let ((warns nil))
            (handler-bind
                ((warning (lambda (w) (push w warns))))
              (ignore-errors
                (compile-file path :output-file "/tmp/.cpl-check-tmp.fasl")))
            ;; /dev/null 起因のfile-error等は除外: compile-fileの戻りを無視
            (dolist (w (nreverse warns))
              (let* ((type (type-of w))
                     (args (ignore-errors (simple-condition-format-arguments w)))
                     (text (ignore-errors (apply #'format nil (simple-condition-format-control w) args))))
                (cond
                  ((search "undefined function" (or text "") :test #'char-equal)
                   (let* ((fname (string (getf args :function))))
                     (emit "E" (or (symbol-line fname lines) 1)
                           (format nil "undefined function: ~a (feature指定忘れ? cpl-* をpush)" fname))))
                  ((search "undefined variable" (or text "") :test #'char-equal)
                   (let* ((vname (string (getf args :variable))))
                     (emit "W" (or (symbol-line vname lines) 1)
                           (format nil "undefined variable: ~a" vname))))
                  (t
                   (emit (if (eq type 'simple-style-warning) "S" "W") 1 (or text (princ-to-string w))))))))
          (format out "COUNT ~a~%" count))))
    (sb-ext:exit :code 0)))

(main)
