;;; cpl-mode.el — comprolib (競プロCLライブラリ) 用 Emacs マイナーモード	-*- lexical-binding: t; -*-
;;;
;;; 機能:
;;;   1. company バックエンド: comprolib シンボルを候補表示
;;;      - バッファ内の (pushnew :cpl-XXX *features*) から有効featureを検出
;;;      - 有効featureの関数のみ候補 (無効featureの関数は出さない = コンパイルされないので)
;;;      - アノテーションにfeature名、docstring 付き
;;;   2. flycheck チェッカー cpl-sbcl:
;;;      - cpl-check.lisp バックエンドで sbcl compile-file の診断を表示
;;;      - feature不足による undefined function を error 表示
;;;   3. マイナーモード cpl-mode:
;;;      - C-c C-c c : feature一覧のトグル (cpl-toggle-feature)
;;;      - C-c C-c l : ライブラリのロード (SLIME REPLへ)
;;;      - C-c C-c k : チェック実行
;;;
;;; インストール (Doom): ~/.doom.d/config.el に
;;;   (add-to-list 'load-path "~/path/to/decisive/emacs/")
;;;   (require 'cpl-mode)
;;;   (add-hook 'lisp-mode-hook #'cpl-mode)
;;; して doom sync 。手動なら M-x cpl-mode。
;;; cpl-library-path / cpl-checker-path は decisive リポジトリ内の
;;; lib/comprolib/ ディレクトリと emacs/cpl-check.lisp を指すように設定する
;;; (デフォルトは旧ローカルパス)。

;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'thingatpt)

(defgroup cpl nil
  "comprolib (競技プログラミング Common Lisp ライブラリ) サポート"
  :group 'languages)

(defcustom cpl-library-path
  (expand-file-name "~/.hermes/workspace/comprolib-lisp/comprolib.lisp")
  "comprolib.lisp のパス。"
  :type 'file)

(defcustom cpl-checker-path
  (expand-file-name "~/.hermes/workspace/comprolib-lisp/cpl-check.lisp")
  "cpl-check.lisp (flycheck バックエンド) のパス。"
  :type 'file)

(defcustom cpl-sbcl-path (executable-find "sbcl")
  "sbcl のパス。"
  :type 'file)

;; ----------------------------------------------------------
;; シンボルデータベース (手動キュレーション: comprolib.lisp の export に対応)
;; ----------------------------------------------------------

(defconst cpl--features
  '(:cpl-math :cpl-combi :cpl-dsu :cpl-fenwick :cpl-segtree :cpl-string :cpl-graph :cpl-all)
  "comprolib の feature キーワード。")

(defconst cpl--symbols
  '((:cpl-math
     ("extgcd" "多値 (g x y): a*x+b*y = gcd(a,b)")
     ("pow-mod" "a^n mod mod (n>=0)")
     ("inv-mod" "mod における逆元 (gcd(a,mod)=1)")
     ("prime-p" "決定的 Miller-Rabin")
     ("factorize" "素因数分解 → 昇順 (p . e) alist")
     ("sieve" "n 以下の素数リスト"))
    (:cpl-combi
     ("make-combi" "(n mod) → nCk 前計算。mod は素数")
     ("comb-nck" "(c n k) → nCk mod。O(1)"))
    (:cpl-dsu
     ("make-dsu" "(n) → Union-Find")
     ("dsu-find" "(d x) → 代表元")
     ("dsu-same" "(d a b) → 同グループか")
     ("dsu-union" "(d a b) → 併合。T/NIL")
     ("dsu-size" "(d x) → グループサイズ")
     ("dsu-groups" "(d) → グループ毎の頂点リスト"))
    (:cpl-fenwick
     ("make-fenwick" "(n &optional init-vec) → BIT")
     ("fw-add" "(f i x): a[i] += x (0-indexed)")
     ("fw-prefix-sum" "(f r): a[0..r) の和")
     ("fw-sum" "(f l r): a[l..r) の和"))
    (:cpl-segtree
     ("make-segtree" "(vec op e) → セグ木")
     ("seg-set" "(s p x): a[p] = x")
     ("seg-get" "(s p) → a[p]")
     ("seg-prod" "(s l r) → a[l..r) の総積")
     ("seg-all-prod" "(s) → 全区間の総積"))
    (:cpl-string
     ("z-algorithm" "(s) → z配列 (最長共通接頭辞)")
     ("make-rhash" "(s) → 2重ローリングハッシュ")
     ("rh-get" "(rh l r) → 多値 (h1 h2)")
     ("rh-equal" "(rh l1 r1 l2 r2) → 部分文字列一致"))
    (:cpl-graph
     ("dijkstra" "(g src) → 距離ベクトル")
     ("make-lca" "(g &optional root) → LCAテーブル")
     ("lca" "(tab u v) → 最小共通祖先")
     ("lca-dist" "(tab u v) → 辺数")
     ("scc" "(g) → 成分リスト (トポ順)"))
    (:cpl-all
     ("chmax" "place < val なら更新して T")
     ("chmin" "place > val なら更新して T")))
  "((feature (symbol docstring)...) ...)")

(defun cpl--buffer-features ()
  "バッファ内の (pushnew :cpl-XXX *features*) から有効featureを集める。"
  (save-excursion
    (goto-char (point-min))
    (let ((feats))
      (while (re-search-forward
              "pushnew\\s-+:\\(cpl-[a-z]+\\)\\s-*\\*features\\*" nil t)
        (let ((kw (intern (concat ":" (match-string 1)))))
          (when (memq kw cpl--features)
            (push kw feats))))
      feats)))

(defun cpl--active-symbols ()
  "有効featureに属する (sym doc feature) のリスト。"
  (let ((feats (cpl--buffer-features)))
    ;; :cpl-all は全feature有効扱い
    (when (memq :cpl-all feats)
      (setq feats cpl--features))
    (cl-loop for (feature . syms) in cpl--symbols
             when (memq feature feats)
             append (cl-loop for (sym doc) in syms
                             collect (list sym doc feature)))))

;; ----------------------------------------------------------
;; company バックエンド
;; ----------------------------------------------------------

(defun cpl--company-candidates (prefix)
  "パッケージ修飾付きprefix (comprolib:foo) からシンボル部を取り出して照合。"
  (let ((bare (if (string-match "[a-z0-9-]+:" prefix)
                  (substring prefix (match-end 0))
                prefix)))
    (cl-loop for (sym _doc _feat) in (cpl--active-symbols)
             when (string-prefix-p bare sym)
             collect sym)))

;;;###autoload
(defun company-cpl (command &optional arg &rest _ignored)
  "comprolib シンボルの company バックエンド。"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-cpl))
    (prefix (and (bound-and-true-p cpl-mode)
                 (or (company-grab-symbol) 'stop)))
    (candidates (cpl--company-candidates arg))
    (annotation
     (let ((cell (cl-find arg (cpl--active-symbols) :key #'car :test #'equal)))
       (when cell (format " [%s]" (nth 2 cell)))))
    (doc-buffer
     (let ((cell (cl-find arg (cpl--active-symbols) :key #'car :test #'equal)))
       (when cell
         (with-current-buffer (get-buffer-create "*company-cpl-doc*")
           (erase-buffer)
           (insert (format "%s — %s\nfeature: %s" (car cell) (cadr cell) (nth 2 cell)))
           (current-buffer)))))
    (sorted t)))

;; ----------------------------------------------------------
;; flycheck チェッカー
;; ----------------------------------------------------------

(defun cpl--checker-stdin (_checker)
  "flycheck の :standard-input 供給関数。"
  (let* ((feats (cpl--buffer-features))
         (feat-str (mapconcat (lambda (f) (string-remove-prefix ":" (symbol-name f)))
                              feats " ")))
    (concat (buffer-file-name) " " feat-str)))

(defun cpl--register-flycheck ()
  "cpl-sbcl チェッカーを登録 (cpl-checker-path の現在値で)。"
  (when (fboundp 'flycheck-define-command-checker)
    (flycheck-define-command-checker 'cpl-sbcl
      "comprolib 対応 SBCL コンパイラチェッカー。
バッファの保存済みファイルを feature 検出付きで compile-file し、
undefined function (feature不足) を error 表示する。"
      :command '("sbcl" "--script"
                 (eval (progn
                         (setenv "CPL_TARGET" (or (buffer-file-name) ""))
                         (setenv "CPL_AUTO" "1")   ; 依存自動解決モード
                         cpl-checker-path)))
      :error-patterns
      '((error line-start
               (one-or-more (not (any "\t")))
               "\t" line "\t" (message)
               line-end))
      :error-exit-code-fn (lambda (_) nil)
      :modes '(lisp-mode)
      :predicate (lambda () (bound-and-true-p cpl-mode)))
    (add-to-list 'flycheck-checkers 'cpl-sbcl)))

(with-eval-after-load 'flycheck
  (cpl--register-flycheck))

;;;###autoload
(defun cpl-check ()
  "バッファを cpl-check.lisp でチェックし、結果を *cpl-check* バッファに表示。"
  (interactive)
  (let* ((feats (cpl--buffer-features))
         (feat-str (mapconcat #'symbol-name feats " "))
         (input (concat (buffer-file-name) " " feat-str "\n"))
         (out (with-temp-buffer
                (let ((proc (start-process "cpl-check" (current-buffer)
                                           cpl-sbcl-path "--script" cpl-checker-path)))
                  (process-send-string proc input)
                  (process-send-eof proc)
                  (while (accept-process-output proc 30))
                  (buffer-string)))))
    (with-current-buffer (get-buffer-create "*cpl-check*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert out)
        (goto-char (point-min)))
      (special-mode)
      (display-buffer (current-buffer)))))

;; ----------------------------------------------------------
;; インタラクティブ: feature トグル
;; ----------------------------------------------------------

;;;###autoload
(defun cpl-toggle-feature (feature)
  "バッファ先頭の (pushnew :cpl-XXX *features*) 行をトグル挿入/削除。"
  (interactive
   (list (completing-read "feature: " (mapcar #'symbol-name cpl--features))))
  (let ((re (format "pushnew\\s-+%s\\s-*\\*features\\*" feature)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward re nil t)
          ;; 削除: 行ごと消す
          (let ((bol (line-beginning-position))
                (eol (line-end-position)))
            (delete-region bol (min (+ eol 1) (point-max))))
        ;; 挿入: in-package 行の後へ
        (goto-char (point-min))
        (if (re-search-forward "^(in-package" nil t)
            (forward-line 1)
          (goto-char (point-min)))
        (insert (format "(pushnew %s *features*)\n" feature))))))

;;;###autoload
(defun cpl-load-library ()
  "SLIME REPL に comprolib と現在のfeature設定を評価させる。"
  (interactive)
  (let* ((feats (cpl--buffer-features))
         (forms (append
                 (cl-loop for f in feats
                          collect (format "(pushnew %s *features*)" f))
                 (list (format "(load %S)" cpl-library-path)))))
    (if (and (fboundp 'slime-connected-p) (slime-connected-p))
        (dolist (form forms)
          (slime-eval `(swank:eval-and-grab-output ,form)))
      (message "SLIME未接続。フォームをkill-ringへ: %s" forms)
      (kill-new (mapconcat #'identity forms "\n")))))

;;;###autoload
(define-minor-mode cpl-mode
  "comprolib (競プロCLライブラリ) サポートマイナーモード。
company候補・flycheck・feature管理。"
  :lighter " cpl"
  (if cpl-mode
      (progn
        (when (and (bound-and-true-p company-backends))
          (add-to-list (make-local-variable 'company-backends) 'company-cpl))
        (when (bound-and-true-p flycheck-mode)
          (flycheck-select-checker 'cpl-sbcl)))
    (setq-local company-backends (default-value 'company-backends))))

;;;###autoload
(add-hook 'lisp-mode-hook #'cpl-mode)

(provide 'cpl-mode)
;;; cpl-mode.el ends here
