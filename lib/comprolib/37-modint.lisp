(in-package :comprolib)

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
