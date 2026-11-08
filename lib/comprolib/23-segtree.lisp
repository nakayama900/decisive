(in-package :comprolib)

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
