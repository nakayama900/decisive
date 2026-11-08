(in-package :comprolib)

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
