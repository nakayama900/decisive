(in-package :comprolib)

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
