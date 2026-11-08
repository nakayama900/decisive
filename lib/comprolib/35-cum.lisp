(in-package :comprolib)

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
