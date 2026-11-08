(in-package :comprolib)

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
