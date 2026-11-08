(in-package :comprolib)

#+(or :cpl-all :cpl-2sat)
(progn

  (defstruct (two-sat (:constructor %make-2sat (n g ans)))
    n g ans)
  (defun make-two-sat (n)
    (%make-2sat n (make-array (* 2 n) :initial-element nil)
                (make-array n :element-type 'bit :initial-element 0)))

  

  ;; ↑の複雑さを避け、直接実装:
  (defun 2sat-add (ts i xi j xj)
    "節 (xi ∨ xj) を追加。i,j は変数index、xi/xj は真理値(その変数が真である含意)。"
    (let ((g (two-sat-g ts)) (n (two-sat-n ts)))
      ;; ¬xi → xj, ¬xj → xi  (ノードk=x_k真, ノードn+k=x_k偽)
      ;; 出発はリテラルの否定ノード、到着はリテラルのノードindexを直接push
      (push (+ (if xj 0 n) j) (aref g (+ (if xi n 0) i)))
      (push (+ (if xi 0 n) i) (aref g (+ (if xj n 0) j)))
      ts))

  (defun 2sat-solve (ts)
    "充足可能なら T して答えを two-sat-ans に格納。不可なら NIL。"
    (let* ((n (two-sat-n ts)) (g (two-sat-g ts))
           (comp (make-array (* 2 n) :element-type 'integer :initial-element -1))
           (order (make-array 0 :adjustable t :fill-pointer 0))
           (low (make-array (* 2 n) :element-type 'integer))
           (num (make-array (* 2 n) :element-type 'integer :initial-element 0))
           (idx 0) (nc 0)
           (stack nil))
      (declare (ignorable order))
      (labels ((dfs (v)
                 (setf (aref low v) (incf idx) (aref num v) idx)
                 (push v stack)
                 (dolist (e (aref g v))
                   (let ((to e))  ; 辺は到着ノードindex (整数) を直接格納
                     (cond ((zerop (aref num to))
                            (dfs to)
                            (setf (aref low v) (min (aref low v) (aref low to))))
                           ((= (aref comp to) -1)
                            (setf (aref low v) (min (aref low v) (aref num to)))))))
                 (when (= (aref low v) (aref num v))
                   (loop for u = (pop stack)
                         do (setf (aref comp u) nc)
                         until (= u v))
                   (incf nc))))
        (loop for v below (* 2 n)
              when (zerop (aref num v)) do (dfs v)))
      ;; TarjanのSCC番号は逆トポ順 (シンク側=トポ順で後が先に確定し小さい番号)。
      ;; ACL規約「トポ順で後のSCCが真」より comp[i] < comp[n+i] なら x_i = T。
      (loop for i below n
            when (= (aref comp i) (aref comp (+ i n)))
              do (return-from 2sat-solve nil))
      (loop for i below n
            do (setf (aref (two-sat-ans ts) i)
                     (if (< (aref comp i) (aref comp (+ i n))) 1 0)))
      t))
) ; end section

;;; ============ :cpl-suffix ============
