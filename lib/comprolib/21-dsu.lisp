(in-package :comprolib)

#+(or :cpl-all :cpl-dsu)
(progn

  (defstruct (dsu (:constructor %make-dsu (par rnk sz)))
    par rnk sz)

  (defun make-dsu (n)
    "n 頂点の Union-Find。"
    (let ((par (make-array n :element-type 'fixnum))
          (sz  (make-array n :element-type 'fixnum :initial-element 1)))
      (loop for i below n
            do (setf (aref par i) i))
      (%make-dsu par
                 (make-array n :element-type 'fixnum :initial-element 0)
                 sz)))

  (defun dsu-find (d x)
    "x の属するグループの代表元。経路圧縮 (path halving)。"
    (let ((par (dsu-par d)))
      (loop while (/= (aref par x) x)
            do (setf (aref par x) (aref par (aref par x))
                     x           (aref par x)))
      x))

  (defun dsu-same (d a b)
    (= (dsu-find d a) (dsu-find d b)))

  (defun dsu-union (d a b)
    "a と b を併合。実際に併合したら T、元々同グループなら NIL。"
    (let* ((par (dsu-par d))
           (rnk (dsu-rnk d))
           (sz  (dsu-sz d))
           (ra (dsu-find d a))
           (rb (dsu-find d b)))
      (when (= ra rb)
        (return-from dsu-union nil))
      (when (< (aref rnk ra) (aref rnk rb))
        (rotatef ra rb))
      (setf (aref par rb) ra)
      (incf (aref sz ra) (aref sz rb))
      (when (= (aref rnk ra) (aref rnk rb))
        (incf (aref rnk ra)))
      t))

  (defun dsu-size (d x)
    (aref (dsu-sz d) (dsu-find d x)))

  (defun dsu-groups (d)
    "グループごとの頂点リスト (各グループ内は昇順、グループは先頭要素昇順)。"
    (let* ((par (dsu-par d))
           (n (length par))
           (tmp (make-hash-table)))
      (loop for i below n
            do (push i (gethash (dsu-find d i) tmp)))
      (sort (loop for v being each hash-value of tmp
                  collect (nreverse v))
            #'< :key #'car)))
) ; end section


;;; ============ :cpl-fenwick ============
