(in-package :comprolib)

#+(or :cpl-all :cpl-combi)
(progn

  (defstruct (combi (:constructor %make-combi (fact inv-fact mod)))
    fact inv-fact mod)

  (defun make-combi (n mod)
    "nCk mod mod を O(1) で返すための前計算 (O(n))。mod は素数前提。"
    (let ((fact (make-array (+ n 1) :element-type 'integer))
          (inv  (make-array (+ n 1) :element-type 'integer)))
      (setf (aref fact 0) 1)
      (loop for i from 1 to n
            do (setf (aref fact i) (mod (* (aref fact (- i 1)) i) mod)))
      ;; 内部 extgcd (math セクション非依存にするためここに置く)
      (flet ((inv% (a)
               (loop with old-r = a and r = mod
                     with old-s = 1 and s = 0
                     until (zerop r)
                     for q = (floor old-r r)
                     do (psetq old-r r r (- old-r (* q r))
                               old-s s s (- old-s (* q s)))
                     finally (return (mod old-s mod)))))
        (setf (aref inv n) (inv% (aref fact n)))
        (loop for i from n downto 1
              do (setf (aref inv (- i 1))
                       (mod (* (aref inv i) i) mod))))
      (%make-combi fact inv mod)))

  (defun comb-nck (c n k)
    "nCk mod mod (前計算済みなら O(1))。範囲外は 0。"
    (let ((lim (- (length (combi-fact c)) 1)))
      (assert (and (<= 0 n lim)) (n) "comb-nck: n=~A は前計算範囲外" n))
    (if (or (< k 0) (< n k))
        0
        (mod (* (aref (combi-fact c) n)
                (aref (combi-inv-fact c) k)
                (aref (combi-inv-fact c) (- n k)))
             (combi-mod c))))
) ; end section


;;; ============ :cpl-dsu ============
