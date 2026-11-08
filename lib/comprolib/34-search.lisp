(in-package :comprolib)

#+(or :cpl-all :cpl-search)
(progn

  (defun binary-search (ok ng pred)
    "meguru法: pred(ok)=T, pred(ng)=NIL の前提で境界を探す。返り値 ok。"
    (loop while (> (abs (- ok ng)) 1)
          do (let ((mid (floor (+ ok ng) 2)))
               (if (funcall pred mid)
                   (setf ok mid)
                   (setf ng mid))))
    ok)

  ;; ソート済みベクトル上の二分
  (defun lower-bound (vec x &key (order #'<))
    "x以上の最初のindex"
    (let ((lo 0) (hi (length vec)))
      (loop while (< lo hi)
            for mid = (floor (+ lo hi) 2)
            if (funcall order (aref vec mid) x)
              do (setf lo (1+ mid))
            else
              do (setf hi mid))
      lo))

  (defun upper-bound (vec x &key (order #'<))
    "xより大きい最初のindex"
    (let ((lo 0) (hi (length vec)))
      (loop while (< lo hi)
            for mid = (floor (+ lo hi) 2)
            if (funcall order x (aref vec mid))
              do (setf hi mid)
            else
              do (setf lo (1+ mid)))
      lo))
) ; end section

;;; ============ :cpl-cum ============
