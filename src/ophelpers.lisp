(in-package :gs2)

;;; helpers for ops

(defun make-push-array () (make-array 0 :adjustable t :fill-pointer 0))

(defun gs2-truthy (x)
  (cond ((gs2-num-p x) (not (zerop x)))
        ((gs2-list-p x) (not (zerop (length x))))
        (t t)))

(defun group-consecutive (y)
  (let ((out (make-push-array)) (cur (make-push-array)))
    (loop for x across y
          for prev = nil then x
          if (and prev (not (gs2-equal prev x)))
            do (vector-push-extend (coerce cur 'vector) out)
               (setf cur (make-push-array))
          do (vector-push-extend x cur)
          finally (vector-push-extend (coerce cur 'vector) out))
    out))

(defun chunks-of (x y)
  (let ((out (make-push-array)))
    (loop while (> (length x) 0)
          do (vector-push-extend
              (if (>= (length x) y) (subseq x 0 y) x)
              out)
             (setf x (subseq x (min y (length x)))))
    out))

(defun step-slice (x y)
  ;; python x[::y]
  (cond ((plusp y)
         (loop for i from 0 below (length x) by y
               collect (aref x i) into acc
               finally (return (coerce acc 'vector))))
        ((minusp y)
         (loop for i from (- (length x) 1) downto 0 by (- y)
               collect (aref x i) into acc
               finally (return (coerce acc 'vector))))
        (t (error 'gs2-error :message "slice step 0"))))

(defun permutations-of (xs)
  (if (zerop (length xs))
      (list (make-array 0))
      (let ((acc nil))
        (labels ((perm (cur rest)
                   (if (zerop (length rest))
                       (push (coerce (reverse cur) 'vector) acc)
                       (loop for i below (length rest)
                             do (perm (cons (aref rest i) cur)
                                      (concatenate
                                       'vector (subseq rest 0 i)
                                       (subseq rest (1+ i))))))))
          (perm nil xs)
          (nreverse acc)))))

(defun cartesian-fold (xss)
  ;; itertools.product(*xss)
  (labels ((prod (lists)
             (if (null lists)
                 (list nil)
                 (loop for x across (first lists)
                       append (mapcar (lambda (r) (cons x r))
                                      (prod (rest lists)))))))
    (mapcar (lambda (l) (coerce (reverse l) 'vector))
            (prod (coerce xss 'list)))))

(defun cartesian-repeat (xs n)
  (cartesian-fold (make-array n :initial-element xs)))

(defun combinations-of (xs n)
  (let ((acc nil))
    (labels ((comb (start cur)
               (if (= (length cur) n)
                   (push (coerce (reverse cur) 'vector) acc)
                   (loop for i from start below (length xs)
                         do (comb (1+ i) (cons (aref xs i) cur))))))
      (comb 0 nil)
      (nreverse acc))))

(defun combinations-with-replacement-of (xs n)
  (let ((acc nil))
    (labels ((comb (start cur)
               (if (= (length cur) n)
                   (push (coerce (reverse cur) 'vector) acc)
                   (loop for i from start below (length xs)
                         do (comb i (cons (aref xs i) cur))))))
      (comb 0 nil)
      (nreverse acc))))

(defun flatten-gs (xs)
  (let ((acc (make-push-array)))
    (labels ((fl (x)
               (if (gs2-list-p x)
                   (loop for i across x do (fl i))
                   (vector-push-extend x acc))))
      (fl xs)
      acc)))

(defun transpose-of (xs)
  (let* ((rows (coerce xs 'list))
         (h (loop for r in rows minimize (length r)))
         (out (make-push-array)))
    (loop for c below h
          do (vector-push-extend
              (coerce (loop for r in rows collect (aref r c)) 'vector)
              out))
    out))

