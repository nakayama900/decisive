(in-package :comprolib)

;;; ============ :cpl-math ============
#+(or :cpl-all :cpl-math)
(progn

  (defun extgcd (a b)
    "多値 (g x y): a*x + b*y = g = gcd(a,b) を満たすもの。"
    (if (zerop b)
        (values a 1 0)
        (multiple-value-bind (g x y) (extgcd b (mod a b))
          (values g y (- x (* (floor a b) y))))))

  (defun pow-mod (a n mod)
    "a^n mod mod (n >= 0)。"
    (declare (type integer a n mod))
    (loop with r = 1
          for b = (mod a mod) then (mod (* b b) mod)
          for e = n then (ash e -1)
          until (zerop e)
          when (logtest e 1)
            do (setf r (mod (* r b) mod))
          finally (return r)))

  (defun inv-mod (a mod)
    "a の mod における逆元 (gcd(a,mod)=1 必須)。mod が素数なら pow-mod でも可。"
    (multiple-value-bind (g x) (extgcd a mod)
      (assert (= g 1) (a mod) "inv-mod: gcd(~A, ~A) = ~A ≠ 1" a mod g)
      (mod x mod)))

  (defun prime-p (n)
    "決定的 Miller-Rabin (64bit 範囲で正確)。"
    (if (< n 2)
        nil
        (let ((bases '(2 3 5 7 11 13 17 19 23 29 31 37)))
          (cond ((member n bases) t)
                ((some (lambda (p) (zerop (mod n p))) bases) nil)
                (t
                 (let ((d (- n 1)) (s 0))
                   (loop while (evenp d)
                         do (setf d (ash d -1))
                            (incf s))
                   (loop for a in bases
                         always (let ((x (pow-mod a d n)))
                                  (or (= x 1)
                                      (= x (- n 1))
                                      (loop for k below (- s 1)
                                            do (setf x (mod (* x x) n))
                                            thereis (= x (- n 1))))))))))))

  (defun factorize (n)
    "素因数分解 (試し割り)。昇順の (素因数 . 指数) の alist。"
    (let ((res '()))
      (flet ((push-p (p)
               (let ((cell (assoc p res)))
                 (if cell
                     (incf (cdr cell))
                     (push (cons p 1) res)))))
        (loop for p = 2 then (if (= p 2) 3 (+ p 2))
              while (<= (* p p) n)
              do (loop while (zerop (mod n p))
                       do (push-p p)
                          (setf n (floor n p))))
        (when (> n 1)
          (push-p n))
        (nreverse res))))

  (defun sieve (n)
    "n 以下の素数のリスト (エラトステネス)。"
    (let ((flags (make-array (+ n 1) :element-type 'bit :initial-element 0))
          (res '()))
      (loop for i from 2 to n
            when (zerop (aref flags i))
              do (push i res)
                 (loop for j from (* i i) to n by i
                       do (setf (aref flags j) 1)))
      (nreverse res))))


;;; ============ :cpl-combi ============
