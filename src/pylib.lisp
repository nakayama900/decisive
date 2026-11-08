(in-package :gs2)

;;; ---------------------------------------------------------------- primes

(defvar *prime-list* nil)
(defvar *sieved* 2)
(defvar *composite* (make-hash-table))
(setf (gethash 1 *composite*) t)

(defun sieve (limit)
  (when (<= limit *sieved*) (return-from sieve))
  (setf *prime-list* nil)
  (loop for i from 2 below limit
        unless (gethash i *composite*)
          do (loop for j from (* i i) below limit by i
                   do (setf (gethash j *composite*) t))
             (push i *prime-list*))
  (setf *prime-list* (nreverse *prime-list*))
  (setf *sieved* limit))

(sieve 1000)

(defun is-prime* (n) (sieve (+ n 1)) (not (gethash n *composite*)))

(defun nth-prime (n)
  (sieve (+ (truncate (* (log (max n 1)) n)) 100))
  (nth (- n 1) *prime-list*))

(defun n-primes (n)
  (sieve (+ (truncate (* (log (max n 1)) n)) 100))
  (coerce (subseq *prime-list* 0 (min n (length *prime-list*))) 'vector))

(defun primes-below (n)
  (sieve (+ n 1))
  (coerce (loop for p in *prime-list* while (< p n) collect p) 'vector))

(defun next-prime (n)
  (loop for i = (+ n 1) then (+ i 1)
        when (is-prime* i) return i))

(defun totient (n)
  (loop for i from 1 to n
        count (= (py-gcd n i) 1)))

(defun factor-num (n exps)
  (loop with p = 2 with res = nil
        while (> n 1)
        do (loop while (zerop (mod n p))
                 do (push p res) (setf n (truncate n p)))
           (setf p (next-prime p))
        finally (let ((fs (nreverse res)))
                  (return
                    (if exps
                        (let (out (cur nil) (cnt 0))
                          (loop for f in fs
                                do (if (and cur (= f cur)) (incf cnt)
                                       (progn (when cur
                                                (push (vector cur cnt) out))
                                              (setf cur f cnt 1)))
                                finally (push (vector cur cnt) out))
                          (coerce (nreverse out) 'vector))
                        (coerce fs 'vector))))))

(defun factor-list (n)
  ;; n is list of [b e] pairs, or flat [b0 e0 b1 e1 ...]
  (let ((pairs
          (if (and (> (length n) 0) (gs2-num-p (aref n 0)))
              (loop for i from 0 below (length n) by 2
                    collect (cons (aref n i) (aref n (1+ i))))
              (loop for x across n collect (cons (aref x 0) (aref x 1))))))
    (reduce #'* (mapcar (lambda (pair) (expt (car pair) (cdr pair))) pairs))))

