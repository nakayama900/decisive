(in-package :gs2)

;;; ---------------------------------------------------------------- primes
;;; prime helpers now built on comprolib (:cpl-math) — sieve / prime-p /
;;; factorize are shared with the competitive-programming library instead of
;;; being duplicated here.

(defvar *prime-list* nil)
(defvar *sieved* 2)

(defun sieve (limit)
  ;; keep the gs2-side incremental prime list backed by comprolib:sieve
  (when (<= limit *sieved*) (return-from sieve))
  (setf *prime-list* (comprolib:sieve limit))
  (setf *sieved* limit))

(sieve 1000)

(defun is-prime* (n)
  (comprolib:prime-p n))

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
  ;; via factorization: phi = n * prod (1 - 1/p)
  (if (= n 1)
      1
      (let ((fs (comprolib:factorize n)))
        (* n (reduce #'* (mapcar (lambda (f) (- 1 (/ (car f) (cdr f)))) fs))))))

(defun factor-num (n exps)
  ;; factorize n using comprolib:factorize, then group into [p e] pairs
  ;; or a flat [p0 e0 p1 e1 ...] vector depending on EXPS
  (let* ((fs (comprolib:factorize n))          ; alist (p . e)
         (pairs (map 'vector (lambda (f) (vector (car f) (cdr f))) fs)))
    (if exps
        (let ((out (make-push-array)))
          (loop for p across pairs
                do (vector-push-extend (aref p 0) out)
                   (vector-push-extend (aref p 1) out))
          out)
        pairs)))
