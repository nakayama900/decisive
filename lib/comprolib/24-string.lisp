(in-package :comprolib)

#+(or :cpl-all :cpl-string)
(progn

  (defun z-algorithm (s)
    "z[i] = s と s[i..] の最長共通接頭辞長 (z[0] = n)。文字列か vector。"
    (let* ((n (length s))
           (z (make-array n :element-type 'fixnum)))
      (unless (zerop n)
        (setf (aref z 0) n)
        (let ((i 1) (j 0))
          (loop while (< i n)
                do (loop while (and (< (+ i j) n)
                                     (char= (char s j) (char s (+ i j))))
                         do (incf j))
                   (setf (aref z i) j)
                   (cond ((zerop j)
                          (incf i))
                         (t
                          (let ((k 1))
                            (loop while (and (< (+ i k) n)
                                              (< (+ k (aref z k)) j))
                                  do (setf (aref z (+ i k)) (aref z k))
                                     (incf k))
                            (incf i k)
                            (decf j k))))))
        z)))

  (defconstant +rh-m1+ 1000000007)
  (defconstant +rh-m2+ 998244353)
  (defconstant +rh-base+ 1000003)

  (defstruct (rhash (:constructor %make-rhash (n h1 h2 p1 p2)))
    n h1 h2 p1 p2)

  (defun make-rhash (s)
    "2重ローリングハッシュ。rh-equal で部分文字列一致判定。"
    (let* ((n (length s))
           (h1 (make-array (+ n 1) :element-type 'integer :initial-element 0))
           (h2 (make-array (+ n 1) :element-type 'integer :initial-element 0))
           (p1 (make-array (+ n 1) :element-type 'integer :initial-element 1))
           (p2 (make-array (+ n 1) :element-type 'integer :initial-element 1)))
      (loop for c across s
            for i from 0
            do (setf (aref h1 (+ i 1)) (mod (+ (* (aref h1 i) +rh-base+)
                                              (char-code c))
                                            +rh-m1+)
                     (aref h2 (+ i 1)) (mod (+ (* (aref h2 i) +rh-base+)
                                              (char-code c))
                                            +rh-m2+)
                     (aref p1 (+ i 1)) (mod (* (aref p1 i) +rh-base+) +rh-m1+)
                     (aref p2 (+ i 1)) (mod (* (aref p2 i) +rh-base+) +rh-m2+)))
      (%make-rhash n h1 h2 p1 p2)))

  (defun rh-get (rh l r)
    "s[l..r) のハッシュ。多値 (h1 h2)。"
    (let ((k (- r l)))
      (values (mod (- (aref (rhash-h1 rh) r)
                      (* (aref (rhash-h1 rh) l) (aref (rhash-p1 rh) k)))
                   +rh-m1+)
              (mod (- (aref (rhash-h2 rh) r)
                      (* (aref (rhash-h2 rh) l) (aref (rhash-p2 rh) k)))
                   +rh-m2+))))

  (defun rh-equal (rh l1 r1 l2 r2)
    "s[l1..r1) = s[l2..r2) か。"
    (multiple-value-bind (a1 b1) (rh-get rh l1 r1)
      (multiple-value-bind (a2 b2) (rh-get rh l2 r2)
        (and (= a1 a2) (= b1 b2)))))
) ; end section


;;; ============ :cpl-graph ============
