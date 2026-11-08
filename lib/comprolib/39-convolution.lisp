(in-package :comprolib)

#+(or :cpl-all :cpl-convolution)
(progn

  (defparameter *ntt-mod* 998244353)
  (defparameter *ntt-g* 3)

  (defun ntt (a invert-p)
    "a をNTT (破壊的)。mod = *ntt-mod*。"
    (let ((n (length a)) (m *ntt-mod*))
      ;; bit reversal
      (loop for i from 0 below n
            with j = 0
            do (let ((jj 0) (k i))
                 (loop for b = (ash n -1) then (ash b -1)
                       while (> b 0)
                       do (setf jj (ash jj 1)
                                jj (logior jj (logand k 1))
                                k (ash k -1)))
                 (when (< i jj)
                   (rotatef (aref a i) (aref a jj)))))
      (loop for len = 2 then (* len 2)
            while (<= len n)
            do (let ((wlen (pow-mod *ntt-g*
                                    (let ((e (floor (- m 1) len)))
                                      (if invert-p (- (- m 1) e) e))
                                    m)))
                 (loop for i from 0 below n by len
                       do (loop for j below (ash len -1)
                                with w = 1
                                do (let ((u (aref a (+ i j)))
                                         (v (mod (* w (aref a (+ i j (ash len -1)))) m)))
                                     (setf (aref a (+ i j)) (mod (+ u v) m)
                                           (aref a (+ i j (ash len -1))) (mod (- u v) m)
                                           w (mod (* w wlen) m)))))))
      (when invert-p
        (let ((ninv (inv-mod n m)))
          (loop for i below n
                do (setf (aref a i) (mod (* (aref a i) ninv) m)))))
      a))

  (defun convolution-mod (a b)
    "整数ベクトル2つの畳み込み mod 998244353 (NTT)。"
    (let* ((na (length a)) (nb (length b)))
      (when (or (zerop na) (zerop nb))
        (return-from convolution-mod (make-array 0)))
      (let* ((res-n (+ na nb -1))
             (n (ash 1 (ceiling (log res-n 2))))
             (fa (make-array n :initial-element 0))
             (fb (make-array n :initial-element 0)))
        (loop for i below na do (setf (aref fa i) (mod (aref a i) *ntt-mod*)))
        (loop for i below nb do (setf (aref fb i) (mod (aref b i) *ntt-mod*)))
        (ntt fa nil)
        (ntt fb nil)
        (loop for i below n do (setf (aref fa i) (mod (* (aref fa i) (aref fb i)) *ntt-mod*)))
        (ntt fa t)
        (make-array res-n
                    :initial-contents
                    (loop for i below res-n collect (aref fa i))))))

  ;; gsll/grid (GSL) が必要な FFT 畳み込みは :cpl-gsll が有効な時のみ定義。
  ;; decisive 同梱版では gsll を optional にしているため、無い環境では
  ;; convolution-fft は未定義 (convolution-mod は NTT で常時使える)。
  #+(and :cpl-gsll (or :cpl-all :cpl-convolution))
  (defun convolution-fft (a b)
  "実数ベクトル2つの畳み込み (倍精度・誤差あり)。gsllのFFT利用。整数に丸めて返す。"
  (let* ((na (length a))
         (nb (length b)))
    (when (or (zerop na) (zerop nb))
      (return-from convolution-fft (make-array 0)))
    (let* ((res-n (+ na nb -1))
           (n (ash 1 (ceiling (log res-n 2))))
           (fa (grid:make-foreign-array '(complex double-float)
                                        :initial-contents
                                        (loop for i below n
                                              collect (complex
                                                       (if (< i na)
                                                           (coerce (aref a i) 'double-float)
                                                           0d0)
                                                       (if (< i nb)
                                                           (coerce (aref b i) 'double-float)
                                                           0d0))))))
      (let ((fft (gsll:forward-discrete-fourier-transform fa)))
        (let ((inv-arr
                (grid:make-foreign-array
                  '(complex double-float)
                  :initial-contents
                  (loop for i below n
                        for x = (grid:gref fft i)
                        for xj = (grid:gref fft (mod (- n i) n))
                        collect (* (/ #c(-0.0d0 -0.5d0) (* 2 n))
                                   (+ x (conjugate xj))
                                   (- x (conjugate xj)))))))
          (let ((res (gsll:backward-discrete-fourier-transform inv-arr)))
            (make-array res-n
                        :initial-contents
                        (loop for i below res-n
                              collect (round (realpart (grid:gref res i))))))))))))


;;; ============ :cpl-flow ============
