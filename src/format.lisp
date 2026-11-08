(in-package :gs2)

;;; python %-format subset used by gs2 programs (see py-format support)
(defun py-format (f args)
  ;; returns (string . n-consumed); python %-format subset
  (let ((out (make-string-output-stream))
        (i 0) (ai 0) (n 0))
    (loop while (< i (length f))
          do (let ((c (char f i)))
               (cond
                 ((char/= c #\%) (write-char c out) (incf i))
                 ((and (< (1+ i) (length f)) (char= (char f (1+ i)) #\%))
                  (write-char #\% out) (incf i 2))
                 (t
                  ;; parse conversion
                  (let ((j (+ i 1)))
                    (loop while (and (< j (length f))
                                      (member (char f j) '(#\- #\+ #\Space #\0 #\#)
                                              :test #'char=))
                          do (incf j))
                    (let ((width-start j))
                      (loop while (and (< j (length f)) (digit-char-p (char f j)))
                            do (incf j))
                      (let ((width (parse-integer f :start width-start :end j
                                                  :junk-allowed t)))
                        (when (and (< j (length f)) (char= (char f j) #\.))
                          (incf j)
                          (loop while (and (< j (length f))
                                           (digit-char-p (char f j)))
                                do (incf j)))
                        (let* ((conv (if (< j (length f)) (char f j) #\d))
                               (arg (and (< ai (length args)) (aref args ai))))
                          (incf n)
                          (when (< ai (length args)) (incf ai))
                          (let ((body
                                  (etypecase arg
                                    (null "")
                                    (string
                                     (case conv
                                       ((#\d #\i #\u) arg)
                                       ((#\s) arg)
                                       ((#\x) arg)
                                       (t arg)))
                                    (integer
                                     (case conv
                                       ((#\d #\i #\u) (format nil "~d" arg))
                                       (#\x (format nil "~x" arg))
                                       (#\X (format nil "~x" arg))
                                       (#\o (format nil "~o" arg))
                                       ((#\s) (format nil "~d" arg))
                                       (t (format nil "~d" arg)))))))
                            (when width
                              (let ((w width) (pad #\Space))
                                (setf body
                                      (if (and w (> w (length body)))
                                          (concatenate 'string
                                                       (make-string (- w (length body))
                                                                    :initial-element pad)
                                                       body)
                                          body))))
                            (write-string body out)
                            (setf i (1+ j)))))))))))
    (cons (get-output-stream-string out) n)))

