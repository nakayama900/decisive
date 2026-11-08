(in-package :gs2)

;;; ---------------------------------------------------------------- tokenizer

(defparameter *string-ends*
  (concatenate 'string
               (map 'string #'code-char '(#x05 #x06 #x9b #x9c #x9d #x9e #x9f))))

(defun char-in (c string) (find c string :test #'char=))

(defun regex-tokenize (prog)
  ;; emulate: re.findall of token_re alternatives joined by |, with re.DOTALL, where
  ;;   token_re = ['\x04[^ENDS]*[ENDS]', '\x01.', '\x02..', '\x03....', '\x07.', '.']
  (let ((tokens nil)
        (i 0)
        (n (length prog)))
    (loop while (< i n)
          do (let ((c (char prog i)))
               (cond
                 ((char= c (code-char #x04))
                  ;; \x04[^ENDS]*[ENDS]
                  (let ((j (+ i 1)))
                    (loop while (and (< j n) (not (char-in (char prog j) *string-ends*)))
                          do (incf j))
                    (if (< j n)
                        (progn (push (subseq prog i (1+ j)) tokens) (setf i (1+ j)))
                        (progn (push (subseq prog i (1+ i)) tokens) (setf i (1+ i))))))
                 ((char= c (code-char #x01))
                  (when (< (+ i 1) n) (push (subseq prog i (+ i 2)) tokens))
                  (incf i 2))
                 ((char= c (code-char #x02))
                  (if (>= (+ i 2) n)
                      (progn (push (subseq prog i) tokens) (setf i n))
                      (progn (push (subseq prog i (+ i 3)) tokens) (setf i (+ i 3)))))
                 ((char= c (code-char #x03))
                  (if (>= (+ i 4) n)
                      (progn (push (subseq prog i) tokens) (setf i n))
                      (progn (push (subseq prog i (+ i 5)) tokens) (setf i (+ i 5)))))
                 ((char= c (code-char #x07))
                  (if (>= (+ i 1) n)
                      (progn (push (subseq prog i) tokens) (setf i n))
                      (progn (push (subseq prog i (+ i 2)) tokens) (setf i (+ i 2)))))
                 (t (push (subseq prog i (1+ i)) tokens) (incf i)))))
    (nreverse tokens)))

(defun prog-starts-with-string-p (prog)
  ;; re.match('^[^\x04]*[ENDS]', prog): true if an END byte appears before any \x04
  (loop for c across prog
        do (cond ((char= c (code-char #x04)) (return nil))
                 ((char-in c *string-ends*) (return t)))
        finally (return nil)))

(defun tokenize (prog)
  ;; string hack
  (when (prog-starts-with-string-p prog)
    (setf prog (concatenate 'string (string (code-char #x04)) prog)))
  (let ((mode nil))
    (when (and (> (length prog) 0) (member (char prog 0) (list (code-char #x30) (code-char #x31) (code-char #x32))
                        :test #'char=))
      (setf mode (char prog 0))
      (setf prog (subseq prog 1)))
    (let* ((tokens (regex-tokenize prog))
           (final nil)
           (blocks (list (%make-gs2-block (make-array 0 :adjustable t
                                                 :fill-pointer 0)))))
      (labels ((code (b) (gs2-block-code b))
             (code-push (b x) (vector-push-extend x (gs2-block-code b)))
             (code-len (b) (length (gs2-block-code b)))
             (code-sub (b num)   ; take last num (python slice: forgiving), delete, return
               (let* ((c (gs2-block-code b))
                      (n (code-len b))
                      (m (min num n)))
                 (let ((ts (make-array m)))
                   (loop for k below m do (setf (aref ts k) (aref c (+ (- n m) k))))
                   (setf (fill-pointer c) (- n m))
                   ts))))
        (loop for tok across (coerce tokens 'vector)
              do (cond
                   ((and (= (length tok) 1) (char= (char tok 0) (code-char #x08))) ; {
                    (push (%make-gs2-block (make-array 0 :adjustable t
                                                  :fill-pointer 0)) blocks)
                    (push (string (code-char #x00)) final))
                   ((and (= (length tok) 1) (char= (char tok 0) (code-char #x09))) ; }
                    (let ((b (pop blocks)))
                      (code-push (first blocks) b))
                    (code-push (first blocks) (pop final)))
                   ((and (= (length tok) 1)
                         (<= #xe0 (char-code (char tok 0)) #xff)
                         (< (logand (char-code (char tok 0)) 7) 6))
                    ;; quick block
                    (let* ((num (1+ (logand (char-code (char tok 0)) 7)))
                           (ts (code-sub (first blocks) num)))
                      (code-push (first blocks) (%make-gs2-block ts))
                      (code-push (first blocks)
                                 (subseq (map 'string #'code-char
                                              '(#x00 #x34 #x35 #x38))
                                         (logand (ash (char-code (char tok 0)) -3) 3)
                                         (1+ (logand (ash (char-code (char tok 0)) -3) 3))))))
                   ((and (= (length tok) 1)
                         (member (char tok 0) (list (code-char #xee) (code-char #xef)) :test #'char=))
                    ;; zipwith
                    (let* ((num (1+ (logand (char-code (char tok 0)) 1)))
                           (ts (code-sub (first blocks) num)))
                      (code-push (first blocks) (%make-gs2-block ts))
                      (code-push (first blocks) (string (code-char #xb1)))))
                   ((and (= (length tok) 1)
                         (member (char tok 0) (list (code-char #xf6) (code-char #xf7)) :test #'char=))
                    ;; dump-map1 / dump-filter1
                    (let ((f (if (char= (char tok 0) (code-char #xf6)) (string (code-char #x34)) (string (code-char #x35))))
                          (x (vector-pop (gs2-block-code (first blocks)))))
                      (code-push (first blocks)
                                 (%make-gs2-block (vector (string (code-char #x0e)) x)))
                      (code-push (first blocks) f)))
                   ((and (= (length tok) 1) (char= (char tok 0) (code-char #xfe))) ; m:
                    (push (%make-gs2-block (make-array 0 :adjustable t
                                                  :fill-pointer 0)) blocks)
                    (push (string (code-char #x34)) final))
                   ((and (= (length tok) 1) (char= (char tok 0) (code-char #xff))) ; f:
                    (push (%make-gs2-block (make-array 0 :adjustable t
                                                  :fill-pointer 0)) blocks)
                    (push (string (code-char #x35)) final))
                   (t (code-push (first blocks) tok))))
        (loop while final
              do (let ((b (pop blocks)))
                   (code-push (first blocks) b))
                 (code-push (first blocks) (pop final)))
        (assert (= (length blocks) 1))
        (let ((main (first blocks)))
          (macrolet ((wrap (&rest code)
                       `(setf main (%make-gs2-block
                                    (apply #'vector
                                           (list ,@code))))))
            (cond ((and mode (char= mode (code-char #x30)))
                   (setf main (%make-gs2-block (vector (string (code-char #x2a)) main (string (code-char #x34)) (string (code-char #x54))))))
                  ((and mode (char= mode (code-char #x31)))
                   (setf main (%make-gs2-block (vector (string (code-char #x2c)) main (string (code-char #x34)) (string (code-char #x55))))))
                  ((and mode (char= mode (code-char #x32)))
                   (setf main (%make-gs2-block
                               (vector (string (code-char #x2a)) (string (code-char #x22)) main (string (code-char #x34)) (string (code-char #x54))))))))
          main)))))

