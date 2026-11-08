(in-package :gs2)

;;; ---------------------------------------------------------------- machine

(defstruct (gs2 (:constructor %make-gs2))
  code stdin stack regs counter junk)

(defvar *gs2-exit* (gensym "GS2-EXIT"))

(defun make-gs2 (code stdin)
  (let ((stdin-gs (to-gs stdin)))
    (%make-gs2
     :code code
     :stdin stdin-gs
     :stack (make-array 1 :adjustable t :fill-pointer 1
                        :initial-element stdin-gs)
     :regs (vector (copy-seq stdin-gs)          ; A
                   (length stdin)               ; B
                   (to-gs code)                 ; C
                   (random 3))                  ; D
     :counter 1
     :junk nil)))

(declaim (inline spush spop speek slen))
(defun spush (m x) (vector-push-extend x (gs2-stack m)))
(defun slen (m) (length (gs2-stack m)))
(defun speek (m &optional (i -1))
  (aref (gs2-stack m) (+ (slen m) i)))
(defun spop (m &key (junk t) (i -1))
  (let ((idx (+ (slen m) i)))
    (when (< idx 0) (error 'gs2-error :message "pop from empty stack"))
    (let ((x (aref (gs2-stack m) idx)))
      (when junk (push x (gs2-junk m)))
      (setf (fill-pointer (gs2-stack m)) idx)
      x)))

(defun stack-slice (m from)
  (let ((v (make-array (- (slen m) from))))
    (loop for i below (length v) do (setf (aref v i) (aref (gs2-stack m) (+ from i))))
    v))

(defun stack-last-n (m n) (stack-slice m (- (slen m) n)))

(defun (setf stack-last-n) (val m n)
  (let* ((st (gs2-stack m))
         (keep (- (slen m) n)))
    (setf (fill-pointer st) keep)
    (vector-push-extend val st)))

(defun stack-insert (m i x) ; python list.insert(i, x) semantics
  (let* ((st (gs2-stack m))
         (idx (max 0 (min (if (minusp i) (+ (slen m) i) i) (slen m)))))
    (vector-push-extend nil st)
    (loop for k from (- (length st) 1) downto (+ idx 1)
          do (setf (aref st k) (aref st (- k 1))))
    (setf (aref st idx) x)))

(defun stack-delete-at (m i)
  (let* ((st (gs2-stack m))
         (idx (max 0 (min (if (minusp i) (+ (length st) i) i) (1- (length st))))))
    (loop for k from idx below (- (length st) 1)
          do (setf (aref st k) (aref st (+ k 1))))
    (setf (fill-pointer st) (- (length st) 1))))

(defun stack-pop-nth (m i) ; python pop(i) with i negative-from-end, then append
  (let ((x (aref (gs2-stack m) (+ (slen m) i))))
    (stack-delete-at m i)
    (vector-push-extend x (gs2-stack m))
    x))

(defun vrange (a &optional (b nil b-p) (step 1))
  (if b-p
      (loop for i from a below b by step collect i into acc
            finally (return (coerce acc 'vector)))
      (loop for i from 0 below a collect i into acc
            finally (return (coerce acc 'vector)))))

(defun iota1 (n) ; range(1, n+1)
  (loop for i from 1 to n collect i into acc
        finally (return (coerce acc 'vector))))

(defun reversed-v (v) (reverse v))

(defun gs2-run (code stdin)
  "Run gs2 CODE (string, latin-1) with STDIN (string). Return stdout string."
  (let ((m (make-gs2 code stdin)))
    (handler-bind ((error
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'gs2-quine))))
      (restart-case
          (progn
            (gs2-eval-tokens m (tokenize code))
            (with-output-to-string (out)
              (loop for x across (gs2-stack m)
                    do (write-string (show x) out))))
        (gs2-quine ()
          :report "gs2 error fallback: print program source"
          (with-output-to-string (out) (write-string code out)))))))

(defun run-file (path &key (stdin *standard-input*))
  (let ((code (with-open-file (s path :element-type '(unsigned-byte 8))
                (flexi-streams-octets->string s)))
        (in (read-remaining stdin)))
    (gs2-run code in)))

(defun read-remaining (stream)
  (with-output-to-string (out)
    (loop for c = (read-char stream nil nil)
          while c do (write-char c out))))

(defun flexi-streams-octets->string (s)
  ;; read raw bytes as latin-1 string
  (with-output-to-string (out)
    (loop for b = (read-byte s nil nil)
          while b do (write-char (code-char b) out))))



(defun block-dispatch (m tok)
  (when (gs2-block-p tok) (spush m tok) (return-from block-dispatch))
  (let ((c0 (char-code (char tok 0))))
      (cond
        ((= c0 #x00))                                   ; nop
        ((= c0 #x01)                                    ; ubyte
         (spush m (char-code (char tok 1))))
        ((= c0 #x02)                                    ; signed short LE
         (spush m (let ((u (logior (char-code (char tok 1))
                                   (ash (char-code (char tok 2)) 8))))
                    (if (>= u #x8000) (- u #x10000) u))))
        ((= c0 #x03)                                    ; signed long LE
         (spush m (let ((u (logior (char-code (char tok 1))
                                   (ash (char-code (char tok 2)) 8)
                                   (ash (char-code (char tok 3)) 16)
                                   (ash (char-code (char tok 4)) 24))))
                    (if (>= u #x80000000) (- u #x100000000) u))))
        ((= c0 #x04) (eval-string-token m tok))
        ((= c0 #x07) (spush m (vector (char-code (char tok 1)))))
        ((= c0 #x0a) (spush m (vector 10)))
        ((= c0 #x0b) (spush m (make-array 0)))
        ((= c0 #x0c) (spush m (%make-gs2-block (make-array 0))))
        ((= c0 #x0d) (spush m (vector 32)))
        ((= c0 #x0e)                                    ; make/extract-array
         (let ((x (spop m)))
           (cond ((gs2-num-p x)
                  (let ((items (copy-seq (stack-last-n m x))))
                    (setf (stack-last-n m x) items)))
                 ((gs2-list-p x) (loop for i across x do (spush m i)))
                 (t (error 'gs2-error :message "make-array / extract-array")))))
        ;; 0x10-0x1a: small numbers
        ((<= #x10 c0 #x1a) (spush m (- c0 #x10)))
        ((= c0 #x1b) (spush m 100))
        ((= c0 #x1c) (spush m 1000))
        ((= c0 #x1d) (spush m 16))
        ((= c0 #x1e) (spush m 64))
        ((= c0 #x1f) (spush m 256))
        (t (op-dispatch m tok c0)))))

(defun gs2-eval-tokens (m block)
  (loop named outer
        for tok across (gs2-block-code block)
        do (if (gs2-block-p tok)
               (spush m tok)
               (progn
                 (when (= (char-code (char tok 0)) #x0f)
                   (return-from outer))
                 (block-dispatch m tok)))))

(defun eval-string-token (m tok)
  (let* ((end (char tok (- (length tok) 1)))
         (body (subseq tok 1 (- (length tok) 1)))
         (strings (map 'list #'to-gs (split-chars body (code-char #x07)))))
    (cond
      ((char= end (code-char #x05)) (loop for s in strings do (spush m s)))
      ((char= end (code-char #x06)) (spush m (coerce strings 'vector)))
      ((char= end (code-char #x9b))                                ; printf
       (let* ((f (to-ps (pop strings))))
         (multiple-value-bind (out n)
             (py-format f (map 'list #'to-ps (stack-last-n m (format-count-% f))))
           (setf (stack-last-n m n) (to-gs out)))))
      ((char= end (code-char #x9c))                                ; regex match
       (let* ((pattern0 (to-ps (pop strings)))
              (c+pat (regex-count pattern0))
              (c (car c+pat)) (pattern (cdr c+pat))
              (s (to-ps (spop m))))
         (spush m (if (if (plusp c)
                          (cl-ppcre:scan (concatenate 'string "^(?:" pattern ")$") s)
                          (cl-ppcre:scan (concatenate 'string "(?s)" pattern) s))
                      1 0))))
      ((char= end (code-char #x9d))                                ; regex sub
       (let* ((repl (to-ps (pop strings)))
              (pattern0 (to-ps (pop strings)))
              (c+pat (regex-count pattern0))
              (c (car c+pat)) (pattern (cdr c+pat))
              (s (to-ps (spop m))))
         (spush m (to-gs (regex-sub pattern repl s c)))))
      ((char= end (code-char #x9e))                                ; regex find
       (let* ((pattern0 (to-ps (pop strings)))
              (c+pat (regex-count pattern0))
              (c (car c+pat)) (pattern (cdr c+pat))
              (s (to-ps (spop m))))
         (spush m (regex-find pattern s c))))
      ((char= end (code-char #x9f))                                ; regex split
       (let* ((pattern0 (to-ps (pop strings)))
              (c+pat (regex-count pattern0))
              (c (car c+pat)) (pattern (cdr c+pat))
              (s (to-ps (spop m))))
         (spush m (regex-split pattern s c))))
      (t (error 'gs2-error :message "bad string end")))))

(defun split-chars (str sep)
  ;; python str.split(sep): split on single-char separator, no empties removed
  (let ((parts nil) (cur (make-string-output-stream)))
    (loop for c across str
          if (char= c sep)
            do (push (get-output-stream-string cur) parts)
          else do (write-char c cur))
    (push (get-output-stream-string cur) parts)
    (nreverse parts)))

(defun format-count-% (f)
  ;; n = f.count('%') - f.count('%%')*2
  (let ((pct 0) (pctpct 0))
    (loop for i below (length f)
          when (char= (char f i) #\%) do (incf pct))
    (loop for i below (1- (length f))
          when (and (char= (char f i) #\%) (char= (char f (1+ i)) #\%))
            do (incf pctpct))
    (- pct (* 2 pctpct))))

(defun regex-count (pattern)
  ;; returns (c . pattern) after stripping prefix
  (cond ((and (> (length pattern) 0) (char= (char pattern 0) #\]))
         (cons 1 (subseq pattern 1)))
        ((and (> (length pattern) 0) (char= (char pattern 0) #\}))
         (cons (char-code (char pattern 1)) (subseq pattern 2)))
        (t (cons 0 pattern))))

