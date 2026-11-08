(in-package :gs2)

;;; python str method emulations (latin-1) — string-level predicates
(defun py-alnum-p (x) (and (plusp (length x)) (every #'(lambda (c) (or (alphanumericp c) (char= c #\V))) x)))
(defun py-alpha-p (x) (and (plusp (length x)) (every #'alpha-char-p x)))
(defun py-digit-p (x) (and (plusp (length x)) (every #'(lambda (c) (digit-char-p c)) x)))
(defun py-space-p (x) (and (plusp (length x)) (every #'(lambda (c) (member c '(#\space #\tab #\newline #\return) :test #'char=)) x)))
(defun py-lower-p (x) (let ((cased (remove-if-not #'alpha-char-p x)))
                        (and (plusp (length cased))
                             (every #'lower-case-p cased))))
(defun py-upper-p (x) (let ((cased (remove-if-not #'alpha-char-p x)))
                        (and (plusp (length cased))
                             (every #'upper-case-p cased))))
(defun py-printable-p (x) (and (every #'(lambda (c) (<= 32 (char-code c) 126)) x)))
(defun py-hexdigit-p (x) (every #'(lambda (c) (find c "0123456789abcdefABCDEF" :test #'char=)) x))

(defun py-str-split-space (s)
  ;; python str.split() with no args: split on runs of whitespace, no empties
  (let ((parts nil) (cur (make-string-output-stream)) (any nil))
    (loop for c across s
          if (member c '(#\space #\tab #\newline #\return) :test #'char=)
            do (when any
                 (push (get-output-stream-string cur) parts)
                 (setf any nil))
          else
            do (write-char c cur) (setf any t))
    (when any (push (get-output-stream-string cur) parts))
    (nreverse parts)))

(defun py-ljust (s w fill)
  (if (>= (length s) w) s
      (concatenate 'string s (make-string (- w (length s))
                                          :initial-element fill))))
(defun py-rjust (s w fill)
  (if (>= (length s) w) s
      (concatenate 'string (make-string (- w (length s))
                                        :initial-element fill) s)))
(defun py-center (s w fill)
  (let ((total (- w (length s))))
    (if (<= total 0) s
        (let* ((left (floor total 2))
               (right (- total left)))
          (concatenate 'string
                       (make-string left :initial-element fill)
                       s
                       (make-string right :initial-element fill))))))

(defun py-str-repr (x)
  ;; python repr of a str: '...' with escapes
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for c across x
          for code = (char-code c)
          do (cond ((= code 92) (write-string "\\\\" out))
                   ((= code (char-code #\')) (write-string "\\'" out))
                   ((= code 10) (write-string "\\n" out))
                   ((= code 9) (write-string "\\t" out))
                   ((= code 13) (write-string "\\r" out))
                   ((or (< code 32) (<= 127 code 159))
                    (format out "\\x~c~c"
                            (char "0123456789abcdef" (floor code 16))
                            (char "0123456789abcdef" (mod code 16))))
                   (t (write-char c out))))
    (write-char #\' out)))

(defun py-repr (x)
  ;; python repr of int / list / block (tokens are strings)
  (cond ((gs2-num-p x) (format nil "~d" x))
        ((stringp x) (py-str-repr x))
        ((gs2-list-p x)
         (with-output-to-string (out)
           (write-char #\[ out)
           (loop for i across x
                 for first = t then nil
                 do (unless first (write-string ", " out))
                    (write-string (py-repr i) out))
           (write-char #\] out)))
        ((gs2-block-p x)
         (with-output-to-string (out)
           (write-string "Block(code=[" out)
           (loop for i across (gs2-block-code x)
                 for first = t then nil
                 do (unless first (write-string ", " out))
                    (write-string
                     (typecase i
                       (string (py-str-repr i))
                       (gs2-block-struct (py-repr i))
                       (t (format nil "~a" i)))
                     out))
           (write-string "])" out)))
        (t (format nil "~a" x))))

(defun eval-map (m f x)
  (let ((l0 (slen m)))
    (loop for i across x
          do (spush m i) (gs2-eval-tokens m f))
    (setf (stack-last-n m (- (slen m) l0)) (stack-slice m l0))))

(defun eval-filter (m f x)
  ;; python: for i in x: push i; evaluate(f); if pop(): push i
  ;;         finally stack[l0:] = [stack[l0:]]
  (let ((l0 (slen m)))
    (loop for i across x
          do (spush m i)
             (gs2-eval-tokens m f)
             (let ((r (spop m)))
               (when (gs2-truthy r) (spush m i))))
    ;; stack[l0:] = [stack[l0:]]
    (let ((tail (make-array (- (slen m) l0))))
      (loop for j from l0 below (slen m)
            do (setf (aref tail (- j l0)) (aref (gs2-stack m) j)))
      (setf (fill-pointer (gs2-stack m)) l0)
      (spush m tail))))

