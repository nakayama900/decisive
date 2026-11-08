;;; gs2-diff.lisp — batch differential-test driver: reads JSON test cases from
;;; stdin ({"code": "<hex or latin1-escaped>", "stdin": "..."} lines), prints
;;; one JSON result per line: {"id":..,"out":"<latin-1 escaped>"}.
;;; Escaping: use \uXXXX for code points; latin-1 output re-escaped the same way.

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre :silent t)
;; load the split system relative to this file's location (tests/.. = repo root)
(push (make-pathname :name nil :type nil
                     :directory (butlast (pathname-directory *load-truename*)))
      asdf:*central-registry*)
(asdf:load-system :decisive)
(in-package :gs2)

(defun unescape-json-str (s)
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length s))
          do (let ((c (char s i)))
               (cond
                 ((char/= c #\\) (write-char c out) (incf i))
                 (t (let ((d (char s (1+ i))))
                      (case d
                        ((#\n) (write-char #\newline out) (incf i 2))
                        ((#\t) (write-char #\tab out) (incf i 2))
                        ((#\\) (write-char #\\ out) (incf i 2))
                        ((#\") (write-char #\" out) (incf i 2))
                        ((#\/) (write-char #\/ out) (incf i 2))
                        ((#\r) (write-char #\return out) (incf i 2))
                        ((#\b) (write-char (code-char 8) out) (incf i 2))
                        ((#\f) (write-char (code-char 12) out) (incf i 2))
                        ((#\u) (let ((cp (parse-integer s :start (+ i 2)
                                                            :end (+ i 6)
                                                            :radix 16)))
                                 (write-char (code-char (logand cp #xff)) out)
                                 (incf i 6)))
                        (t (write-char c out) (incf i 1))))))))))

(defun escape-json-str (s)
  (with-output-to-string (out)
    (loop for c across s
          for code = (char-code c)
          do (cond ((= code 10) (write-string "\\n" out))
                   ((= code 9) (write-string "\\t" out))
                   ((= code 13) (write-string "\\r" out))
                   ((< code 32) (format out "\\u~4,'0x" code))
                   ((= code (char-code #\")) (write-string "\\\"" out))
                   ((= code (char-code #\\)) (write-string "\\\\" out))
                   ((> code 126) (format out "\\u~4,'0x" code))
                   (t (write-char c out))))))

;; crude JSON line parser: {"code": "...", "stdin": "..."} with only those keys
(defun parse-test-line (line)
  (let ((code "") (stdin "") (id ""))
    ;; find "code": then read quoted string
    (flet ((grab (key)
             (let ((pos (search (format nil "\"~a\":" key) line)))
               (when pos
                 (let ((q1 (position #\" line :start (+ pos (length key) 3))))
                   (when q1
                     ;; find closing quote, honoring backslash escapes
                     (let ((i (1+ q1)))
                       (loop while (< i (length line))
                             do (cond ((char= (char line i) #\\) (incf i 2))
                                      ((char= (char line i) #\") (return))
                                      (t (incf i))))
                       (unescape-json-str (subseq line (1+ q1) i)))))))))
      (setf id (or (grab "id") ""))
      (setf code (or (grab "code") ""))
      (setf stdin (or (grab "stdin") "")))
    (values code stdin id)))

(defun main ()
  (loop for line = (read-line *standard-input* nil nil)
        while line
        do (when (> (length (strip-ws line)) 0)
             (multiple-value-bind (code stdin id) (parse-test-line line)
               (let ((out (gs2-run code stdin)))
                 (format t "{\"id\":\"~a\",\"out\":\"~a\"}~%"
                         (escape-json-str id) (escape-json-str out))
                 (finish-output)))))
  (sb-ext:exit))

(defun strip-ws (s)
  (string-trim '(#\space #\tab #\return #\newline) s))

(main)
