;;;; cli/gs2cl.lisp — command-line front end for gs2
;;;
;;; The assembler resolver lives in gs2:assemble (mnemonic table built from
;;; the same opcode comments as gs2c.py).  This entry point parses argv,
;;; assembles the snippet, and either emits bytes or runs the program.

(defpackage :gs2/cli
  (:use #:cl)
  (:export #:main))

(in-package :gs2/cli)

(defparameter +usage+ "usage: gs2cl [options] [snippet]
  -f FILE     assemble FILE (gs2c syntax) and run it
  -c          assemble only, emit gs2 bytes to stdout
  --bytecode  print assembled bytes as hex, do not run
  --          end of options; snippet follows
  snippet     gs2c syntax snippet (mnemonics, numbers, \"strings\", ( ... )
")

(defun slurp-ascii (path)
  ;; read raw bytes as latin-1 string (same approach as gs2:run-file)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (with-output-to-string (out)
      (loop for b = (read-byte s nil nil)
            while b do (write-char (code-char b) out)))))

(defun read-stdin-when-piped ()
  "Read all of stdin if it is a pipe/file; empty string on a tty."
  (if (sb-impl::interactive-stream-p *standard-input*)
      ""
      (with-output-to-string (out)
        (let ((buf (make-array 4096 :element-type 'character)))
          (loop for n = (read-sequence buf *standard-input*)
                do (write-string buf out :end n)
                until (< n (length buf)))))))

(defun write-latin1 (s &optional (stream *standard-output*))
  (loop for c across s
        do (write-byte (char-code c) stream)))

(defun main (argv)
  (handler-case
      (let* ((args (coerce argv 'list))
             (mode :run)
             (file nil)
             (snippet nil))
        (loop while args
              do (let ((a (first args)))
                   (cond
                     ((string= a "-f") (setf file (second args) args (cddr args)))
                     ((string= a "-c") (setf mode :cat args (cdr args)))
                     ((string= a "--bytecode") (setf mode :hex args (cdr args)))
                     ((string= a "-h") (progn (write-line +usage+) (sb-ext:exit :code 0)))
                     ((string= a "--help") (progn (write-line +usage+) (sb-ext:exit :code 0)))
                     (t (setf snippet a args (cdr args))))))
        (unless (or snippet file)
          (write-line +usage+ *error-output*)
          (sb-ext:exit :code 2))
        (let* ((source (if file (slurp-ascii file) snippet))
               (bytes (gs2:assemble source)))
          (ecase mode
            (:cat (write-latin1 bytes))
            (:hex (format t "~{~a~}"
                          (map 'list (lambda (b)
                                       (format nil "~c~c"
                                               (char "0123456789abcdef" (floor b 16))
                                               (char "0123456789abcdef" (mod b 16))))
                               (map 'list #'char-code bytes)))
                  (terpri))
            (:run (let ((stdin (read-stdin-when-piped)))
                    (write-latin1 (gs2:gs2-run bytes stdin)))))))
    (gs2:gs2-error (c)
      (format *error-output* "gs2 error: ~a~%" (gs2:gs2-error-message c))
      (sb-ext:exit :code 1))))
