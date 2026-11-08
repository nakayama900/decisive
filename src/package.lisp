;;; gs2.lisp — gs2 esolang (github.com/nooodl/gs2, v0.2) interpreter +
;;; assembler, faithful port of gs2.py / gs2c.py to Common Lisp (SBCL).
;;;
;;; Values:  integer | list (vector of gs2 values) | block (unevaluated code)
;;; Strings are lists of byte codes (0-255), like the py2 byte strings in gs2.py.
;;;
;;; Usage:
;;;   (gs2:run-file "prog.gs2")            ; stdin/stdout = *standard-input*
;;;   (gs2:gs2-run code-string stdin-string) => stdout string
;;;   (gs2:assemble "read-num range1 m: \"*\" times new-line") => byte vector
;;;
;;; Divergences from the Python original (py2 semantics) are documented in
;;; *gs2-divergences* at the bottom of this file.

(defpackage :gs2
  (:use #:cl)
  (:export #:run-file #:gs2-run #:assemble #:gs2-error #:gs2-error-message
           #:make-gs2 #:gs2-eval-tokens #:tokenize #:show #:to-gs #:to-ps
           #:gs2-block-p #:gs2-block-code #:%make-gs2-block
           #:gs2-num-p #:gs2-list-p #:gs2-stack #:gs2-junk
           #:*gs2-divergences*))
(in-package :gs2)

(declaim (optimize (speed 0) (safety 3) (debug 2)))

(define-condition gs2-error (error)
  ((message :initarg :message :reader gs2-error-message))
  (:report (lambda (c s) (format s "~a" (gs2-error-message c)))))

(defstruct (gs2-block-struct
             (:constructor %make-gs2-block (code))
             (:copier nil))
  code)

(declaim (inline gs2-num-p gs2-list-p gs2-block-p))
(defun gs2-num-p (v) (integerp v))
(defun gs2-list-p (v) (and (vectorp v) (not (stringp v))))
(declaim (inline gs2-block-p gs2-block-code))
(defun gs2-block-p (v) (typep v 'gs2-block-struct))
(defun gs2-block-code (b) (slot-value b 'code))

(deftype gs2-list () '(and vector (not string)))

