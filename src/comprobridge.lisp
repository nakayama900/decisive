(in-package :gs2)

;;; ---------------------------------------------------------------- comprobridge
;;; Short aliases that call into comprolib (:cpl-all) from gs2 snippets.
;;;
;;; Assembler side: an unknown token present in *cpl-aliases* emits
;;;   #xC7 <id-byte>
;;; (0xC7 is unused in the original gs2 opcode map, so this port may claim it.)
;;; Interpreter side: op #xC7 pops N args (top of stack = last arg), calls the
;;; comprolib function, pushes the result.
;;;
;;; Object handles: constructors return a small int handle into *cpl-objects*
;;; (a gs2-visible int is safe in the stack, unlike a raw CL structure).
;;; Calls that need the object accept the handle int back.

(defparameter *cpl-objects* (make-hash-table))       ; handle -> CL object
(defparameter *cpl-next-handle* 1)

(defun cpl-obj (h what)
  (unless (and (integerp h) (gethash h *cpl-objects*))
    (error 'gs2-error :message (format nil "cpl ~a: bad handle ~a" what h)))
  (gethash h *cpl-objects*))

(defparameter *cpl-combi-cache* (make-hash-table))   ; modulus -> combi struct

(defparameter *cpl-aliases* nil)

(defun %cpl-reg (name nargs shape fn)
  (push (list name nargs shape fn) *cpl-aliases*))

;;; -- number theory -------------------------------------------------------
(%cpl-reg "gcd"    2 :first-of-values (lambda (a b) (comprolib:extgcd a b)))
(%cpl-reg "extgcd" 2 :values        (lambda (a b) (comprolib:extgcd a b)))
(%cpl-reg "powmod" 3 :int           (lambda (a e m) (comprolib:pow-mod a e m)))
(%cpl-reg "invmod" 2 :int           (lambda (a m) (comprolib:inv-mod a m)))
(%cpl-reg "ispr"   1 :bool          (lambda (n) (comprolib:prime-p n)))
(%cpl-reg "fact"   1 :alist         (lambda (n) (comprolib:factorize n)))
(%cpl-reg "crt"    4 :values-or-nil (lambda (r1 m1 r2 m2) (comprolib:crt r1 m1 r2 m2)))

;;; -- combinatorics --------------------------------------------------------
(%cpl-reg "nck"    3 :int (lambda (n k m)
                           (let ((c (or (gethash m *cpl-combi-cache*)
                                        (setf (gethash m *cpl-combi-cache*)
                                              (comprolib:make-combi (max n 1) m)))))
                             (comprolib:comb-nck c n k))))

;;; -- data structures (handle-based) ----------------------------------------
(%cpl-reg "fw"     1 :handle (lambda (n) (comprolib:make-fenwick n)))
(%cpl-reg "fwadd"  3 :raw    (lambda (h i x) (comprolib:fw-add (cpl-obj h "fwadd") i x) h))
(%cpl-reg "fwsum"  3 :int    (lambda (h l r) (comprolib:fw-sum (cpl-obj h "fwsum") l r)))
(%cpl-reg "sa"     1 :int-list (lambda (str) (comprolib:suffix-array str)))
(%cpl-reg "lcp"    2 :int-list (lambda (str sa) (comprolib:lcp-array str sa)))

;;; -- search / util ----------------------------------------------------------
(%cpl-reg "lb"     2 :int (lambda (vec x) (comprolib:lower-bound vec x)))
(%cpl-reg "ub"     2 :int (lambda (vec x) (comprolib:upper-bound vec x)))
(%cpl-reg "iota"   1 :int-list (lambda (n) (comprolib:iota n)))
(%cpl-reg "rle"    1 :alist (lambda (seq) (comprolib:run-length-encode seq)))
(%cpl-reg "argmax" 2 :values (lambda (f seq)
                               (comprolib:argmax (cpl-block-fn f) seq)))
(%cpl-reg "argmin" 2 :values (lambda (f seq)
                               (comprolib:argmin (cpl-block-fn f) seq)))

(defun cpl-lookup (name)
  (cdr (assoc name *cpl-aliases* :test #'string=)))

(defun cpl-alias-names ()
  (mapcar #'car *cpl-aliases*))

;;; id table: alias name -> index into *cpl-alias-vector* (same order)
;;; (built after all %cpl-reg calls above)
(defun build-cpl-id-table ()
  (let ((h (make-hash-table :test 'equal))
        (i 0))
    (dolist (e *cpl-aliases*)
      (setf (gethash (car e) h) i)
      (incf i))
    h))

(defparameter *cpl-id-table* (build-cpl-id-table))
