(in-package :gs2)

;;; ---------------------------------------------------------------- helpers

(defun to-gs (ps)
  (declare (type string ps))
  (map '(vector t) #'char-code ps))

(defun %p2c (n)
  (unless (<= 0 n 255) (error 'gs2-error :message "py2 chr range error"))
  (code-char n))

(defun to-ps (gs)
  (cond ((gs2-list-p gs)
         (map 'string #'%p2c gs))
        (t (%p2c gs))))

(defun show (value &optional nest)
  (cond ((gs2-list-p value)
         (let ((out (make-string-output-stream)))
           (loop for x across value do (write-string (show x t) out))
           (get-output-stream-string out)))
        ((and nest (gs2-num-p value)) (string (%p2c value)))
        ((gs2-block-p value) (py-repr value))
        (t (format nil "~a" value))))

(defun gs2-apply2 (fn a b) (funcall fn a b)) ; placeholder for readability

(defun split (a b clean)
  (let ((res (list (make-array 0 :adjustable t :fill-pointer 0)))
        (lb (length b))
        (i 0))
    (loop while (< i (length a))
          if (and (<= (+ i lb) (length a))
                  (loop for j below lb always (eql (aref a (+ i j)) (aref b j))))
            do (push (make-array 0 :adjustable t :fill-pointer 0) res)
               (incf i lb)
          else
            do (vector-push-extend (aref a i) (car res))
               (incf i))
    ;; res is reversed: re-reverse in place order (python res order)
    (let ((ordered (nreverse (mapcar #'identity res))))
      (map 'vector
           (lambda (r) (make-array (length r) :element-type t
                                   :initial-contents r))
           (if clean (remove-if (lambda (r) (zerop (length r))) ordered)
               ordered)))))

(defun join (a b)
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for i from 0 and x across a
          do (unless (zerop i) (loop for c across b do (vector-push-extend c out)))
             (if (gs2-list-p x)
                 (loop for c across x do (vector-push-extend c out))
                 (vector-push-extend x out)))
    out))

(defun set-diff (a b)
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for x across a
          unless (loop for y across b thereis (gs2-equal x y))
            do (vector-push-extend x out))
    out))

(defun set-and (a b)
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for x across a
          when (loop for y across b thereis (gs2-equal x y))
            do (vector-push-extend x out))
    out))

(defun set-or (a b)
  (concatenate 'vector a (set-diff b a)))

(defun set-xor (a b)
  (concatenate 'vector (set-diff a b) (set-diff b a)))

;;; python2 equality across ints/lists/blocks: == is type-sensitive
(defun gs2-equal (x y)
  (cond ((and (gs2-num-p x) (gs2-num-p y)) (= x y))
        ((and (gs2-list-p x) (gs2-list-p y))
         (and (= (length x) (length y))
              (loop for i below (length x)
                    always (gs2-equal (aref x i) (aref y i)))))
        ((and (gs2-block-p x) (gs2-block-p y)) (gs2-equal-block x y))
        (t nil)))

(defun gs2-equal-block (x y)
  (and (= (length (gs2-block-code x)) (length (gs2-block-code y)))
       (loop for a across (gs2-block-code x)
             for b across (gs2-block-code y)
             always (typecase a
                      (gs2-block-struct (and (gs2-block-p b)
                                             (gs2-equal-block a b)))
                      (string (and (stringp b) (string= a b)))
                      (t (eql a b))))))

;;; structural equality used by sorted/sort-key compare: python compares
;;; mixed types by name-ordering of type; we only need int/int and list/list
(defun gs2-cmp (x y)
  (cond ((and (gs2-num-p x) (gs2-num-p y))
         (cond ((< x y) -1) ((> x y) 1) (t 0)))
        ((and (gs2-list-p x) (gs2-list-p y))
         (let ((n (min (length x) (length y))))
           (loop for i below n
                 for c = (gs2-cmp (aref x i) (aref y i))
                 do (when (/= c 0) (return-from gs2-cmp c)))
           (cond ((< (length x) (length y)) -1)
                 ((> (length x) (length y)) 1)
                 (t 0))))
        (t (error 'gs2-error
                  :message "py2 cross-type compare not supported"))))

(defun gs2< (x y) (< (gs2-cmp x y) 0))

;;; python2 stable sort with key; keys are gs2 values (nums or lists)
(defun py-sort (seq &key (key #'identity))
  (stable-sort (copy-seq seq) #'gs2< :key key))

;;; python fractions.gcd (not abs)
(defun py-gcd (a b)
  (loop until (zerop b)
        do (psetf a b b (mod a b)))
  a)

(defun py-lcm (a b)
  (if (and (zerop a) (zerop b))
      0
      (abs (truncate (* a b) (py-gcd a b)))))

