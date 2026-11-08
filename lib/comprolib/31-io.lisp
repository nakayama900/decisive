(in-package :comprolib)

#+(or :cpl-all :cpl-io)
(progn

  ;; Yes/No 出力
  (defun print-yes-no (x &optional (stream *standard-output*))
    (write-line (if x "Yes" "No") stream)
    (values))

  ;; 1行を読んで空白区切り整数のベクトル
  (defun read-int-line (&optional (stream *standard-input*))
    (let ((parts (str:split " " (string-trim '(#\Space #\Return) (read-line stream)))))
      (map 'vector
           (lambda (s) (parse-integer s :junk-allowed t))
           (remove "" parts :test #'string=))))

  ;; N行読み込み
  (defun read-lines (n &optional (stream *standard-input*))
    (loop repeat n collect (read-line stream)))

  ;; 型付き入力マクロ: (input* ((n fixnum) (a (vector fixnum n))) body...)
  (defvar *input-reader-table* (make-hash-table :test #'eq))

  (defun set-input-reader (marker reader)
    (setf (gethash marker *input-reader-table*) reader))

  (defun get-input-reader (marker) (gethash marker *input-reader-table*))

  (defmacro def-input-reader (marker (specvar) &body body)
    `(set-input-reader ',marker (lambda (,specvar) ,@body)))

  (defun input-expand (forms)
    (mapcar (lambda (form)
              (list (car form)
                    (funcall (or (get-input-reader
                                  (if (listp (cadr form)) (car (cadr form)) (cadr form)))
                                 (lambda (s) (error "Unknown input typespec: ~S" s)))
                             (cadr form))))
            forms))

  (def-input-reader fixnum (s) '(read))
  (def-input-reader integer (s) '(read))
  (def-input-reader string (s) '(read-line))
  (def-input-reader list (s) `(loop repeat ,(caddr s) collect (read)))
  (def-input-reader vector (s) `(make-array ,(caddr s) :initial-contents
                                            (loop repeat ,(caddr s) collect (read))))
  (def-input-reader array (s)
    (destructuring-bind (elem dims) (cdr s)
      (if (listp dims)
          `(make-array ',dims :initial-contents
                       (loop for i below ,(first dims)
                             collect (loop repeat ,(second dims) collect (read))))
          `(make-array ,dims :initial-contents (loop repeat ,dims collect (read))))))
  (def-input-reader line (s) `(read-line))

  (defmacro input* (forms &body body)
    "競プロ向け入力束縛: (input* ((n fixnum) (a (vector fixnum n))) ...)"
    (let ((vars (mapcar #'car forms)))
      `(let* ,(input-expand forms)
         (declare (ignorable ,@vars))
         ,@body)))
) ; end section

;;; ============ :cpl-container ============
