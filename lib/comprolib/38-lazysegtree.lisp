(in-package :comprolib)

#+(or :cpl-all :cpl-lazysegtree)
(progn

  ;; 点更新+区間取得+区間作用。S=値, F=作用素。
  ;; make-lazysegtree n op e mapping composition id f
  (defstruct (lseg (:constructor %make-lseg
                     (&key n size log d lz op e mapping composition idf)))
    n size log d lz op e mapping composition idf)

  (defun make-lazysegtree (vec op e mapping composition id)
    (let* ((n (length vec))
           (log (max 1 (ceiling (log (max 1 n) 2))))
           (size (ash 1 log))
           (d (make-array (* 2 size)))
           (lz (make-array size)))
      (loop for i below n
            do (setf (aref d (+ size i)) (aref vec i)))
      (loop for i from (- size 1) downto 1
            do (setf (aref d i) (funcall op (aref d (* 2 i)) (aref d (1+ (* 2 i))))))
      (loop for i below size
            do (setf (aref lz i) id))
      (%make-lseg :n n :size size :log log :d d :lz lz
                  :op op :e e :mapping mapping :composition composition :idf id))))

  (defun lseg-all-apply (s k f)
    (setf (aref (lseg-d s) k)
          (funcall (lseg-mapping s) f (aref (lseg-d s) k)))
    (when (< k (lseg-size s))
      (setf (aref (lseg-lz s) k)
            (funcall (lseg-composition s) f (aref (lseg-lz s) k)))))

  (defun lseg-push (s k)
    (lseg-all-apply s (* 2 k) (aref (lseg-lz s) k))
    (lseg-all-apply s (1+ (* 2 k)) (aref (lseg-lz s) k))
    (setf (aref (lseg-lz s) k) (lseg-idf s)))

  (defun lseg-set (s p x)
    (incf p (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (lseg-push s (ash p (- i))))
    (setf (aref (lseg-d s) p) x)
    (loop for i from 1 to (lseg-log s)
          do (setf (aref (lseg-d s) (ash p (- i)))
                   (funcall (lseg-op s)
                            (aref (lseg-d s) (* 2 (ash p (- i))))
                            (aref (lseg-d s) (1+ (* 2 (ash p (- i)))))))))

  (defun lseg-get (s p)
    (incf p (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (lseg-push s (ash p (- i))))
    (aref (lseg-d s) p))

  (defun lseg-prod (s l r)
    (when (= l r) (return-from lseg-prod (lseg-e s)))
    (incf l (lseg-size s)) (incf r (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (progn
               (when (/= 0 (logand l (1- (ash 1 i))))
                     (lseg-push s (ash l (- i))))
               (when (/= 0 (logand r (1- (ash 1 i))))
                     (lseg-push s (ash (1- r) (- i))))))
    (let ((sml (lseg-e s)) (smr (lseg-e s)))
      (loop while (< l r)
            do (progn
                 (when (logtest l 1)
                   (setf sml (funcall (lseg-op s) sml (aref (lseg-d s) l)))
                   (incf l))
                 (when (logtest r 1)
                   (decf r)
                   (setf smr (funcall (lseg-op s) (aref (lseg-d s) r) smr)))
                 (setf l (ash l -1) r (ash r -1))))
      (funcall (lseg-op s) sml smr)))

  (defun lseg-all-prod (s) (aref (lseg-d s) 1))

  (defun lseg-apply (s l r f)
    (when (= l r) (return-from lseg-apply))
    (incf l (lseg-size s)) (incf r (lseg-size s))
    (loop for i from (lseg-log s) downto 1
          do (progn
               (when (/= 0 (logand l (1- (ash 1 i))))
                     (lseg-push s (ash l (- i))))
               (when (/= 0 (logand r (1- (ash 1 i))))
                     (lseg-push s (ash (1- r) (- i))))))
    (let ((l2 l) (r2 r))
      (loop while (< l2 r2)
            do (progn
                 (when (logtest l2 1)
                   (lseg-all-apply s l2 f) (incf l2))
                 (when (logtest r2 1)
                   (decf r2) (lseg-all-apply s r2 f))
                 (setf l2 (ash l2 -1) r2 (ash r2 -1)))))
    (loop for i from 1 to (lseg-log s)
          do (progn
               (when (/= 0 (logand l (1- (ash 1 i))))
                     (setf (aref (lseg-d s) (ash l (- i)))
                           (funcall (lseg-op s)
                                    (aref (lseg-d s) (* 2 (ash l (- i))))
                                    (aref (lseg-d s) (1+ (* 2 (ash l (- i))))))))
               (when (/= 0 (logand r (1- (ash 1 i))))
                     (setf (aref (lseg-d s) (ash (1- r) (- i)))
                           (funcall (lseg-op s)
                                    (aref (lseg-d s) (* 2 (ash (1- r) (- i))))
                                    (aref (lseg-d s) (1+ (* 2 (ash (1- r) (- i))))))))))
) ; end section

;;; ============ :cpl-convolution ============
