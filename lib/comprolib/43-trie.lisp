(in-package :comprolib)

#+(or :cpl-all :cpl-trie)
(progn

  (defstruct (trie-node (:constructor make-trie-node (children terminal count)))
    children terminal count)
  (defstruct (trie (:constructor %make-trie (root)))
    root)

  (defun make-trie ()
    (%make-trie (make-trie-node (make-hash-table) nil 0)))

  (defun trie-insert (tr s)
    (let ((node (trie-root tr)))
      (incf (trie-node-count node))
      (loop for ch across s
            do (let ((next (or (gethash ch (trie-node-children node))
                               (let ((nn (make-trie-node (make-hash-table) nil 0)))
                                 (setf (gethash ch (trie-node-children node)) nn)
                                 nn))))
                 (setf node next)
                 (incf (trie-node-count node))))
      (setf (trie-node-terminal node) t)
      tr))

  (defun trie-find (tr s)
    "s が登録済み単語なら T"
    (let ((node (trie-root tr)))
      (loop for ch across s
            for next = (gethash ch (trie-node-children node))
            while next
            do (setf node next)
            finally (return (and (trie-node-terminal node) t)))))

  (defun trie-prefix-count (tr s)
    "s を接頭辞とする登録単語の数 (経路count)"
    (let ((node (trie-root tr)))
      (loop for ch across s
            for next = (gethash ch (trie-node-children node))
            while next
            do (setf node next)
            finally (return (trie-node-count node)))))
) ; end section
