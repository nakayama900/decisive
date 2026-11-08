(in-package :gs2)

;;; cl-ppcre based python-regex emulation
(defun regex-sub (pattern repl s c)
  ;; count c=0 means all
  (if (zerop c)
      (cl-ppcre:regex-replace-all pattern s repl)
      (cl-ppcre:regex-replace pattern s repl)))

(defun regex-find (pattern s c)
  ;; re.findall semantics; returns vector of gs2 strings
  (let ((ms (cl-ppcre:all-matches-as-strings pattern s)))
    (if (plusp c)
        (if ms (to-gs (first ms)) (make-array 0))
        (map 'vector #'to-gs ms))))

(defun regex-split (pattern s c)
  ;; re.split semantics with maxsplit c (0 = unlimited): c splits => c+1 parts
  (let ((parts (cl-ppcre:split pattern s :limit (if (zerop c) nil (1+ c)))))
    (map 'vector #'to-gs parts)))

