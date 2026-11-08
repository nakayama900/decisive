;;;; gs2.asd — gs2 esolang interpreter + assembler in Common Lisp
(asdf:defsystem #:gs2
  :description "gs2 esolang (github.com/nooodl/gs2 v0.2) interpreter and assembler, faithful CL port"
  :author "ported to Common Lisp"
  :license "MIT"
  :serial t
  :depends-on (#:cl-ppcre)
  :components
  ((:file "src/package")
   (:file "src/values")
   (:file "src/pylib")
   (:file "src/format")
   (:file "src/regex")
   (:file "src/tokenizer")
   (:file "src/machine")
   (:file "src/strpred")
   (:file "src/ophelpers")
   (:file "src/ops")
   (:file "src/assembler")
   (:file "src/driver")
   (:module "cli"
    :components ((:file "gs2cl")))))
