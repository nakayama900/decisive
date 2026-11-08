;;;; gs2.asd — gs2 esolang interpreter + assembler in Common Lisp
(asdf:defsystem #:gs2
  :description "gs2 esolang (github.com/nooodl/gs2 v0.2) interpreter and assembler, faithful CL port, plus comprolib (competitive-programming library) and a bridge that lets gs2 snippets call comprolib via short aliases"
  :author "ported to Common Lisp"
  :license "MIT"
  :serial t
  :depends-on (#:cl-ppcre
               #:cffi                 ; comprolib deps (gsll optional at runtime)
               #:str
               #:queues.simple-queue
               #:trees
               #:pileup
               #:trivia
               #:iterate)
  :components
  ((:module "lib"
    :components ((:file "comprolib")))
   (:file "src/package")
   (:file "src/values")
   (:file "src/pylib")                 ; uses comprolib sieve/prime-p/factorize
   (:file "src/format")
   (:file "src/regex")
   (:file "src/tokenizer")
   (:file "src/machine")
   (:file "src/strpred")
   (:file "src/ophelpers")
   (:file "src/ops")                   ; includes cpl-call (op #xC7)
   (:file "src/comprobridge")          ; alias table (ops reads *cpl-alias-vector*)
   (:file "src/assembler")             ; resolver emits #xC7 <id> for aliases
   (:file "src/driver")
   (:module "cli"
    :components ((:file "gs2cl")))))
