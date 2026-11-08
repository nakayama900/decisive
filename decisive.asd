;;;; decisive.asd — competitive-programming library (comprolib) + bundled
;;;; gs2 esolang port with a comprolib alias bridge.
(asdf:defsystem #:decisive
  :description "decisive: comprolib (feature-gated competitive-programming library for Common Lisp) with a bundled byte-exact gs2 esolang interpreter/assembler and a bridge that lets gs2 snippets call comprolib via short aliases"
  :author "nakayama900"
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
  ;; comprolib first: the library is the core of this system
  ((:module "lib"
    :components ((:file "comprolib")))
   ;; gs2 port (bundled golfing calculator), subordinated to comprolib
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

;;;; secondary system name kept for convenience (legacy :gs2 loads too)
(asdf:defsystem #:gs2
  :description "alias for :decisive (gs2 port + comprolib bridge)"
  :serial t
  :around-compile (lambda (c) (asdf:load-system :decisive)))
