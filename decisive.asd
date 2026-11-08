;;;; decisive.asd — competitive-programming library (comprolib) + bundled
;;;; gs2 esolang port with a comprolib alias bridge.
(asdf:defsystem #:decisive
  :description "decisive: comprolib (feature-gated competitive-programming library for Common Lisp, one file per feature under lib/comprolib/) with a bundled byte-exact gs2 esolang interpreter/assembler and a bridge that lets gs2 snippets call comprolib via short aliases"
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
  ;; comprolib first: the library is the core of this system.
  ;; one file per feature — every file keeps its #+ reader gate, so ASDF
  ;; still compiles them, but each body is skipped unless its feature is set
  ;; (this system auto-enables :cpl-all in 00-package.lisp).
  ((:module "lib/comprolib"
    :components ((:file "00-package")
                 (:file "10-math")
                 (:file "20-combi")
                 (:file "21-dsu")
                 (:file "22-fenwick")
                 (:file "23-segtree")
                 (:file "24-string")
                 (:file "30-graph")
                 (:file "31-io")
                 (:file "32-container")
                 (:file "33-util")
                 (:file "34-search")
                 (:file "35-cum")
                 (:file "36-ordered")
                 (:file "37-modint")
                 (:file "38-lazysegtree")
                 (:file "39-convolution")
                 (:file "40-flow")
                 (:file "41-2sat")
                 (:file "42-suffix")
                 (:file "43-trie")))
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

(asdf:defsystem #:gs2
  :description "legacy alias: (asdf:load-system :gs2) loads :decisive"
  :depends-on (#:decisive))
