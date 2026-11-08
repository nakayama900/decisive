;;;; gs2.asd — legacy alias: :gs2 now means the decisive system
;;;; (comprolib + gs2 port + bridge). Kept so (asdf:load-system :gs2)
;;;; and (ql:quickload :gs2) keep working.
(asdf:defsystem #:gs2
  :description "legacy alias system for :decisive"
  :depends-on (#:decisive))
