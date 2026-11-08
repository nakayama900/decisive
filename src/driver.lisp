(in-package :gs2)

;;; ---------------------------------------------------------------- divergences

(defparameter *gs2-divergences*
  '("regex: python re vs cl-ppcre — named groups/inline flags/(?s) emulation differ in edge cases; \\1 backrefs in replacement handled by cl-ppcre syntax"
    "printf %X of int uses roman numeral fallback (format ~@r) — matches python only for small values"
    "py2 cross-type comparisons (e.g. sorting mixed int/list) raise gs2-error instead of py2 type-name ordering"
    "random D register / 0x25 random: seeded differently than CPython; sequence of random values differs (statistical behavior identical)"
    "bignum: python ints are arbitrary precision; port keeps bignums too (SBCL), so identical"
    "is-prime etc. use sieve cache like the original; nth-prime bound formula identical"
    "float ops (0x3f log, 0x2d sqrt on negatives) rely on SBCL math; log(x,y) uses (log x y) which may differ from python in last-ulp cases"
    "gs2c string eval(): only \\n \\t \\\\ \" ' escapes supported (python eval handles \\xNN \\uNNNN \\ooo etc.)"))

(defun self-test ()
  (format t "gs2.lisp loaded: ~a mnemonics~%"
          (hash-table-count *mnemonic-table*)))

