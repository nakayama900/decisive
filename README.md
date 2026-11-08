# decisive

**decisive** is a competitive-programming library for Common Lisp (SBCL).

The core is **comprolib** — a feature-gated library where unused sections
are never compiled or loaded (they simply do not appear in the FASL).
Bundled alongside it is **gs2** — a byte-exact Common Lisp port of the
[gs2](https://github.com/nooodl/gs2) esolang (v0.2) interpreter +
assembler, usable as a golfing calculator inside the same system — and a
**bridge** that lets gs2 snippets call comprolib functions through short
aliases.

## comprolib — what's inside

Every section is behind a `#+` reader gate. Load only what you need:

```lisp
(pushnew :cpl-dsu *features*)    ; just DSU
(pushnew :cpl-all *features*)    ; everything
```

| feature | contents |
|---|---|
| `:cpl-math` | extgcd, pow-mod, inv-mod, prime-p, factorize, sieve, crt, floor-sum |
| `:cpl-combi` | make-combi / comb-nck (nCk mod p, O(n) precompute) |
| `:cpl-dsu` | Union-Find (path compression + union by rank) |
| `:cpl-fenwick` | Fenwick tree (BIT) |
| `:cpl-segtree` | point-update / range-query segtree (op/e as functions) |
| `:cpl-lazysegtree` | lazy-propagation segtree |
| `:cpl-string` | z-algorithm, rolling hash (double), suffix array, lcp array, trie |
| `:cpl-graph` | dijkstra, LCA (doubling), SCC (Kosaraju), BFS (incl. grid), Floyd-Warshall |
| `:cpl-flow` | Dinic max-flow, min-cost flow |
| `:cpl-2sat` | 2-SAT |
| `:cpl-convolution` | NTT convolution (mod p); FFT optional via gsll |
| `:cpl-container` | queue/deque (queues), ordered set / multiset (trees AVL), heap (pileup) |
| `:cpl-io` | fast input readers, read-int-line, print-yes-no |
| `:cpl-util` | chmax/chmin, iota, argmax/argmin, run-length-encode, next-permutation, binary-search, lower/upper-bound, cumsum(2d), modint |

Dependencies: `queues.simple-queue`, `trees`, `pileup`, `str`, `cl-ppcre`,
`trivia`, `iterate`, `cffi` (gsll/GSL optional — only for the FFT path).

## gs2 — golfing calculator bundled in

The gs2 port is byte-exact against the Python original (differential
test: 261/261 cases, stdout compared byte for byte).

```console
$ echo 5 | cli/gs2cl.sh 'read-num range1 m: "*" times new-line'
*
**
***
****
*****

$ cli/gs2cl.sh -c '...'            # assemble only, emit bytes
$ cli/gs2cl.sh --bytecode '...'    # hex dump
$ cli/gs2cl.sh -f prog.gs2c < in   # assemble a file and run
```

Library use:

```lisp
(asdf:load-system :decisive)
(comprolib:sieve 10)                                  ; => (2 3 5 7)
(comprolib:prime-p 7)                                 ; => T
(gs2:gs2-run (gs2:assemble "2 10 1000 powmod") "")    ; => "24"
```

`(asdf:load-system :gs2)` still works — it is now a thin alias system
that depends on `:decisive`.

## the bridge: comprolib from gs2 snippets

Unknown symbols in gs2 snippets that match a comprolib alias are
assembled to the custom opcode `#xC7 <id>` (unused in original gs2).

Available aliases: `powmod` `invmod` `gcd` `extgcd` `ispr` `fact` `crt`
`nck` `fw` `fwadd` `fwsum` (Fenwick, handle-based) `sa` `lcp` `lb` `ub`
`iota` `rle` `argmax` `argmin` — `argmax`/`argmin` accept a gs2 block as
the predicate, evaluated through a CL closure.

```console
$ cli/gs2cl.sh '2 10 1000 powmod'
24
$ cli/gs2cl.sh '7 ispr'
1
```

## layout

```
decisive.asd       system definition (:decisive; gs2.asd is a legacy alias)
lib/
  comprolib.lisp   the competitive-programming library (feature-gated)
src/
  package.lisp     packages, conditions, value model
  values.lisp      to-gs/to-ps, show, py2 equality/compare/sort
  pylib.lisp       prime helpers (built on comprolib sieve/prime-p/factorize)
  format.lisp      python %-format subset
  regex.lisp       cl-ppcre based python-regex emulation
  tokenizer.lisp   string literals, quick-blocks, mode prefixes
  machine.lisp     gs2 machine, evaluate, run-file, quine fallback
  strpred.lisp     str.isalnum/... (string-level predicates)
  ophelpers.lisp   shared op helpers
  ops.lisp         opcode dispatch (141 opcodes) + #xC7 comprolib bridge
  comprobridge.lisp alias table, object handles, value conversion
  assembler.lisp   gs2c-syntax assembler + mnemonic resolver (emits #xC7)
  driver.lisp      *gs2-divergences* documentation
cli/
  gs2cl.lisp       command-line entry point
  gs2cl.sh         thin shell wrapper
tests/
  gs2-diff.lisp         batch differential-test driver
  gs2-diff-test.py      261-case oracle comparison
```

## verification

```
$ cd tests && python3 gs2-diff-test.py
total cases: 261
mismatches: 0 / 261
```

## requirements

- SBCL 2.x
- Quicklisp (`cl-ppcre`, `cffi`, `str`, `queues.simple-queue`, `trees`,
  `pileup`, `trivia`, `iterate`)
- gsll/GSL optional (FFT convolution only)

## license

MIT
