# decisive

Common Lisp port of [gs2](https://github.com/nooodl/gs2) (v0.2) — a
golf-oriented stack language — including both the **interpreter** and the
**assembler** (mnemonic resolver), faithful to the Python original at the
byte level.

## Layout

```
gs2.asd            ASDF system definition
src/
  package.lisp     package, conditions, value model (int | list | block)
  values.lisp      to-gs/to-ps, show, py2 equality/compare/sort, gcd
  pylib.lisp       python helpers (factor-list, primes sieve, ...)
  format.lisp      python %-format subset
  regex.lisp       cl-ppcre based python-regex emulation
  tokenizer.lisp   string literals, quick-blocks, mode prefixes
  machine.lisp     gs2 machine, evaluate, run-file, quine fallback
  strpred.lisp     str.isalnum/islower/... (string-level predicates)
  ophelpers.lisp   shared op helpers (chunking, transpose, ...)
  ops.lisp         the big opcode dispatch (141 opcodes)
  assembler.lisp   gs2c-syntax assembler + mnemonic resolver
  driver.lisp      *gs2-divergences* documentation
cli/
  gs2cl.lisp       command-line entry point
  gs2cl.sh         thin shell wrapper
tests/
  gs2-diff.lisp         batch differential-test driver
  gs2-diff-test.py      261-case oracle comparison (python gs2 vs this port)
```

## Usage

### Library

```lisp
(asdf:load-system :gs2)
(gs2:gs2-run "read-num range1 m: \"*\" times new-line" "5")
;; => "*\n**\n***\n****\n*****\n"
(gs2:assemble "read-num range1 m: \"*\" times new-line")
;; => gs2 byte string
(gs2:run-file "prog.gs2")
```

### CLI

```console
$ echo 5 | cli/gs2cl.sh 'read-num range1 m: "*" times new-line'
*
**
***
****
*****

$ cli/gs2cl.sh -c 'read-num range1 m: "*" times new-line'   # bytes only
$ cli/gs2cl.sh --bytecode '...'                              # hex dump
$ cli/gs2cl.sh -f prog.gs2c < input                          # assemble file & run
```

The CLI resolves mnemonics through the same assembler as the library
(`gs2:assemble` — the resolver mirrors how gs2c.py parses the `#=`
opcode comments), so snippets, files and library calls all share one
implementation.

## Verification

`tests/gs2-diff-test.py` runs **261 differential cases** (141 fixed
opcode cases + 120 random programs) against the original Python
implementation and compares stdout **byte for byte**:

```
total cases: 261
mismatches: 0 / 261
```

Known, intentional divergences (RNG stream, regex engine edges, printf
fallbacks) are documented in `gs2:*gs2-divergences*`.

## Requirements

- SBCL 2.x
- Quicklisp with cl-ppcre
