#!/usr/bin/env bash
# gs2cl — run gs2 snippets from the command line
#
# Usage:
#   gs2cl 'snippet'          # assemble snippet, run with empty stdin
#   gs2cl -f prog.gs2c       # assemble file, run
#   echo 'data' | gs2cl 'snippet'   # feed stdin
#   gs2cl -c 'snippet'       # assemble only: emit gs2 bytes to stdout
#   gs2cl --bytecode 'snippet'     # show assembled bytes (hex) without running
#
# Snippets use the gs2c assembler syntax (mnemonics resolved by the built-in
# resolver): read-num range1 m: "*" times new-line  etc.
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

exec sbcl --noinform --disable-debugger \
  --eval "(push (truename \"$DIR/../\") asdf:*central-registry*)" \
  --eval '(asdf:load-system :decisive :quiet t)' \
  --eval "(gs2/cli:main (cdr sb-ext:*posix-argv*))" \
  --quit -- "$@"
