#!/usr/bin/env bash
# decisive — gs2 demo snippets (all verified against the python oracle
# where applicable; run via cli/gs2cl.sh)

# 1. FizzBuzz 1..100 — the interpreter ships a dedicated 1-byte opcode
cli/gs2cl.sh 'fizzbuzz'

# 2. ASCII pyramid — stdin is the height
echo 5 | cli/gs2cl.sh 'read-num range1 m: "*" times new-line'

# 3. Cubes 1..10 — m: opens a map block closed by }
cli/gs2cl.sh '10 range1 m: 3 pow } show-lines'

# 4. Uppercase — per-char arithmetic through a map block
cli/gs2cl.sh '"hello" m: 32 - } show-space'

# 5. Sum 1..100
cli/gs2cl.sh '100 range1 sum show-line'

# 6. Reverse
cli/gs2cl.sh '"hello, gs2" reverse show-line'

# 7. Primality (1 = true)
cli/gs2cl.sh '17 is-prime show-line'

# 8. Exponentiation
cli/gs2cl.sh '2 10 pow show-line'

# --- comprolib bridge (op #xC7) ---

# 9. powmod
cli/gs2cl.sh '2 10 1000 powmod show-line'   # -> 24

# 10. ispr
cli/gs2cl.sh '17 ispr show-line'            # -> 1

# --- hit & blow (4-digit, secret 1234) ---
# hit count: stdin guess vs 1234, digit-wise equality via zip + map
echo 1243 | cli/gs2cl.sh 'read-num digits 1234 digits zip { dup head swap tail head = } m: sum show-line'
# (REPL/library users: the same pipeline evaluated through gs2:gs2-run
#  returns 2 for guess 1243 — verified against the python oracle.)
