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


# --- 言語処理100本ノック (NLP100) 文字列系 ---
# 00. 逆順
cli/gs2cl.sh '"stressed" reverse show-line'                 # -> desserts

# 01. 奇数文字目抽出 ([::2])
cli/gs2cl.sh '"stressed" 2 % "" join show-line'             # -> srse

# 02. 交互連結 (zip + concatmap)
cli/gs2cl.sh '"pato" "taxis" zip { } ^ show-line'           # -> ptaatxoi

# 03. 円周率 (単語ごとの文字数)
cli/gs2cl.sh '"Now I need a drink alcoholic of course" words m: length show-space'

# 05. 文字bi-gram
cli/gs2cl.sh '"iamslayer" dup tail zip m: "" join } show-python'

# 06. bi-gram 集合 X / Y
cli/gs2cl.sh '"paraparaparadise" dup tail zip m: "" join } show-python'
cli/gs2cl.sh '"paragraph" dup tail zip m: "" join } show-python'

# 07. テンプレート連結
cli/gs2cl.sh '"12" " degrees at " "22.4" + + + show-line'

# 09. 単語分割
cli/gs2cl.sh '"I could not believe" words show-python'

# --- AtCoder ABC / ARC ---
# ABC081A: 3数の和
echo "1 2 3" | cli/gs2cl.sh 'read-nums sum show-line'       # -> 6

# ABC042A: 0を含むか
echo "5 7 0" | cli/gs2cl.sh 'read-nums 0 member show-line'  # -> 1

# ARC017A: 素数判定
echo 17 | cli/gs2cl.sh 'read-num is-prime show-line'        # -> 1

# 最大値/最小値 (sort + last/head)
echo "3 9 1" | cli/gs2cl.sh 'read-nums sort last show-line' # -> 9
echo "3 9 1" | cli/gs2cl.sh 'read-nums sort head show-line' # -> 1

# 回文判定
cli/gs2cl.sh '"racecar" dup reverse = show-line'            # -> 1

# ABC081B: 2で割れる数の個数
echo "3 1 4" | cli/gs2cl.sh 'read-nums m: 2 % not } sum show-line'  # -> 1
