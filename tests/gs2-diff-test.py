#!/usr/bin/env python3
"""Differential test harness: gs2-py3.py (oracle) vs gs2.lisp (port).
Generates random-ish programs + targeted cases, compares byte outputs."""
import json, random, subprocess, sys, os

SBCL = "/opt/homebrew/bin/sbcl"
WORKDIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def run_oracle(code_bytes, stdin_bytes):
    p = subprocess.run(["python3", "/tmp/gs2-py3.py", "/dev/stdin"],
                       input=code_bytes, capture_output=True, timeout=20)
    # gs2.py reads argv[1] as file; /dev/stdin works on macOS? Use a temp file instead.
    return p

def run_oracle_f(code_bytes, stdin_bytes, tmp="/tmp/_gs2_case.gs2"):
    with open(tmp, "wb") as f:
        f.write(code_bytes)
    p = subprocess.run(["python3", "/tmp/gs2-py3.py", tmp],
                       input=stdin_bytes, capture_output=True, timeout=20)
    return p.stdout

def run_lisp_batch(cases):
    """cases: list of (id, code_bytes, stdin_bytes). Returns dict id->bytes."""
    lines = []
    for cid, code, stdin in cases:
        lines.append(json.dumps({"id": cid,
                                 "code": code.decode("latin-1"),
                                 "stdin": stdin.decode("latin-1")}))
    inp = "\n".join(lines) + "\n"
    import os as _os
    env = dict(_os.environ)
    env["LC_CTYPE"] = "C.UTF-8"
    p = subprocess.run([SBCL, "--noinform", "--disable-debugger",
                        "--load", os.path.join(WORKDIR, "tests", "gs2-diff.lisp")],
                       input=inp.encode("latin-1"), capture_output=True,
                       timeout=600, cwd=WORKDIR, env=env)
    out = {}
    for line in p.stdout.decode("latin-1").splitlines():
        line = line.strip()
        if line.startswith('{"id"'):
            d = json.loads(line)
            out[d["id"]] = d["out"].encode("latin-1")
    if not out:
        sys.stderr.write("LISP BATCH FAILED:\n" + p.stdout.decode("latin-1", "replace")[-2000:] +
                         p.stderr.decode("latin-1", "replace")[-2000:])
    return out, p

def gen_cases():
    cases = []
    def add(code, stdin=b""):
        cases.append(("c%d" % len(cases), code if isinstance(code, bytes) else bytes(code), stdin))

    # targeted basics
    add(bytes([0x11, 0x12, 0x30, 0x52]))                 # 1 2 + show
    add(bytes([0x66]))                                    # fizzbuzz
    add(bytes([0x56, 0x2f, 0xfe, 0x07, 0x2a, 0x32, 0x0a]), b"7")  # triangle
    add(bytes([0x04]) + b"Hi" + bytes([0x05, 0xf6, 0xe8]), b"hello")  # dump-map
    add(bytes([0x2e]))                                    # range of stdin len
    add(bytes([0x30]) + b"world")                         # quine plaintext
    add(bytes([0x68]))                                    # hello
    add(bytes([0x68, 0x10]))                              # hello x=0?
    add(bytes([0x11, 0x68]))                              # hello 1
    add(bytes([0x84, 0x53, 0x54]))                        # uppercase map-show show-lines
    add(bytes([0x85, 0x2e]))
    add(bytes([0x87, 0x35, 0x12]))
    add(bytes([0x1b, 0x24]))                              # 100 digits
    add(bytes([0x1c, 0x24]))
    add(bytes([0x63, 0x12, 0x13, 0x11, 0x14, 0x80, 0x93]))  # group/permutations
    add(bytes([0x93]), )                                  # permutations of 0
    add(bytes([0x11, 0x15, 0x96]))
    add(bytes([0x10, 0x11, 0x81]))
    add(bytes([0x11, 0x12, 0x80, 0x7a]))
    add(bytes([0x98, 0x54]))
    add(bytes([0x10, 0x13, 0x81, 0x94]))
    add(bytes([0x4e, 0x52]))
    add(bytes([0x4f, 0x52]))
    add(bytes([0x2e, 0x50, 0x53]))
    add(bytes([0x64, 0x91]), )
    add(bytes([0x2c, 0x2d]))
    add(bytes([0x3c, 0x12, 0x15]), )
    add(bytes([0x3d, 0x12, 0x15]))
    add(bytes([0x3e, 0x12, 0x14]))
    add(bytes([0x3f, 0x12, 0x15]))
    add(bytes([0x6c, 0x10, 0x0c]))
    add(bytes([0x6c, 0x11, 0x0c]))
    add(bytes([0x6c, 0x12, 0x0a]))
    add(bytes([0x6c, 0x13, 0x0a]))
    add(bytes([0x6c, 0x14, 0x0a]))
    add(bytes([0x6c, 0x15, 0x0a]))
    add(bytes([0x6b, 0x13]))
    add(bytes([0x6d, 0x30, 0x11, 0x13, 0x15, 0x0b, 0x38, 0x2f]))
    add(bytes([0x77, 0x11, 0x12, 0x10, 0x38, 0x2f]))
    add(bytes([0x70, 0x11, 0x12, 0x52]))
    add(bytes([0x71, 0x11, 0x11, 0x52]))
    add(bytes([0x76, 0x13, 0x11, 0x52]))
    add(bytes([0x28, 0x88]))
    add(bytes([0x64, 0x65, 0x66, 0x67, 0x54]))
    add(bytes([0x46, 0x53, 0x58]))
    add(bytes([0x24, 0x01, 0x2a]))
    add(bytes([0x02, 0x00, 0x80, 0x52]))                 # short -32768? 0x8000
    add(bytes([0x02, 0xff, 0x7f, 0x52]))                 # 32767
    add(bytes([0x03, 0xff, 0xff, 0xff, 0x7f, 0x52]))
    add(bytes([0x7c, 0x10]), )
    add(bytes([0x7d, 0x12]))
    add(bytes([0x7e, 0x11]))
    add(bytes([0x7f, 0x11]))
    add(bytes([0x2a]), b"a\nb\nc\n")
    add(bytes([0x2b, 0x2e, 0x2f]), b"1\n2\n3")
    add(bytes([0x2c, 0x54]), b"hello  world foo")
    add(bytes([0x2d, 0x2e, 0x2f]), b"a b c")
    add(bytes([0x33, 0x13]), b"12345")
    add(bytes([0x34, 0x12]), b"123456")
    add(bytes([0x2f, 0x2f, 0x52]), b"31415")
    add(bytes([0x90, 0x53]), b"banana")
    add(bytes([0x92, 0x13, 0x11, 0x10, 0x14, 0x11, 0x0b, 0x2f, 0x54]), b"abcd")
    add(bytes([0x08, 0x30, 0x09, 0xe1]), b"")            # quick block via { }
    add(bytes([0xb0, 0x52]), b"")
    add(bytes([0xb2, 0x52, 0xb2, 0x52]))
    add(bytes([0xa0, 0x10, 0x50, 0xa0]), b"")            # junk0
    add(bytes([0x10, 0x50, 0xa1, 0x50, 0xa0]), b"")      # junk1
    add(bytes([0xc8, 0x10, 0xcc, 0x11, 0xd1, 0xd0, 0x52]))
    add(bytes([0xd8, 0x10, 0xdc, 0x52]), b"")
    add(bytes([0x0b, 0x0e, 0x52]), b"")
    add(bytes([0x0e, 0x52]), b"")
    add(bytes([0x60, 0x10, 0x11, 0x52, 0x10]))
    add(bytes([0x61, 0x10, 0x11, 0x52, 0x10]))
    add(bytes([0x62, 0x12, 0x13, 0x52, 0x10]))
    add(bytes([0x67, 0x12, 0x52]))
    add(bytes([0x67, 0x12, 0x30, 0x67]))
    add(bytes([0x69, 0x12, 0x0a]))
    add(bytes([0x6a, 0x12, 0x30, 0x6a, 0x52, 0x10]))
    add(bytes([0x78, 0x11, 0x52, 0x10]))
    add(bytes([0x79, 0x11, 0x52, 0x10]))
    add(bytes([0x7a, 0x11, 0x52, 0x10]))
    add(bytes([0x7b, 0x11, 0x52, 0x10]))
    add(bytes([0x5c, 0x20, 0x13, 0x52, 0x10]), b"")
    add(bytes([0x5d, 0x20, 0x13, 0x52, 0x10]), b"")
    add(bytes([0x5e, 0x20, 0x13, 0x52, 0x10]), b"")
    add(bytes([0x5f, 0x52, 0x10]), b"")
    add(bytes([0x88, 0x8f, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e]), b"abc123 XYZ!")
    add(bytes([0x56, 0x52]), b"-42 17")
    add(bytes([0x57, 0x54]), b"3 apples, 5 oranges")
    add(bytes([0x5a, 0x54]), b"abc")
    add(bytes([0x5b, 0x54]), b"123")
    add(bytes([0x58, 0x52, 0x10]))
    add(bytes([0x59, 0x52, 0x10]))
    add(bytes([0x47, 0x30, 0x52, 0x10]))
    add(bytes([0x48, 0x30, 0x52, 0x10]))
    add(bytes([0x49, 0x30, 0x52, 0x10]))
    add(bytes([0x4a, 0x30, 0x52, 0x10]))
    add(bytes([0x4b, 0x4c, 0x53, 0x58]), b"")
    add(bytes([0x4d, 0x53, 0x58]), b"ab")
    add(bytes([0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x30, 0x52, 0x10]))
    add(bytes([0x2a, 0x2b]), b"x\ny")
    add(bytes([0x2c, 0x2d]), b"a b")
    add(bytes([0x0a, 0x0d, 0x30, 0x52, 0x10]))
    add(bytes([0x31, 0x22]), b"")   # word-mode skip? 0x31 = word-mode prefix
    add(bytes([0x30, 0x22]), b"a\nb\nc")  # line mode tail
    add(bytes([0x32, 0x22]), b"a\nb\nc")  # line-mode-skip-first
    add(bytes([0xee, 0x30, 0xb1, 0x52, 0x10]), b"")  # zipwith1
    add(bytes([0xef, 0x31, 0x30, 0xb1, 0x54]), b"")  # zipwith2
    add(bytes([0xf7, 0x88, 0x54]), b"")  # dump-filter1
    add(bytes([0xe0, 0x11, 0x52, 0x10]), b"")  # quick block1 nop
    add(bytes([0xe1, 0x11, 0x52, 0x10]), b"")  # map1
    add(bytes([0xe2, 0x11, 0x52, 0x10]), b"")  # filter1
    add(bytes([0xe3, 0x11, 0x52, 0x10]), b"")  # both1
    add(bytes([0xe8, 0x11, 0x52, 0x10]), b"")  # m1
    add(bytes([0xf0, 0x11, 0x52, 0x10]), b"")  # f1
    add(bytes([0xf8, 0x11, 0x52, 0x10]), b"")  # t1
    # printf
    add(bytes([0x04]) + b"n=%d" + bytes([0x07, 0x13, 0x9b, 0x05]), b"")
    # regex
    add(bytes([0x04]) + b"[0-9]+" + bytes([0x9c, 0x05]), b"abc123")
    add(bytes([0x04]) + b"a" + bytes([0x07, 0x07, 0x9d, 0x05]), b"banana")  # sub a->a?
    add(bytes([0x04]) + b"[aeiou]" + bytes([0x07]) + b"*"+ bytes([0x9d, 0x05]), b"banana")
    add(bytes([0x04]) + b"[0-9]+" + bytes([0x9e, 0x05]), b"a1b22c333")
    add(bytes([0x04]) + b"[,;]" + bytes([0x9f, 0x05]), b"a,b;c")
    add(bytes([0x05]), b"")   # push empty string
    add(bytes([0x06]), b"")   # empty array
    add(bytes([0x04]) + b"ab" + bytes([0x07]) + b"cd" + bytes([0x06]), b"")
    add(bytes([0x0c, 0x20, 0x52, 0x10]), b"")  # empty block eval
    add(bytes([0x22, 0x52, 0x10]), b"ab")
    add(bytes([0x23, 0x52, 0x10]), b"ab")
    add(bytes([0x26, 0x52, 0x10]), b"ab")
    add(bytes([0x27, 0x52, 0x10]), b"ab")
    add(bytes([0x21, 0x52, 0x10]), b"ab")
    add(bytes([0x33, 0x52, 0x10]), b"ab")
    add(bytes([0x10, 0x2f, 0x30, 0x52, 0x10]), b"")
    add(bytes([0x36, 0x10, 0x0c, 0x30, 0x52, 0x10]), b"")
    add(bytes([0x37, 0x10, 0x0c, 0x30, 0x52, 0x10]), b"")
    add(bytes([0x38, 0x10, 0x0c, 0x52, 0x10]), b"")
    add(bytes([0x39, 0x13, 0x11, 0x52, 0x10]), b"")
    add(bytes([0x3a, 0x11, 0x13, 0x15, 0x52, 0x10]), b"")
    add(bytes([0x01, 0x7f, 0x24, 0x53]), b"")  # 255 digits map-show
    add(bytes([0x02, 0xfe, 0xff, 0x24, 0x53]), b"")  # -258? wait 0xfffe = ... LE: fe ff -> 0xfffe -> -2
    add(bytes([0x25, 0x14, 0x0f]), b"")  # random then exit — random differs; SKIP diff? exit makes deterministic rest
    add(bytes([0x2e, 0x33, 0x12, 0x30, 0x52, 0x10]), b"abcdef")

    # random programs (deterministic seed; avoid 0x25 random & 0xb2 counter & mode bytes at start)
    rng = random.Random(42)
    safe = list(range(0x0a, 0x25)) + list(range(0x26, 0x25+0)) + list(range(0x26, 0x100))
    safe = [b for b in safe if b not in (0x25, 0xb2)]
    for _ in range(120):
        n = rng.randint(1, 8)
        prog = bytes(rng.choice(safe) for _ in range(n))
        if prog[0] in (0x30, 0x31, 0x32):
            continue
        stdin = bytes(rng.randint(97, 122) for _ in range(rng.randint(0, 8))) if rng.random() < 0.5 else b""
        add(prog, stdin)
    return cases

def main():
    cases = gen_cases()
    print(f"total cases: {len(cases)}")
    lisp_out, p = run_lisp_batch(cases)
    if not lisp_out:
        print("LISP FAILED entirely"); return 1
    mismatches = []
    for cid, code, stdin in cases:
        py = run_oracle_f(code, stdin)
        li = lisp_out.get(cid)
        if li is None:
            mismatches.append((cid, code, stdin, py, b"<missing>"))
        elif py != li:
            mismatches.append((cid, code, stdin, py, li))
    print(f"mismatches: {len(mismatches)} / {len(cases)}")
    for cid, code, stdin, py, li in mismatches[:20]:
        print(f"--- {cid} code={code.hex()} stdin={stdin!r}")
        print(f"  py : {py[:80]!r}")
        print(f"  cl : {li[:80]!r}")
    return 0 if not mismatches else 2

sys.exit(main())
