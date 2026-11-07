#!/usr/bin/env bash
# Regenerates the mReasoner references and the report.
# Usage (from any folder):  sbcl/tests/generate.sh [seeds...]
# Seeds 1 2 3 by default. Requires sbcl and python3.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"
SEEDS="${*:-1 2 3}"
mkdir -p sbcl/tests/outputs
: > sbcl/tests/outputs/sbcl.log   # compiler warnings

for n in $SEEDS; do
  echo "mReasoner, seed $n"
  SEED=$n OUTPUT=sbcl/tests/outputs/mreasoner-s$n.json \
    sbcl --noinform --non-interactive --load load-sbcl.lisp \
         --load sbcl/tests/gen-reference.lisp > /dev/null 2>> sbcl/tests/outputs/sbcl.log
done

echo "mReasoner with the proposed fixes, seed 1"
FIXES=1 SEED=1 OUTPUT=sbcl/tests/outputs/mreasoner-fixed-s1.json \
  sbcl --noinform --non-interactive --load load-sbcl.lisp \
       --load sbcl/tests/gen-reference.lisp > /dev/null 2>> sbcl/tests/outputs/sbcl.log

python3 -I sbcl/tests/compare.py report
