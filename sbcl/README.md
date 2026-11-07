# Running mReasoner on SBCL: loader, proposed fixes and reference tests

These files accompany the bug report in [issue #2](https://github.com/skhemlani/mReasoner/issues/2). **None of the original mReasoner files is modified**: everything is applied at load time.

Tested with **SBCL 2.2.9** (and Python 3 for the comparison script) on Linux. **Not tested on LispWorks.**

| File | What it does |
| --- | --- |
| `../load-sbcl.lisp` | Loads mReasoner 2.0 (commit `637a162`) in SBCL as is. See the header of the file for the incompatibilities it works around. |
| `proposed-fixes.lisp` | Redefines the affected functions after loading. Each fix is labelled with its issue identifier (BUG-01, BUG-03, BUG-04, BUG-09, DIAG-01, DIAG-02). For BUG-02, BUG-05 and BUG-06 it is only a safeguard: `interpret` signals an error instead of returning a model that violates its premises. |
| `tests/gen-reference.lisp` | Records the current behaviour of mReasoner, in deterministic mode with a fixed seed, as JSON (see below). |
| `tests/generate.sh` | Runs the generator for seeds 1, 2 and 3, once more with the fixes, and writes the report. |
| `tests/compare.py` | Writes `tests/REPORT.md`, or compares two outputs case by case. |
| `tests/json-mini.lisp` | Minimal JSON writer used by the generator. |
| `tests/outputs/` | Outputs for seeds 1–3 (original code) and seed 1 with the fixes. |
| `tests/REPORT.md` | Report generated from those outputs. |

## Quick start

From the repository root:

```bash
sbcl --load load-sbcl.lisp
```

```lisp
(print-model (first (interpret '("All A are B" "Some B are C"))))
(load "sbcl/proposed-fixes.lisp")   ; optional
```

## Reference tests

```bash
sbcl/tests/generate.sh          # about 5 seconds
```

The generator turns stochastic mode off and sets λ = 4.0, σ = ε = ω = κ = 0, and records:

1. The 64 syllogisms with System 1 and System 2, for ω = 0 and ω = 1, next to `+syl-preds-v0.5+`.
2. The 32 immediate inferences (A/I/E/O) and the 32 with "most", next to `*imm-preds-v0.9*` and `*imm-most-preds-v0.99*`.
3. The 32 consistency pairs, next to `*imm-con-preds-v0.9*`.
4. Models built in the four domains (the text of `print-model`), and whether each model satisfies its own premises according to `validate-all-conclusions`.
5. Possible/necessary tasks in the temporal and spatial domains.
6. The cases of the issue (BUG-01, BUG-02, BUG-03, BUG-05, BUG-09 and two limitations).

The three seeds give identical results, so in deterministic mode the outputs are reproducible.

To check a change to mReasoner against the current behaviour:

```bash
FIXES=0 SEED=1 OUTPUT=/tmp/candidate.json \
  sbcl --noinform --non-interactive --load load-sbcl.lisp --load sbcl/tests/gen-reference.lisp
python3 sbcl/tests/compare.py verify sbcl/tests/outputs/mreasoner-s1.json /tmp/candidate.json
```

`tests/REPORT.md` lists the divergences from the stored predictions (14 syllogisms, 3 inferences with "most", 2 consistency pairs), the cases that end in an error, and the 19 cases that the proposed fixes change.

## Notes

- `proposed-fixes.lisp` uses SBCL-specific package-lock forms for BUG-09 (binding `mod`, which is a `COMMON-LISP` symbol). On LispWorks that part would need its own form.
- No fix is proposed for the cause of BUG-06 until the intended semantics of "most … not" is settled (see the issue).
- Compiler warnings go to `tests/outputs/sbcl.log`, which is not versioned.

## Distribution statement

The mReasoner sources carry this statement, which applies to the files they belong to:

> Approved for public release: distribution unlimited. Redistributions of source and binary forms, with or without modification, are permitted if redistributions retain the above distribution statement and the following disclaimer.

The files in this folder and `load-sbcl.lisp` are offered under the same terms, so that they can be merged into mReasoner freely.

Juan Manuel Goya, November 2026.
