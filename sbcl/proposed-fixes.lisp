; ---------------------------------------------------------------------------------
; Proposed fixes for mReasoner 2.0 (commit 637a162)
;
; It does NOT modify the original files: it redefines functions after loading them.
; Usage, after load-sbcl.lisp:
;   (load "sbcl/proposed-fixes.lisp")
;
; Each fix carries the identifier used in the issue
; (https://github.com/skhemlani/mReasoner/issues/2). They are proposals, tested
; only on SBCL 2.2.9. The fix for BUG-09 uses SBCL-specific package-lock forms.
;
; Comments in ASCII: the sources are loaded as Latin-1.
; ---------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------
; BUG-01: what-possibly-follows? uses the variable invalid-conclusions without
; defining it (HighLevel.lisp:63). According to its docstring, with System 2 it
; should return the heuristic conclusions and their weaker counterparts that are
; true in the models. The fix weakens the initial conclusions.
; ---------------------------------------------------------------------------------

(defun what-possibly-follows? (intensions &key (system2 (system2-enabled?)) (v nil))
  (declare (ignore v))
  (trc "Control" (format nil "Set task: Infer what possibly follows from ~{~A~#[~:;, ~]~}"
                         (mapcar #'abbreviate intensions)))
  (let* ((models              (cond ((model-p (first intensions)) intensions)
                                    ((intension-p (first intensions)) (interpret intensions))))
         (model               (first models))
         (initial-conclusions (handler-case (form-initial-conclusion model)
                                (false-heuristic-conclusion-error () (list *nvc*))))
         possible-conclusions)
    (when (not system2) (return-from what-possibly-follows? initial-conclusions))
    (dolist (c (append (form-weaker-conclusions initial-conclusions (list model))   ; BUG-01
                       initial-conclusions))
      (when (validate c models) (push c possible-conclusions)))
    possible-conclusions))

; ---------------------------------------------------------------------------------
; BUG-03: in one-row spatial models, thing-position returns integers (depth 0)
; and manhattan-distance is never computed, so (<= nil ...) fails
; (Scanner.lisp:192-201). The fix computes the one-dimensional distance as
; |pos1 - pos2|.
; ---------------------------------------------------------------------------------

(defmethod validate-model ((intension sp-intension) (model sp-model) &key (validate-true t))
  (let* ((thing1    (first-argument intension))
         (thing2    (second-argument intension))
         (template  (spatial-template intension))
         (pos1      (when (not template) (thing-position thing1 (things model))))
         (pos2      (when (not template) (thing-position thing2 (things model))))
         (relation  (spatial-relation intension))
         (dimension (spatial-dimension intension))
         (max-distance  (spatial-distance intension))
         (max-distance  (if (equals max-distance :infinity) 9999999999 max-distance))
         manhattan-distance test)
    (setf *val-int0* intension)
    (setf *val-mod0* model)
    (cond
     (template
      (setf test (validate-spatial-template-between intension model)))
     ((is-same intension)      (setf test (equals pos1 pos2)))
     ((is-different intension) (setf test (not (equals pos1 pos2))))
     ((or (null pos1) (null pos2) (null (position dimension (dimensions model) :key #'second)))
      (setf test nil))
     (t
      (if (> (depth pos1) 0)
          (progn
            (setf manhattan-distance (manhattan-distance (first pos1) (second pos1) (first pos2) (second pos2)))
            (setf pos1 (nth (position dimension (dimensions model) :key #'second) pos1))
            (setf pos2 (nth (position dimension (dimensions model) :key #'second) pos2)))
          (setf manhattan-distance (abs (- pos1 pos2))))                              ; BUG-03
      (setf test
            (cond
             ((equals relation '-) (and (< pos1 pos2) (<= manhattan-distance max-distance)))
             ((equals relation '+) (and (> pos1 pos2) (<= manhattan-distance max-distance)))))))
    (setf test (and test (validate-superlative intension model)))
    (if validate-true test (not test))))

;; Second part of BUG-03: thing-duplicate-positions (API.lisp:1314) walks a
;; column x row grid and fails with one-row models ((A) (B) (C)), whose cells are
;; not lists of lists. The fix treats each cell as a column with a single row.
(defun thing-duplicate-positions (thing entities)
  (if (<= (depth entities) 2)
      (let (positions)
        (dotimes (column (length entities))
          (let ((cell (mapcar #'get-thing-from-enumerated-duplicate (nth column entities))))
            (when (or (member thing cell)
                      (member (read-from-string (format nil "~A&dummy" thing)) cell))
              (push (list column 0) positions))))
        positions)
      (let (positions)
        (dotimes (column (length entities))
          (dotimes (row (length (first entities)))
            (let ((cell (mapcar #'get-thing-from-enumerated-duplicate (nth row (nth column entities)))))
              (when (or (member thing cell)
                        (member (read-from-string (format nil "~A&dummy" thing)) cell))
                (push (list column row) positions)))))
        positions)))

;---------------------------------------------------------------------------------
; BUG-04: single-atom models ("A", "not A") are built without nesting
; (Builder.lisp:400-405): their possibilities are bare literals, whereas compound
; models hold nested s-models. Other functions rely on that difference
; (unembed-s-models uses is-atomic-model), but print-model (Utilities.lisp:529)
; and validation (validate-possibilities, Scanner.lisp:48) assume the nested form
; and fail. The fix normalizes the atomic model (wraps it in an s-model) only at
; those two points, with :around methods.
; ---------------------------------------------------------------------------------

(defun normalize-s-model (model)
  (if (and (typep model 's-model)
           (possibilities model)
           (notany (lambda (p) (typep p 's-model)) (possibilities model)))
      (make-instance 's-model :poss (list model) :fn (footnote model))
      model))

(defmethod validate-model :around ((intension s-intension) model &key (validate-true t))
  (call-next-method intension (normalize-s-model model) :validate-true validate-true))   ; BUG-04

(defmethod print-model :around ((model s-model) &rest args &key &allow-other-keys)
  (apply #'call-next-method (normalize-s-model model) args))                             ; BUG-04

; ---------------------------------------------------------------------------------
; BUG-02, BUG-05 and BUG-06 (safeguard, not a fix of the cause): in deterministic
; mode interpret can return a model that violates its own premises without any
; warning. For BUG-02, start-model calls start-mod on the existing model
; (Builder.lisp:91-105), and in deterministic mode start-mod does not create
; individuals for the new subject when a model already exists (Builder.lisp:199).
; Implementing combine (Builder.lisp:110, commented out) goes beyond a local fix.
; The minimal safeguard is NOT to return a model that violates its premises:
; interpret checks that every final model satisfies all its premises and, if not,
; signals an explicit error. BUG-06 needs a decision on the semantics of
; "most ... not" before a real fix can be proposed.
; ---------------------------------------------------------------------------------

(define-condition incomplete-model (consistency-error) ()
  (:report (lambda (c stream) (write-string (text c) stream))))

; The original version is saved only once, with fdefinition (with #'interpret
; SBCL would resolve the name at call time and recurse forever).
(unless (fboundp 'interpret-original)
  (setf (fdefinition 'interpret-original) (fdefinition 'interpret)))

(progn
  (defun interpret (intensions)
    (let* ((models (interpret-original intensions))
           (parsed (mapcar (lambda (i) (if (typep i 'intension) i (parse i))) intensions)))
      (dolist (m models)
        (unless (and (validate-all-conclusions parsed (list m))
                     (every (lambda (i)
                              (and (find-referent-in-model (first-argument i) m)
                                   (or (not (slot-boundp i 'second-argument))
                                       (null (second-argument i))
                                       (typep i 's-intension)
                                       (find-referent-in-model (second-argument i) m))))
                            (remove-if-not (lambda (i) (typep i 'q-intension)) parsed)))
          (error 'incomplete-model
                 :text (format nil "BUG-02/05/06: the model does not satisfy all its premises: ~{~A~^, ~}"
                               (mapcar #'abbreviate parsed)))))
      models)))

; ---------------------------------------------------------------------------------
; DIAG-01 and DIAG-02: the syllogism and consistency diagnostics always pass
; (Diagnostics.lisp:73 uses (or ... t); Diagnostics.lisp:243 returns t).
; ---------------------------------------------------------------------------------

(defun diagnose-consistency (intensions old-prediction &key (v nil))
  (let* (model consistency)
    (handler-case
        (setf model (first (consistent? intensions :system2 t))))
    (setf consistency (if model "Y" "N"))
    (if v
        (format t "~3,A,~11,A~15,A~20,A~15,A~%" (abbreviate (first intensions)) (abbreviate (second intensions))
                consistency old-prediction (if (equalp consistency old-prediction) "" "!")))
    (equalp consistency old-prediction)))                                         ; DIAG-02

(defun run-syllogism-diagnostics (&key (v nil))
  (let ((i 0) (ok t)
        (ab (list Aab Iab Eab Oab)) (ba (list Aba Iba Eba Oba))
        (bc (list Abc Ibc Ebc Obc)) (cb (list Acb Icb Ecb Ocb))
        major minor)
    (dotimes (fig 4)
      (case (+ fig 1)
        (1 (setf major ab) (setf minor bc))
        (2 (setf major ba) (setf minor cb))
        (3 (setf major ab) (setf minor cb))
        (4 (setf major ba) (setf minor bc)))
      (dolist (p1 major)
        (dolist (p2 minor)
          (unless (diagnose-syllogism (list p1 p2) (nth i +syl-preds-v0.5+) :v v)    ; DIAG-01
            (setf ok nil))
          (incf i))))
    (if ok "[OK]" "[FAILED]")))

; ---------------------------------------------------------------------------------
; BUG-09: start-mod for q-intension uses the free variable MOD, which does not
; exist (Builder.lisp:207 and 210), in the membership branch ("X is an A"). In
; deterministic mode every sentence about an individual fails with "The variable
; MOD is unbound". The intent, according to canonical-individuals
; (Builder.lisp:153), is to add other members of the predicate when the model is
; new: ((X A) (A)). The fix binds MOD (special) to the model received, before
; start-mod replaces it with a new one (Builder.lisp:199). MOD is a COMMON-LISP
; symbol (the function MOD): binding it requires lifting SBCL's package lock here.
; ---------------------------------------------------------------------------------

(sb-ext:without-package-locks
  (defmethod start-mod :around ((intension q-intension) &key (model nil))
    (declare (sb-ext:disable-package-locks mod))
    (let ((mod model))
      (declare (special mod))
      (call-next-method))))
