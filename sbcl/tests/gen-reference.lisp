; ---------------------------------------------------------------------------------
; Reference generator for mReasoner (current behaviour on SBCL).
;
; Run from the repository root, after load-sbcl.lisp:
;   SEED=1 OUTPUT=sbcl/tests/outputs/mreasoner-s1.json \
;   sbcl --non-interactive --load load-sbcl.lisp --load sbcl/tests/gen-reference.lisp
; With FIXES=1 it loads sbcl/proposed-fixes.lisp first.
;
; It records, with stochastic mode off and a fixed seed:
;   1. Syllogisms (64), System 1 and System 2, with omega = 0 and omega = 1.
;   2. Immediate inferences A/I/E/O (32) and with "most" (32): necessary? possible?
;   3. Consistency of pairs (32), System 1 and System 2.
;   4. Models built in the four domains (print-model text).
;   5. Possible/necessary tasks in the temporal and spatial domains.
;   6. Cases that document the reported bugs and limitations.
; Next to each result it stores the prediction kept in Diagnostics.lisp, when
; there is one, to measure divergences.
; ---------------------------------------------------------------------------------

(load (merge-pathnames "sbcl/tests/json-mini.lisp" *mr-root*))

(defparameter *fixes* (string= (env "FIXES" "0") "1"))
(when *fixes*
  (load (merge-pathnames "sbcl/proposed-fixes.lisp" *mr-root*)))

(defparameter *seed* (parse-integer (env "SEED" "1")))
(defparameter *output* (env "OUTPUT" "sbcl/tests/outputs/mreasoner.json"))

(setf *random-state* (sb-ext:seed-random-state *seed*))
(disable-stochastic-mode)
(setf +lambda+ 4.0 +sigma+ 0.0 +epsilon+ 0.0 +omega+ 0.0 +kappa+ 0.0)

(defun abbr-list (xs)
  (json-arr (sort (remove-duplicates (mapcar #'abbreviate xs) :test #'string=) #'string<)))

(defun yn (x) (if x "Y" "N"))

(defun check-premises (premises models)
  "true if every model satisfies all its premises; error text if validation fails."
  (cond ((null models) :null)
        (t (handler-case
               (if (validate-all-conclusions (mapcar #'parse premises) models) :true :false)
             (error (e) (format nil "ERROR: ~A" (err-str e)))))))

(defmacro guarded (&body body)
  `(handler-case (progn ,@body)
     (error (e) (list :obj (cons "error" (err-str e))))))

; ---------------------------------------------------------------------------------
; 1. Syllogisms
; ---------------------------------------------------------------------------------

(defun diagnostic-prediction (s1 s2)
  "Reproduces the computation of diagnose-syllogism in Diagnostics.lisp."
  (let ((preds (mapcar #'abbreviate (remove-duplicates (append s1 s2) :test #'equals))))
    (when (or (not s2) (set-difference s2 s1 :test #'equals))
      (setf preds (append preds (list "NVC"))))
    (sort (remove-duplicates preds :test #'string=) #'string<)))

(defun syllogisms ()
  (let ((i 0) (out nil)
        (ab (list Aab Iab Eab Oab)) (ba (list Aba Iba Eba Oba))
        (bc (list Abc Ibc Ebc Obc)) (cb (list Acb Icb Ecb Ocb)))
    (dotimes (fig 4)
      (destructuring-bind (major minor)
          (case (+ fig 1) (1 (list ab bc)) (2 (list ba cb)) (3 (list ab cb)) (4 (list ba bc)))
        (dolist (p1 major)
          (dolist (p2 minor)
            (let* ((stored (sort (copy-list (nth i +syl-preds-v0.5+)) #'string<))
                   (row (list :obj
                              (cons "id" (format nil "syllogism-~2,'0D" (1+ i)))
                              (cons "figure" (1+ fig))
                              (cons "premises" (list (abbreviate p1) (abbreviate p2)))
                              (cons "stored_2011" stored))))
              (dolist (omega '(0.0 1.0))
                (setf +omega+ omega)
                (let ((r (guarded
                           (let ((s1 (what-follows? (list p1 p2) :system2 nil))
                                 (s2 (what-follows? (list p1 p2) :system2 t)))
                             (let ((pred (diagnostic-prediction s1 s2)))
                               (list :obj
                                     (cons "system1" (abbr-list s1))
                                     (cons "system2" (abbr-list s2))
                                     (cons "prediction" (json-arr pred))
                                     (cons "matches_stored"
                                           (if (equal pred stored) :true :false))))))))
                  (setf row (append row (list (cons (format nil "omega_~D" (round omega)) r))))))
              (setf +omega+ 0.0)
              (push row out)
              (incf i))))))
    (nreverse out)))

; ---------------------------------------------------------------------------------
; 2. Immediate inferences
; ---------------------------------------------------------------------------------

(defun immediate (premise conclusion)
  "Reproduces diagnose-immediate: returns (necessary possible)."
  (cond
    ((equals premise conclusion) (list "Y" "Y"))
    ((possible? (list conclusion) :given (list premise))
     (list (yn (necessary? (list conclusion) :given (list premise) :system2 t)) "Y"))
    (t (list "N" (yn (possible? (list conclusion) :given (list premise) :system2 t))))))

(defun immediates (name set-a set-b stored-preds)
  (let ((i 0) (out nil))
    (dolist (pairs (list (list set-a set-a) (list set-a set-b)))
      (dolist (p1 (first pairs))
        (dolist (p2 (second pairs))
          (let* ((stored (nth i stored-preds))
                 (r (handler-case (immediate p1 p2)
                      (error (e) (list "ERROR" (err-str e))))))
            (push (list :obj
                        (cons "id" (format nil "~A-~2,'0D" name (1+ i)))
                        (cons "premise" (abbreviate p1))
                        (cons "conclusion" (abbreviate p2))
                        (cons "necessary" (first r))
                        (cons "possible" (second r))
                        (cons "stored" stored)
                        (cons "matches_stored" (if (equal r stored) :true :false)))
                  out))
          (incf i))))
    (nreverse out)))

; ---------------------------------------------------------------------------------
; 3. Consistency
; ---------------------------------------------------------------------------------

(defun consistency ()
  (let ((i 0) (out nil))
    (dolist (figure (list (list Aab Iab Eab Oab) (list Aba Iba Eba Oba)))
      (dolist (q1 (list Aab Iab Eab Oab))
        (dolist (q2 figure)
          (let* ((stored (nth i *imm-con-preds-v0.9*))
                 (s1 (handler-case (yn (consistent? (list q1 q2) :system2 nil))
                       (error (e) (format nil "ERROR: ~A" (err-str e)))))
                 (s2 (handler-case (yn (consistent? (list q1 q2) :system2 t))
                       (error (e) (format nil "ERROR: ~A" (err-str e))))))
            (push (list :obj
                        (cons "id" (format nil "consistency-~2,'0D" (1+ i)))
                        (cons "premises" (list (abbreviate q1) (abbreviate q2)))
                        (cons "system1" s1)
                        (cons "system2" s2)
                        (cons "stored_2012" stored)
                        (cons "matches_stored" (if (equal s2 stored) :true :false)))
                  out))
          (incf i))))
    (nreverse out)))

; ---------------------------------------------------------------------------------
; 4. Models in the four domains
; ---------------------------------------------------------------------------------

(defparameter *model-cases*
  '(("quantified-1" "All A are B" "Some B are C")
    ("quantified-2" "No A are B")
    ("quantified-3" "Some A are not B")
    ("quantified-4" "All A are B" "All B are C")
    ("quantified-5" "Most A are B")
    ("temporal-1" "A happened during B" "C happened after B" "D happened after C")
    ("temporal-2" "A happened before B" "B happened before C")
    ("temporal-3" "A happened during B" "C happened after B")
    ("spatial-1" "A is to the left of B" "B is to the left of C" "C is above D")
    ("spatial-2" "A is to the left of B" "C is to the right of B")
    ("sentential-1" "A or B")
    ("sentential-2" "A ore B")
    ("sentential-3" "if A then B")
    ("sentential-4" "iff A then B")
    ("sentential-5" "A and B")
    ("sentential-6" "not A")
    ; BUG-08: printing of simultaneous events depending on the length of the names
    ("temporal-4" "A happened before B" "A happened before C")
    ("temporal-5" "Ana happened before Beto" "Ana happened before Carla")
    ("temporal-6" "Ana happened before Bob" "Ana happened before Cid")))

(defun models ()
  (mapcar
   (lambda (case-spec)
     (destructuring-bind (id &rest premises) case-spec
       (let* ((r (capture (let ((ms (interpret premises)))
                            (dolist (m ms) (print-model m) (terpri))
                            ms)))
              (ms (first (first r))))
         (list :obj
               (cons "id" id)
               (cons "premises" premises)
               (cons "intensions" (guarded (mapcar #'abbreviate (mapcar #'parse premises))))
               (cons "n_models" (when ms (length ms)))
               (cons "satisfies_premises" (check-premises premises ms))
               (cons "model_text" (second r))
               (cons "error" (third r))))))
   *model-cases*))

; ---------------------------------------------------------------------------------
; 5. Tasks in the temporal and spatial domains
; ---------------------------------------------------------------------------------

(defparameter *task-cases*
  '(("temporal-necessary" ("A happened before B" "B happened before C") "A happened before C")
    ("temporal-possible" ("A happened before B" "A happened before C") "B happened before C")
    ("spatial-necessary" ("A is to the left of B" "B is to the left of C") "A is to the left of C")
    ("spatial-possible" ("A is to the left of B" "A is to the left of C") "B is to the left of C")
    ("spatial-2d-necessary" ("A is to the left of B" "B is to the left of C" "C is above D") "A is to the left of C")
    ("quantified-necessary" ("All A are B" "All B are C") "Some A are C")
    ("sentential-necessary" ("if A then B" "A") "B")))

(defun tasks ()
  (mapcar
   (lambda (case-spec)
     (destructuring-bind (id premises conclusion) case-spec
       (let ((given (mapcar #'parse premises)) (c (parse conclusion)))
         (flet ((t1 (fn s2)
                  (handler-case (yn (funcall fn (list c) :given given :system2 s2))
                    (error (e) (format nil "ERROR: ~A" (err-str e))))))
           (list :obj
                 (cons "id" id)
                 (cons "premises" premises)
                 (cons "conclusion" conclusion)
                 (cons "possible_s1" (t1 #'possible? nil))
                 (cons "possible_s2" (t1 #'possible? t))
                 (cons "necessary_s1" (t1 #'necessary? nil))
                 (cons "necessary_s2" (t1 #'necessary? t)))))))
   *task-cases*))

; ---------------------------------------------------------------------------------
; 6. Reported bugs and limitations (an error or an incomplete result is expected)
; ---------------------------------------------------------------------------------

(defparameter *bug-cases*
  '(("BUG-01-what-possibly-follows" what-possibly-follows? ("All A are B" "All B are C"))
    ("LIMIT-three-premises" what-follows? ("All A are B" "All B are C" "All C are D"))
    ("LIMIT-temporal-conclusion" what-follows? ("A happened before B" "B happened before C"))
    ("BUG-02-combine" interpret ("All A are B" "All C are D" "All B are C"))
    ("BUG-05-temporal-during-after" interpret ("A happened during B" "C happened after B"))
    ("BUG-03-spatial-1d" necessary? ("A is to the left of B" "B is to the left of C") "A is to the left of C")
    ("BUG-09-individual-interpret" interpret ("X is an A" "All A are B"))
    ("BUG-09-individual-what-follows" what-follows? ("X is an A" "All A are B"))))

(defun bugs ()
  (mapcar
   (lambda (case-spec)
     (destructuring-bind (id fn premises &optional conclusion) case-spec
       (let ((r (handler-case
                    (cond
                      ((eq fn 'interpret)
                       (let ((ms (interpret premises)))
                         (list :obj (cons "result" (format nil "~D model(s)" (length ms)))
                               (cons "satisfies_premises" (check-premises premises ms))
                               (cons "model_text"
                                     (with-output-to-string (*standard-output*)
                                       (print-model (first ms)))))))
                      (conclusion
                       (list :obj (cons "result"
                                        (yn (funcall fn (list (parse conclusion))
                                                     :given (mapcar #'parse premises))))))
                      (t
                       (list :obj (cons "result"
                                        (abbr-list (funcall fn (mapcar #'parse premises) :system2 t))))))
                  (error (e) (list :obj (cons "error" (err-str e)))))))
         (list :obj (cons "id" id) (cons "function" (string-downcase (string fn)))
               (cons "premises" premises) (cons "output" r)))))
   *bug-cases*))

; ---------------------------------------------------------------------------------

(json-to-file
 (list :obj
       (cons "program" "mReasoner 2.0 (commit 637a162)")
       (cons "implementation" (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))
       (cons "seed" *seed*)
       (cons "fixes" (if *fixes* "sbcl/proposed-fixes.lisp" :null))
       (cons "parameters" (list :obj (cons "stochastic" :false) (cons "lambda" 4.0)
                                (cons "sigma" 0.0) (cons "epsilon" 0.0) (cons "kappa" 0.0)))
       (cons "syllogisms" (syllogisms))
       (cons "immediate" (immediates "immediate" (list Aab Iab Eab Oab) (list Aba Iba Eba Oba)
                                     *imm-preds-v0.9*))
       (cons "immediate_most" (immediates "most" (list Aab Mab Eab Ma-b) (list Aba Mba Eba Mb-a)
                                          *imm-most-preds-v0.99*))
       (cons "consistency" (consistency))
       (cons "models" (models))
       (cons "tasks" (tasks))
       (cons "bugs" (bugs)))
 *output*)

(format t "~%References written to ~A~%" *output*)
