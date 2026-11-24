; ---------------------------------------------------------------------------------
; Part 5: High Level Functions
; ---------------------------------------------------------------------------------

; Section 5.1: Interpret premises
; Section 5.2: Make an inference given a particular inferential task
; Section 5.3: Generate a conclusion
; Section 5.4: Evaluate an assertion given premises
; Section 5.5: Make judgments of consistency
; Section 5.6: Choose a conclusion given multiple choices

(defun intension-p (intension)
  (typep intension 'intension))

(defun model-p (intension)
  (typep intension 'model))

; ---------------------------------------------------------------------------------
; Section 5.1: Interpret premises
; ---------------------------------------------------------------------------------

(defun interpret (intensions)
  "Interprets premises into a modelset"
  (let (models)
    (dolist (intension intensions)
      (when (not (typep intension 'intension))
        (setf intension (parse intension)))
      (if models (setf models (build-model intension :models models))
        (setf models (build-model intension))))
    (setf (initial-model *tracer*) (first models))
    ;(trace-model (first models))
    models))

; ---------------------------------------------------------------------------------
; Section 5.2: Make an inference given a particular inferential task
; ---------------------------------------------------------------------------------

(defun infer (intensions &key (task #'what-follows?) (system2 (system2-enabled?)) (v nil) (given nil))
  "Interprets premises into a set of models"
  (when (not (typep (first intensions) 'intension))
    (setf intensions (mapcar #'parse intensions)))
  (if (not given)
      (funcall task intensions :system2 system2 :v v)
    (funcall task intensions :system2 system2 :v v :given given)))

; ---------------------------------------------------------------------------------
; Section 5.3: Generate a conclusion
; ---------------------------------------------------------------------------------
 
(defun what-possibly-follows? (intensions &key (system2 (system2-enabled?)) (v nil))
  "Given a set of assertions, fn tries to draw a conclusion a about what is
   possible. If system2 is nil, returns heuristic conclusions, otherwise
   returns both heuristic conclusions as well as weaker counterparts that
   are true in the models of the assertions."
  (declare (ignore v))
  (trc "Control" (format nil "Set task: Infer what possibly follows from ~{~A~#[~:;, ~]~}" (mapcar #'abbreviate intensions)))
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

(defun what-necessarily-follows? (intensions &key (system2 (system2-enabled?)) (v nil))
  "Given a set of assertions, fn tries to draw a conclusion about what is
   necessary. If system2 is nil, returns heuristic conclusions, otherwise
   returns both heuristic conclusions as well as weaker counterparts that
   are necessarily true in the models of the assertions. Calls make-false
   to search for counterexamples for a particular assertion, and if counter-
   examples are found, discards that assertion."
  (trc "Control" (format nil "Set task: Infer what necessarily follows from ~{~A~#[~:;, ~]~}" (mapcar #'abbreviate intensions)))
  (let* ((models               (cond ((model-p (first intensions)) intensions)
                                     ((intension-p (first intensions)) (interpret intensions))))
         (model                (first models))
         (initial-conclusions  (handler-case (form-initial-conclusion model)
                                 (false-heuristic-conclusion-error () (list *nvc*))))
         counterexamples invalid-conclusions valid-conclusions)
    (when (not system2) (return-from what-necessarily-follows? initial-conclusions))
    (mapcan #'(lambda (c) (dolist (m models) (when (setf counterexamples (search-for-counterexamples c m))
                                               (setf models (append models counterexamples))
                                               (push c invalid-conclusions))))
            initial-conclusions)
    (dolist (c (append (form-weaker-conclusions invalid-conclusions (list model))
                       initial-conclusions))
      (when (validate c models) (push c valid-conclusions)))
  valid-conclusions))

(defun what-follows? (intensions &key (system2 (system2-enabled?)) (v nil))
  "The same fn as what-necessarily-follows?"
  (let* ((conclusions (what-necessarily-follows? intensions :system2 system2 :v v))
         (conclusions-abbreviated (mapcar #'abbreviate conclusions)))
    (if (not system2)
        (trc "System 1" (format nil "Conclusion: ~{~A~#[~:;, ~]~}" conclusions-abbreviated))
      (progn
        (when (not conclusions) (setf conclusions-abbreviated (list (abbreviate *nvc*))))
        (trc "System 2" (format nil "Conclusion: ~{~A~#[~:;, ~]~}" conclusions-abbreviated))))
    (setf (response *tracer*) (format nil "~{~A~#[~:; ~]~}" conclusions-abbreviated))
    conclusions))

(defun coerce-intension (intension)
  (if (typep intension 'intension)
      (progn
        (trc "Language" (format nil "Interpreted premise into intension: ~A" (abbreviate intension)))
        intension)
    (parse intension)))

; ---------------------------------------------------------------------------------
; Section 5.4: Evaluate an assertion given premises
; ---------------------------------------------------------------------------------

(defun possible? (intensions &key (system2 (system2-enabled?)) (given nil))
  "Given a set of assertions, this fn decides whether it is possible that the first
   intension in a list of intensions is possible in that model. If :system2 is nil,
   simply checks whether assertion holds in models of given assertions. If :system2
   is t and assertion does not hold in models, searches for alternative models that
   render the assertion true."
  (trc "Control" (format nil "Set task: Given ~{~A~#[~:;, ~]~}, determine if ~{~A~#[~:;, ~]~} is possible"
                         (mapcar #'abbreviate given)(mapcar #'abbreviate intensions)))
  (if (not given) nil
    (let ((models    (cond
                      ((intension-p (first given)) (interpret given))
                      ((model-p (first given)) given)))
          (intension (first intensions))
          new-model)
      (dolist (m models)
        (when (validate intension (list m) :verbose t)
          (setf (response *tracer*) "Possible")
          (trc "System 1" (format nil "Conclusion: ~A is possible" (abbreviate intension)))
          (return-from possible? m))
        (when system2
          (setf new-model (first (search-for-counterexamples intension m)))
          (when new-model
            (setf (final-model *tracer*) new-model)
            (setf (response *tracer*) "Possible")
            (trc "System 2" (format nil "Conclusion: ~A is possible" (abbreviate intension)))
            (return-from possible? new-model))))
      (setf (response *tracer*) "Not possible")
      (trc "System 1" (format nil "Conclusion: ~A is not possible" (abbreviate intension)))
      nil)))

(defun necessary? (intensions &key (system2 (system2-enabled?)) (given nil))
  "Given a set of assertions, this fn decides whether it is necessary that the first
   intension in a list of intensions is possible in that model. If :system2 is nil,
   simply checks whether assertion holds in all models of given assertions. If :system2
   is t and assertion, searches for alternative models that render the assertion false."
  (trc "Control" (format nil "Set task: Given ~{~A~#[~:;, ~]~}, determine if ~{~A~#[~:;, ~]~} is necessary"
                         (mapcar #'abbreviate given)(mapcar #'abbreviate intensions)))
  (if (not given) nil
    (let* ((models    (cond
                      ((intension-p (first given)) (interpret given))
                      ((model-p (first given)) given)))
           (intension (first intensions))
           (validated-model (validate intension models :verbose t))
          new-model)
      (when (not validated-model)
          (progn
            (setf (response *tracer*) "Not necessary")
            (trc "System 1" (format nil "Conclusion: ~A is not necessary" (abbreviate intension)))
             (return-from necessary? nil)))
     
      (if (not system2)
          (progn
            (setf (response *tracer*) "Necessary")
            (trc "System 1" (format nil "Conclusion: ~A is necessary" (abbreviate intension)))
            (return-from necessary? validated-model))
        (progn
          (dolist (m models)
            (setf new-model (first (search-for-counterexamples intension m)))
            (when new-model
              (setf (final-model *tracer*) new-model)
              (setf (response *tracer*) "Not necessary")
              (trc "System 2" (format nil "Conclusion: ~A is not necessary" (abbreviate intension)))
              (return-from necessary? nil)))))
      (setf (response *tracer*) "Necessary")
      (trc "System 2" (format nil "Conclusion: ~A is necessary" (abbreviate intension)))
      models)))

; ---------------------------------------------------------------------------------
; Section 5.5: Making judgments of consistency
; ---------------------------------------------------------------------------------

;;; Consistency problems from Marco
(setf marco-expt-2 '((Aab Ibc Iac)(Aab Ibc Eac)
                     (Aab Obc Oac)(Aab Obc Iac)
                     (Iab Ibc Iac)(Iab Ibc Eac)
                     (Iab Ebc Oac)(Iab Ebc Eac) ; 8 holds in initial model
                     (Eab Ibc Eac)(Eab Ibc Aac)
                     (Eab Ebc Eac)(Eab Ebc Oac) ; 12 holds in initial model
                     (Oab Abc Iac)(Oab Abc Eac) ; 13 does not hold in i m but 14 does
                     (Oab Obc Oac)(Oab Obc Aac)))

(setf marco-expt-3 '((Aab Ibc Iac)(Aab Ibc Oac)(Aab Ibc Aac)(Aab Ibc Eac)    ;  1
                     (Aab Obc Oac)(Aab Obc Iac)(Aab Obc Eac)(Aab Obc Aac)    ;  5 
                     (Iab Ibc Iac)(Iab Ibc Oac)(Iab Ibc Aac)(Iab Ibc Eac)    ;  9
                     (Iab Obc Oac)(Iab Obc Iac)(Iab Obc Aac)(Iab Obc Eac)    ; 13
                     (Eab Abc Eac)(Eab Abc Oac)(Eab Abc Iac)(Eab Abc Aac)    ; 17
                     (Eab Ibc Eac)(Eab Ibc Oac)(Eab Ibc Iac)(Eab Ibc Aac)    ; 21
                     (Eab Ebc Eac)(Eab Ebc Oac)(Eab Ebc Iac)(Eab Ebc Aac)    ; 25
                     (Eab Obc Eac)(Eab Obc Oac)(Eab Obc Iac)(Eab Obc Aac)    ; 29
                     (Oab Ebc Oac)(Oab Ebc Iac)(Oab Ebc Eac)(Oab Ebc Aac)    ; 33
                     (Oab Abc Oac)(Oab Abc Iac)(Oab Abc Aac)(Oab Abc Eac)    ; 37
                     (Oab Ibc Oac)(Oab Ibc Iac)(Oab Ibc Aac)(Oab Ibc Eac)))  ; 41

(setf consistency-problems-1 '((Iab Obc Iac)(Iab Ibc Iac)))

(defun test-consistency (problems)
  "Applies consistent-problem? to a series of problems, e.g., Marco's"
  (let ((models nil)(problem nil)(p-intensions nil)(count 1)) 
          (dolist (problem problems p-intensions)
            (princ count)(princ " ")
            (princ problem) ; => (Iab Ibc Iac)
            (setf count (+ count 1))
            (dolist (premise problem p-intensions)
              (setf p-intensions (append p-intensions (list (eval premise)))))
            (consistent-assertions? p-intensions)
            (setf problem nil)(setf p-intensions nil)(terpri))))

(defun consistent-assertions? (intensions)
  "Assumes 3 initial assertions at the moment"
  (let ((new-intensions1&2 (multiple-implicatures (list (first intensions)(second intensions))))
        (new-intension3 (multiple-implicatures (list (third intensions))))
        (mod-two nil)(mod-three nil))
    (terpri)(princ "Interpretation without Gricean implicatures from Oxy assertions.")
    (setf mod-two   (first (consistent? (list (first intensions)(second intensions)))))
    (setf mod-three (first (consistent? (list (first intensions)(second intensions)(third intensions)) :system2 t)))
    (consistency-output (list (third intensions)) mod-two mod-three)
    (cond((or (> (length new-intensions1&2) 2)(> (length new-intension3) 1))
            (princ "Interpretation with Gricean implicatures from any Oxy assertion in set.")
                 (if (> (length new-intensions1&2) 2)
                     (setf mod-two (first (consistent? new-intensions1&2 :system2 t))))
                 (if (> (length new-intension3) 1)
                     (setf mod-three (first (consistent? (append new-intensions1&2 new-intension3) :system2 t))))
                 (consistency-output (append new-intensions1&2 new-intension3) mod-two mod-three))
         (t (princ "No relevant Gricean implicature.")(terpri) t))))

(defun multiple-implicatures(intensions)
  "returns list of all Gricean intensions from Oxy to Ixy plus originals"
  (let (new-intens)
  (cond((null intensions) nil)
       ((setf new-intens (implicature (first intensions)))
            (cons (first intensions)(cons new-intens (multiple-implicatures (rest intensions)))))
       (t (cons (first intensions) (multiple-implicatures (rest intensions)))))))
             
(defun consistency-output(lis-intensions mod-two mod-three)
  "Rtns output depending on with there is a mod-two and a mod-three, plus call to Levenshtein distance"
  (cond((null mod-two)(terpri)(princ "First two assertions do not hold in any model."))
       ((null mod-three)
            (terpri)(princ "The three assertions do not hold in any model, but the first two hold in: ")
            (print-model (minimize-model mod-two)))  ; ok
       ((validate-all-conclusions lis-intensions (list mod-two))
            (terpri)(princ "Assertions hold in initial model: ")
            (print-model (minimize-model mod-two)))
       (t   (terpri)(princ "Assertions do not hold in initial model:")
            (print-model (minimize-model mod-two))
            (terpri)(princ "Assertions hold in revised model: ")
            (print-model (minimize-model mod-three))
            (terpri)
            (princ "Levenshtein distance between minimal models is: ")
            (princ (distance mod-two mod-three))(terpri)
            (princ "Levenshtein distance between regular models is: ")
            (princ (distance mod-two mod-three :minimize nil))
            ))
  (terpri) t)

(defun implicature(intension)
  "if Oab intension rtns intension for Iab"
  (let ((subj (first (subject intension)))(obj (first (object intension))))
    (if (is-some-not intension)
        (parse (list 'some subj 'are obj)))))

(defun equal-models(model-1 model-2)
  "rtns T iff two models are the same"
  (equal (stringify-model model-1)(stringify-model model-2)))

(defun consistent? (intensions &key (system2 (system2-enabled?)) (v nil))
  "Given a set of assertions, fn tries to build model from assertions
   to check if they're all consistent. If it fails to build a model
   and search parameter is off, then yields nil, i.e., assertions are
   inconsistent. If search parameter is on, it calls search for counter-
   examples, and if search succeeds then returns consistent set of models,
   else nil."
  (trc "Control" (format nil "Set task: Determine if ~{~A~#[~:;, ~]~} are consistent"
                         (mapcar #'abbreviate intensions)))
  (let (models new-models alt-mods found-counterexample?)
    (dolist (intension intensions)
      (setf new-models nil)
      (handler-case
          (setf new-models (if models (build-model intension :models models)
                             (build-model intension)))
        (consistency-error ()
          (if (not system2)
              (progn 
                (setf (response *tracer*) "Inconsistent")
                (trc "System 1" (format nil "Conclusion: ~{~A~#[~:;, ~]~} are inconsistent" (mapcar #'abbreviate intensions)))
                (return-from consistent? nil))
            (progn
              (dolist (model models)
                (setf alt-mods (search-for-counterexamples intension model))
                (mapcar #'(lambda (m) (setf (footnote m)
                                            (append (list intension) (footnote m)))) alt-mods)
                (setf found-counterexample? t)
                (setf new-models (append new-models alt-mods)))
              (when (not new-models)
                (setf (response *tracer*) "Inconsistent")
                (trc "System 2" (format nil "Conclusion: ~{~A~#[~:;, ~]~} are inconsistent" (mapcar #'abbreviate intensions)))
                (return-from consistent? nil))))))
      (setf models new-models))
    (if found-counterexample?
        (progn
          (trc "System 2" (format nil "Conclusion: ~{~A~#[~:;, ~]~} are consistent" (mapcar #'abbreviate intensions)))
          (setf (final-model *tracer*) (first models)))
      (progn
        (trc "System 1" (format nil "Conclusion: ~{~A~#[~:;, ~]~} are consistent" (mapcar #'abbreviate intensions)))
        (setf (initial-model *tracer*) (first models))))
    (setf (response *tracer*) "Consistent")
    models))

; ---------------------------------------------------------------------------------
; Section 5.6: Choose a conclusion given multiple choices
; ---------------------------------------------------------------------------------

(defun which-follows? (intensions &key (system2 (system2-enabled?)) (v nil))
  "The same fn as which-necessarily-follows?"
  (let* ((conclusions (which-necessarily-follows? intensions :system2 system2 :v v))
         (conclusions-abbreviated (mapcar #'abbreviate conclusions)))
    (if (not system2)
        (trc "System 1" (format nil "Conclusion: ~{~A~#[~:;, ~]~}" conclusions-abbreviated))
      (progn
        (when (not conclusions) (setf conclusions-abbreviated (list (abbreviate *nvc*))))
        (trc "System 2" (format nil "Conclusion: ~{~A~#[~:;, ~]~}" conclusions-abbreviated))))
    (setf (response *tracer*) (format nil "~{~A~#[~:; ~]~}" conclusions-abbreviated))
    conclusions))

(defun which-necessarily-follows? (intensions &key (system2 (system2-enabled?)) (given nil))
  "Given a set of assertions, this fn decides whether it is necessary that the first
   intension in a list of intensions is possible in that model. If :system2 is nil,
   simply checks whether assertion holds in all models of given assertions. If :system2
   is t and assertion, searches for alternative models that render the assertion false."
  (trc "Control" (format nil "Set task: Given ~{~A~#[~:;, ~]~}, determine which of ~{~A~#[~:;, ~]~} is necessary"
                         (mapcar #'abbreviate given)(mapcar #'abbreviate intensions)))
  (if (not given) nil
    (let* ((models    (cond
                      ((intension-p (first given)) (interpret given))
                      ((model-p (first given)) given)))
           (intension (first intensions))
           (validated-model (validate intension models :verbose t))
          new-model)
      (when (not validated-model)
          (progn
            (setf (response *tracer*) "Not necessary")
            (trc "System 1" (format nil "Conclusion: ~A is not necessary" (abbreviate intension)))
             (return-from which-necessarily-follows? nil)))
     
      (if (not system2)
          (progn
            (setf (response *tracer*) "Necessary")
            (trc "System 1" (format nil "Conclusion: ~A is necessary" (abbreviate intension)))
            (return-from which-necessarily-follows? validated-model))
        (progn
          (dolist (m models)
            (setf new-model (first (search-for-counterexamples intension m)))
            (when new-model
              (setf (final-model *tracer*) new-model)
              (setf (response *tracer*) "Not necessary")
              (trc "System 2" (format nil "Conclusion: ~A is not necessary" (abbreviate intension)))
              (return-from which-necessarily-follows? nil)))))
      (setf (response *tracer*) "Necessary")
      (trc "System 2" (format nil "Conclusion: ~A is necessary" (abbreviate intension)))
      models)))

; ---------------------------------------------------------------------------------
; Distribution statement
; ----------------------
; Approved for public release: distribution unlimited. Redistributions of source and
; binary forms, with or without modification, are permitted if redistributions retain
; the above distribution statement and the following disclaimer.
; 
; Disclaimer
; ----------
; The software is supplied "as is" without warranty of any kind.
;
; As the owner of the software, the United States, the United States Department of
; Defense, and their employees: (1) disclaim any warranties, express or implied,
; including but not limited to any implied warranties of merchantability, fitness
; for a particular purpose, title or non-infringement, (2) do not assume any legal
; liability or responsibility for the accuracy, completeness, or usefulness of the
; software, (3) do not represent that use of the software would not infringe
; privately owned rights, (4) do not warrant that the software will function
; uninterrupted, that it is error-free or that any errors will be corrected.
;
; Portions of the software resulted from work developed by or for the U.S.
; Government subject to the following license: the Government is granted for itself
; and others acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
; license in this computer software to reproduce, prepare derivative works, to
; perform or display any portion of that work, and to permit others to do so for
; Government purposes.