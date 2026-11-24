; ---------------------------------------------------------------------------------
; Part 7: Scanning procedures
; ---------------------------------------------------------------------------------

; Section 7.1: Scan a model to validate that an intension holds within it
; Section 7.2: "Scanning" and "validating" NVC conclusions
; Section 7.3: Scanning and validating sentential models
; Section 7.4: Scanning and validating quantificational models
; Section 7.5: Scanning and validating temporal models
; Section 7.6: Scanning and validating spatial models

; ---------------------------------------------------------------------------------
; Section 7.1: Scan a model to validate that an intension holds within it
; ---------------------------------------------------------------------------------

(defun validate-all-conclusions (conclusions models &key (validate-true t))
  "Validates all supplied conclusions given a list of models
   by recursively calling validate"
  (if (not (first conclusions))
      t
    (and (validate (first conclusions) models :validate-true validate-true)
         (validate-all-conclusions (rest conclusions) models :validate-true validate-true))))

(defun filter-validated-models (conclusion models &key (validate-true t))
  "Drops any model from a list of models if one or both of the following hold:
   - The conclusion doesn't hold in the model
   - Any of the intensions don't hold in the model"
  (let* ((filtered-models (remove-if-not #'(lambda (x) (validate conclusion (list x) :validate-true validate-true)) models)))
    (when filtered-models
      (remove-if-not #'(lambda (x) (validate-all-conclusions (footnote x) (list x) :validate-true t)) filtered-models))))

(defun validate (intension modelset &key (validate-true t) (verbose nil) (s2 nil)) ; ssk
  "Validate gets a set of models and calls validate-model; if validate-true is t, then
   checks whether the intension holds in every model in the set. Else checks
   whether intension doesn't hold in every model."
  (let (result)
    
    (when (and intension modelset)
      (setf result t)
      (dolist (model modelset)
        (when (not (validate-model intension model :validate-true validate-true))
          (setf result nil)
          (return))))

    (when verbose
      (trc (if s2 "System 2" "System 1")
           (format nil "Validated that ~A ~A in model(s)"
                   (abbreviate intension)
                   (if result "holds" "does not hold"))))
    result))

; ---------------------------------------------------------------------------------
; Section 7.2: "Scanning" and "validating" NVC conclusions
; ---------------------------------------------------------------------------------

(defmethod validate-model ((intension null-intension) (model model) &key (validate-true t)) ; ssk
  "Null intensions (No valid conclusion) vacuously get validated as validate-true in every model."
  validate-true)

; ---------------------------------------------------------------------------------
; Section 7.3: Scanning and validating sentential models
; ---------------------------------------------------------------------------------

(defmethod validate-possibilities ((intension s-intension) embedded-model)
  "This fn validates atomic and simple-compound s-intensions against a
   set-theoretic semantics for sentential reasoning."
  (let* ((1st             (if (is-atom intension) (first-clause intension) (first-clause (first-clause intension))))
         (2nd             (if (is-atom intension) (second-clause intension) (first-clause (second-clause intension))))
         (models-1st      (find-referent-in-model 1st embedded-model))
         (models-2nd      (find-referent-in-model 2nd embedded-model))
         (models-both     (find-referent-in-modelset (list 2nd) models-1st))
         (models-1st-only (remove-models models-2nd models-1st))
         (models-2nd-only (remove-models models-1st models-2nd))
         (models-neither  (remove-models (append models-1st-only models-2nd-only models-both) (mapcar #'possibilities (possibilities embedded-model))))
         (validate        (cond
                           ((is-affirmative-atom intension)
                            (= (length models-1st) (length (possibilities embedded-model))))
                           ((is-negative-atom intension)
                            (not models-1st))
                           ((is-and intension)
                            (and models-both (not models-1st-only) (not models-2nd-only)))
                           ((is-nor intension)
                            (and models-neither (not models-both) (not models-1st-only) (not models-2nd-only)))
                           ((is-ori intension)
                            (or models-1st models-2nd))
                           ((is-ore intension)
                            (and (or models-1st-only models-2nd-only) (not (or models-both models-neither))))
                           ((is-if intension)
                            (not models-1st-only))
                           ((is-iff intension)
                            (and (or models-both models-neither) (not models-1st-only) (not models-2nd-only)))
                           ((is-not intension)
                            (and (not models-both) (not models-1st-only) (not models-2nd-only)))
                           (t (error "Can't validate connective")))))
#|    (format t "     ~%1st clause: ~A~
                    ~%2nd clause: ~A~
                    ~%     Model: ~A~
                    ~%Models-1st: ~A~
                    ~%Models-2nd: ~A~
                   ~%Models-both: ~A~
               ~%Models-1st only: ~A~
               ~%Models-2nd only: ~A~
                ~%Models-neither: ~A~
                      ~%Validate: ~A~%" 1st 2nd embedded-model models-1st models-2nd models-both models-1st-only models-2nd-only models-neither validate) |#
    validate))

(defmethod validate-model :around ((intension s-intension) model &key (validate-true t))
  "Converts atomic model to s-model (if needed) before passing off to validate-model.
   Contributed by Juan Manuel Goya."
  (call-next-method intension (normalize-s-model model) :validate-true validate-true))

(defmethod validate-model ((intension s-intension) model &key (validate-true t))
  "Validate-model for s-intensions works differently depending on the type of sentential
   connective in the intension. For conjunctions (A and B) it checks that both A and B
   hold in the model. For exclusive disjunctions, it checks that either A or B but not
   both hold in the model. For inclusive disjunctions, it checks that either A or B or
   both hold in the model. And for negations, it checks that A does *not* hold in the
   model."
  (let (validate)
    (if (or (is-atom intension) (is-simple-compound intension))
        (setf validate (validate-possibilities intension model))
      (let* ((clause1-validated (validate-model (first-clause intension) model))
             (clause2-validated (when (not (is-not intension)) (validate-model (second-clause intension) model)))
             validate-possible-list validate-impossible-list)
    
;        (format t "Input intension: ~A~%~%" (abbreviate intension))
;        (inspect-model model)
;        (format t "~%Clause 1 validated: ~A Clause 2 validated: ~A~%" clause1-validated clause2-validated) 
        
        (setf validate-possible-list
              (list
               (if (is-possible (both intension))        (and clause1-validated clause2-validated)              t)
               (if (is-possible (first-only intension))  (and clause1-validated (not clause2-validated))        t)
               (if (is-possible (second-only intension)) (and (not clause1-validated) clause2-validated)        t)
               (if (is-possible (neither intension))     (and (not clause1-validated) (not clause2-validated))  t)))
        (setf validate-impossible-list
              (list
               (if (is-impossible (both intension))        (not (and clause1-validated clause2-validated))             t)
               (if (is-impossible (first-only intension))  (not (and clause1-validated (not clause2-validated)))       t)
               (if (is-impossible (second-only intension)) (not (and (not clause1-validated) clause2-validated))       t)
               (if (is-impossible (neither intension))     (not (and (not clause1-validated) (not clause2-validated))) t)))

        (setf validate
              (and (notevery #'null validate-possible-list)
                   (notany #'null validate-impossible-list)))))

    (if validate-true validate (not validate))))

; ---------------------------------------------------------------------------------
; Section 7.4: Scanning and validating quantificational models
; ---------------------------------------------------------------------------------

(defmethod validate-model ((intension q-intension) (model q-model) &key (validate-true t)) ; ssk
  "Validate for q-intensions gets a model, and checks that an intension holds for that model.
   Validate checks whether subj and obj are related in models-subj-obj as specified in intension.
   It then calls validate-boundaries to check that the relations between the subj and obj
   meet the boundary specifications in the intension."
  (let ((tests (list
                (validate-cardinality intension model)
                (validate-numprop intension model)
                (validate-boundaries intension model))))
    ;(print tests)
    (if validate-true
        (not (member nil tests))
      (member nil tests))))

(defun validate-cardinality (intension model) ; ssk
  "Validates that the referent cardinality of a given model with respect to subj
   holds as specified in the given intension. For example, suppose intension is:
   ((((? 3) (> 2)) (? 3) ((= CARDINALITY)) T T) (A) (INCLUDE (A) (B)))
   In this case,
   (validate-cardinality '(A) intension '(((A) (B)) ((A) (B)) ((A) (B)) (T22))) => T
   (validate-cardinality '(A) intension '(((C) (B)) ((C) (B)) ((A) (B)) (T22))) => nil"
  (let* ((subj (list (subject intension)))
         (cardinality (get-referent-cardinality subj model))
         (conditions (find-cardinality-condition intension)))
    (evaluate-cardinality-conditions cardinality conditions)))

(defmethod validate-numprop ((intension q-intension) (model q-model)) ; ssk
  "Validates that a given model meets the numprop requirement of the given intension"
  (let* ((subj                 (list (subject intension)))
         (obj                  (list (object intension)))
         (condition            (find-numprop-condition intension))
         (subj&obj-cardinality (if (negative-intension intension)
                                   (get-subj-wo-obj-cardinality subj obj model)
                                 (get-subj&obj-cardinality subj obj model))))
    (if condition (equal condition subj&obj-cardinality)
      t)))
  
(defmethod validate-boundaries ((intension q-intension) (model q-model)) ; ssk
  "Validates that a given model meets the boundaries of the given intension, e.g.
   EXAMPLES COMING SOON"
  (let* ((subj                 (list (subject intension)))
         (obj                  (list (object intension)))
         (cardinality          (get-referent-cardinality subj model))
         (conditions           (boundary intension))
         (subj&obj-cardinality (if (negative-intension intension)
                                   (get-subj-wo-obj-cardinality subj obj model)
                                 (get-subj&obj-cardinality subj obj model))))
    (evaluate-boundary-conditions subj&obj-cardinality cardinality conditions)))

; ---------------------------------------------------------------------------------
; Section 7.5: Scanning and validating temporal models
; ---------------------------------------------------------------------------------

(defmethod validate-model ((intension t-intension) (model t-model) &key (validate-true t))
  "Validate for t-intensions gets a model, and checks that an intension holds for that model.
   Validate checks whether subj and obj are related in models-subj-obj as specified in precedence
   portion of t-intension."
  (let* ((precedence (precedence intension))
         (test (first precedence))
         (posX (event-range (second precedence) (moments model)))
         (posY (event-range (third precedence) (moments model))))
    (if (or (null (first posX)) (null (second posX)) (null (first posY)) (null (second posY)))
        (setf test nil)
      (setf test
            (case test
              ('<       (< (second posX) (first posY)))
              ('>       (> (first posX) (second posY)))
              ('properly-include
               (and (< (first posX) (first posY))
                    (> (second posX) (second posY))))
              ('include
               (and (<= (first posX) (first posY))
                    (>= (second posX) (second posY)))))))
    (if validate-true test (not test))))

; ---------------------------------------------------------------------------------
; Section 7.6: Scanning and validating spatial models
; ---------------------------------------------------------------------------------

(defmethod validate-model ((intension sp-intension) (model sp-model) &key (validate-true t) (perceptual-distance nil))
  "Validate for sp-intensions gets a model, and checks that an intension holds for that model.
   Validate checks whether subj and obj are related in models-subj-obj as specified in the
   sp-intension."
  (let* ((thing1             (first-argument intension))
         (thing2             (second-argument intension))
         (template           (spatial-template intension))  ; for "between" relations
         (pos1               (when (not template) (thing-position thing1 (things model))))
         (pos2               (when (not template) (thing-position thing2 (things model))))
         (relation           (spatial-relation intension))
         (dimension          (spatial-dimension intension))
         (relevant-position  (position dimension (dimensions model) :key #'second)) ;; relevant dimension position
         max-distance relevant-pos1 relevant-pos2 ancillary-pos1 ancillary-pos2 manhattan-distance test)

    
    (setf max-distance (if perceptual-distance
                           (if (equals perceptual-distance :infinity) 9999999999 perceptual-distance)
                         (if (equals (spatial-distance intension) :infinity) 9999999999 (spatial-distance intension))))
      
    ; (setf *val-int0* intension)
    ; (setf *val-mod0* model)
    ; (setf *val-pos1* pos1)
    ; (setf *val-pos2* pos2)
    
    (cond
     (template
      (setf test (validate-spatial-template-between intension model))) ;; validate against a template if relevant
     ((is-same intension)      (setf test (equals pos1 pos2)))         ;; or check that they're in the same position
     ((is-different intension) (setf test (not (equals pos1 pos2))))   ;; or in different positions
     ((or (null pos1) (null pos2) (null (position dimension (dimensions model) :key #'second)))
      (setf test nil))                                                 ;; or that both objects are observed                                 
     (t
      (progn
        (if (> (depth pos1) 0)
            (progn
              (setf relevant-pos1 (nth relevant-position pos1))
              (setf relevant-pos2 (nth relevant-position pos2))
              (setf ancillary-pos1     (nth (- 1 relevant-position) pos1))
              (setf ancillary-pos2     (nth (- 1 relevant-position) pos2))
              (setf ancillary-pos1-max (eval (list relation (nth (- 1 relevant-position) pos1) max-distance)))
              (setf ancillary-pos2-max (eval (list relation (nth (- 1 relevant-position) pos2) max-distance)))
              (setf manhattan-distance
                    (cond
                      ((equals relevant-position 0) (manhattan-distance relevant-pos1 ancillary-pos1 relevant-pos2 ancillary-pos2))
                      ;                 ((equals relevant-position 1) (manhattan-distance ancillary-pos1 relevant-pos1 ancillary-pos2 relevant-pos2))
                      ))
              (setf test
                    (cond
                      ((equals relation '-) (and (< relevant-pos1 relevant-pos2) (<= manhattan-distance max-distance)))
                      ((equals relation '+) (and (> relevant-pos1 relevant-pos2) (<= manhattan-distance max-distance))))))
              
            (progn
              (setf manhattan-distance (abs (- pos1 pos2)))
              (setf test
                    (cond
                      ((equals relation '-) (and (< pos1 pos2) (<= manhattan-distance max-distance)))
                      ((equals relation '+) (and (> pos1 pos2) (<= manhattan-distance max-distance))))))))))

    (setf test (and test (validate-superlative intension model)))
    (if validate-true test (not test))))

(defmethod validate-superlative ((intension sp-intension) (model sp-model))
  (let* ((thing2           (object intension))
         (thing2-name      (get-thing-from-enumerated-duplicate thing2))
         (thing2-position  (thing-position thing2 (things model)))
         (thing2-positions (thing-duplicate-positions thing2-name (things model)))
         (thing2-modifier  (object-modifier intension)))
    (if (or (< (length thing2-positions) 2) (null thing2-modifier))    ;; if either there aren't duplicate things or if there's no superlative
        t                                                              ;; then superlative is trivially validated
      (cond ((is-superlative-leftmost thing2-modifier)
             (equals (first thing2-position) (apply 'min (mapcar #'first thing2-positions))))
            ((is-superlative-rightmost thing2-modifier)
             (equals (first thing2-position) (apply 'max (mapcar #'first thing2-positions))))
            ((is-superlative-lowermost thing2-modifier)
             (equals (second thing2-position) (apply 'min (mapcar #'second thing2-positions))))
            ((is-superlative-uppermost thing2-modifier)
             (equals (second thing2-position) (apply 'max (mapcar #'second thing2-positions))))))))

(defmethod validate-spatial-template-between (intension model)
  ""
  (let* ((referents       (flatten (list (first-argument intension) (second-argument intension))))
         (positions       (mapcar #'(lambda (x) (thing-position x (things model))) referents))
         (reduced-model-x (mapcar #'(lambda (y) (mapcar #'(lambda (x) (intersection referents x)) y)) (things model)))
         (reduced-model-x (remove-if #'null (mapcar #'(lambda (y) (remove-if #'null y)) reduced-model-x)))
         (reduced-model-y (mapcar #'(lambda (y) (mapcar #'(lambda (x) (intersection referents x)) y)) (transpose-list (things model))))
         (reduced-model-y (remove-if #'null (mapcar #'(lambda (y) (remove-if #'null y)) reduced-model-y)))
         (distance        (spatial-distance intension))
         (distance        (if (equals distance :infinity) 9999999999 distance))
         test)

    (setf test (or (member reduced-model-x
                           (mapcar #'(lambda (y) (mapcar #'(lambda (x) (list (list x))) y)) (spatial-template intension))  ;; :x axis
                           :test #'equals)
                   (member reduced-model-y
                           (mapcar #'(lambda (y) (mapcar #'(lambda (x) (list (list x))) y)) (spatial-template intension))  ;; :x axis
                           :test #'equals)))
    (when (and (is-between intension) (= distance 1))
      (setf test (and test
                      (or (= 1 (length (remove-duplicates (mapcar #'first positions))))
                          (= 1 (length (remove-duplicates (mapcar #'second positions))))))))
    test))

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