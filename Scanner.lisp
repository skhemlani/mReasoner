; ---------------------------------------------------------------------------------
; Part 7: Scanning procedures
; ---------------------------------------------------------------------------------

; Section 7.1: Scan a model to validate that an intension holds within it
; Section 7.2: Heuristics for scanning model to form initial conclusions

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

(defmethod validate-model ((intension null-intension) (model model) &key (validate-true t)) ; ssk
  "Null intensions (No valid conclusion) vacuously get validated as validate-true in every model."
  validate-true)

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

(defmethod validate-model ((intension sp-intension) (model sp-model) &key (validate-true t))
  "Validate for sp-intensions gets a model, and checks that an intension holds for that model.
   Validate checks whether subj and obj are related in models-subj-obj as specified in the
   sp-intension."
  (let* ((thing1    (first-argument intension))
         (thing2    (second-argument intension))
         (template  (spatial-template intension))  ; for "between" relations
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
      (progn
        ;(format t "pos1: ~A~%pos2: ~A~%distance: ~A~%" pos1 pos2 max-distance)
        (when (> (depth pos1) 0)
          (setf manhattan-distance (manhattan-distance (first pos1) (second pos1) (first pos2) (second pos2)))
          (setf pos1 (nth (position dimension (dimensions model) :key #'second) pos1))
          (setf pos2 (nth (position dimension (dimensions model) :key #'second) pos2)))
        (setf test
              (cond
               ((equals relation '-) (and (< pos1 pos2) (<= manhattan-distance max-distance)))
               ((equals relation '+) (and (> pos1 pos2) (<= manhattan-distance max-distance))))))))

    (setf test (and test (validate-superlative intension model)))

    (if validate-true test (not test))))

(defmethod validate-superlative ((intension sp-intension) (model sp-model))
  (let* ((thing2           (object intension))
         (thing2-name      (get-thing-from-enumerated-duplicate thing2))
         (thing2-position  (thing-position thing2 (things model)))
         (thing2-positions (thing-duplicate-positions thing2-name (things model)))
         (thing2-modifier  (object-modifier intension)))
    (if (or (< (length thing2-positions) 2) (null thing2-modifier))    ;; if either there aren't duplicate things or if there's no superlative
        t                                    ;; then superlative is trivially validated
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
; Section 7.2: Heuristics for scanning model to form initial conclusions
; --------------------------------------------------------------------------------

(defmethod form-initial-conclusion ((model q-model))
  "The algorithm 3-4-11
   The intial conclusions are guided by the quantifier in the dominant premise and the figure. 
   i. The dominance of moods is as follows: 
      some_not_ > no > some > all
   ii.The conclusion depends on the figure of the premises, the mood of the dominant premise,
      especially the most dominant O mood
   1. A-B B-C premises: Use dominant premise to make an A-C conclusion from the model.
   2. B-A C-B premises: Use dominant premise to make C-A conclusion. 
                        then use dominant quantifier to make A-C conclusion.
   3. A-B C-B premises: If premises in same mood use it to make A-C conclusion,
                        else returns mood & figure of dominant premise and converse
   4. B-A B-C premises: If premises in same mood use it to make A-C conclusion
                        else returns mood & figure of dominant premise and converse.

form-initial-conclusion
   find-footnote - in API
   get-syllogistic figure - in API
   figure-1 figure-2 figure-3 figure-4
      get-syll-end-terms - API
      dominant-mood
         same-moods
   check-conclusions
      validate - in BuildModel
      verbalize-conclusion - see below"
  (let* ((intensions (footnote model))
         (intension1 (copy-class-instance (first intensions)))
         (intension2 (copy-class-instance (second intensions))))
    (cond
     ((> (length intensions) 2)
      (error "Too many premises to run heuristics."))
     ((not (intersection (terms intension1) (terms intension2) :test #'equalp))
      (error "Assertions have no terms in common."))
     (t
      (case (get-syllogistic-figure intension1 intension2)
        (1 (setf concl-lis (figure-1 intension1 intension2)))
        (2 (setf concl-lis (figure-2 intension1 intension2)))
        (3 (setf concl-lis (figure-3 intension1 intension2)))
        (4 (setf concl-lis (figure-4 intension1 intension2))))))
   (check-conclusions concl-lis model)))

(defmethod check-conclusions (concl-lis (model q-model))
  "ok checks conclusions and puts them into English using *lexicon*
   (check-conclusions  '(A) '(C) 
   '(((((? 3) (> 2)) (? 2) ((<= CARDINALITY) (> 0)) T NIL) (C) (INCLUDE (C) (A))) 
    ((((? 3) (> 2)) (? 2) ((<= CARDINALITY) (> 0)) T NIL) (A) (INCLUDE (A) (C))))
   '(((B)) ((C)) ((A) (B) (C)) ((A) (B) (C)) ((A) (B)) (T3)))
    => ((SOME A ARE C) (SOME C ARE A))"
  (let ((validated-conclusions (mapcan #'(lambda (c)
                                           (when (validate c (list model)) (list c))) concl-lis)))
    (when (not validated-conclusions)
       (error 'false-heuristic-conclusion-error :text (format nil "Heuristic conclusion is false: ~A~%~%" (abbreviate (first concl-lis)))))
    (randomize validated-conclusions)))

(defmethod swap-subject ((intension q-intension) subj &key (negate-predicate nil))
  (let ((new-int (copy-class-instance intension)))
    (setf (subject new-int) subj)
    (when negate-predicate
      (setf new-int (preserve-negative-predicate new-int)))
    new-int))

(defmethod swap-object ((intension q-intension) obj &key (negate-predicate nil))
  (let ((new-int (copy-class-instance intension)))
    (setf (object new-int) obj)
    (when negate-predicate
      (setf new-int (preserve-negative-predicate new-int)))
    new-int))

(defun preserve-negative-predicate (new-int)
  (when (not (or (negative-relation (relation new-int))
                 (is-none new-int)))
    (setf (relation new-int) 'not-include)
    (setf new-int (copy-class-instance new-int)))
  new-int)

(defmethod figure-1 ((intension1 q-intension) (intension2 q-intension))
  " ok draws conclusion for abbc figure 1
   If dominant premise is intens-1 form conclusion based on it but including
   end-2 as its object
   Else form conclusion based on intens-2 but making end-1 its subject"
  (trc "System 1" (format nil "Applied figure 1 heuristic to ~A and ~A"
                          (abbreviate intension1) (abbreviate intension2)))
  (let ((end1 (first (get-syll-end-terms intension1 intension2)))
        (end2 (second (get-syll-end-terms intension1 intension2)))
        (dominant-intension (dominant-mood intension1 intension2))
        conclusion)
    (if (equalp dominant-intension intension1)
          (setf conclusion (swap-object intension1 end2 :negate-predicate (is-none intension2)))
      (setf conclusion (swap-subject intension2 end1 :negate-predicate (is-none intension1))))    
    (list conclusion)))

(defmethod figure-2 ((intension1 q-intension) (intension2 q-intension))
  " ok draws conclusion or conclusions for figure 2 b-a c-b 
   Bias towards C-A from figure of intenss especially with most dominant O intens
   Otherwise return two conclusions based on dominant intens: C-A and A-C."
  (trc "System 1" (format nil "Applied figure 2 heuristic to ~A and ~A"
                          (abbreviate intension1) (abbreviate intension2)))
  (let ((mood1 (mood intension1))(mood2 (mood intension2))
        (end1 (first (get-syll-end-terms intension1 intension2)))
        (end2 (second (get-syll-end-terms intension1 intension2)))
        (dominant-intension (dominant-mood intension1 intension2))
        conclusion)
    (cond
     ((equal mood1 'O) (setf conclusion (list (swap-subject intension1 end2))))
     ((equal mood2 'O) (setf conclusion (list (swap-object intension2 end1))))
     ((equal dominant-intension intension1) 
      (setf conclusion
            (list (swap-subject intension1 end2)
                  (swap-object (swap-subject dominant-intension end1) end2))))
     (t
      (setf conclusion
            (list (swap-object intension2 end1)
                  (swap-subject (swap-object dominant-intension end2) end1)))))
    conclusion))

(defmethod figure-3 ((intension1 q-intension) (intension2 q-intension))
  " ok draws conclusion or conclusions from abcb figure 3 
   If intenss in same mood use it to make A-C conclusion,
   elseif O use it to make conclusion in its figure,
   else returns mood & figure of dominant intens and converse"
  (trc "System 1" (format nil "Applied figure 3 heuristic to ~A and ~A"
                          (abbreviate intension1) (abbreviate intension2)))
  (let ((mood1 (mood intension1))(mood2 (mood intension2))
        (end1 (first (get-syll-end-terms intension1 intension2)))
        (end2 (second (get-syll-end-terms intension1 intension2)))
        (dom-intens (dominant-mood intension1 intension2))
        conclusion)
    (cond
     ((equal (mood intension1)
             (mood intension2))
      (setf conclusion (list (swap-object intension1 end2))))
     ((or (is-some-not intension1)
          (is-setmem intension1 :n 1))
      (setf conclusion (list (swap-object intension1 end2 :negate-predicate (is-none intension2)))))
     ((or (is-some-not intension2)
          (is-setmem intension2 :n 1))
      (setf conclusion (list (swap-object intension2 end1 :negate-predicate (is-none intension1))))) 
     ((equal dom-intens intension1)
      (setf conclusion
            (list (swap-object intension1 end2)
                  (swap-subject (swap-object intension1 end1) end2))))
     ((equal dom-intens intension2)
      (setf conclusion
            (list (swap-object intension2 end1)
                  (swap-subject (swap-object intension2 end2) end1)))))
    conclusion))

(defmethod figure-4 ((intension1 q-intension) (intension2 q-intension))
  " ok draws conclusion or conclusions for babc figure 4 
   (figure-4 (parse '(all b are a))(parse '(some b are c))) =>
     (((((? 3) (> 2)) (? 2) ((<= CARDINALITY) (> 0)) T NIL) (A) (INCLUDE (A) (C))))
   If intenss in same mood use it to make A-C conclusion
   elseif O intens use it to make conclusion in its figure
   else returns mood & figure of dominant intens and converse."
  (trc "System 1" (format nil "Applied figure 4 heuristic to ~A and ~A"
                          (abbreviate intension1) (abbreviate intension2)))
  (let* ((mood1 (mood intension1))(mood2 (mood intension2))
        (end-terms (get-syll-end-terms intension1 intension2))
        (end1 (first end-terms)) (end2 (second end-terms))
        (dom-intens (dominant-mood intension1 intension2))
        conclusion)
    (cond
     ((equal mood1 mood2)
      (setf conclusion (list (swap-subject intension2 end1)))) ;ok
      ((equal (mood dom-intens) 'O)
       (setf conclusion
             (if (equal dom-intens intension1)
                 (list (swap-subject intension1 end2))
               (list (swap-subject intension2 end1)))))        ;ok
#|     ((and (equal mood1 'I)(equal mood2 'E))
      (setf conclusion
            (list (swap-subject intension2 end1))))            ;ok
     ((and (equal mood1 'E)(equal mood2 'I))
      (setf conclusion
            (list (swap-subject (swap-object intension1 end2) end1)))) ;ok |#
     ((equal dom-intens intension2)
      (setf conclusion
            (list (swap-subject intension2 end1)
                  (swap-subject (swap-object intension2 end1) end2)))) ;ok
     (t
      (setf conclusion
            (list (swap-subject (swap-object intension1 end2) end1)
                  (swap-subject intension1 end2)))))
    conclusion))

(defmethod dominant-mood ((intension1 q-intension) (intension2 q-intension))
   "ok rtns intens in dominant mood
   (dominant-mood (parse '(all a are b))(parse '(some c are b))) => 
    ((((? 3) (> 2)) (? 2) ((<= CARDINALITY) (> 0)) T NIL) (C) (INCLUDE (C) (B)))

    If both intenss in same mood calls same-moods
    Otherwise rtns dominant intens according to the order
     O > E > I > A"
  (let ((quant-1 (list (cardinality intension1) (numprop intension1) (boundary intension1)
                       (polarity intension1) (footnotes intension1)))
        (quant-2 (list (cardinality intension2) (numprop intension2) (boundary intension2)
                       (polarity intension2) (footnotes intension2)))
        (neg-mood-1 (negative-relation (relation intension1)))
        (neg-mood-2 (negative-relation (relation intension2)))
        dominant-intens)
    (cond((and (equal quant-1 quant-2)(equal neg-mood-1 neg-mood-2))
          (same-moods intension1 intension2))
         ((or (setf dominant-intens (is-setmem   intension1 :n 1))
              (setf dominant-intens (is-setmem   intension2 :n 1))
              (setf dominant-intens (is-some-not intension1))
              (setf dominant-intens (is-some-not intension2))
              (setf dominant-intens (is-most-not intension1))
              (setf dominant-intens (is-most-not intension2))
              (setf dominant-intens (is-none     intension1))
              (setf dominant-intens (is-none     intension2))
              (setf dominant-intens (is-some     intension1))
              (setf dominant-intens (is-some     intension2))
              (setf dominant-intens (is-most     intension1))
              (setf dominant-intens (is-most     intension2)))
          dominant-intens))))

(defmethod same-moods ((intension1 q-intension) (intension2 q-intension))
  "ok deals with premises that are both in the same mood
  (same-moods (parse '(all a are b))(parse '(all c are b))) =>
  ((((? 3) (> 2)) (? 3) ((= CARDINALITY)) T T) (A) (INCLUDE (A) (B)))

  If premise-1 has end-term subj then rtns it (figs 1 and 3)
  else rtns premise-2 (figs 2 and 4)"
    (if (equal (subject intension1) (first (get-syll-end-terms intension1 intension2)))
        intension1     
      intension2))

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