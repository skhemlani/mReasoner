; ---------------------------------------------------------------------------------
; Part 9: Searching for counterexamples
; ---------------------------------------------------------------------------------

; Section 9.1:  Search for counterexamples
; Section 9.2:  Constraint finding
; Section 9.3:  Break models apart
; Section 9.4:  Add an individual
; Section 9.5:  Move individuals
; Section 9.6:  Negate individuals
; Section 9.7:  Minimize, drop, and add
; Section 9.8:  Shifting moments earlier or later
; Section 9.9:  Exhaustive search
; Section 9.10: Levenshtein distance calculation

; ---------------------------------------------------------------------------------
; Section 9.1: Search for counterexamples
; ---------------------------------------------------------------------------------

(defun search-for-counterexamples (conclusion model)
  "Takes a conclusion and a model tests if conclusion holds in the model.
   Executes search strategies specific to the domain; if conclusion holds in the
   model, then validates that it does not hold in any putative counterexample; if
   it doesn't hold in the model, then validates that it does in a putative alternative
   model."
  (let ((strategies (search-strategies model))
        (validated  (validate conclusion (list model))))
    (trc "System 2" (format nil "Engaging search for ~A ~A"
                            (if validated "counterexamples of" "alternative models that satisfy")
                            (abbreviate conclusion)))
    (execute-search-strategies conclusion model strategies :validate-true (not validated))))

(defmethod execute-search-strategies ((conclusion null-intension) (model model) (strategies list) &key (validate-true nil))
  "System 2 does not engage in counterexample search for null-intensions"
  nil)

(defmethod execute-search-strategies ((conclusion intension) (model model) (strategies list) &key (validate-true nil))
  "Calls all falsifying or satisfying fns as provided in the 'strategies' list of tuples;
   when each strategy returns a counterexample, adds counterexample
   to list; once all strategies have been executed, traces and returns list."
  (let (c counterexamples strategy)
    (dolist (s strategies)
      (when (setf c (eval (list (first s) conclusion model ':validate-true validate-true)))
        (if (listp c)
            (setf counterexamples (append c counterexamples))
          (push c counterexamples))
        (setf strategy (second s))))

    (when (and counterexamples
               (validate conclusion counterexamples :validate-true validate-true))
      (trc "System 2" (format nil "Found ~A of ~A by ~A~A"
                              (if validate-true "alternative" "counterexample")
                              (abbreviate conclusion)
                              strategy
                              (if (> (length counterexamples) 1)
                                  (format nil " (+ ~A more)" (- (length counterexamples) 1))
                                "")) :m (first counterexamples))
;      (trace-model (first counterexamples))
)
    
    counterexamples))

(defmethod search-strategies ((conclusion q-model))
  "There are five counterexample search strategies for quantificational models,
   as described in Khemlani & Johnson-Laird (2012). The processes of inference. They
   include:
   - moving properties from one individual to another
   - breaking properties apart from one another to create new individuals
   - adding individuals
   - breaking and adding negative individuals
   - negating individuals"
  '((move-individuals "moving individuals")
    (break-and-add-negative "breaking, adding individuals")
    (add-individuals "adding individuals")
    (break-model "breaking individuals")
    (negate-individuals "negating individuals")
    (replace-properties "replacing properties")
;    (exhaustive-search "exhaustive search")
))

(defmethod search-strategies ((conclusion t-model))
  "There are N counterexample search strategies for temporal models,
   as described in Khemlani et al. (under review). Naive temporality. They
   include:
   - shifting event earlier
   - shifting event later"
  '((shift-event-earlier "shifting event earlier")
    (shift-event-later "shifting event later")
    (expand-event "expanding event")
    (convert-to-punctate-and-shift "converting to punctate and shifting")
;    (exhaustive-search "exhaustive search")
))

; ---------------------------------------------------------------------------------
; Section 9.2: Constraint finding
; ---------------------------------------------------------------------------------

(defun recursively-find-constraints (property fn &optional (constraints (find-constraints property fn)))
  "Gets initial set of constraints for property, then iterates through
   all members of the set of constraints and checks to see if there are
   any additional constraints that apply. If any new constraints are found,
   then applies recursively-find-constraints one the new set of constraints.
   (recursively-find-constraints '(A) '(list Aab Aac))
   => '((B) (A) (C))
   (recursively-find-constraints '(A) (list Eab Ebc))
   => '((A) (- B))"
  (let ((new-constraints constraints))
    (dolist (c constraints)
      (setf new-constraints
            (remove-duplicates (append new-constraints (find-constraints c fn)) :test #'equalp))
      (setf new-cont new-constraints))
    (if (every #'(lambda (x) (member x constraints)) new-constraints)
        new-constraints
      (remove-duplicates
       (append new-constraints
               (recursively-find-constraints property fn
                                             (set-difference new-constraints
                                                             constraints :test #'equal)))
       :test #'equal))))

(defun find-constraints (property fn)
  "Returns constraint list for a particular property, i.e.,
   a list of all properties that *must* occur with that property.
   (find-constraints '(A) '((include (A) (B)) (include (A) (C))))
   => '((B) (A) (C))"
  (if (null fn) nil
    (remove-duplicates (append (if (equal '- (first property)) nil
                                 (find-constraint property (first fn)))
                               (find-constraints property (rest fn)))
                       :test #'equal)))

(defmethod find-constraint (property (fn q-intension))
  "If property != subject, then nil.
   Else, if footnote is in A-mood, return subject and object
   Else, if footnote is in E-mood, return subject and -object
   Else return nil"
  (let ((subj (list (subject fn)))
        (obj (list (object fn))))
    (when (equal property subj)
      (cond
       ((is-all fn)    (list subj obj))
       ((is-none fn)   (list subj (negate-property obj)))
       ((is-setmem fn) (list subj obj))
       (t              nil)))))

(defmethod find-constraint (property (fn t-intension))
  "If property != subject, then nil.
   Else, if footnote shows while or during relation, return subject and object
   Else return nil"
  (when (or (equal (first property) (subject fn))
            (equal (first property) (object fn)))
    (cond
     ((is-while fn) (list (list (subject fn)) (list (object fn))))
;;   ((is-during fn) (list (subject fn)) (list (list (object fn))))            ;;; NOT WORKING!!! ssk 27-10-2015
     (t           nil))))

; ---------------------------------------------------------------------------------
; Section 9.3: Break models apart
; ---------------------------------------------------------------------------------

#|
break-and-add-negative                       -- structure similar to break-model, but calls add-negative after break-individuals
break-model
   find-subject                              -- finds subj (in API)
   find-object                               -- finds obj (in API)
   find-footnote                             -- finds fn (in API)
   remove-footnote                           -- removes fn (in API)
   break-individuals                         -- breaks set of individuals
      break-subject-from-object              -- breaks subj away from obj for each individual
         get-all-properties-from-individual  -- gets all properties from individual
      validate-all-conclusions               -- validates set of conclusions against set of models
|#

(defmethod break-and-add-negative ((conclusion q-intension) (model q-model) &key (validate-true nil))
  "Takes a model and tries to break every instance of subj and obj apart;
   if it succeeds, returns broken model, else nil."
  (let* ((subj (list (subject conclusion)))
         (obj (list (object conclusion)))
         (fn (footnote model))
         broken-model
         (broken-indivs           (break-individuals conclusion subj obj fn (individuals model)))
         (broken-model-subj-obj   (add-negative conclusion (make-instance 'q-model :indivs broken-indivs :fn fn) :validate-true validate-true))
         (broken-indivs-neg-subj  (break-individuals conclusion (negate-property subj) obj fn (individuals model)))
         (broken-model-neg-subj   (add-negative conclusion (make-instance 'q-model :indivs broken-indivs-neg-subj :fn fn) :validate-true validate-true))
         (broken-indivs-neg-obj   (break-individuals conclusion subj (negate-property obj) fn (individuals model)))
         (broken-model-neg-obj    (add-negative conclusion (make-instance 'q-model :indivs broken-indivs-neg-obj :fn fn) :validate-true validate-true)))
    (setf broken-model
          (cond
           ((and broken-model-subj-obj (validate conclusion (list broken-model-subj-obj) :validate-true validate-true))
            broken-model-subj-obj)
           ((and broken-model-neg-subj (validate conclusion (list broken-model-neg-subj) :validate-true validate-true))
            broken-model-neg-subj)
           ((and broken-model-neg-obj (validate conclusion (list broken-model-neg-obj) :validate-true validate-true))
            broken-model-neg-obj)
           (t nil)))
    (when (and broken-model (validate-all-conclusions fn (list broken-model)))
      broken-model)))

(defmethod break-model ((conclusion q-intension) (model q-model) &key (validate-true nil))
  "Takes a model and tries to break every instance of subj and obj apart;
   if it succeeds, returns broken model, else nil."
  (let* ((broken-indivs (break-individuals conclusion (list (subject conclusion)) (list (object conclusion))
                                           (footnote model)  (individuals model)))
         (broken-model  (copy-class-instance model)))
    (setf (individuals broken-model) broken-indivs)
    (if (and (validate conclusion (list broken-model) :validate-true validate-true)
             (validate-all-conclusions (footnote model) (list broken-model)))
        broken-model
      nil)))

(defmethod break-individuals ((conc q-intension) subj obj fn old-indivs &optional (new-indivs nil))
  "Recursively iterates through every individual; if it finds an
   individual in which both subj and obj appear, it breaks the two
   apart. It then checks to see if the model with the broken indiv
   validates against its footnotes. If so, it incorporates the new
   indivs into the new-model, otherwise it 'undoes' the break by
   incorporating the old individual into the new-model."
    (if (null old-indivs) new-indivs
      (let ((old-indiv (first old-indivs))
            new-individuals temp-indivs temp-model)
        (if (and (member subj old-indiv :test #'equalp)
                 (member obj old-indiv :test #'equalp))
            (setf new-individuals (break-subject-from-object subj obj old-indiv fn))
          (setf new-individuals (list old-indiv)))
        (setf temp-indivs (append new-indivs new-individuals (rest old-indivs)))
      ;;; debug printing:
      (setf temp-model (make-instance 'q-model :indivs temp-indivs))
      ;(print new-individuals)
      ;(print-model temp-model)
      ;(format t "     validate returned: ~A~%~%" (validate-all-conclusions fn (list temp-model)))
      (if (validate-all-conclusions fn (list temp-model))
          (break-individuals conc subj obj fn (rest old-indivs) (append new-indivs new-individuals))
        (break-individuals conc subj obj fn (rest old-indivs) (append new-indivs (list old-indiv)))))))

(defun break-subject-from-object (subj obj individual fn)
  "Break an obj away from a subj within a given individual by iterating
   through each property of the individual. For each property,
   If the property = subj, then adds property to piece1 (but NOT piece2).
   Else if property = obj, then adds property to piece2 (but NOT piece1).
   In any other situation, it adds the property to BOTH piece1 and piece2.
   Examples: Coming soon...ssk 3/6/2011"
  (let* (piece1 piece2 constraints)
    (dolist (property individual)
      (cond
       ((equalp property subj)
        (setf piece1 (append piece1 (list property))))
       ((equalp property obj)
        (when (and (equalp '- (first obj)) (not (member property piece1 :test #'equal)))
          (setf piece1 (append piece1 (list (negate-property property)))))
        (setf piece2 (append piece2 (list property))))
       ((setf constraints (recursively-find-constraints property fn))
        ;(setf piece1 (append piece1 constraints))
        (setf piece2 (append piece2 (list property))))
       (t
        (setf piece1 (append piece1 (list property)))
        (setf piece2 (append piece2 (list property))))))
    (cond
     ((some #'is-most fn)      (list piece1 piece2 piece2))  ; ssk 2017-06-29 this is a stub for a smarter system to deal with "most"
     (t                        (list piece1 piece2)))))

; ---------------------------------------------------------------------------------
; Section 9.4: Add an individual
; ---------------------------------------------------------------------------------

(defmethod add-individuals ((conclusion q-intension) (model q-model) &key (validate-true nil))
  "First attempts to falsify or satisfy depending on progressive addition of individuals;
   If that doesn't work,
      then if satisfying,
            if negative in conclusion calls add-affirmative else add-negative
      else if falsifying
            if negative in conclusion calls add-negative else add-affirmative"
  (let ((counterexample (add-progressively conclusion model :validate-true validate-true)))
    (when (null counterexample)
      (setf counterexample
            (if validate-true
                (if (negative-intension conclusion)
                    (add-affirmative conclusion model :validate-true validate-true)
                  (add-negative conclusion model :validate-true validate-true))
              (if (negative-intension conclusion)
                  (add-negative conclusion model :validate-true validate-true)
                (add-affirmative conclusion model :validate-true validate-true)))))
    counterexample))

#|
There are two high-level functions, one to try to refute affirmative conclusions:
  (add-affirmative conclusion model)
and one to try to refute negative conclusions:
   (add-negative conclusion model)
They each return a new model iff they succeed, else nil.

add-affirmative        -- tries to add individual, an end term, to refute affirmative conclusion
   add-affirm          -- adds indiv who is not subject of a footnote
      footnote-value   -- rtns value of footnote
      is-subject-of-fn -- rtns t iff end term is subject of a footnote
         depth         -- detects with one or more footnotes (function in another file)
         subj-of-fn    -- detects that end term is subject of footnote
      add-individual   -- to model, just before the footnote
   validate              -- in another file  |#

(defmethod add-affirmative ((conclusion q-intension) (model q-model) &key (number 1) (validate-true nil))
  "ok 3-6-11 if it can add individual to model to refute affirmative conclusion rtns new model
   but it only adds individuals who are not subjects of an include relation
   (add-affirmative '(all a are c) tom) => 
         (((B) (A) (C)) ((B) (A) (C)) ((B) (A) (C)) ((A)) (T7))
   Calls add-affirm to try to add end term. If conclusion is NOT validated in new-model
   then it is refuted and so rtns new-model"
  (let ((end-1 (list (subject conclusion)))
        (end-2 (list (object conclusion)))
        (new-model (copy-class-instance model)))
    (dotimes (i number) (setf (individuals new-model) (add-affirm end-1 end-2 new-model)))
    (when (and (validate conclusion (list new-model) :validate-true validate-true)
               (validate-all-conclusions (footnote new-model) (list new-model)))
      new-model)))

(defmethod add-affirm (end-1 end-2 (model q-model))
  "ok 3-6-11 adds first of the two end terms that is not subject of footnote to model 
   (add-affirmative '(C) '(A) tom) =>
   (((B) (A) (C)) ((B) (A) (C)) ((B) (A) (C)) ((A)) (T134))
   if end-1 is not subject of include in footnote adds end-1 as indiv to model
   else ditto for end-2"
  (cond((not (member end-1 (mapcar #'subject (footnote model)) :test #'equalp))
        (add-individual end-1 model))
       ((not (member end-2 (mapcar #'subject (footnote model)) :test #'equalp))
        (add-individual end-2 model))
       (t (append (add-individual end-1 model)))))

(defmethod add-individual (property (model q-model))
  "ok 3-6-11 adds individual to model
   (add-individual '(a) tom) (((B) (A) (C)) ((B) (A) (C)) ((B) (A) (C)) ((A)))"
  (let* ((constraints (recursively-find-constraints property (footnote model)))
         (new-property (if constraints constraints (list property))))
    (reverse (cons new-property (reverse (individuals model))))))

#|
add-negative          -- adds properties to individuals to try to refute negative conclusion
   add-negat          -- tries to add end-1 or end-2 to model, and rtns new or original model
     footnote-value   -- rtns value of footnote, e.g., (include '(A) '(B)) 
     is-subject-of-fn -- tests whether end term is subject of footnote value, see above
     recursively-find-constraints -- finds mid-term, or its neg that must also be added with end-term
     add-neg          -- adds end-term to every individual containing other end term
        add-properties -- adds list of one or more properties to indiv
           add-one-property -- adds one property to indiv or rtns nil if contradiction
              member-property -- in another file
              negate-property -- in another file           
   validate             -- in other file

    |#

(defmethod add-negative ((conclusion q-intension) (model q-model) &key (validate-true nil))
  "Add-negative -- adds properties to individuals to try to refute negative conclusion, adds
   all of them together. 
   if adds property to individual in model of negative premise(s) to refute conclusion
     rtns new-model else nil 
    (setf tom (initial-conclusion '((no a are b)(all b are c)))) => 
      (((A) (- B)) ((A) (- B)) ((A) (- B)) ((B) (C)) ((B) (C)) ((B) (C)) (T272)) 
    (add-negative '(no a are c) tom) and (add-negative '(no c are a)) both =>
    => (((A) (- B) (C)) ((A) (- B) (C)) ((A) (- B) (C)) ((B) (C)) ((B) (C)) ((B) (C)) (T274))
   If add-negat adds individuals and so new-model not equal to existing model then
   If conclusion is not true in model returns model"
  (let ((end-1 (list (subject conclusion))) (end-2 (list (object conclusion)))
        (new-model (copy-class-instance model)))
    (if (setf (individuals new-model) (add-negat end-1 end-2 model))
        (when (validate conclusion (list new-model) :validate-true validate-true) new-model))))

(defmethod add-negat (end-1 end-2 (model q-model))
 "ok tries to add end-1 to individual in model, else tries to add end-2, calls
     recursively-find-constraints to check whether middle terms have to be added too,
     and either returns unchanged model or new model in which it has validated premises 
  (add-negat '(c) '(a) '(((B) (- A) (C)) ((B) (- A) (C)) ((B) (C)) ((A)) ((A)) ((- A)) (T209)))
  => (((B) (- A) (C)) ((B) (- A) (C)) ((B) (C) (A)) ((A) (C) (B)) ((A) (C) (B)) ((- A)) (T209))
  If add-neg adds end term, and both premises are true in resulting mod-1,
     sets model to mod-1; otherwise sets mod-1 to nil
  If add-neg adds end term to model and both premises are true in resulting mod-2
      rtns mod-2
  elseif mod-1 not null rtns it; else rtns unchanged model"
 (let* ((model-indivs (copy-list (individuals model)))
        (fn (footnote model))
        (end-1-constraints (recursively-find-constraints end-1 fn))
        (end-2-constraints (recursively-find-constraints end-2 fn))
        (indv-1 (add-neg end-1 end-2 model-indivs end-1-constraints))
        (indv-2 (add-neg end-2 end-1 model-indivs end-2-constraints))
        (indv-1&2 (add-neg end-2 end-1 indv-1 end-2-constraints)))
   (cond
    ((and (validate-all-conclusions fn (list (make-instance 'q-model :indivs indv-1&2 :fn nil)))
          (not (equalp model-indivs indv-1&2)))
     indv-1&2)
    ((and (validate-all-conclusions fn (list (make-instance 'q-model :indivs indv-2 :fn nil)))
          (not (equalp model-indivs indv-2)))
     indv-2)
    ((and (validate-all-conclusions fn (list (make-instance 'q-model :indivs indv-1 :fn nil)))
          (not (equalp model-indivs indv-1)))
     indv-1)
    (t model-indivs))))

(defun add-neg (end-1 end-2 indivs end-1-constraints)
 "adds end-1, or end-1-constraints, which either includes middle term, its neg, or is nil,
  to every individual containing other end term, end-2 in model and so
  returns either new model or an unchanged model
  (setf model '(((B) (- A) (C)) ((B) (- A) (C)) ((B) (C)) ((A)) ((A)) ((- A)) (T207))
  (add-neg '(c) '(a) model '((C) (B))) => 
  (((B) (- A) (C)) ((B) (- A) (C)) ((B) (C)) ((A) (C) (B)) ((A) (C) (B)) ((- A)) (T207))
  Set new-properties to end-1-constraints (if non-null) or to list of end-1
  For each indiv in model, if end-2 is in it,  neither end-1 nor its negation is, and
  adding new-properties rtns new-indiv then add new-indiv to model; otherwise leave indiv 
  unchanged; rtn final model."
 (let (new-indivs new-properties new-indiv)
   (if end-1-constraints 
       (setf new-properties (remove-itm end-2 end-1-constraints))
     (setf new-properties (list end-1)))
   (dolist (indiv indivs new-indivs)
         (if (and (memberlis end-2 indiv)
                  (not(memberlis end-1 indiv))
                  (not(memberlis (negate-property end-1) indiv))
                  (setf new-indiv (add-properties new-properties indiv)))
             (setf new-indivs (append new-indivs (list new-indiv)))
           (setf new-indivs (append new-indivs (list indiv)))))))

(defun add-properties (properties indiv)
  "Adds list of properties to indiv calling add-one-property, rtns nil
   if property in list is negation of one in indiv
   (add-properties '((- c)(d)) '((a)(- c)(- d)(e))) => nil
   (add-properties '((- c)(- d)) '((a)(b)(- c))) => ((A) (B) (- C) (- D))"
  (if (null properties) 
      indiv
    (if (setf indiv (add-one-property (car properties) indiv))
        (add-properties (cdr properties) indiv))))

(defun add-one-property(property indiv)
  "If property is in indiv rtn indiv
   elseif its negation is in indiv rtn nil
   else add it to indiv
   (add-one-proprety '(c) '((a)(- b)) => ((a)(- b)(c))"
  (cond ((member-property property indiv) indiv)
        ((member-property (negate-property property) indiv) nil)
        (t (append indiv (list property)))))

(defun memberlis(prop lis-of-lis)
  "checks whether property is in lis-of-lis
   (memberlis '(- a) '((b)(- a))) => T"
  (dolist (lis lis-of-lis)
    (if (equal prop lis)(return t))))

(defun is-model-breakable (subj obj fn)
  "Checks to see if model is breakable; does so by grabbing the
   constraints on the subj (s-constraints) and the constraints on the
   obj (o-constraints). If subj and obj are NOT either both members of
   the s-constraints, or else both members of the o-constraints, then
   model is breakable."
  (let ((s-constraints (recursively-find-constraints subj fn))
        (o-constraints (recursively-find-constraints obj fn)))
    (not (or (and (member subj s-constraints :test #'equalp)
                  (member obj s-constraints :test #'equalp)
                  (< (position subj s-constraints :test #'equalp)
                     (position obj s-constraints :test #'equalp)))
             (and (member subj o-constraints :test #'equalp)
                  (member obj o-constraints :test #'equalp)
                  (< (position subj o-constraints :test #'equalp)
                     (position obj o-constraints :test #'equalp)))))))

(defmethod add-progressively ((conclusion q-intension) (model q-model) &key (validate-true t))
 "!!! SSK 12.2.13: documentation needed"
 (let* ((fn (footnote model))
        (terms (remove-duplicates (mapcan #'terms fn) :test #'equals))
        (subject (list (subject conclusion)))
        (object (list (object conclusion)))
        (unconstrained-terms (remove-if #'(lambda (x) (recursively-find-constraints x fn)) terms))
        (number-to-add       (+ (get-referent-cardinality subject model)
                                (get-referent-cardinality object model)))
        (subject-constraints (recursively-find-constraints subject fn))
        (object-constraints  (recursively-find-constraints object fn))
        (validated-models     (add-individual-progressively conclusion (copy-list (individuals model)) fn
                                                           unconstrained-terms subject-constraints object-constraints
                                                           :validate-true validate-true :number-to-add number-to-add)))
   
   validated-models))

(defmethod add-individual-progressively ((conclusion q-intension) model-indivs footnote
                                         unconstrained-terms subject-constraints object-constraints
                                         &key (validate-true t) (number-to-add 0))
  "!!! SSK 12.2.13: documentation needed"
  (if (= 0 number-to-add) nil
    (let* ((subject (list (subject conclusion)))
           (object (list (object conclusion)))
           (indv-1 (add-neg subject object model-indivs subject-constraints))
           (indv-2 (add-neg object subject model-indivs object-constraints))
           (indv-1&2 (add-neg object subject indv-1 object-constraints))
           (new-model-indivs (append model-indivs (list unconstrained-terms)))
           (models (list model-indivs new-model-indivs indv-1 indv-2 indv-1&2))
           (models (remove-if #'(lambda (x) (equals model-indivs x)) models))
           (models (mapcar #'(lambda (x) (make-instance 'q-model :indivs x)) models))
           (validated-models (remove-if-not #'(lambda (x) (validate conclusion (list x) :validate-true validate-true)) models))
           (validated-models (remove-if-not #'(lambda (x) (validate-all-conclusions footnote (list x))) validated-models)))
      (if validated-models
          (progn
            (mapcar #'(lambda (x) (setf (footnote x) footnote)) validated-models)
            (first validated-models))
        (add-individual-progressively conclusion new-model-indivs footnote
                                      unconstrained-terms subject-constraints object-constraints
                                      :validate-true validate-true
                                      :number-to-add (1- number-to-add))))))

; ---------------------------------------------------------------------------------
; Section 9.5: Move individuals
; ---------------------------------------------------------------------------------

#|
  move-individuals returns a model that is inconsistent with the conclusion.
     -- move-individuals            - iterates through all possible combination of individuals within the
                                    model and for each, collects possible moves and swaps that are
                                    inconsistent with the conclusion
     -- get-unique-comb-of-indivs - removes any repeated individuals from the model and then finds all
                                  - combinations of individuals (order does not matter)
     -- get-moves-swaps-from-indivs  - given two individuals in a model, it checks whether they can be 
                                      swapped/moved. If the move/swap is legal, then do it as many times
                                       as possible and collect each.
     -- get-number-possible-moves-swaps - since each move/swap involves two individuals, the number of
                                          possible moves/swaps is the number of the least frequent
                                          individual involved
     -- legal-move-p              - determines whether a move is legal. Resulting individuals must have an
                                    end term, must not be blocked by footnotes, and cannot be subsets of
                                    one another or have any directly opposing properties (A and - A)
     -- legal-swap-p              - determines a whether a swap is legal. Individuals must have an end 
                                    term, and must not be subsets of each other. Additionally, the new 
                                    individuals must not be blocked by footnotes.
     -- make-move                 - performs the move on the model
     -- make-swap                 - performs the swap on the model
     -- move-til-inconsistent     - performs a move til the model is inconsistent or it can't be moved n times
     -- swap-til-inconsistent     - performs a swap til the model is inconsistent or it can't be moved n times
     -- create-new-indivs-for-swap - creates the new individuals for swap
     -- blocked-by-footnotes      - checks whether there are any universals in the list of intensions
                                       that require a different property for the indiv
                                       -- NOTE: this needs to change to checking the resulting model to 
                                       see if its consistent
|#

(defmethod move-individuals ((conclusion q-intension) (model q-model)  &key (validate-true nil))
"Returns an individual that is the product of a move or a swap from model that falsifies/validates conclusion based on validate-true.
This function iterates through the unique pairs of indivs until it finds one that can be moved/swapped to create a counterexample."
  (let* ((individuals (individuals model))
         (unique-comb-of-indivs (get-unique-comb-of-indivs individuals)))
    (if (null unique-comb-of-indivs)
        nil
      (do* ((i 0 (+ i 1))
            (pair-of-indivs (nth 0 unique-comb-of-indivs) (nth i unique-comb-of-indivs))
            (new-move-swap (get-move-swap-from-indivs pair-of-indivs model conclusion :validate-true validate-true)
                           (get-move-swap-from-indivs pair-of-indivs model conclusion :validate-true validate-true)))
           ((or (not (null new-move-swap)) (= i (- (length unique-comb-of-indivs) 1 ))) new-move-swap)))))
  
(defun get-unique-comb-of-indivs (individuals)
"Given a model, this function returns a list where each element is a unique combination of indivs.
It does this by first eliminating the duplicates (individuals that are equivalent).
Then, iterate through the list to get each possible pair of individuals.
(get-unique-comb-of-indivs '(((A)(B)(C)) ((A)(B)) ((B)(C)) ((A)(B)) (T0)) )
=> ((((A)(B)(C)) ((A)(B))) (((A)(B)(C)) ((B)(C))) (((A)(B)) ((B)(C))))"
  (let ((reduced-model  (remove-duplicates individuals :test #'individual-equal)))
    (if (= (length reduced-model) 1)
        nil
    (do* ((indiv-number 0 (+ indiv-number 1))
          (indiv (nth 0 reduced-model) (nth indiv-number reduced-model))
          (combinations nil))
       ((= indiv-number (- (length reduced-model) 1))  combinations)
      (do* ((other-indiv-number (+ indiv-number 1) (+ other-indiv-number 1))
            (other-indiv (nth other-indiv-number reduced-model) (nth other-indiv-number reduced-model)))
           ((= other-indiv-number (length reduced-model)))
        (push (list indiv other-indiv) combinations))))))

(defmethod get-move-swap-from-indivs (pair-of-indivs (model q-model) (conclusion q-intension) &key (validate-true nil))
  "Given a pair of individuals, a model, a conclusion and the validation parameter, returns the first move/swap that is legal and inconsistent with the conclusion.
   First, checks to see if there is a legal move. If so, performs the move up til the maximum possible until it finds one that falsifies the conclusion and is still consistent with the premises.
   Repeats procedure for each potential swap.
   (start-indiv to end-indiv and vice versa).
   If the move is legal, iteratively calls move-n-times and appends the result to moves swaps.
   Example:
   A B
   A B
   A B
     C
     C
     C
  There are three possible moves here -- move 1 C, move 2 Cs and move 3 Cs."
  (block fn
    (let* ((list-of-premises (footnote model))
           (set-of-end-terms (get-end-terms list-of-premises))
           (end-term-1 (caar set-of-end-terms))
           (end-term-2 (cadar set-of-end-terms))
           (start-indiv (car pair-of-indivs))
           (end-indiv (cadr pair-of-indivs))
           (num-possible-moves  (get-number-possible-moves-swaps start-indiv end-indiv model)))
      (cond ((and (legal-move-p start-indiv end-indiv
                                list-of-premises end-term-1 end-term-2)
                  (setf new-model (move-til-inconsistent model start-indiv end-indiv num-possible-moves conclusion :validate-true validate-true)))
             (return-from fn new-model))
            ((and (legal-swap-p start-indiv end-indiv
                           list-of-premises end-term-1 end-term-2)
                  (setf new-model (swap-til-inconsistent model start-indiv end-indiv end-term-1 end-term-2 num-possible-moves conclusion :validate-true validate-true)))
             (return-from fn new-model))
            ((and (legal-swap-p end-indiv start-indiv 
                         list-of-premises end-term-1 end-term-2)
                  (setf new-model (swap-til-inconsistent model end-indiv start-indiv end-term-1 end-term-2 num-possible-moves conclusion :validate-true validate-true)))
             (return-from fn new-model))
            (t nil)))))

(defmethod move-til-inconsistent ((model q-model) indiv end-indiv n-times (conclusion q-intension) &key (validate-true nil))
  "Given the ingredients for a move, performs it n-times on the model and then returns the model/
   (move-n-times '(((A)(B)) ((A)(B)) ((A)(B)) ((C)) ((C)) (T1)) '((C)) '((A)(B)) 1)
   => (((A)(B)(C)) ((A)(B)) ((A)(B)) ((C)) (T1))
   (move-n-times '(((A)(B)) ((A)(B)) ((A)(B)) ((C)) ((C)) (T1)) '((C)) '((A)(B)) 2)
   => (((A)(B)(C)) ((A)(B)(C)) ((A)(B)) (T1))"
  (let ((new-model (make-instance 'q-model :indivs (individuals model) :fn (footnote model)))
        (list-of-premises (footnote model)))
    (do* ((i 0 (+ i 1))
          (next-indiv-index-num
           (position indiv (individuals new-model) :test #'individual-equal)
           (position indiv (individuals new-model) :test #'individual-equal))
          (next-end-indiv-index-num
           (position end-indiv (individuals new-model) :test #'individual-equal)
           (position end-indiv (individuals new-model) :test #'individual-equal))
          (new-model (make-move next-indiv-index-num
                                next-end-indiv-index-num new-model)
                     (make-move next-indiv-index-num
                                next-end-indiv-index-num new-model))
          (done (if (and 
                     (consistent-w-premises-p new-model list-of-premises) 
                     (validate-model conclusion new-model :validate-true validate-true))
                    T
                  nil)
                (if (and 
                     (consistent-w-premises-p new-model list-of-premises) 
                     (validate-model conclusion new-model :validate-true validate-true))
                    T
                  nil)))
         ((or done
              (= i n-times)) (if (or (not (= i n-times))
                                     done)
                                 new-model
                               nil)))))

(defmethod swap-til-inconsistent ((model q-model) start-indiv end-indiv end-term-1 end-term-2 n-times (conclusion q-intension) &key (validate-true nil))
  "Given the ingredients for a swap, performs it n-times on the model and then returns the model
   (swap-n-times '(((A)(B)(C)) ((A)(B)(C)) ((A)(B)(C)) ((- C)) ((- C)) (T1)) '((- C)) '((A)(B)(C)) '((A)) '((C)) 1)
   =>(((A)(B)(- C)) ((A)(B)(C)) ((A)(B)(C)) ((B)(C)) ((- C)) (T1))
   (swap-n-times '(((A)(B)(C)) ((A)(B)(C)) ((A)(B)(C)) ((- C)) ((- C)) (T1)) '((- C)) '((A)(B)(C)) '((A)) '((C)) 2)
   =>(((A)(B)(- C)) ((A)(B)(- C)) ((A)(B)(C)) ((B)(C)) ((B)(C)) (T1))"
  (let ((new-model (make-instance 'q-model :indivs (individuals model) :fn (footnote model)))
        (list-of-premises (footnote model)))
    (do* ((i 1 (+ i 1))
          (next-start-indiv-index-num
           (position start-indiv (individuals new-model) :test #'individual-equal)
         (position start-indiv (individuals new-model) :test #'individual-equal))
          (next-end-indiv-index-num
           (position end-indiv (individuals new-model) :test #'individual-equal)
           (position end-indiv (individuals new-model) :test #'individual-equal))
          (new-model 
           (make-swap next-start-indiv-index-num
                      next-end-indiv-index-num
                      new-model end-term-1 end-term-2)
           (make-swap next-start-indiv-index-num
                      next-end-indiv-index-num
                      new-model end-term-1 end-term-2))
          (done (if (and 
                     (consistent-w-premises-p new-model list-of-premises) 
                     (validate-model conclusion new-model :validate-true validate-true))
                    T
                  nil)
                (if (and 
                     (consistent-w-premises-p new-model list-of-premises) 
                     (validate-model conclusion new-model :validate-true validate-true))
                    T
                  nil)))
         ((or done
              (= i n-times)) (if (or (not (= i n-times))
                                     done)
                                 new-model
                               nil)))))

(defmethod get-number-possible-moves-swaps (start-indiv end-indiv (model q-model))
  "Given two individuals and a model, counts the number of equivalent individuals in the model.
   (get-number-possible-moves-swaps '((A)) '((B)) '(((A)(B)) ((A)) ((A)) ((B)) (T1)))
    => 1"
  (min
   (count-if #'(lambda(x) (individual-equal x start-indiv)) (individuals model))
   (count-if #'(lambda(x) (individual-equal x end-indiv)) (individuals model))))
      
(defun legal-move-p (indiv other-indiv footnotes end-term-1 end-term-2)
  "A move is where all of the properties in one individual are moved to another individual, in effect appending
   new properties, and the old individual is removed. This function checks whether two individuals, indiv and
   end-indiv, can be combined through a move operation. In order to determine whether the move is a legal, there
   are criteria:
   1) Both must have an end term
   2) does end-indiv have any properties that are the negation of indiv's properties
   3) do the footnotes explicitly forbid using any of indiv's properties with end-indiv's properties?
   4) does the destination indiv already have all of the indiv's properties (meaning, the move would have no effect)
   5) does the indiv already have all of the destination indiv's properties (meaning, the move would have no effect)
   If any of the criteria are true, return false."
 (and
  (contains-one-end-term indiv end-term-1 end-term-2)
  (contains-one-end-term other-indiv end-term-1 end-term-2)
  (not
   (or
    (conflicting-properties indiv other-indiv)
    (blocked-by-footnotes indiv footnotes)
    (blocked-by-footnotes other-indiv footnotes)
    (have-all-properties indiv other-indiv)))))

(defun legal-swap-p  (start-indiv end-indiv footnotes end-term-1 end-term-2)
  "This function checks whether two individuals, indiv and end-indiv, can be used in a swap.
   The criteria for swapping is as follows:
   MUST: contain exactly one matching negated end term.
   This means that the start-indiv (the one with only one end term) has either end-term-1, end-term-2 or
   either one negated and end-indiv has the same end-term, negated, along with another end-term.
   MUST NOT:
   1) Be forbidden by footnotes. This refers to the new individuals that would be created from the swap
   2) Likewise, but this for the other new individual
   3) Be a subset of the other. Basically, we want swaps to create a new individual. This isn't possible
      if either contains all the properties of the other"
  (if (and
       (contains-one-end-term start-indiv end-term-1 end-term-2)
       (contains-two-end-terms end-indiv end-term-1 end-term-2)
       (one-matching-neg-end-term start-indiv end-indiv end-term-1 end-term-2))
      (let* ((new-indivs-for-swap (create-new-indivs-for-swap start-indiv end-indiv
                                                              end-term-1 end-term-2))
             (new-start-indiv (car new-indivs-for-swap))
             (new-end-indiv (cadr new-indivs-for-swap)))
        (not
         (or
          (blocked-by-footnotes new-start-indiv footnotes)
          (blocked-by-footnotes new-end-indiv footnotes))))
    nil))

(defmethod make-move (indiv-number other-indiv-number (model q-model))
  "Given two indices within a model that correspond to individuals,
   Remove the first indiv from the model.
   Replace the second individual with a new individual that contains all of the properties from both indivs."
  (if (or (null indiv-number) (null other-indiv-number))
      model
    (let ((new-indiv (merge-individuals (nth indiv-number (individuals model))
                                        (nth other-indiv-number (individuals model)))))
      (setf (individuals model) 
            (remove-nth-from-list indiv-number 
                                  (replace-nth-from-list (individuals model) other-indiv-number new-indiv)))
      model)))

(defmethod make-swap (start-indiv-number end-indiv-number (model q-model) end-term-1 end-term-2)
  "This function takes two individuals and swaps them.
   Example
   s: C
   e: A B -C
   result: A B C and B C
   s: -B C
   e: A B C
   result: A -B C and B C

   Usage: (make-swap 4 1 '(((B) (A) (C)) ((B) (A) (- C)) ((B) (A)) ((B) (- C)) ((C)) ((- C))) '(A) '(C))
          => (((B) (A) (C)) ((B) (A) (C)) ((B) (A)) ((B) (- C)) ((B) (- C)) ((- C)))"
  (let* ((start-indiv (nth start-indiv-number (individuals model)))
         (end-indiv (nth end-indiv-number (individuals model)))
         (new-indivs-for-swap (create-new-indivs-for-swap start-indiv end-indiv
                                                          end-term-1 end-term-2))
         (new-start-indiv (car new-indivs-for-swap))
         (new-end-indiv (cadr new-indivs-for-swap)))
    (setf (individuals model) (replace-nth-from-list (individuals model) start-indiv-number new-start-indiv))
    (setf (individuals model) (replace-nth-from-list (individuals model) end-indiv-number new-end-indiv))
    model))

(defun one-matching-neg-end-term (start-indiv end-indiv end-term-1 end-term-2)
"This function checks two indivs to make sure that the indivs have 1 (and only 1) matching negated end term."
  (let ((matching-negated-end-terms (get-matching-neg-end-terms start-indiv end-indiv
                                                                end-term-1 end-term-2)))
    (equal 1 (length matching-negated-end-terms))))

(defun get-matching-neg-end-terms (start-indiv end-indiv end-term-1 end-term-2)
  "Builds a list of end-terms shared by two indivs in which one indiv has the end-term
   and the other indiv has the negation of that end-term."
  (let ((neg-end-terms))
    (do ((i 0 (+ i 1)))
        ((= i (length start-indiv)) neg-end-terms)
      (if (has-property (negate-property (nth i start-indiv)) end-indiv)
          (setf neg-end-terms (append neg-end-terms (list (nth i start-indiv))))))))     

(defun create-new-indivs-for-swap (start-indiv end-indiv  end-term-1 end-term-2)
  "When making a swap between origin and destination indiv, two new indivs will be created subject to a few constraints, as outlined in make-swap.
   This function creates those individuals.
   (create-new-indivs-for-swap '((C)) '((A) (B) (- C)) '(A) '(C)) => (((B) (- C)) ((A) (B) (C)))"
  (let* ((start-indiv-matching-end-term
          (first (get-matching-neg-end-terms start-indiv end-indiv
                                             end-term-1 end-term-2)))
         (end-indiv-matching-end-term
          (first (get-matching-neg-end-terms end-indiv start-indiv
                                             end-term-1 end-term-2)))
         (end-indiv-other-end-term
          (first (remove-if #'(lambda(x) (or
                                          (property-equal end-indiv-matching-end-term x)
                                          (not
                                           (or
                                            (property-equal end-term-1 x)
                                            (property-equal end-term-2 x)
                                            (property-equal (negate-property end-term-1) x)
                                            (property-equal (negate-property end-term-2) x))))) end-indiv)))
         (new-end-indiv (remove-if #'(lambda(x) (property-equal end-indiv-matching-end-term x))
                                   (merge-individuals start-indiv end-indiv)))
         (new-start-indiv (remove-if #'(lambda(x) (property-equal end-indiv-other-end-term x))
                                     end-indiv)))
    (list new-start-indiv new-end-indiv)))

(defun blocked-by-footnotes (indiv footnotes)
  "Iterates through list of footnotes and determines whether the indiv directly violates any
   of the predicates. This only applies for universals.
   For Affirmative, check if both the subj and the negated predicate occur in the indiv.
   For Negative, check if the subj and the predicate occur in the indiv. 
   If this is the case, return T.
   (blocked-by-footnotes '((A)(B)) (list (parse '(No As are Bs)))) => T
   (blocked-by-footnotes '((A)(- B)) (list (parse '(All As are Bs)))) => NIL
   (blocked-by-footnotes '((A)(B)) (list (parse '(All As are Bs)))) => NIL
   (blocked-by-footnotes '((A)(B)) (list (parse '(Some As are Bs)))) => NIL"
  (do* ((f-num 0 (+ f-num 1))
        (cur-fn (nth 0 footnotes) (nth f-num footnotes))
        (cur-polarity (polarity cur-fn))
        (cur-fn-subj (subject cur-fn))
        (cur-fn-obj (object cur-fn))
        (found nil))
       ((or found (= f-num (length footnotes))) found)
    (if (or (is-all cur-fn) (is-none cur-fn))
        (if cur-polarity
            (if (and 
                 (has-property cur-fn-subj indiv) 
                 (has-property (negate-property cur-fn-obj) indiv))
                  (setf found T))
          (if (and 
               (has-property cur-fn-subj indiv) 
               (has-property cur-fn-obj indiv))
                (setf found T))))))

(defun contains-one-end-term (indiv end-term-1 end-term-2)
  "Checks whether an indiv contains an end term
   (contains-one-end-term '((B)) '(A) '(C)) => Nil
   (contains-one-end-term '((A)) '(A) '(C)) => T"
  (and
   (or 
    (has-property end-term-1 indiv)
    (has-property end-term-2 indiv)
    (has-property (negate-property end-term-1) indiv)
    (has-property (negate-property end-term-2) indiv))
   (not
    (and
     (has-property end-term-1 indiv)
     (has-property end-term-2 indiv)))))

(defun contains-two-end-terms (indiv end-term-1 end-term-2)
  "Checks whether two (possibly negated) end terms"
  (and
   (or
    (has-property end-term-1 indiv)
    (has-property (negate-property end-term-1) indiv))
   (or
    (has-property end-term-2 indiv)
    (has-property (negate-property end-term-2) indiv))))

(defun consistent-w-premises-p (model footnotes)
  (validate-all-conclusions footnotes (list model)))

; ---------------------------------------------------------------------------------
; Section 9.6: Negate individuals
; ---------------------------------------------------------------------------------

(defmethod negate-individuals ((conclusion q-intension) (model q-model) &key (validate-true nil))
  "Systematically negates individuals"
  (let ((subj (list (subject conclusion)))
        (obj (list (object conclusion)))
        (counterexample (copy-class-instance model))
        individual)
    (dotimes (i (length (individuals counterexample)))
      (setf individual (nth i (individuals counterexample)))
      (when (and (member-property subj individual)
                 (member-property obj individual))
        (setf individual (delete-if #'(lambda (x) (equal x obj)) individual))
        (setf individual (append individual (list (negate obj))))
        (setf (nth i (individuals counterexample)) individual)))
    (if (and (validate conclusion (list counterexample) :validate-true validate-true)
             (validate-all-conclusions (footnote counterexample) (list counterexample)))
        counterexample
      nil)))

; ---------------------------------------------------------------------------------
; Section 9.7: Minimize, drop, and add
; ---------------------------------------------------------------------------------

(defmethod minimize-drop-and-add-individuals ((conclusion q-intension) (model q-model) &key (validate-true nil))
  "This fn first minimizes a model; then it progressively drops each individual
   and attempts to add additional properties."
  (let* ((minimized-model (minimize-model model))
         dropped-model counterexample)
    (dolist (i (individuals minimized-model))
      (setf dropped-model (copy-class-instance minimized-model))
      (setf (individuals dropped-model) (remove i (individuals dropped-model) :test #'equals))
      (setf counterexample (add-individuals conclusion dropped-model :validate-true validate-true))
      (when (and counterexample
;                 (validate conclusion (list counterexample) :validate-true validate-true)
 ;                (validate-all-conclusions (footnote counterexample) (list counterexample))
)
        (return-from minimize-drop-and-add-individuals counterexamples)))) )

(defmethod replace-properties ((conclusion q-intension) (model q-model) &key (validate-true nil))
  "This fn first minimizes a model; then it progressively drops each individual
   and attempts to add additional properties."
  (let* ((subject   (list (subject conclusion)))
         (constraints (recursively-find-constraints subject (footnote model)))
         (indivs    (individuals model))
         (new-model (copy-class-instance model))
         counterexample)
    (when (not constraints) (setf constraints subject))
    (setf (individuals new-model) (drop-unconstrained-properties indivs subject constraints))
    (setf counterexample (add-individuals conclusion new-model :validate-true validate-true))
    (when (and counterexample
               (validate conclusion (list counterexample) :validate-true validate-true)
               (validate-all-conclusions (footnote counterexample) (list counterexample)))
      counterexample)))

(defun drop-unconstrained-properties (individuals subject constraints)
  (if (null individuals) nil
    (if (find-referent-in-individual subject (first individuals))
        (cons (list constraints) (drop-unconstrained-properties (rest individuals) subject constraints))
      (cons (first individuals) (drop-unconstrained-properties (rest individuals) subject constraints)))))

; ---------------------------------------------------------------------------------
; Section 9.8: Shifting event earlier or later
; ---------------------------------------------------------------------------------

(defmethod shift-event-earlier ((conclusion t-intension) (model t-model) &key (validate-true nil))
  (let* ((subj     (subject conclusion))
         (obj      (object conclusion))
         (subj-r   (event-range subj (moments model)))
         (obj-r    (event-range obj (moments model))))
    (unless (and (is-punctate subj model) (is-punctate obj model))
      (setf model (convert-to-durative subj model))
      (setf model (convert-to-durative obj model))
      (setf subj-r (event-range subj (moments model)))
      (setf obj-r (event-range obj (moments model))))
    (setf models
          (if (or (null (first subj-r)) (null (first obj-r)))
              (list model)
            (append (progressively-shift-earlier conclusion subj model subj-r :validate-true validate-true)
                    (progressively-shift-earlier conclusion obj model obj-r :validate-true validate-true))))
    (first models)))

#|(defmethod progressively-shift-subject-and-object ((conclusion t-intension) (model t-model) &key (validate-true nil))
  (let* ((subj     (subject conclusion))
         (obj      (object conclusion))
         (subj-r   (event-range subj (moments model)))
         (obj-r    (event-range obj (moments model))))
    (if (or (null (first subj-r)) (null (first obj-r)))
        nil
      (append
       (progressively-shift-earlier conclusion subj model subj-r :validate-true validate-true)
       (progressively-shift-earlier conclusion obj model obj-r :validate-true validate-true)))))|#

(defun progressively-shift-earlier (conclusion event model range &key (validate-true nil))
  (if (< (first range) 1) nil
    (let* ((constraints (recursively-find-constraints (list event) (footnote model)))
           (shifted-models (shift-earlier-by-one event model range))
           models temporary-model)
      (setf models (cons model shifted-models))
      (when constraints
        (dolist (concurrent-event (remove-from event constraints))
          (setf temporary-model (first (shift-earlier-by-one (first concurrent-event) (second shifted-models)
                                                             (event-range (first concurrent-event) (moments (second shifted-models))))))
          (setf temporary-model (first (shift-earlier-by-one (first concurrent-event) temporary-model
                                                             (event-range (first concurrent-event) (moments temporary-model)))))))
      (when temporary-model (setf models (cons temporary-model models)))
      (setf models (filter-validated-models conclusion models :validate-true validate-true))
      (if models models
        (progressively-shift-earlier conclusion event model (list (1- (first range))
                                                                  (1- (second range))) :validate-true validate-true)))))

(defun shift-earlier-by-one (event model range)
  "Shifts an event marker in a model earlier by one, but does so in a way that either
   appends to the succeeding moment or creates a new moment before the preceding moment."
  (if (null (first range)) (list model)
    (let* ((model1 (copy-class-instance model))
           (model2 (copy-class-instance model)))
      (if (is-punctate event model)
          (progn
            (setf (moments model1) (remove-from `(,event) (moments model1)))
            (setf (moments model1) (insert-at `((,event)) (moments model1) (1- (first range)) :append t))
            (setf (moments model2) (remove-from `(,event) (moments model2)))
            (setf (moments model2) (insert-at `((,event)) (moments model2) (1- (first range)) :append nil)))
        (progn
          (setf (moments model1) (remove-from `(,event START) (moments model1)))
          (setf (moments model1) (insert-at `((,event START)) (moments model1) (1- (first range)) :append t))
          (setf (moments model1) (remove-from `(,event END) (moments model1)))
          (setf (moments model1) (insert-at `((,event END)) (moments model1) (1- (second range)) :append t))
          (setf (moments model2) (remove-from `(,event START) (moments model2)))
          (setf (moments model2) (insert-at `((,event START)) (moments model2) (1- (first range)) :append nil))
          (setf (moments model2) (remove-from `(,event END) (moments model2)))
          (setf (moments model2) (insert-at `((,event END)) (moments model2) (1- (second range)) :append nil))))
      (list model1 model2))))

(defmethod shift-event-later ((conclusion t-intension) (model t-model) &key (validate-true nil))
  (let* ((subj     (subject conclusion))
         (obj      (object conclusion))
         (subj-r   (event-range subj (moments model)))
         (obj-r    (event-range obj (moments model)))
         models)
    (unless (and (is-punctate subj model) (is-punctate obj model))
      (setf model (convert-to-durative subj model))
      (setf model (convert-to-durative obj model))
      (setf subj-r (event-range subj (moments model)))
      (setf obj-r (event-range obj (moments model))))
    (if (or (null (first subj-r)) (null (first obj-r)))
        (list model)
      (append (progressively-shift-earlier conclusion subj (first (shift-to-end subj model subj-r)) subj-r :validate-true validate-true)
              (progressively-shift-earlier conclusion subj (second (shift-to-end subj model subj-r)) subj-r :validate-true validate-true)
              (progressively-shift-earlier conclusion obj (first (shift-to-end obj model obj-r)) obj-r :validate-true validate-true)
              (progressively-shift-earlier conclusion obj (second (shift-to-end obj model obj-r)) obj-r :validate-true validate-true)))))

(defun shift-to-end (event model range)
  "Shifts an event marker in a model later by one, but does so in a way that either
   appends to the preceding moment or creates a new moment before the preceding moment."
  (let* ((model1 (copy-class-instance model))
         (model2 (copy-class-instance model)))
    (if (is-punctate event model)
        (progn
          (setf (moments model1) (remove-from `(,event) (moments model1)))
          (setf (moments model1) (insert-at `((,event)) (moments model1) (1- (length (moments model))) :append t))
          (setf (moments model2) (remove-from `(,event) (moments model2)))
          (setf (moments model2) (insert-at `((,event)) (moments model2) (1- (length (moments model))) :append nil)))
      (progn
        (setf (moments model1) (remove-from `(,event END) (moments model1)))
        (setf (moments model1) (remove-from `(,event START) (moments model1)))
        (setf (moments model1) (insert-at `((,event END)) (moments model1) (length (moments model1)) :append t))
        (setf (moments model1) (insert-at `((,event START)) (moments model1) (- (length (moments model1))
                                                                                (1+ (duration event model))) :append t))
        (setf (moments model2) (remove-from `(,event END) (moments model2)))
        (setf (moments model2) (remove-from `(,event START) (moments model2)))
        (setf (moments model2) (insert-at `((,event END)) (moments model2) (length (moments model2)) :append nil))
        (setf (moments model2) (insert-at `((,event START)) (moments model2) (- (length (moments model2))
                                                                                (duration event model)) :append nil))))
    (list model1 model2)))

(defun shift-start-earlier-by-one (event model range)
  "Shifts an event's START marker in a model earlier by one, but does so in a way that either
   appends to the succeeding moment or creates a new moment before the preceding moment."
  (if (null (first range)) (list model)
    (let* ((model1 (copy-class-instance model))
           (model2 (copy-class-instance model)))
      (setf (moments model1) (remove-from `(,event START) (moments model1)))
      (setf (moments model1) (insert-at `((,event START)) (moments model1) (1- (first range)) :append t))
      (setf (moments model2) (remove-from `(,event START) (moments model2)))
      (setf (moments model2) (insert-at `((,event START)) (moments model2) (1- (first range)) :append nil))
      (list model1 model2))))

(defun shift-end-later-by-one (event model range)
  "Shifts an event's END marker in a model later by one, but does so in a way that either
   appends to the succeeding moment or creates a new moment before the preceding moment."
  (if (null (first range)) (list model)
    (let* ((model1 (copy-class-instance model))
           (model2 (copy-class-instance model)))
      (setf (moments model1) (remove-from `(,event END) (moments model1)))
      (setf (moments model1) (insert-at `((,event END)) (moments model1) (second range) :append t))
      (setf (moments model2) (remove-from `(,event END) (moments model2)))
      (setf (moments model2) (insert-at `((,event END)) (moments model2) (1+ (second range)) :append nil))
      (list model1 model2))))

(defun progressively-expand-event (conclusion event model range &key (validate-true nil))
  (if (or (< (first range) 1) (> (second range) (length (moments model)))) nil
    (let* ((shifted-start    (shift-start-earlier-by-one event model range))
           (shifted-end      (shift-end-later-by-one event model range))
           (shifted-end1     (first shifted-end))
           (shifted-end2     (second shifted-end))
           (shifted-end1-r   (event-range event (moments shifted-end1)))
           (shifted-end2-r   (event-range event (moments shifted-end2)))
           (shifted-models   (append shifted-start
                                     shifted-end
                                     (shift-end-later-by-one event shifted-end1 shifted-end1-r)
                                     (shift-end-later-by-one event shifted-end2 shifted-end2-r)))
           (validated-models (filter-validated-models conclusion shifted-models :validate-true validate-true)))
      (if validated-models validated-models
       (mapcan #'(lambda (x)
                    (progressively-expand-event conclusion event x (list (1- (first range))
                                                                         (second range)) :validate-true validate-true))
                shifted-models)))))

(defmethod expand-event ((conclusion t-intension) (model t-model) &key (validate-true nil))
  (let* ((subj   (subject conclusion))
         (obj    (object conclusion))
         (subj-r (event-range subj (moments model)))
         (obj-r  (event-range obj (moments model)))
         (model  (copy-class-instance model))
         (model  (convert-to-durative subj model))
         (model  (convert-to-durative obj model))
         (expanded-s (progressively-expand-event conclusion subj model subj-r :validate-true validate-true)) 
         (expanded-o (progressively-expand-event conclusion obj model obj-r :validate-true validate-true)))
    (append expanded-s expanded-o)))

(defmethod convert-to-punctate-and-shift ((conclusion t-intension) (model t-model) &key (validate-true nil))
  (let* ((subj   (subject conclusion))
         (obj    (object conclusion))
         (model1 (convert-to-punctate subj model))
         (model2 (convert-to-punctate obj model))
         (model1 (first (shift-to-end subj model1 (event-range subj (moments model1)))))
         (model2 (first (shift-to-end obj model2 (event-range obj (moments model2))))))
    (append (progressively-shift-earlier conclusion subj model1 (event-range subj (moments model1)) :validate-true validate-true)
            (progressively-shift-earlier conclusion obj model2 (event-range obj (moments model2)) :validate-true validate-true))))

; ---------------------------------------------------------------------------------
; Section 9.9: Exhaustive search
; ---------------------------------------------------------------------------------

(defmethod exhaustive-search ((conclusion q-intension) (model q-model) &key (validate-true nil))
  (let* ((terms (remove-duplicates (mapcan #'terms (footnote model)) :test #'equals))
         k-combinations models)
    (dotimes (i *exhaustive-search-depth*)
      (setf k-combinations (combinations-with-replacement (all-combinations terms conclusion) (+ 2 i)))
      (setf models (mapcar #'(lambda (x) (make-instance 'q-model :indivs x :fn (footnote model))) k-combinations))
      (setf models (remove-if-not #'(lambda (x) (validate-all-conclusions (footnote model) (list x))) models))
      (setf models (remove-if-not #'(lambda (x) (validate conclusion (list x) :validate-true validate-true)) models))
      (when models (return-from exhaustive-search (first models))))
    nil))

(defmethod combinations-with-replacement (lst k)
  (cond
   ((= 0 k)   '(()))
   ((null lst) '())
   (t 
    (append
     (mapcar #'(lambda(x) (cons (car lst) x))
             (combinations-with-replacement lst (- k 1)))
     (combinations-with-replacement (rest lst) k)))))

#| (defmethod exhaustive-search ((conclusion t-intension) (model t-model) &key (validate-true nil))
  (let* ((terms (remove-duplicates (flatten (mapcar #'terms (footnote model)))))
         (temporal-terms (progn 
                           (mapcan #'(lambda (x) (list (list x 'START) (list x 'END))) terms)))
         all-orders temp-model)
    (if (< (length temporal-terms) *exhaustive-search-depth*)
        (progn
          (setf all-orders (all-permutations temporal-terms))
          (format t "~A" (length all-orders))
          (dolist (o all-orders)
            (setf temp-model (make-instance 't-model :moments o :fn (footnote model)))
            (when (and (validate-all-conclusions (footnote model) (list temp-model))
                       (validate conclusion (list temp-model) :validate-true validate-true))
              (return-from exhaustive-search temp-model))))
      (error "Too many terms to conduct exhaustive search")))) |#

; ---------------------------------------------------------------------------------
; Section 9.10: Levenshtein distance
; ---------------------------------------------------------------------------------
; Introduced in Ragni, M., Khemlani, S., & Johnson-Laird, P.N. (2014). The evaluat-
; ion of the consistency of quantified assertions. Memory & Cognition, 42, 53-66.
; ---------------------------------------------------------------------------------

(defun remove-first (elt seq)
  (let ((remove-first t))
    (remove-if (lambda (e) (when (and remove-first (equal elt e))
                             (setq remove-first nil)
                             t))
               seq)))

(defmethod distance ((mod1 q-model) (mod2 q-model) &key (minimize t))
  "Calculates Levenshtein distance between mod1 and mod2."
  (let ((mod1 (copy-class-instance mod1))
        (mod2 (copy-class-instance mod2))
        (diff 0) indiv-diff removable-indiv)
    (when minimize (setf mod1 (minimize-model mod1) mod2 (minimize-model mod2)))

    (dolist (i1 (individuals mod1))                                               ; 1. filter out equivalent individuals
      (if minimize
        (dolist (i2 (individuals mod2))
          (when (= (entity-difference i1 i2) 0)
            (setf (individuals mod1) (remove-first i1 (individuals mod1))
                  (individuals mod2) (remove-first i1 (individuals mod2)))))
        (when (and (individuals mod1) (model-has-entity i1 mod2))
          (setf (individuals mod1) (remove-first i1 (individuals mod1))
                (individuals mod2) (remove-first i1 (individuals mod2))))))

    (dolist (i1 (individuals mod1))                                               ; 2. for each indiv in mod1, remove it 
      (if (individuals mod2)                                                      ;    and most similar indiv in mod2
          (dolist (i2 (individuals mod2))
            (when (or (null indiv-diff) (> indiv-diff (entity-difference i1 i2))) ;    when indiv-diff isn't set or when
              (setf indiv-diff (entity-difference i1 i2) removable-indiv i2)))    ;    entity-difference is smaller than
        (setf indiv-diff (entity-difference i1 nil) removable-indiv nil))         ;    current indiv-diff, update indiv-diff
      (incf diff indiv-diff)
      (setf (individuals mod1) (remove-first i1 (individuals mod1))               ;    remove i1 and the most similiar indiv
            (individuals mod2) (remove-first removable-indiv (individuals mod2))  ;    to i1
            indiv-diff nil removable-indiv nil))

    (dolist (i2 (individuals mod2))                                               ; 3. for each indiv in mod2, remove it 
      (if (individuals mod1)                                                      ;    and most similar indiv in mod1
          (dolist (i1 (individuals mod1))
            (when (or (null indiv-diff) (> indiv-diff (entity-difference i2 i1))) ;    same as above
              (setf indiv-diff (entity-difference i2 i1) removable-indiv i1)))
        (setf indiv-diff (entity-difference i2 nil) removable-indiv nil))
      (incf diff indiv-diff)
      (setf (individuals mod2) (remove-first i2 (individuals mod2))
            (individuals mod1) (remove-first removable-indiv (individuals mod1))
            indiv-diff nil removable-indiv nil))
    diff))

(defun contains-redundancies? (model)
  "Predicate -- checks whether model contains redundant
   entities, i.e., entities that can be removed without
   affecting the truth of any of the intensions in the
   footnote."
  (not (equals (shorten-model model) model)))

(defmethod shorten-model ((model q-model))
  "If possible, removes one redundant individual from
   a model."
  (let ((shortened-model (make-instance 'q-model :indivs (eliminate-redundant-entity (entities model)) :fn (footnote model))))
    (if (validate-all-conclusions (footnote shortened-model) (list shortened-model))
        shortened-model
      model)))

(defun eliminate-redundant-entity (entities &key (eliminated nil))
  "Removes one redundant entity from set of entities"
  (cond ((null entities) nil)
        (eliminated entities)
        ((member (first entities) (rest entities) :test #'individual-equal)
         (eliminate-redundant-entity (rest entities) :eliminated t))
        (t
         (cons (first entities) (eliminate-redundant-entity (rest entities))))))

(defmethod minimize-model ((model q-model))
  "Given a q-model, returns one token of each type for them.
   Example of recovery of one token of each type
   (setf A (first (consistent? (list Aab Iab) :d 0))) 
   (setf B (first (consistent? (list Aab Iab Eac) :d 0)))
   (minimize-model A)"
  (let ((minimized-model (make-instance 'q-model :indivs (minimize-entities (entities model)) :fn (footnote model))))
    (if (validate-all-conclusions (footnote minimized-model) (list minimized-model))
        minimized-model
      model)))

(defun minimize-entities (entities)
  "given entities rtns one token of each type"
  (cond ((null entities) nil)
        ((member (first entities) (rest entities) :test #'individual-equal)
         (minimize-entities (rest entities)))
        (t
         (cons (first entities) (minimize-entities (rest entities))))))

(defmethod distance ((mod1 t-model) (mod2 t-model) &key (minimize t))
  "Need to create Levenshtein distance fn for temporal models"
  0)