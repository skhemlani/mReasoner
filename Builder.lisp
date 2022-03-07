; ---------------------------------------------------------------------------------
; Part 7: Model building
; ---------------------------------------------------------------------------------

; Section 7.1: Decide what to do with premise to build models
; Section 7.2: Set theoretic semantics for building models of sentential intensions
; Section 7.3: Combining models
; Section 7.4: Starting a model, adding an object
; Section 7.5: Adding a subject
; Section 7.6: Validating an intension within a model

; ---------------------------------------------------------------------------------
; Section 7.1: Decide what to do with premise to build models
; ---------------------------------------------------------------------------------

(defun trace-build-model (intension &key (m nil) (s2 nil))
  (let ((system (if s2 "System 2" "System 1")))
    (if m (trc system (format nil "Initiated model building of ~A given ~A" (abbreviate intension) m))
      (trc system (format nil "Initiated model building of ~A" (abbreviate intension))))))

(defun build-model (intension &key (models nil))
 "Decides what to do with intension as function of whether or not its 1st and 2nd arguments already
  occur in models in the modelset. It operates by recovering the first and second arguments from
  the intension, then it calls the following procedures depending on the models:
   Validate intension in existing model(s)
   Add-subject to existing model(s)
   Add-object to existing model(s)
   Combine models
   Start new model"
   (let* ((m               (mapcar #'copy-class-instance models))
          (1st             (first-argument intension))
          (2nd             (second-argument intension))
          (models-1st      (find-referent-in-modelset 1st models))
          (models-2nd      (find-referent-in-modelset 2nd models))
          (models-both     (find-referent-in-modelset 2nd models-1st))
          (models-1st-only (remove-models models-2nd models-1st))
          (models-2nd-only (remove-models models-1st models-2nd)))
     (cond
      (models-both           (confirm intension models-both))
      ((and models-1st-only
            models-2nd-only) (combine intension models-1st-only models-2nd-only))
      (models-1st-only       (add-second-argument intension models-1st-only))
      (models-2nd-only       (add-first-argument intension models-2nd-only))
      ((or (null models)
           (and (not models-1st)
                (not models-2nd))) (start-model intension :models models)))))

(defun build-n-print (intension &key (m nil))
  "Builds then prints model"
  (let ((model (build-model intension :models m)))
    (if (> (length model) 1)
        (print-models model)
      (print-model (first model)))
    (terpri)(terpri)
    model))

(defun bnp (intension &key (m nil))
  "Builds then prints model (abbreviation)"
  (build-n-print intension :m m))

(defmethod confirm (intension models)
  "Confirms whether the intension of a new premise holds in the set
   of models. The premise's subject and object must already exist in
   the model for this function to be called. If the intension is validated,
   it's added to the "
  (if (not (validate intension models))
      (error 'consistency-error
             :text (format nil "~{~A~^, ~} failed to build the model"
                           (mapcar #'abbreviate (append (footnote (first models))
                                                        (list intension)))))
    (mapcar #'(lambda (m) 
                (when (not (member intension (footnote m) :test #'equals))
                  (add-footnote m intension))) models))
  models)

(defmethod confirm ((intension s-intension) models)
  ""
  (if (< (length models) 2)
      (list (start-mod intension (first models)))
    (error "[SSK 2019-11-15]: Too many models to combine")))

(defun add-second-argument (intension models)
  "Add-second-argument calls start-mod to add an object to each model in modelset.
   It assumes that each model in model set contains the subject of the intension"
  (let* (outmodels)
    (dolist (mod models outmodels)
       (if (stochastic-enabled?)
           (setf outmodels (append outmodels (list (funcall #'start-mod-stochastically intension :model (copy-class-instance mod)))))
         (setf outmodels (append outmodels (list (funcall #'start-mod intension :model (copy-class-instance mod)))))))
    (trc "System 1" (format nil "Added second argument of ~A to model" (abbreviate intension)) :m outmodels)
    outmodels))

(defun start-model (intension &key (models nil))
  "Calls start-mod to build the model, which is appended to modelset"
  (trc "System 1" (format nil "Started model of ~A" (abbreviate intension)) :m models)
  (let* (outmodels)
    (if (stochastic-enabled?)
        (if (null models)
            (setf outmodels (list (start-mod-stochastically intension)))
          (dolist (mod models outmodels)
            (setf outmodels (append outmodels (list (funcall #'start-mod-stochastically intension :model (copy-class-instance mod)))))))
      (if (null models)
          (setf outmodels (list (start-mod intension)))
        (dolist (mod models outmodels)
          (setf outmodels (append outmodels (list (funcall #'start-mod intension :model (copy-class-instance mod))))))))
    outmodels))

; ---------------------------------------------------------------------------------
; Section 7.3: Combining models
; ---------------------------------------------------------------------------------

#| (defmethod combine ((mod1 q-model) (mod2 q-model))
  "Combines two separate q-models by aligning matched properties together"
  (let*
      ((individuals1      (copy-list (individuals mod1)))
       (individuals2      (copy-list (individuals mod2)))
       (properties1       (recover-each-property (extract-all-individuals individuals1)))
       (properties2       (recover-each-property (extract-all-individuals individuals2)))
       (common-properties (intersection properties1 properties2 :test #'equals))
       matched-properties matched-individual combined-model)

    (dolist (m1-indiv individuals1)
      (setf matched-properties (intersection m1-indiv common-properties :test #'equals))
      (setf matched-individual
            (find-if #'(lambda (x) (has-property (first matched-properties) x))
                     individuals2))
      (when matched-individual
        (setf individuals2 (remove-if #'(lambda (x) (has-property (first matched-properties) x))
                                      individuals2 :count 1)))
      (push (merge-individuals m1-indiv matched-individual) combined-model))

    (print individuals1)
    (print individuals2)
    (setf combined-model (append (reverse combined-model) individuals2))
    combined-model)) |#

; ---------------------------------------------------------------------------------
; Section 7.4: Starting a model, adding an object
; ---------------------------------------------------------------------------------

; -------------------------- For quantificational models --------------------------

(defmethod canonical-individuals ((intension q-intension))
 (let* ((subject (list (subject intension)))
        (object  (list (object intension))))
   (cond
    ((is-all          intension)  `((,subject ,object)))
    ((is-some         intension)  `((,subject ,object) (,subject)))
    ((is-none         intension)  `((,subject ,(negate object)) (,(negate subject) ,object)))
    ((is-some-not     intension)  `((,subject ,(negate object)) (,subject ,object) (,object)))
    ((or (is-maj      intension)
         (is-most     intension)) `((,subject ,object) (,subject)))
    ((or (is-min      intension)
         (is-most-not intension)) `((,subject ,(negate object)) (,subject ,object)))
    ((is-setmem   intension :n 1) (if (affirmative-intension intension)
                                      `((,subject ,object) (,object))
                                    `((,subject ,(negate object)) (,object)))))))

(defmethod start-mod-stochastically ((intension q-intension) &key (model nil) (attempt *build-attempts*))
  (let* ((capacity           (generate-size))
         (full-individuals   (all-combinations (list (list (subject intension)) (list (object intension))) intension))
         (canon-individuals  (canonical-individuals intension))
         sample-individuals individuals new-model footnote)

    (if (null model)
        (setf footnote (list intension))
      (progn
        (setf canon-individuals (remove-duplicates
                                 (append canon-individuals
                                         (individuals model)
                                         (cartesian-product canon-individuals (individuals model)))))
        (setf footnote (append (list intension) (footnote model)))))

    (loop repeat capacity do
          (setf sample-individuals (if (build-canonical?) canon-individuals full-individuals))
          (push (nth (random (length sample-individuals)) sample-individuals) individuals))
    (setf new-model (make-instance 'q-model :indivs individuals :fn footnote :capacity capacity))
    (cond
     ((and (validate-all-conclusions (footnote new-model) (list new-model))
           (find-referent-in-model (subject intension) new-model)
           (find-referent-in-model (object intension) new-model))  new-model)
     ((> attempt 0)                                (rebuild-attempt (reverse (footnote new-model)) :attempt (1- attempt)))
     (t                                            (error "Could not construct model.")))))

(defun rebuild-attempt (intensions &key (attempt *build-attempts*))
  (if (null intensions) nil
    (start-mod-stochastically (first intensions) :model (rebuild-attempt (rest intensions)) :attempt attempt)))

(defmethod start-mod ((intension q-intension) &key (model nil))
"if no model, makes cardinal number of arg
 if negative polarity, such as 'no', negates predicate argument, '(B) => (- B)
 if negative polarity, such as 'some not', negates predicate too
 if null footnote, i.e., 'some, adds outliers
 if numprop less that 1 (it's a proportion) so multiplies it by cardinality to yield a number that is a 
          proportion of cardinality
 adds property in predicate to individuals in model having art in them"
  (let* ((card    (cardinality-value intension))
         (numprop (numprop-value intension))
         (subj    (list (subject intension)))
         (obj     (list (object intension))))
    (when (null model) (setf model (make-individuals card subj))) ; inserts card initial individuals

    (cond
     ; For set-membership assertions
     ((is-setmem intension :n 1)
      (cond
       ((negative-intension intension)
        (setf (individuals model) (add-new-property subj (negate-property obj) (individuals model) 1))
        (when (null mod) (setf (individuals model) (append (individuals model) (individuals (make-individuals 3 obj))))))
       ((affirmative-intension intension)
        (setf (individuals model) (add-new-property subj obj (individuals model) 1))
        (when (null mod) (setf (individuals model) (append (individuals model) (individuals (make-individuals 2 obj))))))))

     ; For assertions with determiner: "most"
     ((or (is-most intension)
          (is-maj intension))
      (cond
       ((negative-intension intension)
        (setf (individuals model)
              (add-new-property subj (negate-property obj) (individuals model) (- (get-referent-cardinality subj model) 1)))
        (setf (individuals model)
              (add-new-property subj obj (individuals model) (- card numprop))))
       ((affirmative-intension intension)
        (setf (individuals model)
              (add-new-property subj obj (individuals model) (- (get-referent-cardinality subj model) 1))))))

     ((is-min intension)
      (cond
       ((affirmative-intension intension)
        (setf (individuals model)
              (add-new-property subj (negate-property obj) (individuals model) (- (get-referent-cardinality subj model) 1)))
        (setf (individuals model)
              (add-new-property subj obj (individuals model) (- card numprop))))
       ((negative-intension intension)
        (setf (individuals model)
              (add-new-property subj obj (individuals model) (- (get-referent-cardinality subj model) 1))))))

     ; For assertions with determiner: "none"
     ((is-none intension)
      (setf (individuals model)
            (append (individuals model) (individuals (make-individuals 1 obj))))
      (setf (individuals model)
            (add-new-property subj (negate-property obj) (individuals model) (get-referent-cardinality subj model))))

     ; For assertions with quantifier: "some_not"
     ((is-some-not intension)
      (setf (individuals model)
            (append (individuals model) (individuals (make-individuals 1 obj))))
      (setf (individuals model)
            (add-new-property subj (negate-property obj) (individuals model) numprop)))

     ; For assertions with determiner: "some"
     ((is-some intension)
      (setf (individuals model)
            (add-new-property subj obj (individuals model) numprop)))

     ; For assertions with determiner: "all"
     ((is-all intension)
      (cond
       ((affirmative-intension intension)
        (setf (individuals model)
              (add-new-property subj obj (individuals model) (get-referent-cardinality subj model))))
       ((negative-intension intension)
        (setf (individuals model)
              (add-new-property subj (negate-property obj) (individuals model) (get-referent-cardinality subj model)))))))

    (add-footnote model intension)

    (if (stochastic-enabled?)
        (if (validate-all-conclusions (footnote model) (list model))
            model
          (first (interpret (footnote model))))
      model)))

(defun add-new-property (old-property new-property indivs numprop)
  " ok adds a new-property to n individuals in model that contain old-property
   (add-new-property '(A) '(B) '( ((A))((A))((A)) ) 1) =>
     (((A) (B)) ((A)) ((A)))
   (add-new-property '(A) '(B) '( ((A))((A))((A)) ) 3)
     (((A) (B)) ((A) (B)) ((A) (B)))
   But drops footnote, because start-mod updates it"
  (cond
   ((null indivs) nil)
   ((and (not (member-property new-property (first indivs)))
         (member-property old-property (first indivs))
         (not (member-property (negate-property new-property) (first indivs)))
         (> numprop 0))
    (cons (add-property new-property (first indivs) old-property)
          (add-new-property old-property new-property (rest indivs) (decf numprop))))
   (t
    (cons (first indivs) (add-new-property old-property new-property (rest indivs) numprop)))))

(defun add-property (property indiv &optional old-property)
  "ok adds property, which is a list to allow for negatives, to an individual, e.g. 
   (add-property '(baker) '((artist)(chemist))) => ((ARTIST) (CHEMIST) (BAKER))"
  (if old-property
      (let* ((pos-in-indiv     (position old-property indiv :test 'equal))
             (pos-in-rev-indiv (position old-property (reverse indiv) :test 'equal)))
        (if (>= pos-in-indiv pos-in-rev-indiv)
            (append indiv (list property))
          (append (list property) indiv)))
    (reverse (cons property (reverse indiv)))))

(defun member-property (property indiv)
  "ok [from file highlevel.lisp] checks whether individual in model has property
   rtns property iff it is in indiv
    (member-property '(- b) '((a)(- b)(c))) => (- B)"
  (cond((null indiv) nil)
       ((equal property (car indiv)) property)
       (t (member-property property (cdr indiv)))))

; ------------------------------ For temporal models ------------------------------

(defmethod start-mod-stochastically ((intension t-intension) &key (model nil) (attempt *build-attempts*))
  (let* ((model (start-mod intension)))
    (setf (capacity model) (generate-size))
    model))

(defmethod start-mod ((intension t-intension) &key (model nil))
  "if no model, creates new model with just the subject
   if 'before' intension, inserts object in (subject position + 1)
   if 'after' intension, inserts object in subject position (shifts subject up)
   if 'while' intension, inserts object in subject position (doesn't shift subject)
   else if 'during' intension, inserts durational object around subject
   NB fn calculates position of subject as an (x y) tuple where x = start position
   and y = end position; x = y in the event of a punctate event"
  (let* ((subj  (subject intension))
         (obj   (object intension))
         (prec  (precedence intension))
         subj-range)
    (when (null model) (setf model (make-instance 't-model :moments `(((,subj))) :fn nil)))

    (setf subj-range (event-range subj (moments model)))

    (if (or (not (stochastic-enabled?)) (not (build-canonical?)))
        (add-object-at-first-free-fit intension obj model subj-range)
      (add-object-at-first-fit intension obj model subj-range))

    (add-footnote model intension)
    model))

(defmethod add-object-at-first-free-fit ((intension t-intension) obj model range)
  "Adds an object in accordance with the 'first-free-fit' strategy (Ragni & Knauff, 2013).
   Event objects are inserted at the end of the array so as to avoid being inserted in
   between adjacent events. Hence, A before B, B after C yields C A B and not A C B."
  (cond
   ((is-before intension)
    (setf (moments model) (insert-at `((,obj)) (moments model) (length (moments model)))))
   ((is-after intension)
    (setf (moments model) (insert-at `((,obj)) (moments model) 0)))
   ((is-while intension)
    (setf (moments model) (insert-at `((,obj)) (moments model) (first range) :append t)))
   ((is-during intension)
    (setf (moments model) (insert-at `((,obj END)) (moments model) (1+ (second range))))
    (setf (moments model) (insert-at `((,obj START)) (moments model) (first range))))))

(defmethod add-object-at-first-fit ((intension t-intension) obj model range)
  "Adds an object in accordance with the 'first-fit' strategy (Ragni & Knauff, 2013).
   Event objects are inserted in between adjacent events. Hence, A before B, B after C
   yields A C B and not C A B."
  (cond
   ((is-before intension)
    (setf (moments model) (insert-at `((,obj)) (moments model) (1+ (second range)))))
   ((is-after intension)
    (setf (moments model) (insert-at `((,obj)) (moments model) (second range))))
   ((is-while intension)
    (setf (moments model) (insert-at `((,obj)) (moments model) (first range) :append t)))
   ((is-during intension)
    (setf (moments model) (insert-at `((,obj END)) (moments model) (1+ (second range))))
    (setf (moments model) (insert-at `((,obj START)) (moments model) (first range))))))

; ----------------------- For sentential connectives -----------------------

(defmethod start-mod-stochastically ((intension s-intension) &key (model nil) (attempt *build-attempts*))
  (let* ((model (start-mod intension)))
    (setf (capacity model) (generate-size))
    model))

(defmethod start-mod ((intension s-intension) &key (model nil))
  ""
  (let* ((both        (when (is-initial (both intension))
                        (list (first-clause intension) (second-clause intension))))
         (first-only  (when (is-initial (first-only intension))
                        (list (first-clause intension))))
         (second-only (when (is-initial (second-only intension))
                        (list (second-clause intension))))
         (neither     (when (is-initial (neither intension))
                        (list (negate (first-clause intension)) (negate (second-clause intension)))))
         1st-or-2nd-only possibilities)

    (when (system2-enabled?)
      (setf
       both        (when (is-possible (both intension))
                     (list (first-clause intension) (second-clause intension)))
       first-only  (when (is-possible (first-only intension))
                     (list (first-clause intension) (negate (second-clause intension))))
       second-only (when (is-possible (second-only intension))
                     (list (negate (first-clause intension)) (second-clause intension)))
       neither     (when (is-possible (neither intension))
                     (list (negate (first-clause intension)) (negate (second-clause intension))))))

    (cond
     ((is-affirmative-atom intension)
      (setf model
            (make-instance 's-model :poss (list (list (first-clause intension))) :fn (list intension))))
     ((is-negative-atom intension)
      (setf model
            (make-instance 's-model :poss (list (list '- (first-clause intension))) :fn (list intension))))
     (t
      (setf 1st-or-2nd-only
            (if (system2-enabled?)
                (append (build-intersection first-only) (build-intersection second-only))
              (append (build-union first-only (negate (second-clause intension)))
                      (build-union second-only (negate (first-clause intension))))))
      (setf possibilities
            (if (or (is-ori intension) (is-ore intension))
                (append 1st-or-2nd-only (build-intersection neither) (build-intersection both))
              (append (build-intersection both) 1st-or-2nd-only (build-intersection neither))))
      (setf model
            (make-instance 's-model :poss possibilities :fn (list intension)))))

    (when model
      (setf (possibilities model)
            (remove-duplicates (cartesian-product-of-models (list (possibilities mod)
                                                                  (possibilities model)))
                               :test #'equals))
      (setf (footnote model)
            (append (footnote mod)
                    (footnote model)))
      (setf model (make-instance 's-model :poss (embed-s-models model) :fn (footnote model))))
    model))

(defun build-union (intensions footnote)
  "Takes a list of intensions; if list is nil, return.
   Else, start a new model based on the individual intensions, and
   keep the models separate from one another; unembed any embedded models"
  (cond
   ((null intensions) nil)
   ((or (not (stochastic-enabled?)) (and (stochastic-enabled?) (build-canonical?)))
    (let* ((possibilities (mapcar #'(lambda (y) (start-mod y)) intensions))
           (possibilities (unembed-s-models possibilities)))
      (dolist (m possibilities)
        (when footnote (add-footnote m footnote)))
      possibilities))
   ((stochastic-enabled?)
    (build-intersection (append intensions (list footnote))))
   (t
    (error "Error in BUILD-UNION."))))

(defun embed-s-models (model)
  ""
  (let (models)
    (if (= (depth (possibilities model)) 3) ; i.e., embedded model
        (setf models (mapcar #'(lambda (x) (make-instance 's-model :poss x :fn nil))
                             (possibilities model)))
      (setf models (list model)))
    (dolist (m models)
      (setf (footnote m) (footnote model)))
    models))

(defun unembed-s-models (m)
  "Helper fn for build-union; for models that are 'embedded', i.e.,
   models that contain additional s-models, unembed-s-models flattens
   the structure out by recursively working through each model and, if
   necessary, replacing the embedded s-model with the models that are
   embedded within it."
  (cond
   ((null m) nil)
   ((and (typep m 's-model))
    (if (is-atomic-model m) (list m)
      (possibilities m)))
   (t (append (unembed-s-models (first m))
              (unembed-s-models (rest m))))))

(defun build-intersection (intensions)
  "Takes a list of intensions; if list is nil, return.
   Else, start a new model based on the individual intensions, then
   combine the model by taking the cartesian product of the models"
  (if (null intensions) nil
    (let* ((possibilities (mapcar #'(lambda (y) (start-mod y)) intensions))
           (possibilities (combine-list-of-s-models possibilities)))
      possibilities)))

(defun combine-list-of-s-models (list)
  "Helper fn for build-intersection; this fn applies the Cartesian product to its
   input models. If an atomic model is created, it returns that model; if
   an embedded model is created, where the Cartesian product yields multiple
   models, then a list of those individual models is returned"
  (let* ((model (make-instance 's-model
                              :poss (cartesian-product-of-models (mapcar #'possibilities list))
                              :fn (copy-instance-list (flatten (mapcar #'footnote list)))))
         models fn-list)
    (embed-s-models model)))

(defun cartesian-product-of-models (list)
  "This fn receives inputs in three formats. It detects the appropriate format, extracts the relevant
   possibilities, and applies the fn cartesian-product to the properly formatted possibilities. The
   formats are as follows:
   ( possibility-list , possibility-list ) -- simply applies cartesian-product to each possibility list
   ( atomic s-model ) -- returns possibilities
   ( possibility list OR atomic s-model , possibility list OR atomic s-model ) -- if atomic s-model,
   converts to possibilities and then applies cartesian-product, else grabs possibilities"
  (cond
   ((notany #'(lambda (x) (is-atomic-model x)) (flatten list))                    ; (possibility-list , possibility-list)
    (first (cartesian-product (list (first list)) (list (second list)))))
   ((and (= (length (flatten list)) 1) (is-atomic-model (first (flatten list))))  ; (atomic s-model)
    (possibilities (first (flatten list))))
   (t
    (let* ((model1  (first list))                                                 ; possibility list OR atomic s-model 
           (model2  (second list))                                                ; for either input
           (model1  (if (every #'(lambda (x) (is-atomic-model x)) model1)
                        (mapcar #'possibilities model1) (list model1)))
           (model2  (if (every #'(lambda (x) (is-atomic-model x)) model2)
                        (mapcar #'possibilities model2) (list model2)))
           (product (cartesian-product model1 model2)))
      (if (= (length product) 1)
          (first product)
        product)))))

(defun inspect-model (model)
  (if (every #'(lambda (x) (typep x 's-model)) (entities model))
      (progn 
        (format t "Primary intension: ~{~a~^, ~}~%" (mapcar #'abbreviate (footnote model)))
        (format t "Models: ~{~a~^, ~}~%" (mapcar #'(lambda (x) (format nil "~A" (entities x))) (entities model)))
        (format t "Intensions: ~{~a~^; ~}" (mapcar #'(lambda (x) (mapcar #'abbreviate (footnote x))) (entities model))))))

(defun cartesian-product (models1 models2)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------"
  (cond ((null models1) nil)
        (t (append (conjoin (car models1) models2) (cartesian-product (cdr models1) models2)))))

(defun conjoin (mod models)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------
   OK adds model, perhaps of only one item, to end of each member of models"
  (let ((new nil))
    (cond((null models) nil)
         ((setf new (join (car models) mod))(cons new (conjoin mod (cdr models))))
         (t (conjoin mod (cdr models))))))

(defun join (mod1 mod2) ;pjl (taken from propositional.lisp)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------
   OK returns model containing all tokens in mod1 and mod2 but without
   duplicates, and returns nil if token and its negation occur  
   '(?) corresponds to indeterminate models
   contra detects contradictions btn models
   match detects duplicates"
  (cond((null mod1) mod2)
       ((equal (car mod1) '?) mod2)
       ((equal (car mod2) '?) mod1)
       ((contra (car mod1) mod2) nil)
       ((match (car mod1)  mod2)(join (cdr mod1) mod2))
       (t (join (cdr mod1) (append mod2 (list (car mod1))))) ))

(defun contra (item mod) ;pjl (taken from propositional.lisp)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------
   OK if item is a contradiction of member of mod rtns t
      if item neg tries to match unnegated item;  if item is affirmative, tries
      to match negated item  --  either case signifies contradiction"
  (cond
   ((null mod) nil)
   (t (match (negate item) mod))))

(defmethod negate ((models list)) ;pjl (taken from propositional.lisp)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------
   NEGATION of sets of models or individual items
   OK returns the complement of a set of models, or does simple negation of
   a single item"
  (let ((atomlis nil))
    (cond((listp (car models)) (setf atomlis (findatms models nil))
          (comp models (all-possible atomlis)))
         (t (negate-property models)))))

(defun all-possible (model) ;pjl (taken from propositional.lisp)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------
   OK generates all possible models from one model
   null.cdr.model sets up two seed models in a list, e.g. (((d))((- d)))
   stick on car.model to each model in recursive call, and negate.car.model 
   appending the two lists
   (allpos '((a)(b)(c)(d)))(allpos nil)(allpos '((a)))(allpos '((a)(- b)))"
  (cond((null (rest model))(list (list (first model))(list (negate-property (first model)))))
       (t (append (mapcar #'(lambda(mod)(cons (first model) mod))(all-possible (rest model)))
                  (mapcar #'(lambda(mod)(cons (negate-property (first model)) mod))(all-possible (rest model)))))))

(defun comp (models allposmodels) ;pjl (taken from propositional.lisp)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------
   OK rtns the complement to a set of models from allposmodels"
  (cond ((null models) allposmodels)
        (t (comp (cdr models) (remove-itm (car models) allposmodels)))))

(defun remove-itm (itm lis) ;pjl (taken from propositional.lisp)
  "--------------------------------------------------------------------------------
   -- This function is originally written by Phil Johnson-Laird; it was taken    --
   -- from propositional.lisp, his implementation of the model theory circa 1995 --
   --------------------------------------------------------------------------------
   Removes itm, which must be a list, from lis-of-lis,
   e.g. '(a) '((b)(a)(c)) -> '((b)(c))  
   scales up where first parameter at one level less than 
   second,  and will remove lis even if order of items differs."
  (cond((null lis) nil)
       ((matchlists itm (first lis))(remove-itm itm (rest lis)))
       (t (cons (first lis) (remove-itm itm (rest lis))))))

; ------------------------------ For spatial models ------------------------------

(defmethod start-mod-stochastically ((intension sp-intension) &key (model nil) (attempt *build-attempts*))
  (let* ((model (start-mod intension)))
    (setf (capacity model) (generate-size))
    model))

(defmethod start-mod ((intension sp-intension) &key (model nil))
  "if no model, creates new model with just the subject; immediately sends intension to
   add-object-thing with the object and the subject/"
  (let* ((subj  (subject intension))
         (obj   (object intension)))
    (when (null model) (setf model (make-instance 'sp-model :things `((,subj)) :dims nil :fn nil)))
    (add-object-thing intension obj model subj)
    (add-footnote model intension)
    model))

(defmethod add-object-thing ((intension sp-intension) obj model subj)
  "1. Sets the 'relation' of the intension as either :left-right, :below-above, :behind-front.
   2. Sets whether the current model needs to be expanded ('expand'). 
   3. Interprets the 'direction' of the relation, i.e., either :plus or :minus, relative to the
      subject and object.
   4. Adds the dimensionality to the model
   5. Sets the 'axis' to see how the relation is represented in the model, i.e., :x, :y, or :z
   6. Expands the 'direction' of the relation depending on whether the model represents 1, 2,
      or 3 dimensions.
   Once all this info is known, it is passed to insert-thing-at, where the object is the thing
   to be inserted and the subject is the target of the insertion."
  (let* ((dimension (spatial-dimension intension))
         (expand (and (dimensions model) (not (member dimension (flatten (dimensions model))))))
         (direction (if (or (is-right intension) (is-above intension) (is-front intension)) :minus :plus))
         axis)
    (add-dimensionality intension model)
    (setf axis (first (first (member dimension (dimensions model) :test #'(lambda (x y) (member x y :test #'equals))))))
    (setf direction (case (length (dimensions model))
                      (1 direction)
                      (2 (case axis (:x (list direction nil)) (:y (list nil direction))))
                      (3 (case axis (:x (list direction nil nil)) (:y (list nil direction nil)) (:z (list nil nil direction))))))
    (setf (things model) (insert-thing-at obj (things model) subj :direction direction :expand expand))))

(defun insert-thing-at (thing list target &key (direction nil) (expand nil))
  "Inserts a thing in a list of things at a target, relative to the number of dimensions
   represented in the model."
  (let* ((pos (thing-position target list)))
    (cond 
     ((numberp pos)
      (insert-1d-thing-at thing list target :direction direction :expand expand))
     ((= 2 (length pos))
      (insert-2d-thing-at thing list target :direction direction :expand expand))
     ((= 3 (length pos))
      (insert-3d-thing-at thing list target :direction direction :expand expand))
     (t (error "Cannot handle reasoning in > 3 dimensions.")))))

(defun insert-1d-thing-at (thing list target &key (direction nil) (expand nil))
  "Inserts item into list (of lists) at the target at the specified 'direction' (required).
   If 'expand' is t, then the model needs to be expanded to a 2nd dimension to accommodate thing.
   If direction is :plus, adds item to the end of the list; if direction is :minus, adds it to
   the front of the list.
   Adopts a first-free-fit strategy of insertion (see Ragni & Knauff, 2013)."
  (when (not direction) (error "No direction specified."))
  (if (or (not (stochastic-enabled?)) (build-canonical?))
      (cond ; implements first-free-fit strategy
       (expand                          (insert-and-2d-expand thing list target :direction direction))
       ((null list)                     nil)
       ((and (symbolp thing)
             (equals direction :plus))  (attempt-merge thing (append list (list (list thing))) :direction direction))
       ((and (symbolp thing)
             (equals direction :minus)) (attempt-merge thing (append (list (list thing)) list) :direction direction))
       ((and (listp thing)
             (equals direction :plus))  (attempt-merge thing (append list (list thing)) :direction direction))
       ((and (listp thing)
             (equals direction :minus)) (attempt-merge thing (append (list thing) list) :direction direction)))
    (error "Need to implement fit strategy (see Ragni & Knauff, 2013).")))

(defun attempt-merge (thing things &key (direction nil))
  "By default, the fn insert-1d-thing at simply tacks the 'thing' onto the right or the
   left of the set of things. This fn attempts to merge the two rightmost things or the
   two leftmost things."
  (cond
   ((= (depth thing) 0) things)
   ((= (depth thing) 1) (error "Improperly formed spatial model."))
   ((= (depth thing) 2)
    (let ((pos (first (thing-position (first (find-if-not #'null thing)) things))))
      (cond
       ((equals direction :plus)
        (append (first (split-sequence nil things :start 0 :end (- (length things) 2)))
                (merge-two-lists (nth (1- pos) things) (nth pos things))
                (when (> (1- (length things)) pos)
                  (first (split-sequence nil things :start (1+ pos) :end (1- (length things)))))))
       ((equals direction :minus)
        (append (merge-two-lists (nth 0 things) (nth 1 things))
                (first (split-sequence nil things :start 2 :end (length things))))))))))

(defun merge-two-lists (list1 list2)
  (let* ((merged-lists (mapcar #'(lambda (x y) (find-if-not #'null (list x y))) list1 list2))
         (all-items (remove-if #'null (append list1 list2))))
    (if (equal (length (remove-if #'null merged-lists)) (length all-items))
        (list merged-lists)
      (list list1 list2))))

(defun insert-and-2d-expand (thing list target &key (direction nil))
  "OK 2018-10-03 ssk
   Inserts item into list (of lists) at the target at the specified 'direction' (required).
   Does so in a manner congruent with insert-1d-thing-at, but replaces every atomic object with a list,
   and adds the 'thing' either in the front or in the back of the target (or NIL, otherwise)."
  (cond
   ((not direction) (error "No direction specified."))
   ((listp direction) (setf direction (find-if-not #'null direction))))
  (cond
   ((null list)                       nil)
   ((and (member target (first list)) (equals direction :plus))
    (cons (list (first list) (list thing)) (insert-and-2d-expand thing (rest list) target :direction direction)))
   ((and (member target (first list)) (equals direction :minus))
    (cons (list (list thing) (first list)) (insert-and-2d-expand thing (rest list) target :direction direction)))
   (t
    (cons (if (equals direction :plus) (list (first list) nil) (list nil (first list)))
          (insert-and-2d-expand thing (rest list) target :direction direction)))))

(defun insert-2d-thing-at (thing list target &key (direction nil) (expand nil))
  "OK 2018-10-03 ssk -- (NB: eventually this fn needs to control 3D expansion)
  This fn inserts a thing into a 2D model by relying on a 1D insertion.
  It first creates a template of what to insert (called the 'thing-column') by grabbing
  the column that the target is in, clearing out any elements in the column that aren't
  the target, then substituting the thing for the target. Then it simply shoves the
  thing-column into the 2D model at the position of the target-column, using the fn
  insert-1d-thing-at.
  Note: 1D insertions are trivial in the x-direction -- simply append the new object
  at the target. They're trickier in the y-direction, but this is solved by
  simply transposing the 2D model, appling a 1D insertion, and then transposing again.
  That's what the x-shift and y-shift tests are doing below."
  (let* ((x-shift (first direction))
         (y-shift (second direction))
         (list    (if y-shift (transpose-list list) list)))
    (let* ((position      (position target (mapcar #'flatten list)
                                    :test #'(lambda (x y) (member x (flatten y) :test #'equals))))
           (target-column (nth position list))
           (predicate     #'(lambda (x) (member target (flatten x))))
           (thing-column  (substitute-if-not nil predicate target-column))
           (thing-column  (substitute-if (list thing) predicate thing-column)))
      (if x-shift 
          (insert-1d-thing-at thing-column list target-column :direction x-shift)
        (transpose-list (insert-1d-thing-at thing-column list target-column :direction y-shift))))))

(defmethod add-dimensionality ((intension sp-intension) (model sp-model))
  "This fn sets the 'dimensions' slot of the model. The slot keeps track of the dimensions
   that are currently represented in the model, and the particular axis where it's represented.
   Suppose, e.g., that the premises are: A is to the left of B / B is above C. In this case,
   the dimensions would be: (dimensions model) => ((:X :LEFT-RIGHT) (:Y :BELOW-ABOVE)).
   Alternatively, if the premises are: A is above B / B is to the left of C; then the
   dimensions would be: (dimensions model) => ((:X :BELOW-ABOVE) (:Y :LEFT-RIGHT))."
  (let* ((axis (case (length (dimensions model)) (0 ':x) (1 ':y) (2 ':z) (otherwise nil)))
         (dimension (spatial-dimension intension)))
    (unless (or (null axis) (member dimension (flatten (dimensions model))))
      (setf (dimensions model) (append (dimensions model) (list (list axis dimension)))))))

; ---------------------------------------------------------------------------------
; Section 7.5: Adding a subject
; ---------------------------------------------------------------------------------

(defmethod add-first-argument ((intension q-intension) models)
  "3-16-11
   If pol = t
     If numprop = card in quant then add subj to each indiv with obj in model
     else [numprop < card] add one subj to obj&a, or obj&not-a, or obj in model
         add one outlier c
   Else [pol = nil]
     If numprop = card in quant then add numprop subj-and-negate-obj to model
     Else [numprop < card] add one subj-and-negate-obj to model, and one outlier c
   (print-model (n-add-subject (parse '(some c are not b))(car (start-model (parse '(no b are a))))))
   B  -A
   B  -A
   B  -A
       A
       A
       A
  -B       C
           C"
  (let* (outmodels)
    (if (stochastic-enabled?)
        (dolist (mod models outmodels)
          (setf outmodels (append outmodels (list (funcall #'start-mod-stochastically intension :model (copy-class-instance mod))))))
      (progn
        (mapcar #'(lambda (m) (n-add-subject intension m)) models)
        (mapcar #'(lambda (m) (add-footnote m intension)) models)
        (setf outmodels models)))
    (trc "System 1" (format nil "Added subject of ~A to model" (abbreviate intension)) :m models)
    outmodels))

(defmethod n-add-subject ((intension q-intension) (model q-model))
  (let* ((subject (list (subject intension)))
         (object (list (object intension)))
         (cardinality (cardinality-value intension))
         (numprop   (numprop-value intension))
         (relation (relation intension))
         (n-objects (find-referent-individuals-in-model object model))
         (individuals (individuals model)))
    (setf (individuals model)
          (cond
           ((is-setmem intension :n 1)
            (if (negative-relation relation)
                (add-new-property (negate object) subject individuals 1)
              (add-new-property object subject individuals 1)))
           ((is-all intension)
            (add-new-property object subject individuals (length n-objects)))
           ((is-some intension)
            (append (add-new-property object subject individuals 1)(list (list subject))))
           ((is-none intension)
            (append (individuals model) 
                    (make-complex-individuals cardinality (list subject (negate object)))))
           ((is-some-not intension)
            (append (individuals model)
                    (make-complex-individuals 1 (list subject (negate object)))
                    (list (list subject))))
           (t (error "Unrecognized intension."))))
    model))

(defmethod add-first-argument ((intension t-intension) models)
  "Add subject (event) to model in which object is present"
  (setf models (mapcar #'(lambda (m) (add-subject-event intension m)) models))
  (mapcar #'(lambda (m) (add-footnote m intension)) models)
  (trc "System 1" (format nil "Added subject of ~A to model" (abbreviate intension)) :m models)
  models)

(defmethod add-subject-event ((intension t-intension) (model t-model))
  "Very similar function to start-mod (for t-intensions) above.
   if 'before' intension, inserts subject in object position (shifts subject up)
   if 'after' intension, inserts subject in (object position +1)
   if 'while' intension, matches subject to object position(s)
   else if 'during' intension, inserts subject between object
   NB fn calculates object position as an (x y) tuple where x = start position
   and y = end position; x = y in the event of a punctate event"
  (let* ((subj  (subject intension))
         (obj (object intension))
         (obj-range (event-range obj (moments model))))

    (when (and (is-during intension) (is-punctate obj model))
      ;; when a new event happens during a punctate event, convert the punctate event to durative
      (setf model (convert-to-durative obj model))
      (setf obj-range (event-range obj (moments model))))

    (if (or (not (stochastic-enabled?)) (not (build-canonical?)))
        (add-subject-at-first-fit intension subj model obj-range)
      (add-subject-at-first-free-fit intension subj model obj-range))
    model))

(defmethod add-subject-at-first-free-fit ((intension t-intension) subj model range)
  "Nearly identical to add-object-at-first-free-fit:
   Adds an subject in accordance with the 'first-free-fit' strategy (Ragni & Knauff, 2013).
   Event objects are inserted at the end of the array so as to avoid being inserted in
   between adjacent events. Hence, A before B, C before B yields C A B and not A C B."
  (cond
   ((is-before intension)
    (setf (moments model) (insert-at `((,subj)) (moments model) 0)))
   ((is-after intension)
    (setf (moments model) (insert-at `((,subj)) (moments model) (length (moments model)))))
   ((is-while intension)
    (setf (moments model) (insert-at `((,subj)) (moments model) (first range) :append t)))
   ((is-during intension)
    (setf (moments model) (insert-at `((,subj END)) (moments model) (second range)))
    (setf (moments model) (insert-at `((,subj START)) (moments model) (1+ (first range)))))))

(defmethod add-subject-at-first-fit ((intension t-intension) subj model range)
  "Nearly identical to add-object-at-first-fit:
   Adds a subject in accordance with the 'first-fit' strategy (Ragni & Knauff, 2013).
   Event objects are inserted in between adjacent events. Hence, A before B, C before B
   yields A C B and not C A B."
  (cond
   ((is-before intension)
    (setf (moments model) (insert-at `((,subj)) (moments model) (first range))))
   ((is-after intension)
    (setf (moments model) (insert-at `((,subj)) (moments model) (1+ (first range)))))
   ((is-while intension)
    (setf (moments model) (insert-at `((,subj)) (moments model) (first range) :append t)))
   ((is-during intension)
    (setf (moments model) (insert-at `((,subj END)) (moments model) (second range)))
    (setf (moments model) (insert-at `((,subj START)) (moments model) (1+ (first range)))))))

(defmethod add-first-argument ((intension sp-intension) models)
  "Add subject (thing) to model in which object is present"
  (mapcar #'(lambda (m) (add-subject-thing intension (subject intension) m (object intension))) models)
  (mapcar #'(lambda (m) (add-footnote m intension)) models)
  (trc "System 1" (format nil "Added subject of ~A to model" (abbreviate intension)) :m models)
  models)

(defmethod add-subject-thing ((intension sp-intension) thing model obj)
  "1. Sets the 'relation' of the intension as either :left-right, :below-above, :behind-front.
   2. Sets whether the current model needs to be expanded ('expand'). 
   3. Interprets the 'direction' of the relation, i.e., either :minus or :plus, relative to the
      subject and object. (NB: this is the opposite from add-object-thing above).
   4. Sets the 'axis' to see how the relation is represented in the model, i.e., :x, :y, or :z
   5. Expands the 'direction' of the relation depending on whether the model represents 1, 2,
      or 3 dimensions.
   Once all this info is known, it is passed to insert-thing-at, where the subject is the thing
   to be inserted and the object is the target of the insertion."
  (let* ((relation (cond ((or (is-left intension)   (is-right intension)) :left-right)
                         ((or (is-below intension)  (is-above intension)) :below-above)
                         ((or (is-behind intension) (is-front intension) ) :behind-front)))
         (expand (not (member relation (flatten (dimensions model)))))
         (direction (if (or (is-right intension) (is-above intension) (is-front intension)) :plus :minus))
         (axis (first (first (member relation (dimensions model) :test #'(lambda (x y) (member x y :test #'equals)))))))
    (setf direction (case (length (dimensions model))
                      (1 direction)
                      (2 (case axis (:x (list direction nil)) (:y (list nil direction))))
                      (3 (case axis (:x (list direction nil nil)) (:y (list nil direction nil)) (:z (list nil nil direction))))))
    (setf (things model) (insert-thing-at thing (things model) obj :direction direction :expand expand))
    (add-dimensionality intension model)))

; ---------------------------------------------------------------------------------
; Section 7.6: Validate an intension within a model
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
         (distance  (spatial-distance intension))
         (distance  (if (equals distance :infinity) 9999999999 distance))
         test)
    (cond
     (template
      (setf test (validate-spatial-template-between intension model)))
     ((is-same intension)      (setf test (equals pos1 pos2)))
     ((is-different intension) (setf test (not (equals pos1 pos2))))
     ((or (null pos1) (null pos2) (null (position dimension (dimensions model) :key #'second)))
      (setf test nil))
     (t
      (progn
;        (format t "pos1: ~A~%pos2: ~A~%distance: ~A~%" pos1 pos2 distance)
        (when (> (depth pos1) 0)
          (setf pos1 (nth (position dimension (dimensions model) :key #'second) pos1))
          (setf pos2 (nth (position dimension (dimensions model) :key #'second) pos2)))

        (setf test
              (cond
               ((equals relation '-) (and (< pos1 pos2) (< (abs (- pos1 pos2)) distance)))
               ((equals relation '+) (and (> pos1 pos2) (< (abs (- pos1 pos2)) distance))))))))

    (if validate-true test (not test))))

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
