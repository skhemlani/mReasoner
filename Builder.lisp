; ---------------------------------------------------------------------------------
; Part 6: Model building
; ---------------------------------------------------------------------------------

; Section 6.1: Decide what to do with premise to build models
; Section 6.2: Combining models
; Section 6.3: Starting a model, adding an object
; Section 6.4: Adding a subject

; ---------------------------------------------------------------------------------
; Section 6.1: Decide what to do with premise to build models
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
; Section 6.3: Combining models
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
; Section 6.3: Starting a model, adding an object
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
  (let* ((model (start-mod intension :model model)))
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
         new-model 1st-or-2nd-only possibilities)

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
      (setf new-model
            (make-instance 's-model :poss (list (list (first-clause intension))) :fn (list intension))))
     ((is-negative-atom intension)
      (setf new-model
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
      (setf new-model
            (make-instance 's-model :poss possibilities :fn (list intension)))))

    (when model
      (setf (possibilities new-model)
            (remove-duplicates (cartesian-product-of-models (list (possibilities model)
                                                                  (possibilities new-model)))
                               :test #'equals))
      (setf (footnote new-model)
            (append (footnote model)
                    (footnote new-model)))
      (setf new-model (make-instance 's-model :poss (embed-s-models new-model) :fn (footnote new-model))))
    new-model))

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
  (let* ((model (start-mod intension :model model)))
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
    (cond ; implements nearest fit strategy (ff-strategy in R & K 2013)
     (expand                          (insert-and-2d-expand thing list target :direction direction))
     ((null list)                     nil)
     ((and (symbolp thing)
           (equals direction :plus))  (attempt-merge thing (insert-at (list thing) list (1+ (thing-position target list))) :direction direction))
     ((and (symbolp thing)
           (equals direction :minus)) (attempt-merge thing (insert-at (list thing) list (thing-position target list)) :direction direction))
     ((and (listp thing)
           (equals direction :plus))  (insert-at thing list (1+ (first (thing-position (first (flatten target)) list)))))
     ((and (listp thing)
           (equals direction :minus)) (insert-at thing list (first (thing-position (first (flatten target)) list)))))))

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
; Section 6.4: Adding a subject
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