; ---------------------------------------------------------------------------------
; Part 1: Application Programming Interface (API) for mReasoner
; ---------------------------------------------------------------------------------

; Section 1.1:  Model manipulation functions
; Section 1.2:  Intension manipulation functions
; Section 1.3:  Helper fns for high-level fns
; Section 1.4:  Low-level model fns
; Section 1.5:  Property fns
; Section 1.6:  Individual fns
; Section 1.7:  Model fns
; Section 1.8:  Modelset fns
; Section 1.9:  Footnote fns
; Section 1.10: Intension fns
; Section 1.11: Tracer classes and functions

; ---------------------------------------------------------------------------------
; Section 1.1: Model manipulation functions
; ---------------------------------------------------------------------------------

(defmethod model-size ((model t-model))
  "Gets the size of a temporal model, i.e., the number of distinct events, punctate
   or durational, represented in the model."
  (length (remove-if #'(lambda (x) (or (equal x 'START)
                                       (equal x 'END)))
                     (remove-duplicates (flatten (moments model))))))

(defmethod find-referent-in-model (referent (model q-model)) ;ml
  (helper-find-ref-in-model referent (individuals model)))

(defmethod find-referent-in-model (referent (model t-model))
  (or (helper-find-ref-in-model (list referent) (moments model))
      (helper-find-ref-in-model (list referent 'START) (moments model))
      (helper-find-ref-in-model (list referent 'END) (moments model))))

(defmethod find-referent-in-model (referent (model sp-model))
  (thing-position referent (things model)))

(defun thing-position (thing things)
  "Gets position of a specified object (thing) from list representing a vector (depth = 2), matrix (depth = 3),
   or 3D grid (depth = 4)."
  (let ((tester  (lambda (x y) (member x (flatten y) :test #'equals))))
    (case (depth things)
      (2 (position thing things :test tester))
      (3 (let* ((list-pos (position thing things :test tester)))
           (when list-pos (list list-pos (thing-position thing (nth list-pos things))))))
      (4 (let* ((list-pos (position thing things :test tester)))
           (when list-pos (append (list list-pos) (thing-position thing (nth list-pos things)))))))))

(defmethod find-referent-in-model ((referent s-intension) (model s-model))
    (when (is-atom referent)
      (find-referent-in-model (first-argument referent) model)))

(defmethod find-referent-in-model ((referent symbol) (model s-model))
  (if (is-atomic-model model)
      (find-referent-in-modelset (list referent) (list (possibilities model)))
    (find-referent-in-modelset (list referent) (mapcar #'possibilities (possibilities model)))))

(defun find-referent-in-modelset (referent models) ; pjl
  "Tries to find referent in set of models
   It returns list of all models in which it finds referent"
  (cond((null models) nil)
       ((and (typep (first models) 'list) (find-referent-in-individual referent (first models)))
        (cons (first models) (find-referent-in-modelset referent (rest models))))
       ((and (typep (first models) 'model) (find-referent-in-model referent (first models)))
        (cons (first models) (find-referent-in-modelset referent (rest models))))
       (t (find-referent-in-modelset referent (rest models)))))

(defun helper-find-ref-in-model (referent model) ; pjl+ml
  "Tries to find a referent in a model"
  (cond((null model) nil)
       ((find-referent-in-individual referent (first model)) referent)
       (t (helper-find-ref-in-model referent (rest model)))))

(defun find-referent-in-individual (referent individual) ; pjl
  "Tries to find a referent in an individual, e.g.,
   (find-referent-in-individual '(A) '((A) (B))) => (A)"
  (cond
    ((null individual) nil)
    ((member referent individual :test #'property-equal) referent)))

(defun remove-models(tbr-mods modelset) ; pjl+ssk
  "ok eliminates one set of models from another
   (remove-models '( (((A)(B))((A)(B))) (((D) (B)) ((D) (B)))) test-modelset)
    => ((((A) (C)) ((A) (C))) (((D) (A)) ((D) (A))) (((D) (B)) ((D) (B))))"
  (dolist (tbr-mod tbr-mods)
    (setf modelset (remove-model tbr-mod modelset)))
  modelset)

(defun remove-model(mod modelset) ;pjl
  "ok eliminates one model from modelset
  (remove-mod '(((A)(B))((A)(B))) test-modelset) =>
   ((((A) (C)) ((A) (C))) (((D) (A)) ((D) (A))) (((D) (B)) ((D) (B))))"
 (cond((null modelset) nil)
      ((and (typep (first modelset) 'model)
            (equal (entities (first modelset)) (entities mod)))
       (remove-model mod (cdr modelset)))
      ((and (typep (first modelset) 'list)
            (equal (first modelset) mod))
       (remove-model mod (cdr modelset)))
      (t (cons (car modelset) (remove-model mod (cdr modelset))))))

(defmethod find-referent-individuals-in-model (referent (model q-model)) ; ssk + ml
  "Finds and returns all referent individuals in a model.
   (find-referent-individuals-in-model '(A) '(((- A) (B)) ((A) (B)) ((A) (B)) (T20)))
   => (((A) (B)) ((A) (B)))"
  (let ((ref-indivs nil))
    (dolist (individual (individuals model))
      (when (member referent individual :test #'property-equal)
        (push individual ref-indivs)))
    ref-indivs))

(defmethod find-subj&obj-in-model (subj obj (model q-model)) ; ssk
  "Finds and returns all referent individuals in a model.
   (find-subj&obj-in-model '(A) '(B) '(((- A) (B)) ((A) (- B))  ((A) (B)) ((A) (B)) (T20)))
   => (((A) (B)) ((A) (B)))"
  (let ((subj&obj-model nil))
    (dolist (individual (individuals model))
      (when (and (member subj individual :test #'property-equal)
                 (member obj individual :test #'property-equal))
        (push individual subj&obj-model)))
    subj&obj-model))

(defmethod find-subj-wo-obj-in-model (subj obj (model q-model)) ; ssk
  "Finds and returns all referent individuals in a model.
   (find-subj-wo-obj-in-model '(A) '(B) '(((- A) (B)) ((A) (- B))  ((A) (B)) ((A) (B)) (T20)))
   => (((A) (B)) ((A) (B)))"
  (let ((ref-indivs-model (find-referent-individuals-in-model subj model))
        (subj-wo-obj-model nil))
    (dolist (individual ref-indivs-model)
      (when (not (and (member subj individual :test #'property-equal)
                      (member obj individual :test #'property-equal)))
        (push individual subj-wo-obj-model)))
    subj-wo-obj-model))

(defmethod get-referent-cardinality (referent (model q-model)) ; ssk
   "Calculates cardinality of model with respect to a given referent, e.g.,
   (get-referent-cardinality '(A) '(((- A) (B)) ((A) (B)) ((A) (B)) (T20)))
   => 2"
  (length (find-referent-individuals-in-model referent model)))

(defmethod get-subj&obj-cardinality (subj obj (model q-model)) ; ssk
   "Calculates cardinality of model subset that includes individuals that are
    both subj & obj, e.g.,
   (get-subj&obj-cardinality '(A) '(B) '(((A) (C)) ((A) (B)) ((A) (B)) (T20)))
   => 2"
  (length (find-subj&obj-in-model subj obj model)))

(defmethod get-subj-wo-obj-cardinality (subj obj (model q-model)) ; ssk
   "Calculates cardinality of model subset that includes individuals that are
    subjs but NOT objs, e.g.,
   (get-subj-wo-obj-cardinality '(A) '(B) '(((A) (C)) ((A) (B)) ((A) (B)) (T20)))
   => 1"
  (length (find-subj-wo-obj-in-model subj obj model)))

(defmethod equals ((a symbol) (b symbol))
  (equal a b))

(defmethod equals ((a list) (b list))
  (equal a b))

(defmethod equals ((a string) (b string))
  (string-equal a b))

(defmethod equals (a b)
  (when (equals (type-of a) (type-of b))
    (equals a b)))

(defmethod equals ((entity-1 q-model) (entity-2 q-model)) ;ml
  "This function returns true or false depending on whether the two input
   models are equivalent. Equivalent in this case means that for all the
   individuals in mod1 there is an equivalent individual in mod2. . This ensures that the search is exhaustive.
   (is-equivalent-model '(((A))((B))) '(((A))((B)))) => T
   (is-equivalent-model '(((A))((B))) '(((A)))) => NIL"
  (and  
   (subsetp (footnote entity-1) (footnote entity-2) :test #'equals)
   (subsetp (footnote entity-2) (footnote entity-1) :test #'equals)
   (subsetp (individuals entity-1) (individuals entity-2) :test #'individual-equal)
   (subsetp (individuals entity-2) (individuals  entity-1) :test #'individual-equal)
   (and (equal (length (individuals entity-1)) (length (individuals entity-2))))))

(defmethod equals ((mod1 s-model) (mod2 s-model))
   (and  
    (subsetp (footnote mod1) (footnote mod2) :test #'equals)
    (subsetp (footnote mod2) (footnote mod1) :test #'equals)
    (subsetp (possibilities mod1) (possibilities mod2) :test #'possibilities-equal)
    (subsetp (possibilities mod2) (possibilities mod1) :test #'possibilities-equal)
    (and (equal (length (possibilities mod1)) (length (possibilities mod2))))))

(defun possibilities-equal (pos1 pos2)
  (cond
   ((and (is-atomic-model pos1) (is-atomic-model pos2))
    (and (individual-equal (possibilities pos1) (possibilities pos2))
         (subsetp (footnote pos1) (footnote pos2) :test #'equals)
         (subsetp (footnote pos2) (footnote pos1) :test #'equals)))
   ((and (is-atomic-modelset pos1) (is-atomic-modelset pos2))
    (subsetp (possibilities pos1) (possibilities pos2) :test #'possibilities-equal)
    (subsetp (possibilities pos2) (possibilities pos1) :test #'possibilities-equal))
   ((and (listp pos1) (listp pos2))
    (subsetp pos1 pos2 :test #'equals)
    (subsetp pos2 pos1 :test #'equals))))

(defmethod equals ((entity-1 intension) (entity-2 intension))
  (when (equalp (class-of entity-1) (class-of entity-2))
    (let ((slot-names (mapcar #'(lambda (x) (slot-definition-name x)) (class-direct-slots (class-of entity-1)))))
      (every #'(lambda (x) (equals (slot-value entity-1 x) (slot-value entity-2 x))) slot-names))))
 
(defun has-negative-properties (individual) ; ssk
  "Returns t if a list within individual contains '-"
  (let ((neg-prop nil))
    (when (> (depth individual) 1)
      (dolist (property individual)
        (when (member '- property) (setf neg-prop t))))
    neg-prop))

(defun remove-nth-from-list (index-num model)
 ; "Given a list of the form '(((A)(C)) ((A)(D)) ((A)(E)) ((A)(F)))
 ;  and a number, remove the nth individual (counting from 0) and return the resulting model E.G.
 ;  (remove-nth-from-list 1 '(((A)(C)) ((A)(D)) ((A)(E)) ((A)(F))))
 ;  =>(((A)(C)) ((A)(E)) ((A)(F))))"
  (append (subseq model 0 index-num) (subseq model (+ 1 index-num)(length model))))

(defun make-individuals (number arg) ; pjl
  "Given a cardinal number and a property
   rtns model of that number of individuals
   (make-individuals 3 '(A)) => (((A)) ((A)) ((A)))"
  (let (indivs)
    (dotimes (i number)(setf indivs (cons (list arg) indivs)))
    (make-instance 'q-model :indivs indivs :fn nil)))

(defun make-complex-individuals(number indiv) ; pjl
  "Given a cardinal number and indiv such as '((c)(-b))
   makes that number of them
   (make-complex-individuals 3 '((a)(- b))) =>
   (((A) (- B)) ((A) (- B)) ((A) (- B)))"
  (let(model)
    (dotimes (i number)(setf model (append (list indiv) model)))
    model))

(defun negate-property (property) ; pjl
  "Negates a property to be include in individual
   (negate-property '(A)) => (- A)"
  (if (equal (first property) '-)(rest property)
      (cons '- property)))

(defun entity-difference (indivs1 indivs2)
  "Returns the # of differences between two individuals.
   First, this algorithm iterates through one of the individuals'
   properties (ind1) and removes all co-occurrences.
   Second, it iterates through the remaining properties of ind1
   and tests to see if there are any pairs that are identical
   except for a negation, e.g., (B) and (- B). If there are,
   increment diff for each one and remove both.
   Finally, return diff + length remaining properties in ind1
   and ind2."
  (let ((ind1        (copy-list indivs1))
        (ind2        (copy-list indivs2))
        (diff 0) temp)
    (dolist (i ind1)
      (when (find i ind2 :test #'equal)
        (setf ind1 (remove i ind1 :test #'equal) ind2 (remove i ind2 :test #'equal))))
    (dolist (i ind1)
      (setf temp (or (find i ind2 :test #'(lambda (x y) (equal (negate-property x) y)))
                     (find i ind2 :test #'(lambda (x y) (equal (rest x) y)))))
      (when temp
        (incf diff)
        (setf ind1 (remove i ind1 :test #'equal) ind2 (remove temp ind2 :test #'equal))))
    (+ diff (length (flatten ind1)) (length (flatten ind2)))))

; ---------------------------------------------------------------------------------
; Section 1.2: Intension manipulation functions
; ---------------------------------------------------------------------------------

(defmethod abbreviate ((model model))
  (format nil "~A" model))

(defmethod abbreviate ((intension null-intension))
  (abbreviation intension))

(defmethod abbreviate ((intension q-intension))
  "Makes an abbreviation of a premise 
   (abbreviate (parse '(some a are not b))) => 'Oab'"
  (let ((subj (aref (string-downcase (symbol-name (first (subject intension)))) 0))
        (obj  (aref (string-downcase (symbol-name (first (object intension)))) 0))
        mood)
    (setf mood
          (cond
           ((is-most intension)        (format nil "M~A~A~A" subj (if (negative-intension intension) "-" "") obj))
           ((is-few intension)         (format nil "F~A~A~A" subj (if (negative-intension intension) "-" "") obj))
           ((is-some-not intension)    (format nil "O~A~A" subj obj))
           ((is-none intension)        (format nil "E~A~A" subj obj))
           ((is-some intension)        (format nil "I~A~A" subj obj))
           ((is-all intension)         (format nil "A~A~A" subj obj))
           ((is-setmem intension :n 1) (format nil "~A~A~A" subj (if (negative-intension intension) "-isnot-" "-is-") obj))
           (t (error "Abbreviation error: Assertion not supported by mReasoner."))))
    mood))

(defmethod abbreviate ((intension t-intension))
 (let ((rel (string-downcase (relation intension)))
       (subj (aref (string-downcase (subject intension)) 0))
       (obj  (aref (string-downcase (object intension)) 0)))
   (format nil "~A(~A,~A)" rel subj obj)))

(defmethod abbreviate ((intension sp-intension))
 (let ((rel  (string-downcase (relation intension)))
       (subj (string-downcase (subject intension)))
       (obj  (string-downcase (object intension))))
   (format nil "~A(~A,~A)" rel subj obj)))

(defmethod abbreviate ((intension s-intension))
  (cond
   ((is-affirmative-atom intension) (format nil "~A" (first-clause intension)))
   ((is-negative-atom intension) (format nil "¬~A" (first-clause intension)))
   ((is-and intension) (format nil "and(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
   ((is-nor intension) (format nil "nor(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
   ((is-ori intension) (format nil "or(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
   ((is-ore intension) (format nil "xor(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
   ((is-if intension)  (format nil "if(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
   ((is-iff intension) (format nil "iff(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
   ((is-not intension) (format nil "not(~A)" (abbreviate (first-clause intension))))))

(defmethod abbreviate ((intension c-intension))
  (cond
     ((is-cause intension) (format nil "cause(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
     ((is-enable intension) (format nil "enable(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))
     ((is-prevent intension) (format nil "prevent(~A,~A)" (abbreviate (first-clause intension)) (abbreviate (second-clause intension))))))

(defmethod get-syllogistic-figure ((premise-1 q-intension) (premise-2 q-intension)) ; pjl, slight mods from ssk + ml
  "Determines figure of the two premises by establishing the respective
   grammatical roles of the two end terms, e.g.,
   (get-syllogistic-figure (parse '(all b are a)) (parse '(all b are c)))
   => 4"
   (let* ((subj-1 (subject premise-1))
          (obj-1 (object premise-1))
          (subj-2 (subject premise-2))
          (obj-2 (object premise-2))
          (end-1 (first (get-syll-end-terms premise-1 premise-2)))
          (end-2 (second (get-syll-end-terms premise-1 premise-2)))) 
     (cond((and (equal subj-1 end-1)(equal obj-2  end-2)) 1)
          ((and (equal obj-1  end-1)(equal subj-2 end-2)) 2)
          ((and (equal subj-1 end-1)(equal subj-2 end-2)) 3)
          ((and (equal obj-1  end-1)(equal obj-2  end-2)) 4)
          (t (error "Not an orthodox syllogistic figure")))))

(defun negative-relation (relation) ; pjl
  (member relation (list 'not-include) :test #'equal))

(defmethod find-cardinality-condition ((intension q-intension)) ; ssk
  "Given an intension, returns the cardinality condition of the intension, e.g.,
   (find-cardinality-condition '((((? 4) (> 4)) (? 3) ((<= CARDINALITY) (>= 3)) T NIL) (A) (INCLUDE (A) (B))))
   => ((> 4))"
  (let ((card (cardinality intension))
        (condition nil))
    (dolist (ccond card)
      (when (not (equal '? (first ccond)))
        (push ccond condition)))
    condition))

(defmethod find-numprop-condition ((intension q-intension)) ; ssk
  "Given an intension, returns the cardinality condition of the intension, e.g.,
   (find-cardinality-condition '((((? 4) (> 4)) (? 3) ((<= CARDINALITY) (>= 3)) T NIL) (A) (INCLUDE (A) (B))))
   => ((> 4))"
  (when (not (equal '? (first (numprop intension))))
    (numprop-value intension)))

(defun evaluate-cardinality-conditions (value conditions) ; ssk
  "Given a value and a set of partial cardinality conditions, returns t or nil
   depending on whether all conditions have been met. E.g.,
   (evaluate-cardinality-conditions 5 '((> 3) (> 4) (> 1))) => T
   (evaluate-cardinality-conditions 5 '((> 3) (> 4) (> 100))) => nil"
  (let ((evaluated nil))
    (dolist (condn conditions)
      (push (eval (list (first condn) value (second condn))) evaluated))
    (not (member nil evaluated))))

(defun evaluate-boundary-conditions (value cardinality condns) ; ssk
  "Given a value and a set of boundary conditions, returns t or nil
   depending on whether all conditions have been met. E.g.,
   (evaluate-boundary-conditions 3 5 '((<= CARDINALITY) (>= 3))) => T
   (evaluate-boundary-conditions 2 5 '((<= CARDINALITY) (>= 3))) => NIL"
  (let ((evaluated nil)
        (eval-list nil)
        (conditions (subst cardinality 'cardinality condns)))
    (dolist (condn conditions)
      (push (list (first condn) value (second condn)) eval-list)
      (push (eval (list (first condn) value (second condn))) evaluated))
;    (print conditions)
;    (print eval-list) ; printing for debugging purposes only
;    (print evaluated)
    (not (member nil evaluated))))

(defmethod negative-intension ((intension q-intension))
  "Checks whether intension is negative by checking that either
   predicate is negative, quantifier is negative, but not both"
  (let ((neg-quant (not (polarity intension)))
        (neg-rel (negative-relation (relation intension)))) 
    (and (or neg-quant neg-rel)
         (not (and neg-quant neg-rel)))))
  
(defmethod affirmative-intension ((intension intension))
  (not (negative-intension intension)))

; ---------------------------------------------------------------------------------
; Section 1.3: Helper fns for high level fns
; ---------------------------------------------------------------------------------

(defmethod terms ((intension q-intension))
  (list (subject intension) (object intension)))

(defmethod get-syll-end-terms ((premise1 q-intension) (premise2 intension))
  "Interface for syllogistic premises to get-end-terms"
  (first (get-end-terms (list premise1 premise2))))

(defun get-end-terms (intensions)
  "Given a set of intensions corresponding to a hypothetical set of premises,
   returns a list contain possible pairs of end terms. The end-terms are derived by
   treating all premises of the word Quant Subj-property Obj-property as a direct link
   from one node, one called subj-property, to another node, called obj-property. This
   information is stored in a matrix called connectivity-matrix. Then, this graph is
   traversed, iterating through all possible starting points, to find a complete list of
   possible paths. This list is then sorted using a custom insertion sort. Currently, there
   are two options for possible sort strategies, though ultimately, this is a theoretical
   question that needs answering. In addition to length, one can also sort by length and then
   use end-term-frequency-p to consider ties (assuming that only one set of end terms is desired).
   Finally, pick end terms is called on the sorted list and chooses the top n paths, where each
   path is tied for the longest length, and reports the end term properties of the start and
   end of the path.

   Function Calls:
   - build-properties-and-frequency-list returns a list, the car of which is the frequency of
     every property in the intensions
   - build-connectivity-matrix uses information to construct a matrix
   - find-all-paths-through matrix uses depth first search to traverse graph created and returns
     a list of paths given a starting node
   - pick-end-terms takes a list of paths that has been sorted and returns the end terms and the
     head and tail of the top n paths where n is the set with the longest length

   Usage: (get-end-terms '( ((((? 3)(> 2)) (? 3) ((= cardinality))       t   t) (A) (include (A) (B)))  
                            ((((? 3)(> 2)) (? 3) ((= cardinality))       t   t) (B) (include (B) (C))) 
                            ((((? 3)(> 2)) (? 2) ((< cardinality)(> 0))  t nil) (A) (include (A) (C))) 
                            ((((? 3)(> 2)) (? 3) ((= cardinality))       t   t) (C) (include (C) (D)))))
           =>(((A) (D)))"
  (let* ((frequency-count (build-frequency-list intensions))
         (properties-list (mapcar #'(lambda(x) (car x)) frequency-count))
         (connectivity-matrix (build-connectivity-matrix intensions properties-list)))    
    ;(princ "Frequency-Count: ")(princ frequency-count)(terpri)
    (let ((all-paths nil))
      (do ((i 0 (+ i 1)))
          ((= i (length properties-list)))
        (setf all-paths (append (second (find-all-paths-through-matrix connectivity-matrix i (list (list i) (list)))) all-paths)))
      (setf all-paths (path-sort all-paths))
    ;(princ all-paths)(terpri)
    (pick-end-terms all-paths properties-list))))

(defun build-frequency-list (intensions)
  "Iterates through the list of intensions and builds frequency-count.
   Frequency-Count is a list of the form ((property frequency)(property frequency)...)
   where property is a property ((A) or (- A) for example) and frequency is an integer
   counting how many times it has occurred in both subj and obj positions. Hypothetically,
   if this function were modified, it could construct independent frequency-count lists for
   both subj and obj positions, if this were theoretically motivated.

   It works by checking to see if the property is already in the list. If it is, then it adds
   one to the frequency for that property. If not, then it creates a new entry and appends it to
   the list with frequency 1."
  (let ((frequency-count (list)))
    (dolist (intension intensions frequency-count)
      (let ((subj (subject intension))
            (obj  (object intension)))
        (if (not (member subj frequency-count :key #'car :test #'equal))
            (setf frequency-count (append frequency-count (list (list subj 1))))
          (let ((subj-freq (cadr (find subj frequency-count :key #'car :test #'equal))))
            (setf frequency-count (substitute-if (list subj (+ subj-freq 1)) #'(lambda(x) (equal (car x) subj)) frequency-count))))          
        (if (not (member obj frequency-count :key #'car :test #'equal))
            (setf frequency-count (append frequency-count (list (list obj 1))))
          (let ((obj-freq (cadr (find obj frequency-count :key #'car :test #'equal))))
            (setf frequency-count (substitute-if (list obj (+ obj-freq 1)) #'(lambda(x) (equal (car x) obj)) frequency-count))))))))

(defun pick-end-terms (path properties-list)
  "This function accepts a list of paths and a list of all possible properties.
   It determines the property name of the head and tail of the first path in the list.
   If the next path in the list is of equal length, it recursively calls itself with the
   rest of the paths.

   Usage: (pick-end-terms '((0 1 2 3) (0 2 3) (1 2 3) (2 3) (3)) '((A) (B) (C) (D))) => (((A) (D)))" 
  (let* ((subj-term (elt properties-list (car (first path))))
         (obj-term (elt properties-list (car (last (first path)))))
         (end-terms-list (list (list subj-term obj-term))))
;    (if (and (> (length path) 1) (equal (length (first path)) (length (second path))))
;      (if (not (null freq-count))
;          (setf end-terms-list (append end-terms-list (pick-end-terms (rest path) properties-list freq-count)))
;        (setf end-terms-list (append end-terms-list (pick-end-terms (rest path) properties-list))))
;      end-terms-list)))
    end-terms-list))

(defun path-sort (paths)
 "Sorts using length and then term order assumes that no path will be repeated (ie have equal
  length and elements)"
  (sort paths  #'(lambda (x y) (if (equal (length x) (length y))
                                   (let ((is-less nil))
                                     (do ((i 0 (+ i 1)))
                                         ((or (= i (length x)) is-less) is-less)
                                       (if (< (nth i x) (nth i y))
                                           (setf is-less T))))
                                 (> (length x) (length y))))))                          

(defun end-terms-more-frequent-p (path1 path2 freq-count)
  "Predicate function which compares the frequency of the sum of the head and tail terms for
   two different paths. The numbers in the path refer to the order index in the frequency list.
   Usage: (end-terms-more-frequent-p '(1 2 3) '(2 3) '(((A) 2) ((B) 2) ((C) 3) ((D) 1))) => NIL
          (end-terms-more-frequent-p '(2 3) '(1 2 3) '(((A) 2) ((B) 2) ((C) 3) ((D) 1))) => T"
  (let* ((properties-list (mapcar #'(lambda(x) (car x)) freq-count))
         (path1-head-freq (cadr (elt freq-count (position-if #'(lambda(x) (equal (car x) (elt properties-list (first path1)))) freq-count))))
         (path2-head-freq (cadr (elt freq-count (position-if #'(lambda(x) (equal (car x) (elt properties-list (first path2)))) freq-count))))
         (path1-tail-freq (cadr (elt freq-count (position-if #'(lambda(x) (equal (car x) (elt properties-list (car (last path1))))) freq-count))))
         (path2-tail-freq (cadr (elt freq-count (position-if #'(lambda(x) (equal (car x) (elt properties-list (car (last path2))))) freq-count)))))
    (> (+ path1-head-freq path1-tail-freq) (+ path2-head-freq path2-tail-freq))))


(defun build-connectivity-matrix (intensions properties-list)
  "Given a set of intensions, constructs a matrix that represents a directed graph. The matrix
   will be n x n, where n = length of properties-list. For every intension of the form Quant
   Subj-Prop Obj-Prop, it searches properties-list to and finds the index of that property and
   then puts a one in row subj-index-number in column obj-number. Note that these 1s are not
   considered symmetrical so there is no entry in the inverse.

   Usage:
   (build-connectivity-matrix '( ((((? 3)(> 2)) (? 3) ((= cardinality))      t   t) (A) (include (A) (B)))  
                                 ((((? 3)(> 2)) (? 3) ((= cardinality))      t   t) (B) (include (B) (C))) 
                                 ((((? 3)(> 2)) (? 2) ((< cardinality)(> 0)) t nil) (A) (include (A) (C))) 
                                 ((((? 3)(> 2)) (? 3) ((= cardinality))      t   t) (C) (include (C) (D))))
                                        '((A)(B)(C)(D)))
    =>#2A((X 1 1 X) (X X 1 X) (X X X 1) (X X X X))"
  (let ((connectivity-matrix (make-array (list (length properties-list) (length properties-list)) :initial-element 'x)))
    (dolist (intension intensions connectivity-matrix)
      (let* ((subj (subject intension))
             (obj  (object intension))
             (subj-number (position subj properties-list :test #'equal))
             (obj-number (position obj properties-list :test #'equal)))
        (setf (aref connectivity-matrix subj-number obj-number) 1)
        (setf (aref connectivity-matrix obj-number subj-number) 1)))))
  
(defun find-all-paths-through-matrix (matrix row-number paths)
  "Given a matrix representing a directed graph, returns a list of all paths that originate at the
   node specified by row-number by recursively exploring unseen nodes (nodes not in visited-list.
   The function works by checking each value in the current row. If its a 1, that means there is a
   connection and it recursively calls itself starting at the connected node (which can't be itself
   or an already visited node) with an updated visited list. When it reaches a node that has no viable
   connections, it returns. 
   Usage:
   (find-all-paths-through-matrix #2A((X 1 1 X) (X X 1 X) (X X X 1) (X X X X)) 0 (list (list 0) (list)))
   => ((0) ((0 2 3) (0 1 2 3)))"
  (let ((cur-row-contents (extract-matrix-row matrix row-number))
        (visited-list (car paths)))
    (if (terminal-node-p cur-row-contents)
        (list visited-list (list visited-list))
      (setf visited-list (find-all-sub-paths matrix row-number paths cur-row-contents)))))

(defun extract-matrix-row (matrix row)
  (let ((row-contents (list)))
    (dotimes (cur-col-num (array-dimension matrix 0) row-contents)
      (setf row-contents (append row-contents (list (aref matrix row cur-col-num)))))))

(defun terminal-node-p (row)
  (not (member 1 row :test #'equal)))

(defun find-all-sub-paths (matrix row-number paths cur-row-contents )
  (let ((sub-paths (list))
        (found-unvisited-node nil)
        (visited-list (car paths)))
    (do ((cur-col-num 0 (+ cur-col-num 1)))
        ((equal cur-col-num (length cur-row-contents)))
      (if (and 
           (equal (nth cur-col-num cur-row-contents) 1) 
           (not (equal cur-col-num row-number)) ;reflexive, should be impossible
           (not (member cur-col-num visited-list)))
          (let* ((new-stuff (find-all-paths-through-matrix matrix cur-col-num (list (append visited-list (list cur-col-num)) (list))))
                 (new-list-of-paths (cadr new-stuff)))
            (setf sub-paths (append new-list-of-paths sub-paths))
            (setf found-unvisited-node t))))              
    (if (not found-unvisited-node)
        (setf sub-paths (list visited-list)))
    (list visited-list sub-paths)))

; ---------------------------------------------------------------------------------
; Section 1.4: Low level model functions
; ---------------------------------------------------------------------------------

(defun matchlists (lis1 lis2)
  "Rtns lis2 iff it and lis1 have identical members,e.g. 
   ((a)(b)) = ((b)(a))
   & so can be used to compare models. "
  (cond((equal lis1 lis2) lis2)
       ((null lis1) nil)
       ((and (matchl lis1 lis2)(matchl lis2 lis1)) lis2)))

(defun matchl (lis1 lis2)
  "Checks that each member of lis1 is in lis2, e.g. 
   '((a)(b)) in '((b)(a)(c)) => T; ignores negation if itms 
   are directly compared, e.g. 
   '(a) and '(- a) => T; but not if mods are compared"
  (cond((null lis1) t)
       ((match (first lis1) lis2)(matchl (rest lis1) lis2))))

(defun match (item mod)
  "Matchs item with list, e.g. 'a and '(b a) -> t; '(a) 
   and '((b)(a)) => t;
   '((a)(b)) and '( ((a)(b)) c) => t provided order of elements 
   in item is same as in mod;  item can be atom, lis, 
   or lis-of-lis"
  (cond((null mod) nil)
       ((equal item (first mod)) t)
       (t (match item (rest mod)))))

; ---------------------------------------------------------------------------------
; Section 1.5: Property Functions
; ---------------------------------------------------------------------------------

(defun property-equal (property-1 property-2)
"This function compares two properties using the standard equality function.
(property-equal '(A) '(A)) => T
(property-equal '(A) '(B)) => NIL
(property-equal '(A) '(- A)) => NIL"
  (equal property-1 property-2))

(defun negative-property (property)
"Checks whether a property is negative.
(negative-property '(A)) => NIL
(negative-property '(- A)) => T"
  (and
   ;(p-property-p property)
   (equal (car property) '-)))

; ---------------------------------------------------------------------------------
; Section 1.6: Q-model Functions
; ---------------------------------------------------------------------------------

(defun individual-equal (individual-1 individual-2)
"This function returns true if the individuals have the same set of properties, regardless of ordering. 
It assumes that individuals will be properly formed (ie, no repeat properties).
(individual-equal '((A)) '((A))) => T
(individual-equal '((A)) '((A)(B))) => NIL"
  (have-all-properties individual-1 individual-2))

(defun has-property (property individual)
"This function determines whether a property is part of a list of properties.
(has-property '(A) '((A)(B))) => T
(has-property '(A) '((B)(C))) => NIL"
  (member property individual :test #'property-equal))

(defun have-all-properties (indiv-1 indiv-2)
  "Checks whether either indiv is a subset (in terms of properties) of the other."
  (and
   (subsetp indiv-1 indiv-2 :test #'property-equal)
   (subsetp indiv-2 indiv-1 :test #'property-equal)))

(defun conflicting-properties (indiv-1 indiv-2)
  "A check to see whether two indivs are compatible. If they contain the same propety but negated, they are not.
   Ex: A B -C and A B C - the Cs are the same but negated.
   (conflicting-properties '((A) (B) (- C)) '((A) (B) )) => T"
  (let ((cartesian-product (cartesian-product (list indiv-1) (list indiv-2))))
    (and 
     (remove-if-not #'(lambda(x) (equal (negate-property (first x)) (second x))) cartesian-product) 
     T)))

(defun merge-individuals (indiv-1 indiv-2)
  (remove-duplicates (append indiv-1 indiv-2) :test #'property-equal))

; ---------------------------------------------------------------------------------
; Section 1.7: S-model functions
; ---------------------------------------------------------------------------------

(defun is-atomic-model (model)
  "Tests whether a given model is an atomic model, i.e., an s-model that contains
   only atoms"
  (and (typep model 's-model)
       (every #'listp (entities model))
       (every #'symbolp (mapcar #'first (entities model)))))

(defun is-atomic-modelset (model)
  "Test whether a given s-model is comprised of atomic models"
  (and (typep model 's-model)
       (every #'is-atomic-model (entities model))))

(defmethod model-has-entity (entity (model q-model))
  "Checks whether a model contains an individual.
   (model-has-indivs '((A)) '( ((A)(B)) ((A)) (T0) ) ) => T
   (model-has-indivs '((A)) '( ((A)(B)) ((C)) (T0) ) ) => NIL"
  (member entity (individuals model) :test #'individual-equal))

(defmethod add-footnote ((model model) (new-intension intension))
  "DESTRUCTIVE
   Takes a model and an intension and updates the footnote in the model."
  (setf (footnote model) (append (footnote model) (list (copy-class-instance new-intension))))
  model)

; ---------------------------------------------------------------------------------
; Section 1.9: Intension functions
; ---------------------------------------------------------------------------------

(defmethod cardinality-value ((intension q-intension))
  "Returns concrete value of cardinality, e.g.,
   (cardinality-value Iab) => 4"
  (let
      ((assumption)
       (exact-value))
    (dolist (condition (cardinality intension))
      (if (equal (first condition) '?)
          (setf assumption (second condition))
        (if (equal (first condition) '=)
            (setf exact-value (second condition)))))
    (if exact-value
        exact-value
      assumption)))

(defmethod numprop-value ((intension q-intension))
  "Returns concrete value of numprop, e.g.,
   (numprop-value Iab) => 2"
  (cadr (numprop intension)))

(defmethod is-setmem ((intension q-intension) &key (n (numprop-value intension)))
  "If intension is a set membership relation, rtn it, else nil"
  (when (and (equalp (numprop intension) `(= ,n))
             (equalp (boundary intension) `((= ,n)))
             (polarity intension) (footnotes intension))
    intension))

(defmethod is-most ((intension q-intension))
  "If intension specifies 'most', rtn it, else nil"
  (when (and (equalp (boundary intension) '((< CARDINALITY) (> (* 0.5 CARDINALITY))))
             (polarity intension)
             (footnotes intension))
    intension))

(defmethod is-most-not ((intension q-intension))
  "If intension specifies 'most_not', rtn it, else nil"
  (when (and (is-most intension)
             (equalp (relation intension) 'NOT-INCLUDE))
    intension))

(defmethod is-few ((intension q-intension))
  "If intension specifies 'few', rtn it, else nil"
  (when (and (equalp (boundary intension) '((< (* 0.5 CARDINALITY)) (> 0)))
             (polarity intension)
             (footnotes intension))
    intension))
  
(defmethod is-all ((intension q-intension))
  "If intension is in mood A, rtn it, else nil"
  (when (and (equalp (boundary intension) '((= CARDINALITY)))
             (polarity intension)
             (footnotes intension))
    intension))

(defmethod is-none ((intension q-intension))
  "If intension is in mood E, rtn it, else nil"
  (when (and (equalp (boundary intension) '((= CARDINALITY)))
             (not (polarity intension))
             (footnotes intension))
    intension))

(defmethod is-some ((intension q-intension))
  "If intension is in mood I, rtn it, else nil"
  (when (and (equalp (boundary intension) '((<= CARDINALITY) (> 0)))
             (not (negative-intension intension)))
    intension))

(defmethod is-some-not ((intension q-intension))
  "If intension is in mood O, rtn it, else nil"
  (when (and (equalp (boundary intension) '((<= CARDINALITY) (> 0)))
             (negative-intension intension))
    intension))

(defmethod mood ((intension q-intension))
  "Outputs mood of assertion
   (mood (parse '(some a are not b))) => O "
    (cond
     ((is-all intension)      'A)
     ((is-some-not intension) 'O)
     ((is-none intension)     'E)
     ((is-some intension)     'I)
     ((is-most intension)     'M)
     ((is-few intension)      'F)
     ((is-setmem intension)  `(X ,(is-setmem intension)))
     (t (error "Assertion cannot be intrepreted"))))

(defmethod is-before ((intension t-intension))
  "If intension is 'before' relation, rtn it, else nil"
  (when (equal (first (precedence intension)) '<)
    intension))

(defmethod is-after ((intension t-intension))
  "If intension is 'after' relation, rtn it, else nil"
  (when (equal (first (precedence intension)) '>)
    intension))

(defmethod is-while ((intension t-intension))
  "If intension is 'while' relation, rtn it, else nil"
  (when (equal (first (precedence intension)) 'include)
    intension))

(defmethod is-during ((intension t-intension))
  "If intension is 'during' relation, rtn it, else nil"
  (when (equal (first (precedence intension)) 'properly-include)
    intension))

(defmethod relation ((intension t-intension))
  "Outputs relation of assertion
   (relation (parse '(A happened before B))) => B "
  (cond
   ((is-before intension) 'before)
   ((is-after intension) 'after)
   ((is-while intension) 'while)
   ((is-during intension) 'during)
   (t (error "Assertion cannot be intrepreted"))))

(defmethod is-right ((intension sp-intension))
  "If intension is 'right' relation, rtn it, else nil"
  (when (and (equals (spatial-relation intension) '+)
             (equals (spatial-dimension intension) :left-right))
    intension))

(defmethod is-left ((intension sp-intension))
  "If intension is 'left' relation, rtn it, else nil"
  (when (and (equals (spatial-relation intension) '-)
             (equals (spatial-dimension intension) :left-right))
    intension))

(defmethod is-above ((intension sp-intension))
  "If intension is 'above' relation, rtn it, else nil"
  (when (and (equals (spatial-relation intension) '+)
             (equals (spatial-dimension  intension) :below-above))
    intension))

(defmethod is-below ((intension sp-intension))
  "If intension is 'below' relation, rtn it, else nil"
  (when (and (equals (spatial-relation intension) '-)
             (equals (spatial-dimension intension) :below-above))
    intension))

(defmethod is-front ((intension sp-intension))
  "If intension is 'front' relation, rtn it, else nil"
  (when (and (equals (spatial-relation intension) '+)
             (equals (spatial-dimension intension) :behind-front))
    intension))

(defmethod is-behind ((intension sp-intension))
  "If intension is 'behind' relation, rtn it, else nil"
  (when (and (equals (spatial-relation intension) '-)
             (equals (spatial-dimension intension) :behind-front))
    intension))

(defmethod is-between ((intension sp-intension))
  "If intension is 'between' relation, rtn it, else nil"
  (let* ((template (spatial-template intension))
         (middle (first-argument intension))
         (sides  (second-argument intension)))
      (when (and template
                 (symbolp middle)
                 (listp sides)
                 (member (list (first sides) middle (second sides)) template :test #'equals)
                 (member (list (second sides) middle (first sides)) template :test #'equals))
        intension)))

(defmethod is-same ((intension sp-intension))
  "If intension is 'in same place' relation, rtn it, else nil"
  (when (equals (spatial-relation intension) :equal)
    intension))

(defmethod is-different ((intension sp-intension))
  "If intension is 'in different place' relation, rtn it, else nil"
  (when (equals (spatial-relation intension) :not-equal)
    intension))

(defmethod relation ((intension sp-intension))
  "Outputs relation of assertion
   (relation (parse '(A is behind B))) => B "
  (cond
   ((is-right intension)     'right)
   ((is-left intension)      'left)
   ((is-above intension)     'above)
   ((is-below intension)     'below)
   ((is-front intension)     'front)
   ((is-behind intension)    'behind)
   ((is-same intension)      'same)
   ((is-different intension) 'different)
   (t (error "Assertion cannot be intrepreted"))))

(defmethod is-possible (modal-status)
  (or (equal modal-status 'possible)
      (equal modal-status 'initial)
      (equal modal-status 'fact)))

(defmethod is-initial (modal-status)
  (equal modal-status 'initial))

(defmethod is-mental-model (modal-status)
  (is-initial modal-status))

(defmethod is-fact (modal-status)
  (equal modal-status 'fact))

(defmethod is-impossible (modal-status)
  (equal modal-status 'impossible))

(defmethod is-affirmative-atom ((intension s-intension))
  "If intension is an affirmative atom 'A', rtn it, else nil"
  (when (and (first-clause intension)
             (null (second-clause intension))
             (is-possible (first-only intension))
             (null (second-only intension))
             (null (both intension))
             (null (neither intension)))
    intension))

(defmethod is-negative-atom ((intension s-intension))
  "If intension is a negative atom '-A', rtn it, else nil"
  (when (and (first-clause intension)
             (symbolp (first-clause intension))
             (null (second-clause intension))
             (is-impossible (first-only intension))
             (null (second-only intension))
             (null (both intension))
             (null (neither intension)))
    intension))

(defmethod is-atom ((intension s-intension))
  "If intension is an atom (A or -A), rtn it, else nil"
  (or (is-affirmative-atom intension)
      (is-negative-atom intension)))

(defmethod is-simple-compound ((intension s-intension))
  (and (is-atom (first-clause intension))
       (is-atom (second-clause intension))))

(defmethod is-and ((intension s-intension))
  "If intension is a conjunction (A and B), rtn it, else nil"
  (when (and (is-impossible (first-only intension))
             (is-impossible (second-only intension))
             (is-possible   (both intension))
             (is-impossible (neither intension)))
    intension))

(defmethod is-nor ((intension s-intension))
  "If intension is a conjunction (A and B), rtn it, else nil"
  (when (and (is-impossible (first-only intension))
             (is-impossible (second-only intension))
             (is-impossible  (both intension))
             (is-possible (neither intension)))
    intension))

(defmethod is-ori ((intension s-intension))
  "If intension is an inclusive disjunction (A v B), rtn it, else nil"
  (when (and (is-possible   (first-only intension))
             (is-possible   (second-only intension))
             (is-possible   (both intension))
             (is-impossible (neither intension)))
    intension))

(defmethod is-ore ((intension s-intension))
  "If intension is an inclusive disjunction (A xor B), rtn it, else nil"
  (when (and (is-possible   (first-only intension))
             (is-possible   (second-only intension))
             (is-impossible (both intension))
             (is-impossible (neither intension)))
    intension))

(defmethod is-if ((intension s-intension))
  "If intension is a conditional (A -> B), rtn it, else nil"
  (when (and (equals 'S-INTENSION (type-of intension))
             (is-impossible (first-only intension))
             (is-possible   (second-only intension))
             (is-possible   (both intension))
             (is-possible   (neither intension)))
    intension))

(defmethod is-iff ((intension s-intension))
  "If intension is a biconditional (A <-> B), rtn it, else nil"
  (when (and (is-impossible (first-only intension))
             (is-impossible (second-only intension))
             (is-possible (both intension))
             (is-possible (neither intension)))
    intension))

(defmethod is-not ((intension s-intension))
  "If intension is a sentential negation, e.g., not(A <-> B), rtn it, else nil"
  (when (and (typep (first-clause intension) 'intension)
             (is-impossible (first-only intension))
             (null (second-only intension))
             (null (both intension))
             (null (neither intension)))
    intension))

(defmethod is-cause ((intension c-intension))
  "If intension is a cause, rtn it, else nil"
  (when (and (is-impossible (first-only intension))
             (is-possible   (both intension))
             (is-possible   (neither intension)))
    intension))

(defmethod is-enable ((intension c-intension))
  "If intension represents an enabling condition, rtn it, else nil"
  (when (and (is-possible (first-only intension))
             (is-possible (both intension))
             (is-possible (neither intension)))
    intension))

(defmethod is-prevent ((intension c-intension))
  "If intension represents an prevention condition, rtn it, else nil"
  (when (and (is-possible (first-only intension))
             (is-possible (second-only intension))
             (is-possible (neither intension)))
    intension))

(defun reverse-modal (x)
  "Returns reversed modal status, i.e., impossible <-> possible"
  (cond
   ((null x) nil)
   ((or (equal x 'possible)
            (equal x 'fact)
            (equal x 'initial))
    'impossible)
   ((equal x 'impossible)
    'possible)))

; ---------------------------------------------------------------------------------
; Section 1.11: Tracer classe and functions
; ---------------------------------------------------------------------------------

(defclass tracer ()
  ((enabled :accessor enabled :initarg :e   :initform nil)
   (steps   :accessor steps   :initarg :s   :initform -1)
   (verbose :accessor verbose :initarg :v   :initform nil)
   (runtime :accessor runtime :initarg :r   :initform (get-internal-run-time))
   (response      :accessor response :initarg :r :initform nil)
   (initial-model :accessor initial-model :initarg :im :initform nil)
   (final-model :accessor final-model :initarg :fm :initform nil)
   (trace         :accessor trace-output   :initarg :tr  :initform nil))
  (:documentation "Class for tracer logging"))

(defparameter *tracer* (make-instance 'tracer)
  "Parameter that holds any tracing information during the course of any 'run' of
   an inference.")

(defun compute-runtime ()
  "Converts runtime to process cycle"
  (- (get-internal-run-time) (runtime *tracer*)))

(defun trace-header ()
  "Adds header to system trace on initial output of tracer and on every tracer reset"
  (case (steps *tracer*)
    (-1
     (format t "---- --------- ----------------------------------------------------------------- ------- ~%")
     (format t "Step System    Description                                                       Runtime  ~%")
     (format t "---- --------- ----------------------------------------------------------------- ------- ~%")
     (setf (runtime *tracer*) (get-internal-run-time))
     (incf (steps *tracer*))
     (format t "~4@<~A~> ~9@<~A~> ~65@<~A~> ~7@<~A~>~%"
             (steps *tracer*) "--" "Initialized trace" (compute-runtime)))
    (0
     (format t "---- --------- ----------------------------------------------------------------- ------- ~%")
     (setf (runtime *tracer*) (get-internal-run-time))
     (format t "~4@<~A~> ~9@<~A~> ~65@<~A~> ~7@<~A~>~%"
             (steps *tracer*) "--" "Reset trace" (compute-runtime)))))

(defun tracer (system description &key (model nil))
  "Adds tracing line to system trace"
  (let ((model (cond
                ((null model) nil)
                ((listp model) (mapcar #'copy-class-instance model))
                (model         (copy-class-instance model)))))
    (when (enabled *tracer*)
        (when (member system (list "System 1" "System 2" "Control" "Language") :test #'string-equal)
          (incf (steps *tracer*)))
        (push (list (if (string-equal system "") "" (steps *tracer*)) system description (compute-runtime) model)
              (trace-output *tracer*))
      (when (verbose *tracer*)
        (trace-header)
        (format t "~4@<~A~> ~9@<~A~> ~65@<~A~> ~7@<~A~>~%" (if (string-equal system "") "" (steps *tracer*))
                system description (compute-runtime))))))

(defun trc (system description &key (m nil))
  "Abbreviation wrapper fn for tracer"
  (tracer system description :model m))

(defun initialize-tracer (&key (enabled t) (steps -2) (verbose nil) (runtime (get-internal-run-time)))
  "Initializes tracer to defaults based on parameters"
  (setf *tracer* (make-instance 'tracer :e enabled :s steps :v verbose :r runtime)))

(defun enable-tracer (&key (verbose nil))
  "Enables tracer and sets trace verbosity"
  (if *tracer*
      (progn
        (setf (enabled *tracer*) t)
        (setf (verbose *tracer*) verbose))
    (initialize-tracer :v verbose)))

(defun disable-tracer ()
  "Disables tracer"
  (setf (enabled *tracer*) nil))

(defun reset-tracer ()
  "Resets tracer"
  (unless (< (steps *tracer*) 0)
    (setf (steps *tracer*) -1))
  (setf (trace-output *tracer*) nil)
  (setf (response *tracer*) nil)
  (setf (initial-model *tracer*) nil)
  (setf (final-model *tracer*) nil)
  (setf (runtime *tracer*) (get-internal-run-time))
  *tracer*)

(defun trace-model (model)
  "Outputs model in tracer format"
  (let (lines
        (output (make-array 0
                            :element-type 'character 
                            :adjustable t 
                            :fill-pointer 0)))
    (print-model model :template nil :output output)
    (setf lines (rest (split-sequence (format nil "~%") output)))
    (trc "Printer" (format nil "Printing model ~A" model))
    (dolist (line lines)
      (trc "" line))))

; ---------------------------------------------------------------------------------
; Section 1.12: Command-line arguments
; ---------------------------------------------------------------------------------

(defun get-command-line-arguments ()
  (if (find-package :cl-launch)
    (symbol-value (find-symbol (string :*arguments*) :cl-launch))
    (progn
      #+sbcl (cdr sb-ext:*posix-argv*)
      #+clozure (cdr (ccl::command-line-arguments))
      #+gcl (cdr si:*command-args*)
      #+ecl (loop for i from 1 below (si:argc) collect (si:argv i))
      #+cmu (cdr extensions:*command-line-strings*)
      #+allegro (cdr (sys:command-line-arguments))
      #+lispworks (cdr sys:*line-arguments-list*)
      #+clisp ext:*args*
      #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
      (error "get-command-line-arguments not supported for your implementation"))))

(defun run-system-command (command args)
  #+ccl (run-program command args :output *standard-output*))

(defun find-argument (arg)
(let ((argument (member arg (get-command-line-arguments) :test #'string-equal)))
   (when argument (if (> (length argument ) 1) (second argument) argument))))

; ---------------------------------------------------------------------------------
; Section 1.13: JSON utility functions
; ---------------------------------------------------------------------------------

(defun read-from-json (input)
  (let ((in (open (file-path input) :if-does-not-exist nil))
        (json " "))
    (when in
      (loop for line = (read-line in nil)
            while line do 
            (progn
              (setf json (concatenate 'string json (format nil "~A~%" line)))))
      (close in))
    (jsown:parse json)))

(defun write-to-json (json output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (format out (jsown:to-json json))))

; ---------------------------------------------------------------------------------
; Section 1.14: mReasoner REPL
; ---------------------------------------------------------------------------------

(defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (simple-condition 
         (ERR) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition 
         (ERR) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))

(defun mreasoner-repl ()
  (do ((+eof+ (gensym))
       (hist 1 (1+ hist)))
      (nil)
    (format t "~%~A [~D]> " "mReasoner" hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit)(exit)(continue)) :test (function equal)))
       (return-from mreasoner-repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /))))

; ---------------------------------------------------------------------------------
; Section 1.15: Deliver standalone executable for Syllogism Challenge
; ---------------------------------------------------------------------------------

(defun print-syllogism-challenge-header ()
	(format t "---------------------------------------------------------------~%~
	           mReasoner version ~A (~A)                                     ~%~
                   Binary for Ragni and colleagues' Syllogism Challenge          ~%~
	           Copyright (C) 2017 by S. Khemlani and P.N. Johnson-Laird      ~%~
                                                                                 ~%~
                   Type '(syllogism-challenge \"AA1\")' to run the syllogism     ~%~
                   challenge using the default parameter settings. To modify the ~%~
                   parameter settings, type '(manual)'. Type '(quit)' to quit    ~%~
                   mReasoner.                                                    ~%~
                                                                                 ~%~
                   This software is licensed under a Creative Commons Attribution-~%~
                   NonCommercial-ShareAlike 4.0 International License. For more  ~%~
                   information on this license, please visit:                    ~%~
                                                                                 ~%~
                          http://creativecommons.org/licenses/by-nc-sa/4.0/      ~%~
		   ---------------------------------------------------------------~%"
                *version*
                "2017-09-06"))

(defun get-syllogistic-premises (syllogism)
  (let* ((syllogisms '(("AA1" (list Aab Abc)) ("AA2" (list Aba Acb)) ("AA3" (list Aab Acb)) ("AA4" (list Aba Abc))
                       ("AI1" (list Aab Ibc)) ("AI2" (list Aba Icb)) ("AI3" (list Aab Icb)) ("AI4" (list Aba Ibc))
                       ("AE1" (list Aab Ebc)) ("AE2" (list Aba Ecb)) ("AE3" (list Aab Ecb)) ("AE4" (list Aba Ebc))
                       ("AO1" (list Aab Obc)) ("AO2" (list Aba Ocb)) ("AO3" (list Aab Ocb)) ("AO4" (list Aba Obc))
                       ("IA1" (list Iab Abc)) ("IA2" (list Iba Acb)) ("IA3" (list Iab Acb)) ("IA4" (list Iba Abc))
                       ("II1" (list Iab Ibc)) ("II2" (list Iba Icb)) ("II3" (list Iab Icb)) ("II4" (list Iba Ibc))
                       ("IE1" (list Iab Ebc)) ("IE2" (list Iba Ecb)) ("IE3" (list Iab Ecb)) ("IE4" (list Iba Ebc))
                       ("IO1" (list Iab Obc)) ("IO2" (list Iba Ocb)) ("IO3" (list Iab Ocb)) ("IO4" (list Iba Obc))
                       ("EA1" (list Eab Abc)) ("EA2" (list Eba Acb)) ("EA3" (list Eab Acb)) ("EA4" (list Eba Abc))
                       ("EI1" (list Eab Ibc)) ("EI2" (list Eba Icb)) ("EI3" (list Eab Icb)) ("EI4" (list Eba Ibc))
                       ("EE1" (list Eab Ebc)) ("EE2" (list Eba Ecb)) ("EE3" (list Eab Ecb)) ("EE4" (list Eba Ebc))
                       ("EO1" (list Eab Obc)) ("EO2" (list Eba Ocb)) ("EO3" (list Eab Ocb)) ("EO4" (list Eba Obc))
                       ("OA1" (list Oab Abc)) ("OA2" (list Oba Acb)) ("OA3" (list Oab Acb)) ("OA4" (list Oba Abc))
                       ("OI1" (list Oab Ibc)) ("OI2" (list Oba Icb)) ("OI3" (list Oab Icb)) ("OI4" (list Oba Ibc))
                       ("OE1" (list Oab Ebc)) ("OE2" (list Oba Ecb)) ("OE3" (list Oab Ecb)) ("OE4" (list Oba Ebc))
                       ("OO1" (list Oab Obc)) ("OO2" (list Oba Ocb)) ("OO3" (list Oab Ocb)) ("OO4" (list Oba Obc))))
         (syllogistic-premises (find syllogism syllogisms :test #'(lambda (x y) (equal x (first y))))))
    (if syllogistic-premises
      (eval (second syllogistic-premises))
      (error "Improperly formatted syllogism code"))))

#+ccl (defun syllogism-challenge (syllogism &key (directory "~/") (lambda 3.0) (epsilon 0.45) (sigma 0.5) (omega 1.0) (N 1))
         (initialize-tracer :verbose t)
              (parameter-search
               (list (list (get-syllogistic-premises syllogism)
                           "What follows?" '() T lambda epsilon sigma omega syllogism NIL NIL T))
               :directory directory :N N :verbose nil :parameters (list (list lambda epsilon sigma omega))))

#+ccl (defun syllogism-challenge-main ()
	(if (> (length (get-command-line-arguments)) 0)
            (let ((syllogism (find-argument "-syllogism"))
                  (directory (find-argument "-directory"))
                  (lambda (read-from-string (find-argument "-lambda")))
                  (epsilon (read-from-string (find-argument "-epsilon")))
                  (sigma (read-from-string (find-argument "-sigma")))
                  (omega (read-from-string (find-argument "-omega")))
                  (N (read-from-string (find-argument "-N"))))
             ;(initialize-tracer :verbose nil)
              (format t "~74@<Initializing mReasoner 0.9.6245...~>[DONE]~%")
              (format t "~74@<~A~>[DONE]~%"
                     (format nil "Setting parameters as follows: ~A = ~A, ~A = ~A, ~A = ~A, ~A = ~A" 
                             #\u+03bb lambda #\u+025b epsilon #\u+03c3 sigma #\u+03c9 omega))
             (format t "~74@<~A~>"
                     (format nil "Generating synthetic data for ~A participants..." N))
             (parameter-search
               (list (list (get-syllogistic-premises syllogism)
                           "What follows?" '() T lambda epsilon sigma omega syllogism  NIL NIL T))
               :directory directory :N N :verbose nil :parameters (list (list lambda epsilon sigma omega)))
             (format t "[DONE]~%")
             (format t "~74@<~A~>[DONE]~%"
                     (format nil "Writing data to ~A ..." directory)))
          (progn
            (print-syllogism-challenge-header)
            (mreasoner-repl))))

; ---------------------------------------------------------------------------------
; Section 1.16: Deliver standalone executable for CMRAS system (CCL)
; ---------------------------------------------------------------------------------

(defun print-cmras-header ()
  (format t "---------------------------------------------------------------~%~
	     mReasoner version ~A (~A)                                      ~%~
             CMRAS system for spatial reasoning                             ~%~
	     Copyright (C) 2018 by S. Khemlani and P.N. Johnson-Laird       ~%~
                                                                            ~%~
             This software is licensed under a Creative Commons Attribution-~%~
             NonCommercial-ShareAlike 4.0 International License. For more   ~%~
             information on this license, please visit:                     ~%~
                                                                            ~%~
                    http://creativecommons.org/licenses/by-nc-sa/4.0/       ~%~
             ---------------------------------------------------------------~%"
                *version* "2018-10-05"))

(defmethod to-json-object (instance)
  (let* ((class       (class-of instance))
         (slots       (class-slots class))
         json)
    (loop for slot in slots do
          (let ((slot-name (symbol-name (slot-definition-name slot)))
                (slot-val (slot-value instance (slot-definition-name slot))))
            #|(format t "~A ~A~%" slot-name slot-val)|#
            (cond
             ((or (equal t slot-val) (null slot-val) (numberp slot-val) (stringp slot-val))
              (setf json (append json (list (cons slot-name (jsown:to-json slot-val))))))
             ((symbolp slot-val)
              (setf json (append json (list (cons slot-name (symbol-name slot-val))))))
             ((or (listp slot-val) (typep slot-val 'intension) (typep slot-val 'model))
              (setf json (append json (list (cons slot-name (to-json-object slot-val))))))
             )))
    (setf json (append (list :obj (cons "CLASS" (format nil "~A" (type-of instance))))
                       json))
    json))

(defmethod to-json-object ((list list))
  (mapcar #'to-json-object list))

(defmethod to-json-object ((number number))
  (jsown:to-json number))

(defmethod to-json-object ((ratio ratio))
  (jsown:to-json ratio))

(defmethod to-json-object ((float float))
  (jsown:to-json float))

(defmethod to-json-object ((string string))
  (jsown:to-json string))

(defmethod to-json-object ((symbol symbol))
  (symbol-name symbol))

(defun export-as-json (object pathname)
  (with-open-file (output pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format output (jsown:to-json* (to-json-object object)))))

(defun get-json-premises (json-object)
  (let ((obj (rest json-object)))
    (mapcar #'rest (rest (nth (position "premises" obj :test #'equals :key #'first) obj)))))

(defun parse-premise-to-intension (json-premise)
  (let* ((obj1 (read-from-string (rest (assoc "firstObject" json-premise :test 'equal))))
         (rel  (convert-to-relation (rest (assoc "relation" json-premise :test 'equal))))
         (obj2 (read-from-string (rest (assoc "secondObject" json-premise :test 'equal)))))
    (parse (flatten `(,obj1 ,rel ,obj2)))))

(defun parse-premises-to-intensions (json-premises)
  (mapcar #'parse-premise-to-intension json-premises))

(defun parse-observation-to-model (json-observation)
(let* ((obj    (rest json-observation))
       (things (rest (nth (position "observations" obj :test #'equals :key #'first) obj)))
       (things (mapcar #'(lambda (x)
                           (mapcar #'(lambda (y)
                                       (cond
                                        ((stringp y)
                                         (list (read-from-string y)))
                                        ((listp y)
                                         (mapcar #'read-from-string y)))) x)) things))
       (things (mapcar #'(lambda (x) (substitute nil '(-) x :test #'equals)) things))
       (things (remove-if #'(lambda (x) (every #'null x)) things))
       (things (transpose-list (reverse things)))
       (things (remove-if #'(lambda (x) (every #'null x)) things)))
  (make-instance 'sp-model :things things :fn nil :dims '((:X :LEFT-RIGHT) (:Y :BELOW-ABOVE)))))
	
(defun convert-to-relation (relation)
  (cond
   ((string= relation "leftOf")    '(is to the left of))
   ((string= relation "rightOf")   '(is to the right of))
   ((string= relation "behind")    '(is behind))
   ((string= relation "inFrontOf") '(is in front of))
   ((string= relation "above")     '(is above))
   ((string= relation "below")     '(is below))))

(defun test-proximal-1d-relations (model object1 object2)
  (let* ((left    (parse `(,object1 is directly to the left of ,object2)))
         (right   (parse `(,object1 is directly to the right of ,object2)))
         (above   (parse `(,object1 is directly above ,object2)))
         (below   (parse `(,object1 is directly below ,object2)))
         (front   (parse `(,object1 is directly in front of ,object2)))
         (behind  (parse `(,object1 is directly behind ,object2)))
         conclusions-list)
    (mapcar #'(lambda (x) (when (validate x (list model)) (push x conclusions-list)))
            (list left right above below front behind))
    conclusions-list))

(defun test-1d-relations (model object1 object2)
  (let* ((same    (parse `(,object1 is in the same place as ,object2)))
         (left    (parse `(,object1 is to the left of ,object2)))
         (right   (parse `(,object1 is to the right of ,object2)))
         (above   (parse `(,object1 is above ,object2)))
         (below   (parse `(,object1 is below ,object2)))
         (front   (parse `(,object1 is in front of ,object2)))
         (behind  (parse `(,object1 is behind ,object2)))
         conclusions-list)
    (mapcar #'(lambda (x) (when (validate x (list model)) (push x conclusions-list)))
            (list same left right above below front behind))
    conclusions-list))

(defun find-all-relations (model)
  (let* ((things       (flatten (things model)))
         (combinations (combinations-with-replacement things 2))
         (combinations (remove-if #'(lambda (x) (equals (first x) (second x))) combinations))
         (relations    (mapcar #'(lambda (x) (funcall #'test-1d-relations model (first x) (second x))) combinations))
         (relations    (flatten relations)))
    relations))

(defun filter-relations (relations)
  (let ((same-relations (remove-if-not #'is-same relations)))
    (dolist (same-rel same-relations)
      (dolist (rel (mapcar #'copy-class-instance relations))
        (when (equals (first-argument rel) (first-argument same-rel))
          (let* ((rel2 (copy-class-instance rel)))
            (setf (first-argument rel2) (second-argument same-rel))
            (when (member rel2 relations :test #'equals)
              (setf relations (remove-if #'(lambda (x) (equals x rel2)) relations))))))))
  relations)

(defmethod to-english ((intension sp-intension))
  (case (relation intension)
    (same   (format nil "~A is in the same place as ~A" (first-argument intension) (second-argument intension)))
    (left   (format nil "~A is to the left of ~A"       (first-argument intension) (second-argument intension)))
    (right  (format nil "~A is to the right of ~A"      (first-argument intension) (second-argument intension)))
    (above  (format nil "~A is above ~A"                (first-argument intension) (second-argument intension)))
    (below  (format nil "~A is below ~A"                (first-argument intension) (second-argument intension)))
    (front  (format nil "~A is in front of ~A"          (first-argument intension) (second-argument intension)))
    (behind (format nil "~A is behind ~A"               (first-argument intension) (second-argument intension)))
    (otherwise (error "Can't identify relation."))))

(defun process-task (json model output)
  (let* ((type (if (footnote model) :premises :observations))
         (obj  (rest json))
         (task (position "task" obj :test #'equals :key #'first))
         (task (when task (read-from-string (rest (nth task obj)))))
         (args (position "arguments" obj :test #'equals :key #'first))
         (args (when args (rest (nth args obj))))
         description validation validation-output)
    (case type
      (:premises     nil)
      (:observations (when (not task) (setf task 'describe))))
    (case task
      (describe
       (format t "")
       (setf description
             (format nil "~{~#[None~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;,~%~]~}~]~:}"
                     (mapcar #'(lambda (x) (to-english x)) (filter-relations (find-all-relations model)))))
       (format t "Description~%-----------~%~A.~%~%" description)
       (setf (description output) description))
      (validate
       (case type
         (:premises     nil)
         (:observations
          (format t "~45@<Queried relation~>~15<Holds in model~>~%~
                     ------------------------------------------------------------~%")
          (dolist (relation args)
            (handler-case
                (setf validation-output (if (validate (parse relation) (list model)) "[YES]" " [NO]"))
              (parser-error () (setf validation-output "[ERROR]")))
            (format t "~45@<~A.~>~15<~A~>~%" relation validation-output)
            (push (list relation validation-output) validation))
          (format t "~%")
          (setf (validation output) (reverse validation))))))
    output))

(defun process-premises-or-observations (json &key (print-model t))
  (let ((output (make-instance 'json-output))
        (model-string (make-array '(0) :element-type 'base-char
                                  :fill-pointer 0 :adjustable t))
        model)
    (handler-case
        (if (equals (first json) :obj)
            (progn
              (cond
               ((some #'(lambda (x) (equals "premises" x)) (mapcar #'first (rest json)))
                (parse-premises-to-intensions json)
                (setf model (first (interpret (parse-premises-to-intensions (get-json-premises json))))))
               ((some #'(lambda (x) (equals "observations" x)) (mapcar #'first (rest json)))
                (setf model (parse-observation-to-model json))))
              (when print-model (print-model model) (format t "~%"))
              (setf (model output) model)
              (with-output-to-string (s model-string)
                (print-model model :output s))
              (setf (model-string output) (split-sequence (format nil "~%") model-string))
              (process-task json model output))
          (progn
            (format t "Error: improperly formatted JSON file.~%")
            (setf (error-log output) "Improperly formatted JSON file.")))
      (consistency-error ()
        (format t "Inconsistent relations; unable to build model.~%")
        (setf (error-log output) "Inconsistent relations; unable to build model.")))))

(defun process-json-file (input-file output-file)
  (let* ((input-json (jsown:parse (read-file-to-string input-file)))
         (output-json (process-premises-or-observations input-json)))
    (if (and output-file output-json)
        (write-to-json (to-json-object output-json) output-file))))

(defun process-input (&key (input-file nil) (output-file nil))
  (initialize-tracer)
  (let* ((input-file  (if input-file input-file (find-argument "--input")))
         (output-file (if output-file output-file (find-argument "--output"))))
    (if input-file
        (process-json-file input-file output-file)
      (progn
        (print-cmras-header)
        (mreasoner-repl)))))
