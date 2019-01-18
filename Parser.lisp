; ---------------------------------------------------------------------------------
; Part 4: Parser
; ---------------------------------------------------------------------------------

#|  
A set of functions that use the grammar and lexicon to parse input and call the
compositional semantics to create models.  The parser is based on Code from Paradigms 
of AI Programming Copyright (c) 1991 Peter Norvig.

Section 4.1: Grammar and lexicon
Section 4.2: Parser
Section 4.3: Compositional semantics
Section 4.4: Memoization and parsing efficiency
Section 4.5: Inflectional morphology
Section 4.6: Built-in intensions
|#

; ---------------------------------------------------------------------------------
; Section 4.1: Grammar and lexicon
; ---------------------------------------------------------------------------------

(defparameter *grammar*
  '(; Atomic sentences and connectives  ---------------------------------------
    (S             -> (Var)                               sem-aff-var)
    (S             -> (Neg Var)                           sem-neg-var)
    (S             -> (Punct S Conn S)                    sem-cscs)
    (S             -> (S Conn S)                          sem-scs)
    (S             -> (S Conn-ExDisj Else-Adv S)          sem-sxors)
    (S             -> (Det-Neither S Conn-Nor S)          sem-nor)
    (S             -> (Punct S C-Antec S)                 sem-cscs)
    (S             -> (S C-Antec S)                       sem-scs)
    (S             -> (C-Antec S C-Co S)                  sem-cond)
    (S             -> (It-Pro S-Cop Neg Def-Art Case-N
                              Det-That S)                 sem-neg)
    ; Monadic sentences  ------------------------------------------------------
    (S             -> (Quant-NP Pred-VP)                  sem-monadic)
    (S             -> (Name S-Cop Indef-Art Noun)         sem-set-member)
    (S             -> (Name S-Cop Neg Indef-Art Noun)     sem-set-nonmember)
    (S             -> (Name S-Cop Adj)                    sem-set-member-adj)
    (S             -> (Name S-Cop Neg Adj)                sem-set-nonmember-adj)
    (Pred-VP       -> (P-Cop Nouns)                       sem-affirmative-pred)
    (Pred-VP       -> (P-Cop Predet-Both
                             Nouns Conn Nouns)            sem-affirmative-pred-conn)
    (Pred-VP       -> (P-Cop Predet-Either
                             Nouns Conn Nouns)            sem-affirmative-pred-conn)
    (Pred-VP       -> (P-Cop Neg Nouns)                   sem-negative-pred)
    ; Temporal sentences  -----------------------------------------------------
    (S             -> (Var Temporal-VP)                   sem-temporal)
    (S             -> (Var Temporal-PP)                   sem-temporal)
    (Temporal-VP   -> (Temp-Cop Temp-Rel Var)             sem-temporal-relation)
    (Temporal-PP   -> (Temp-Cop Temp-Prep Var)            sem-temporal-preposition)
    (Temporal-PP   -> (Temp-Verb Temp-Prep Var)           sem-temporal-preposition)
    (Temporal-PP   -> (Temp-Verb Temp-Rel Var)            sem-temporal-preposition)
    ; Spatial sentences and relations  ----------------------------------------
    (S             -> (Var Spatial-VP)                    sem-spatial)
    (Spatial-VP    -> (S-Cop Spatial-PP)                  sem-spatial-vp)
    (Spatial-VP    -> (S-Cop Adv-Directly Spatial-PP)     sem-spatial-adv-vp)
    (Spatial-PP    -> (To-Prep Def-Art Spat-Horiz-Rel
                               Of-Prep Var)               sem-spatial-horizontal)
    (Spatial-PP    -> (Spat-Vert-Rel Var)                 sem-spatial-vertical)
    (Spatial-PP    -> (In-Prep Spat-Depth-Rel Of-Prep
                               Var)                       sem-spatial-depth)
    (Spatial-PP    -> (In-Prep Spat-BW-Rel Var Conn Var)  sem-spatial-between)
    (Spatial-PP    -> (In-Prep Def-Art Same-Rel
                               Spat-Place-N Spat-Compare
                               Var)                       sem-spatial-same)
    (Spatial-PP    -> (In-Prep Indef-Art Diff-Rel
                               Spat-Place-N Spat-Compare
                               Var)                       sem-spatial-different)
    ; Causal sentences  -------------------------------------------------------
    (S             -> (S Causal-Verb S)                   sem-causal)
    (S             -> (Punct S Causal-Verb S)             sem-ccausal)
    ; Noun phrases (quantificational, temporal) -------------------------------
    (Quant-NP      -> (Det Nouns)                         sem-quant)
    (Quant-NP      -> (Det-Scalar Comp-Than Num Nouns)    sem-scalar-quant)
    (Quant-NP      -> (Adv-Exactly Num Nouns)             sem-numerical-quant)

    ; Verb phrases (predicates, temporal, causal) -----------------------------


    ; Terminals and particles -------------------------------------------------
    (Conn           -> and                                 and)
    (Conn           -> or                                  ori)
    (Conn           -> ore                                 ore)
    (Conn-ExDisj    -> or                                  or)
    (Conn-Nor       -> nor                                 nor)
    (Else-Adv       -> else                                else)
    (C-Antec        -> if                                  if)
    (C-Antec        -> iff                                 iff)
    (C-Co           -> then                                then)
    (Neg            -> not                                 not)
    (Punct          -> comma                               comma)
    (Prob           -> probability                         probability)
    (P-Cop          -> are                                 are)
    (S-Cop          -> is                                  is)
    (Comp-Than      -> than                                than)
    (Indef-Art      -> an                                  an)
    (Indef-Art      -> a                                   a)
    (Def-Art        -> the                                 the)
    (Det-That       -> that                                that)
    (Det-Neither    -> neither                             neither)
    (Predet-Both    -> both                                both)
    (Predet-Either  -> either                              either)
    (It-Pro         -> it                                  it)
    (Case-N         -> case                                case)
    (Adv-Directly   -> directly                            directly)
    (Spat-Horiz-Rel -> right                               +)
    (Spat-Horiz-Rel -> left                                -)
    (Spat-Vert-Rel  -> above                               +)
    (Spat-Vert-Rel  -> below                               -)
    (Spat-Depth-Rel -> front                               +)
    (Spat-Vert-Rel  -> behind                              (-))
    (Spat-BW-Rel    -> between                             between)
    (Same-Rel       -> same                                same)
    (Diff-Rel       -> different                           different)
    (Spat-Place-N   -> place                               place)
    (Spat-Compare   -> as                                  as)
    (To-Prep        -> to                                  to)
    (Of-Prep        -> of                                  of)
    (On-Prep        -> on                                  on)
    (In-Prep        -> in                                  in)
    (Temp-Cop       -> happened                            (t t))
    (Temp-Verb      -> started                             (t nil))
    (Temp-Verb      -> ended                               (nil t))
    (Temp-Prep      -> at                                  =)
    (Temp-Rel       -> before                              <)
    (Temp-Rel       -> after                               >)
    (Temp-Rel       -> while                               include)
    (Temp-Rel       -> during                              properly-include)
    (Causal-Verb    -> causes                              causes)
    (Causal-Verb    -> enables                             enables)
    (Causal-Verb    -> prevents                            prevents)

    (Det         -> most    (make-instance 'q-intension
                                           :card '((? 4) (>= 2))
                                           :np   '(? 3)
                                           :bnd  '((< cardinality) (> (* .5 cardinality)))
                                           :pol  t
                                           :fn   t :subj nil :rel nil))
    (Det         -> few     (make-instance 'q-intension
                                           :card '((? 4) (>= 2))
                                           :np   '(? 1)
                                           :bnd  '((< (* .5 cardinality)) (> 0))
                                           :pol  t
                                           :fn   t :subj nil :rel nil))
    (Adv-Exactly -> exactly (make-instance 'q-intension
                                           :card '((? (+ N 1)) (>= N))
                                           :np   '(= N)
                                           :bnd  '((= N))
                                           :pol  t
                                           :fn   t :subj nil :rel nil))
    (Det         -> all     (make-instance 'q-intension
                                           :card '((? 4) (>= 1))
                                           :np   '(? 4)
                                           :bnd  '((= cardinality))
                                           :pol  t
                                           :fn   t :subj nil :rel nil))
    (Det         -> some    (make-instance 'q-intension
                                           :card '((? 4) (>= 1))
                                           :np   '(? 2)
                                           :bnd  '((<= cardinality)(> 0))
                                           :pol  t
                                           :fn   nil :subj nil :rel nil))
    (Det         -> no      (make-instance 'q-intension
                                           :card '((? 4) (>= 1))
                                           :np   '(? 4)
                                           :bnd  '((= cardinality))
                                           :pol  nil
                                           :fn   t :subj nil :rel nil))
    (Det-Scalar  -> more    (make-instance 'q-intension
                                           :card '((? (+ N 2)) (>= (+ N 1)))
                                           :np   '(? (+ N 1))
                                           :bnd  '((< cardinality)(> N))
                                           :pol  t
                                           :fn   nil :subj nil :rel nil))
    (Det-Scalar  -> less    (make-instance 'q-intension
                                           :card '((? (+ N 0)) (> N 1))
                                           :np   '(? (- N 1))
                                           :bnd  '((< N) (> 0))
                                           :pol  t
                                           :fn   nil :subj nil :rel nil))))

(defparameter *open-categories* '(Nouns Noun Var Name Num Adj)
  "Categories to consider for unknown words")

(defparameter *plural-words*
  '(
    (artists artist)
    (beekeepers beekeeper)
    (chemists chemist)
    (high-tech-companies high-tech-company)
    (innovators innovator)
    (well-managed-firms well-managed-firm)
    (US-companies US-company)))

; ---------------------------------------------------------------------------------
; Section 4.2: Parser
; ---------------------------------------------------------------------------------

(defstruct (rule (:type list))
  lhs -> rhs sem)

(defstruct (parse) "A parse tree and a remainder." tree rem)

(defstruct (tree (:type list) (:include rule) (:copier nil)
                 (:constructor new-tree (lhs sem rhs))))

(defun parse-lhs (parse) (tree-lhs (parse-tree parse)))

(defun lexical-rules (word)
  "Return a list of rules with word on the right-hand side"
  (let* ((plurals (find-all word *plural-words* :key #'first :test #'equalp))
         (words word)
         (word (if plurals (second (first plurals)) word)))
    (append (find-all word *grammar* :key #'rule-rhs :test #'equal)
            (mapcar #'(lambda (cat) `(,cat -> ,words ,word)) *open-categories*))))

(defun rules-starting-with (cat)
  "Return a list of rules where cat starts the rhs, e.g.,
  (rules-starting-with 'Quant-NP) => ((S -> (QUANT-NP PRED-VP) SEM-MONADIC))"
  (find-all cat *grammar*
            :key #'(lambda (rule) (first-or-nil (rule-rhs rule)))))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil"
  (if (consp x) (first x) nil))

(defun parser (words)
  "Return all complete parses of a list of words"
  (clear-memoize 'parse-words)
  (mapcar #'parse-tree (complete-parses (parse-words words))))

(defun complete-parses (parses)
  "Those parses that are complete (have no remainder)."
  (remove-if-not #'null parses :key #'parse-rem))

(defun parse-words (words)
  "Bottom-up parse, returning all parses of any prefix of words.
   This version has semantics."
  (unless (null words)
    (mapcan #'(lambda (rule)
                (extend-parse (rule-lhs rule) (rule-sem rule)
                              (list (first words)) (rest words) nil))
            (lexical-rules (first words)))))

(defun extend-parse (lhs sem rhs rem needed)
  "Look for the categories needed to complete the parse."
  (if (null needed)
      ;; If nothing needed, return parse and upward extensions
      (let ((parse (make-parse :tree (new-tree lhs sem rhs) :rem rem)))
        (unless (null (apply-semantics (parse-tree parse)))
          (cons parse
                (mapcan
                 #'(lambda (rule)
                     (extend-parse (rule-lhs rule) (rule-sem rule)
                                   (list (parse-tree parse)) rem
                                   (rest (rule-rhs rule))))
                 (rules-starting-with lhs)))))
    ;; otherwise try to extend rightward
    (mapcan
     #'(lambda (p)
         (if (eq (parse-lhs p) (first needed))
             (extend-parse lhs sem (append1 rhs (parse-tree p))
                           (parse-rem p) (rest needed))))
     (parse-words rem))))

(defun apply-semantics (tree)
  "For terminal nodes, just fetch the semantics.
   Otherwise, apply the sem function to its constituents."
  (if (terminal-tree-p tree)
      (tree-sem tree)
    (setf (tree-sem tree)
          (apply (tree-sem tree)
                 (mapcar #'tree-sem (tree-rhs tree))))))

(defun terminal-tree-p (tree)
  "Does this tree have a single word on the rhs?"
  (and (equal 1 (length (tree-rhs tree)))
       (atom (first (tree-rhs tree)))))

(defmethod parse ((words string))
  "When parse is fed a string, it splits the string up, converts it to a list,
   and parses the list"
  #+lispworks (setf words (split-sequence " " words))
  #+ccl (setf words (split-sequence #\Space words))
  (parse (mapcar #'read-from-string words)))

(defmethod parse ((words list))
  "Return all possible meanings of a phrase. Throw away the syntactic part."
  (trc "Language" (format nil "Parsed string ~A" words))
  (let* ((parsetrees (parser words))
         (parses (remove-duplicates
                  (remove-if-not #'(lambda (x) (equal 's (first x))) parsetrees) :test #'equals)))
    (case (length parses)
          (0         (error 'parser-error :text "syntax error."))
          (1         (tree-sem (first parses)))
          (otherwise (first (mapcar 'tree-sem parses))))))
;          (otherwise (error "Syntactic ambiguity detected.")))))

(defun find-all	(item sequence &rest keyword-args
                      &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item, according,
   to the keywords. Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
           :test (complement test) keyword-args)))

(defun append1 (items item)
  "Add item to end of list of items"
  (append items (list item)))

; ---------------------------------------------------------------------------------
; Section 4.3: Compositional semantics
; ---------------------------------------------------------------------------------

(defun sem-aff-var (var)
  (make-instance 's-intension :first-clause var :second-clause nil
                        :first-only  'possible
                        :second-only nil
                        :both        nil
                        :neither     nil))

(defun sem-neg-var (neg var)
  (make-instance 's-intension :first-clause var :second-clause nil
                        :first-only  'impossible
                        :second-only nil
                        :both        nil
                        :neither     nil))

(defun sem-cscs (comma first conn second)
  (sem-scs first conn second))

(defun sem-sxors (first conn else second)
  (sem-scs first 'ore second))

(defun sem-nor (neither first nor second)
  (sem-scs first 'nor second))

(defun sem-scs (first conn second)
  (case conn
    (ori (make-instance 's-intension :first-clause first :second-clause second
                        :first-only  'initial
                        :second-only 'initial
                        :both        'initial
                        :neither     'impossible))
    (ore (make-instance 's-intension :first-clause first :second-clause second
                        :first-only  'initial
                        :second-only 'initial
                        :both        'impossible
                        :neither     'impossible))
    (and (make-instance 's-intension :first-clause first :second-clause second
                        :first-only  'impossible
                        :second-only 'impossible
                        :both        'initial
                        :neither     'impossible))
    (nor (make-instance 's-intension :first-clause first :second-clause second
                        :first-only  'impossible
                        :second-only 'impossible
                        :both        'impossible
                        :neither     'initial))
    (if  (make-instance 's-intension :first-clause second :second-clause first
                        :first-only  'impossible
                        :second-only 'possible
                        :both        'initial
                        :neither     'possible))
    (iff (make-instance 's-intension :first-clause second :second-clause first
                        :first-only  'impossible
                        :second-only 'impossible
                        :both        'initial
                        :neither     'initial))
))

(defun sem-cond (iffy ant then con)
  (case iffy
    (if  (make-instance 's-intension :first-clause ant :second-clause con
                        :first-only  'impossible
                        :second-only 'possible
                        :both        'initial
                        :neither     'possible))
    (iff (make-instance 's-intension :first-clause ant :second-clause con
                        :first-only  'impossible
                        :second-only 'impossible
                        :both        'initial
                        :neither     'initial))))

(defun sem-neg (&rest parse)
  (let ((int (first (last parse))))
    (negate int)))
  
(defmethod negate ((int intension))
  "Negates an intension by applying sentential negation, where an given
   intension is set as the first-clause of a new intension."
  (make-instance 's-intension
                 :first-clause int
                 :second-clause nil
                 :first-only  'impossible
                 :second-only nil
                 :both        nil
                 :neither     nil))

(defmethod negate ((intension s-intension))
  "Implements small scope principled described and validate in Khemlani, Orenes, & Johnson-Laird (2014)."
  (setf negated (copy-class-instance intension))
  (setf (both negated)        (reverse-modal (both intension))
        (first-only negated)  (reverse-modal (first-only intension))
        (second-only negated) (reverse-modal (second-only intension))
        (neither negated)     (reverse-modal (neither intension)))
  (cond ((is-affirmative-atom intension) (setf (first-only negated) 'impossible))
        ((is-negative-atom intension) (setf (first-only negated) 'possible))
        ((is-and intension) (setf (neither negated)     'initial))
        ((is-ori intension) (setf (neither negated)     'initial))
        ((is-ore intension) (setf (neither negated)     'initial))
        ((is-if  intension) (setf (first-only negated)  'initial))
        ((is-iff intension) (setf (first-only negated)  'initial
                                  (second-only negated) 'initial)))
  negated)

#|(defmethod negate ((int s-intension))
  "Negates an s-intension by swapping impossible states to possible
   and vice versa. Handles conjunctions specially to yield sentential
   negation instead of intensional negation, since there is no lexicalized
   version of 'nand' (not-and) or 'noth' (not-both); see Horn (1972)"
  (if (or (is-and int) (is-if int))
      (call-next-method)
    (let* ((negated-semantics (list (reverse-modal (first-only int)) (reverse-modal (second-only int))
                                    (reverse-modal (both int)) (reverse-modal (neither int))))
           (negated-semantics (if (= (length (all-positions 'possible negated-semantics)) 1)
                                  (substitute 'initial 'possible negated-semantics)
                                negated-semantics)))
      (make-instance 's-intension
                     :first-clause  (first-clause int)
                     :second-clause (second-clause int)
                     :first-only    (nth 0 negated-semantics)   
                     :second-only   (nth 1 negated-semantics)   
                     :both          (nth 2 negated-semantics)   
                     :neither       (nth 3 negated-semantics))))) |#

#|
Intensions contain five parameters. The role of each parameter is summarized here:

i.  Cardinality: the size of the overall set, A, which is the argument of the determiner in the 
             quantifer, e.g. "Some A". It has two sorts of values, illustrated here:
                (>= 4) means the cardinality is known to be >= 4, and other similar assertions, 
                       involving >, <, =, and so (= 4) means the cardinality is known to be 4.
                (?  4) means a cardinality of 4 has been arbitrarily assumed. 
             This system meets requirement 1. in our list above for the size of A.

ii.  Number: the number of members of A in the quantifier that meet the relational specification. 
             In most cases, this is an arbitrarily assigned number, such as (? 3). The (? 3) 
             constraint serves as a cue for building an initial model, but that initial model rests 
             on the arbitrary assumption that M indivs should be built, which is an assumption that 
             can be violated. 

             For some quantifiers, the number is non-arbitrary, e.g., "Exactly 3 As are Bs". This value is
             represented as (= 3), and it cannot be violated.

iii. Boundary: the range of values that the number paramter can take on. The value can specify an upper bound,
               a lower bound, or both. For example, for "Some A are B", the boundary is: 
               ((< cardinality) (> 0)) 
               Note that the upper bound in the above boundary, (< cardinality), uses the symbol 'cardinality
               to indicate that a corresponding model can have as many members that meet the relational
               specification as there are members in total.

               Let's break that down with the example above: "Some A are B" starts out with a cardinality of
               (? 3), and so the upper bound, (< cardinality), means that the system can represent up to 2 As
               that are Bs. Of course, the cardinality of (? 3) can be modifed to, say, (? 5), at which point
               the system can represent up to 4 As that are Bs.

             This system meets requirements 1. and 2. in our list above, i.e., we can tell whether a determiner
             is numerical or not by checking whether first.parameter rtns '?' or not.

iv.  Polarity (of the quantifier): t = affirmative, nil = negative

v.   Universality t = The quantifier is a universal one, such as 'all' or 'none',
             nil = the quantifier is an existential one, such as'some.  
             On Feb 22nd, SK and JL decided that the content of a footnote should always be the 
             intension of the assertion.  Hence the change in the interpretation of this parameter.
|#

(defun sem-monadic (quant&subj pred&obj)
  (let* ((quant (first quant&subj))
         (subj (second quant&subj))
         (pred (first pred&obj))
         (obj  (second pred&obj)))
    (setf (relation quant) pred)
    (setf (subject quant) subj)
    (setf (object quant) obj)
    (copy-class-instance quant)))

(defun sem-scalar-quant (quant than num subj)
  (declare (ignore than))
  (if (integerp num)
      (progn
        (setf quant (subst num 'n quant))
        (setf (second (first (second (fourth quant))))   (eval (second (first (second (fourth quant))))))
        (setf (second (second  (second (fourth quant)))) (eval (second (second (second (fourth quant))))))
        (setf (second (second (sixth quant)))            (eval (second (second (sixth quant)))))
        (list (eval quant) (list subj)))
    (error "Integer expected")))

(defun sem-numerical-quant (quant num subj)
  (if (integerp num)
      (progn
        (setf quant (subst num 'n quant))
        (setf (second (first (second (fourth quant))))   (eval (second (first (second (fourth quant))))))
        (list (eval quant) (list subj)))
    (error "Integer expected")))

(defun sem-quant (quant subj)
  (list (eval quant) (list subj)))

(defun sem-affirmative-pred (cop obj)
  (declare (ignore cop))
  (list 'include (list obj)))

(defun sem-affirmative-pred-conn (cop predet obj1 conn obj2)
  (declare (ignore cop) (ignore predet))
  (list 'include (list obj1 conn obj2)))

(defun sem-negative-pred (cop neg obj)
  (declare (ignore cop neg))
  (list 'not-include (list obj)))

(defun sem-set-member (name cop indef-art noun)
  (declare (ignore cop indef-art))
  (make-instance 'q-intension :card '((= 1)) :np '(= 1) :bnd '((= 1))
                 :pol t :fn t :subj (list name) :obj (list noun)
                 :rel 'include))

(defun sem-set-member-adj (name cop adj)
  (declare (ignore cop indef-art))
  (make-instance 'q-intension :card '((= 1)) :np '(= 1) :bnd '((= 1))
                 :pol t :fn t :subj (list name) :obj (list adj)
                 :rel 'include))

(defun sem-set-nonmember (name cop neg indef-art noun)
  (declare (ignore neg cop indef-art))
  (make-instance 'q-intension :card '((= 1)) :np '(= 1) :bnd '((= 1))
                 :pol t :fn t :subj (list name) :obj (list noun)
                 :rel 'not-include))

(defun sem-set-nonmember-adj (name cop neg adj)
  (declare (ignore neg cop indef-art))
  (make-instance 'q-intension :card '((= 1)) :np '(= 1) :bnd '((= 1))
                 :pol t :fn t :subj (list name) :obj (list adj)
                 :rel 'not-include))

(defun sem-temporal (subj int)
  (setf (subject int) subj)
  (when (precedence int)
    (if (or (equalp (precedence int) 'include)
            (equalp (precedence int) 'properly-include))
        (if (numberp (object int))
            (error "Integer not expected.")
          (setf (precedence int) (list (precedence int) (object int) (subject int))))
      (setf (precedence int) (list (precedence int) (subject int) (object int)))))
  int)

(defun sem-temporal-relation (happened relation obj)
  (make-instance 't-intension :obj obj :prec relation :start nil :end nil :ref nil :ut nil))

(defun sem-temporal-preposition (verb preposition obj)
  (cond
   ((equal preposition 'include)
    (error (format nil "'While' relation (include) cannot be interpreted for punctate events and time points")))
   ((numberp obj)
    (if (equal preposition 'properly-include)
        (error (format nil "'During' relation (properly-include) cannot be interpreted for punctate events and time points"))
      (let ((int (make-instance 't-intension :obj nil :prec nil :start nil :end nil :ref nil :ut nil)))
        (when (first verb) (setf (start-time int) (list preposition obj)))
        (when (second verb) (setf (end-time int) (list preposition obj)))
        int)))
   (t (let ((int (make-instance 't-intension :obj nil :prec nil :start nil :end nil :ref nil :ut nil)))
        (when (first verb) (setf (start-time int) (list preposition obj)))
        (when (second verb) (setf (end-time int) (list preposition obj)))
        int))))

(defun sem-causal (first relation second)
  (case relation
    (causes  (make-instance 'c-intension :first-clause first :second-clause second
                        :first-only  'impossible
                        :second-only 'possible
                        :both        'initial
                        :neither     'possible
                        :constraint   nil))
    (enables (make-instance 'c-intension :first-clause first :second-clause second
                        :first-only  'possible
                        :second-only 'impossible
                        :both        'initial
                        :neither     'possible
                        :constraint   nil))
    (prevents (make-instance 'c-intension :first-clause first :second-clause second
                        :first-only  'initial
                        :second-only 'possible
                        :both        'impossible
                        :neither     'possible
                        :constraint   nil))))

(defun sem-ccausal (comma first relation second)
  (sem-causal first relation second))

(defun sem-spatial (subject int)
  (setf (subject int) subject)
  (when (spatial-template int)
    (setf (spatial-template int)
          (mapcar #'(lambda (x) (substitute subject 'subject x :test #'equals)) (spatial-template int))))
  int)

(defun sem-spatial-vp (is int)
  (setf (spatial-distance int) :infinity)
  int)

(defun sem-spatial-adv-vp (is directly int)
  (if (member (spatial-relation int) '(:equal :not-equal))
      (setf (spatial-distance int) :infinity)
    (setf (spatial-distance int) 1))
  int)

(defun sem-spatial-horizontal (to the1 relation of obj)
  (make-instance 'sp-intension :obj obj :rel relation :dim :left-right :temp nil :dist nil))

(defun sem-spatial-vertical (relation obj)
  (if (listp relation)
      (make-instance 'sp-intension :obj obj :rel (first relation) :dim :behind-front :temp nil :dist nil)
  (make-instance 'sp-intension :obj obj :rel relation :dim :below-above :temp nil :dist nil)))

(defun sem-spatial-depth (in relation of obj)
  (make-instance 'sp-intension :obj obj :rel relation :dim :behind-front :temp nil :dist nil))

(defun sem-spatial-between (in between obj1 and obj2)
  (make-instance 'sp-intension :obj (list obj1 obj2) :rel nil :dim nil :dist nil :temp (list (list obj1 'subject obj2)
                                                                                             (list obj2 'subject obj1))))

(defun sem-spatial-same (in the same place as obj)
  (make-instance 'sp-intension :obj obj :rel :equal :dim nil :temp nil :dist nil))

(defun sem-spatial-different (in a different place as obj)
  (make-instance 'sp-intension :obj obj :rel :not-equal :dim nil :temp nil :dist nil))

; ---------------------------------------------------------------------------------
; Section 4.4: Memoization and parsing efficiency
; ---------------------------------------------------------------------------------

(defun memo (fn name key test)
  "Return a memo-function of fn"
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
              (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'equal))
  "Replace fn-name's global definition with a memoized version"
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name) fn-name key test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function"
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

(memoize 'lexical-rules)
(memoize 'rules-starting-with)
(memoize 'parse-words :test 'eq)
; (memoize 'parse :test 'equal)

(defun use (grammar)
  "Switch to a new grammar"
  (clear-memoize 'rules-starting-with)
  (clear-memoize 'lexical-rules)
  (length (setf *grammar* grammar)))

; ---------------------------------------------------------------------------------
; Section 4.5: Inflectional morphology
; ---------------------------------------------------------------------------------

#| Inflectional morphology: given a word as input it recovers its grammatical inflection, 
if any, looks up the root of the word in the *lexicon*, and returns its grammatical 
category, subcategory, and semantics, taking into account its inflection. The 
morphological analysis concerns only the inflectional morphology of nouns, verbs, 
and adjectives. It does not do derivational morphology (e.g., bicycle derives from 
cycle, lazily derives from lazy).  |# 


; The *lexicon* and the *suffixes*

#| The global *lexicon* has separate entries for different parts of speech of the 
same lexeme, but each entry allows for different syntactic subcategories and meanings.  
Current parts of speech include
(C) - connectives
(N) - nouns
(V I)(V T)(V DIT)(V DATV)(V COP)(V INF) - verbs and their subcategories
(ADJ) - adjectives
(ADV) - adverbs
(P)   - prepositions
(POSSESSIVE) - apostrophes
(NEGATION)   - not, n't
(SYNCATEGOREMATIC) - terms that add nothing to meaning
(PUNCT) - punctuation, such as commas and periods
(INTEROG) e.g. why  (QMARK)
|#

(setf *lexicon* '(
;  word      morphology                  Part          Semantics   Other parts of speech
;            function                    of speech                 and their semantics
((",")       nil                         ((punct comma)(sem nil)))
((".")       nil                         ((punct period)(sem period)))
(("(")       nil                         ((punct left-bracket)(sem nil)))
((")")       nil                         ((punct right-bracket)(sem nil)))
(("'s")      nil                         ((possessive sing)(sem possessive 1)))
(("'")       nil                         ((possessive plural)(sem possessive 2)))
(("not")     nil                         ((negation)(sem negation 1)))
(("n't")     nil                         ((negation)(sem negation 2)))
(("and")     nil                         ((c conjunction)(sem conjunction)))
(("either")  nil                         ((syncategorematic)(sem nil))((det excl-disjunction)(sem det ore)))
(("or")      nil                         ((c inclus-disjunction)(sem ori))((c excl-disjunction)(sem ore)))
(("if")      nil                         ((c conditional)(sem ifi)))
(("if and only if") nil                  ((c biconditional)(sem iff)))
(("then")    nil                         ((syncategorematic)(sem nil))((adv temporal)(sem t-then 1)))
(("tall")   (regularAdj)                 ((adj)(sem tall 1)))
(("short")  (regularAdj)                 ((adj)(sem short 1)))
(("big")    (regularAdj)                 ((adj)(sem big 1)))
(("thin")   (regularAdj)                 ((adj)(sem thin 1)))
(("mad")    (regularAdj)                 ((adj)(sem mad 1)))
(("tiny")   (regularAdj)                 ((adj)(sem tiny 1)))
(("silly")  (regularAdj)                 ((adj)(sem silly 1)))
(("friendly")(regularAdj)                ((adj)(sem friendly 1)))
(("cute")   (regularAdj)                 ((adj)(sem cute 1)))      ; others include rude,bare
(("nice")   (regularAdj)                 ((adj)(sem nice 1)))
(("large")  (regularAdj)                 ((adj)(sem large 1)))
(("gentle") (regularAdj)                 ((adj)(sem gentle 1)))
(("wise")   (regularAdj)                 ((adj)(sem wise 1)))
(("white")  (regularAdj)                 ((adj)(sem white 1)))
(("good")   (irregularAdj-normal)        ((adj)(sem good 1)))
(("better") (irregularAdj-comp)          ((adj)(sem good 1)))
(("best")   (irregularAdj-super)         ((adj)(sem good 1)))
(("belief") (regularN)                   ((n)(sem belief 1)))
(("believe") (regularV)                  ((v i)(sem believe 1))((v t)(sem believe 2))((v that)(sem believe 3)))
(("elapse") (regularV)                   ((v i)(sem elapse 1)))
(("eat")    (irregularV-4)               ((v i)(sem eat 1))   ((v t)(sem eat 2)))  ; (form4 "ate") in separate entry
(("eaten")  (participle)                 ((v i)(sem eat 1))   ((v t)(sem eat 2))) 
(("ate")    (past_form)                  ((v i)(sem eat 1))   ((v t)(sem eat 2)))
(("love")   (regularV)                   ((v t)(sem love 1)))
(("love")   (regularN)                   ((n)(sem love 2)))
(("friend") (regularN)                   ((n)(sem friend 1)))
(("hand")   (regularV)                   ((v dit)(sem hand 1)) ((v dat)(sem hand 2))) 
(("hand")   (regularN)                   ((n)(sem hand 3)))                   
(("give")   (irregularV-4)               ((v i)(sem give 1))  ((v dit)(sem give 2)) ((v dat)(sem give 3))) ;(form4 gave")
(("gave")   (past_form)                  ((v i)(sem give 1))  ((v dit)(sem give 2)) ((v dat)(sem give 3)))
(("duck")   (regularN)                   ((n) (sem duck 1)))
(("duck")   (regularV)                   ((v i)(sem duck 2))  ((v t)(sem duck 3)))
(("be")     (rootV)                      ((v cop)(sem be 0)))
(("am")     (presentV_1st)               ((v cop)(sem be 1)))
(("'m")     (presentV_1st)               ((v cop)(sem be 1)))
(("are")    (presentV_2nd)               ((v cop)(sem be 2)))
(("'re")    (presentV_2nd)               ((v cop)(sem be 2)))
(("is")     (presentV_3rd)               ((v cop)(sem be 3)))
(("'s")     (presentV_3rd)               ((v cop)(sem be 3)))
(("was")    (past_1st)                   ((v cop)(sem be 4)))
(("were")   (past_plural)                ((v cop)(sem be 5)))
(("been")   (participle)                 ((v cop)(sem be 6)))
(("being")  (progressive)                 ((v cop)(sem be 7)))
(("do")     (presentV_1st2nd)            ((v aux)(sem do 1)))
(("does")   (presentV_3rd)               ((v aux)(sem do 2)))
(("did")    (past_form)                  ((v aux)(sem do 3)))
(("doing")  (progressive)                ((v aux)(sem do 4))) ; ?THIS ENTRY is NEC? to avoid DO+ing
(("done")   (participle)                 ((v aux)(sem do 5)))
(("daisy")  (regularN)                   ((n)(sem daisy 1)))
(("bathe")  (regularN)                   ((n)(sem bathe 1)))
(("bathe")  (regularV)                   ((v i)(sem bathe 2)) ((v t)(sem bathe 3)))
(("bath")   (regularN)                   ((n)(sem bath 1)))
(("bath")   (regularV)                   ((v t)(sem bath 2)))
(("wash")   (regularN)                   ((n)(sem wash 1)))
(("wash")   (regularV)                   ((v i)(sem wash 2))  ((v t)(sem wash 3)))
(("tax")    (regularN)                   ((n)(sem tax 1)))
(("tax")    (regularV)                   ((v t)(sem tax 2)))
(("kiss")   (regularN)                   ((n)(sem kiss 1)))
(("kiss")   (regularV)                   ((v i)(sem kiss 2))((v t)(sem kiss 3)))
(("fizz")   (regularN)                   ((n)(sem fizz 1)))
(("fizz")   (regularV)                   ((v i)(sem fizz 2)))
(("knife")  (irregularN1)                ((n)(sem knife 1)))   
(("knife")  (regularV)                   ((v t)(sem knife 2)))
(("wife")   (regularN)                   ((n)(sem wife 1)))
(("try")    (regularN)                   ((n) (sem try 1)))
(("try")    (regularV)                   ((v i)(sem try 2))  ((v t)(sem try 3)) ((v inf)(sem try 4)))
(("play")   (regularN)                   ((n)(sem play 1))   ((n)(sem play 2)))
(("play")   (regularV)                   ((v i)(sem play 3)) ((v t)(sem play 4)))
(("speed")  (regularN)                   ((n)(sem speed 1)))
(("speed")  (regularV)                   ((v i)(sem speed 2)))
(("speeding") (regularN)                 ((n)(sem speeding 1)))
(("flee")   (regularV)                   ((v i)(sem flee 1)))
(("fled")   (past_form)                  ((v i)(sem flee 1)))
(("whinge") (regularN)                   ((n)(sem whinge 1)))
(("whinge") (regularV)                   ((v i)(sem whinge 2)))
(("sue")    (regularV)                   ((v t)(sem sue 1)))
(("tooth")  (regularN)                   ((n) (sem tooth 1)))
(("teeth")  (plural_N)                   ((n) (sem tooth 1)))
; (("teeth")  (regularV)                 ((v i)(sem teeth 2)))  LOOK UP to see if it's "teethe"
(("smooth") (regularAdj)                 ((adj)(sem smooth 1)))
(("smooth") (regularV)                   ((v t)(sem smooth 2)))
(("soothe") (regularV)                   ((v t)(sem soothe 1)))
(("smash")  (regularN)                   ((n)(sem smash 1)))
(("smash")  (regularV)                   ((v t)(sem smash 2)))
(("call")   (regularN)                   ((n)(sem call 1)))
(("call")   (regularV)                   ((v i)(sem call 2)) ((v t)(sem call 3)))
(("libel")  (regularN)                   ((n)(sem libel 1)))
(("libel")  (regularV)                   ((v t)(sem libel 2)))
(("chop")   (regularN)                   ((n)(sem chop 1)) ((n)(sem chop 2)))
(("chop")   (regularV)                   ((v t)(sem chop 3)))
(("panic")  (regularN)                   ((n)(sem panic 1)))
(("panic")  (regularV)                   ((v i)(sem panic 2)) ((v t)(sem panic 3)))
(("leave")  (regularV)                   ((v i)(sem leave 1)) ((v t)(sem leave 2)))
(("left")   (past_form)                  ((v i)(sem leave 1)) ((v t)(sem leave 2)))
(("left")   (irregularAdj-normal)        ((adj)(sem left 3)))
(("lead")   (regularN)                   ((n)(sem lead 1)))  ; e.g. mass noun, and in separate meaning count noun
(("lead")   (regularV)                   ((v t)(sem lead 2)))
(("led")    (past_form)                  ((v t)(sem lead 2)))
(("evade")  (regularV)                   ((v t)(sem evade 1)))
(("hurl")   (regularN)                   ((n)(sem hurl 1)))
(("hurl")   (regularV)                   ((v t)(sem hurl 2)) ((v dat)(sem hurl 3)))
(("crawl")  (regularN)                   ((n)(sem crawl 1)))
(("crawl")  (regularV)                   ((v i)(sem crawl 2)))
(("curve")  (regularN)                   ((n)(sem curve 1)))
(("curve")  (regularV)                   ((v i)(sem curve 2)) ((v t)(sem curve 3)))
(("barge")  (regularN)                   ((n)(sem barge 1)) ((n)(sem barge 2)))
(("barge")  (regularV)                   ((v i)(sem barge 3)) ((v t)(sem barge 4)))  
(("back")   (regularN)                   ((n)(sem back 1)) ((n)(sem back 2)))
(("back")   (regularV)                   ((v i)(sem back 3)) ((v t)(sem back 4))) 
(("cut")    (regularN)                   ((n)(sem cut 1)) ((n)(sem cut 2)))
(("cut")    (irregularV-1)               ((v t)(sem cut 3))((v t)(sem cut 4))) 
(("come")   (irregularV-2)               ((v i)(sem come 1)))
(("came")   (past_form)                  ((v i)(sem come 1)))
(("build")  (irregularV-3)               ((v i)(sem build 1))((v t)(sem build 2)))
(("built")  (past_form)                  ((v i)(sem build 1))((v t)(sem build 2)))
(("building") (regularN)                 ((n)(sem building 1)))
(("break")  (regularN)                   ((n)(sem break 1)))
(("break")  (irregularV-4)               ((v t)(sem break 1)))
(("broke")  (past_form)                  ((v t)(sem break 1)))
(("broken") (participle)                 ((v t)(sem break 1)))
(("get")    (irregularV-4)               ((v t)(sem get 1))((v inf)(sem get 2)))
(("got")    (past_form)                  ((v t)(sem get 1))((v inf)(sem get 2)))
(("gotten") (participle)                 ((v t)(sem get 1))((v inf)(sem get 2))) ))

(setf *suffixes* '(                               
;    inflection entry-new-end    entry-word-end  function       syntactic categories
     (("'m")    nil                   nil       #'push_back     ((V present sing 1)))
     (("'re")   nil                   nil       #'push_back     ((V present sing 2)))
     (("'s")    nil                   nil       #'push_back     ((possessive)(V present sing 3)))
     (("'")     nil                   nil       #'push_back     ((possessive)))
     (("n't")   nil                   nil       #'push_back     ((negation)))

     (("es")   ("i")                 ("y")      nil             ((N plural)(V present sing 3)))   
     (("es")   ("v")                 ("fe")     nil             ((N plural)))    
     (("es")   ("h" "x" "ss" "zz")    nil       nil             ((N plural)(V present sing 3)))
     (("s")     nil                   nil       nil             ((N plural)(V present sing 3)))

     (("er")    nil                   nil       nil             ((Adj comparative)))
     (("er")    nil                   nil       #'last=pen      ((Adj comparative)))
     (("er")   ("i")                 ("y")      nil             ((Adj compararive)))
     (("er")("c" "d" "g" "l" "r" "s" "t" "v" "z")("e") nil      ((Adj comparative)))
     (("est")   nil                   nil       nil             ((Adj superlative)))
     (("est")   nil                   nil       #'last=pen      ((Adj superlative)))
     (("est")  ("i")                 ("y")      nil             ((Adj superlative)))
     (("est")("c" "d" "g" "l" "r" "s" "t" "v" "z")("e") nil     ((Adj superlative)))

     (("ing")  ("ck")                 ("c")      nil             ((V progressive)))           
     (("ing")  ("c" "g" "s" "v" "z")  ("e")      nil             ((V progressive)))           
     (("ing")  ("l")                   nil       nil             ((V progressive)))          
     (("ing")   nil                    nil       #'last=pen      ((V progressive)))          

     (("ed")   ("i")                  ("y")      nil             ((V participle)))          
     (("ed")   ("ck")                 ("c")      nil             ((V past)))                
     (("ed")   ("c" "g" "s" "v" "z")  ("e")      nil             ((V past)))                
     (("ed")   ("l")                   nil       nil             ((V past)))                
     (("ed")   nil                     nil      #'last=pen       ((V past)))))

; Three functions for looking up parts of lexemes, i.e., entries in the *lexicon*
(defun word_part(lexeme)
  "returns word in lexeme"
  (caar lexeme)) 

(defun category_part (lexeme)
  "returns major syntactic category in lexeme, e.g., V"
  (caar (third lexeme)))

(defun morphology_part(lexeme)
  "returns morphology function from lexeme, e.g., IRREGULARV-4"
 (caadr lexeme))     

; Morphological analysis -- the top level functions

(defun parse_word(word &optional print-toggle)
  ; Highest level function - it tries three methods, e.g., 
  ; (parse_word "bathed") rtns results from two of them
  (let ((lex *lexicon*) new output)
    (if (setf new (morph_into_entries (lookup_word word nil)))
        (setf output (append output new)))
    (if print-toggle (print (list 1 new)))
    (if (setf new (cut_simple_infl word))
        (setf output (append output new)))
    (if print-toggle (print (list 2 new)))
    (if (setf new (parse_inflection word))
        (setf output (append output new)))
    (if print-toggle (print (list 3 new)))
    (if output
        output
      (not_in_lexicon word))))

(defun not_in_lexicon(word)
  (princ "The word, ")(princ word)
  (princ ", in not in the program's current lexicon.")(terpri))

; (cut_simple_infl "hands'") => NIL, prevents cutting "'" from end of word, using fact it has fun in *suffixes*
(defun cut_simple_infl(word)
  "cuts inflection from word and lookup in lex, it cuts first inflection that fits in list rtned from *suffixes*"
  (let ((lex *lexicon*)(inflections (get_inflections *suffixes*)) partsofspeech lexeme temp)
  (dolist (infl inflections)
    (if (and (end word infl)
             (not (equal '(function push_back)(find_item 'fun (lookup_infl infl))))
             (setf partsofspeech (get_partsofspeech_from (lookup_infl infl)))
             (setf lexeme (morph_into_entries (lookup_word (cutoff infl word) infl partsofspeech))))     
        (return lexeme))))) 

(defun find_item(kind suffix-entry)
  ; finds the various kinds of entry in *suffixes* e.g. (find_item 'new-end (sixth *suffixes*)) => ("i")
  (cond((equal 'inflection kind)(first(first suffix-entry)))
       ((equal 'new-end    kind)(second suffix-entry))
       ((equal 'word-end   kind)(first (third suffix-entry)))
       ((equal 'fun        kind)(fourth suffix-entry))
       ((equal 'syn-cats   kind)(fifth suffix-entry))
       (t (print '(error no such item in suffix-entry)))))    

(defun lookup_infl (infl)
  ; rtns first entry in *suffixes* for infl, e.g. (lookup_infl "es") => (("es") ("i") ("y") NIL etc)
  (let ((inflections *suffixes*))
    (dolist(entry inflections)
      (if (equal (find_item 'inflection entry) infl)
          (return entry)))))

; (get_partsofspeech_from '(("s") nil nil nil ((N plural)(V present sing 3)))) => (V N)
(defun get_partsofspeech_from(suffix-entry)
  "rtns list of main parts of speech from entry in *suffixes*"
  (let (output)
    (dolist(part (fifth suffix-entry))
      (pushnew (first part) output))
    output))
 
(defun get_inflections(inflections)
  ; rtns a list of all the distinct inflections, (get_inflections *suffixes*) => ("'" "s" "es" etc)
  (let (output)
    (dolist(entry inflections output)
      (if (not (member_lis (caar entry) output))
          (push (caar entry) output)))
    output))

(defun parse_inflection(word)
  "parses three sorts of inflection 1 needs new end to root 2 abbreviations such as hand's 3 penult = last chopped"
  (let ((lex *lexicon*)(suffixes *suffixes*) entry root out-root partsofspeech inflection entry-new-end
         entry-word-end new-end lexeme)
    (dolist(entry suffixes)        ; determines root to look-up in *lexicon*
      (setf inflection (find_item 'inflection entry) entry-new-end (find_item 'new-end entry)
                      entry-word-end (find_item 'word-end entry) fun (find_item 'fun entry))
      (or (and entry-new-end entry-word-end (end word inflection)(setf lexeme (substitute_new_end word entry))) 
          (and (end word inflection)(equal fun '(function push_back))
               (setf *input* (funcall (eval fun)(cutoff inflection word)))
               (setf lexeme (morph_into_entries (lookup_word inflection nil))))
          (and (end word inflection)(equal fun '(function last=pen))             ; chopped => chop + ed
               (setf out-root (funcall (eval fun)(cutoff inflection word)))
               (setf lexeme (morph_into_entries (lookup_word out-root inflection partsofspeech)))))
      (if lexeme (return lexeme)))))

(defun substitute_new_end(word entry)
  "replaces in word new-end for entry-word-end obtained from entry, and rtns morph_into_entry of lookup_word" 
  (let* ((inflection (find_item 'inflection entry))(entry-new-end (find_item 'new-end entry)) lexeme
         (entry-word-end (find_item 'word-end entry))(root (cutoff inflection word))
         (new-end (rootend_in_entry root entry-new-end))(partsofspeech (get_partsofspeech_from entry)))
    (if (setf lexeme (morph_into_entries                                                              ; 1 knives
                       (lookup_word (replace_str new-end entry-word-end root) inflection partsofspeech))) 
        lexeme                                                                             
      (morph_into_entries (lookup_word (add_to_end root entry-word-end) inflection partsofspeech))))) ; 2 barged
        
(defun rootend_in_entry(root entry-new-end)
  ; rtns end of root that occurs in lis of entry-new-end, e.g. (rootend_in_entry "bath" '("a" "ath" "b")) => "ath""
  (dolist(entry entry-new-end)
    (if (end root entry)
        (return entry))))

; (lookup_word "bath" "s" '(N)) => (("bath" ((PLURAL)) (((N) (SEM BATH 1)))))
(defun lookup_word(word suffix &optional category)
  "looks up word in *lexicon*, given suffix and required syntax, runs morphology fn if lexeme contains it"
  (let* ((lex *lexicon*) output entry morph-fn morph-result)
    (dolist (lexeme lex)
      (cond((and (equal word (word_part lexeme))
                 (setf morph-fn (morphology_part lexeme)) ; (print (list 'm 1 morph-fn))
                 (setf morph-result (funcall morph-fn suffix))
                 (setf entry (list (word_part lexeme) morph-result (cddr lexeme)))
                 (or (member (category_part lexeme) category)(null category)))  
                     (push entry output))
           ((and (equal word (word_part lexeme))(null (morphology_part lexeme))
                 (setf entry (list (word_part lexeme)(second lexeme)(cddr lexeme))) ; (print (list 'm 2 morph-fn)))
                     (push entry output)))))
    output))

#| (morph_into_entries (lookup_word "bath" "s")) =>
(("bath" (N (PLURAL)) (SEM BATH 1)) 
 ("bath" (V T (PRESENT SING 3)) (SEM BATH 2))) |#
(defun morph_into_entries(entries)
  "puts morphology (second entry) behind part of speech (first pos) in each entry"
  (let (output)
  (dolist (entry entries)
      (dolist (partofspeech (third entry))
        (if (second entry)
            (push (list (first entry)(append (first partofspeech)(second entry))(cadr partofspeech)) output)
        (push (list (first entry)(first partofspeech)(cadr partofspeech)) output))))
    output))

; Part 3: Morphological functions in parsing
       
(defun irregularV-1(suffix)
  ; deals with "cut"
  (if (null suffix)        
      (append (presentV_1st)(presentV_2nd)(past_form)(participle))
    (regularV suffix)))

(defun irregularV-2(suffix)
  ; deals with "come"
  (if (null suffix)        
      (append (presentV_1st)(presentV_2nd)(participle))
    (regularV suffix)))

(defun irregularV-3(suffix)
  ; deals with "build"
  (if (null suffix)        
      (append (presentV_1st)(presentV_2nd))
  (regularV suffix)))

(defun irregularV-4(suffix)
  ; deals with "eat", "break" and "get"
  (cond((null suffix)(append (presentV_1st)(presentV_2nd)))
       ((equal suffix "en")(participle))
       (t (regularV suffix))))
    
(defun regularV(suffix)
  (cond((null suffix)        (append (presentV_1st)(presentV_2nd)))
       ((equal suffix "s")   (presentV_3rd))
       ((equal suffix "es")  (presentV_3rd))
       ((equal suffix "ed")  (append (past_form)(participle)))
       ((equal suffix "ing") (progressive))))

(defun presentV_1st(&optional dummy)
  ; used e.g. for "am", note use of optional dummy parameter for uniformity
  (list '(present sing 1)))

(defun presentV_2nd(&optional dummy)
 ;used e.g. for "are"
 (list '(present sing 2) '(present plural _x)))

(defun presentV_3rd(&optional dummy)
  ; used e.g. for "is"
  (list '(present sing 3)))

(defun presentV_1st2nd(&optional dummy)
  ; used for "do"
  (list (presentV_1st)(presentV_2nd)))

(defun past_1st(&optional dummy)
  ; used for "was"
  (list '(past sing 1) '(past sing 3)))

(defun past_plural(&optional dummy)
  ; used for "were"
(list '(past sing 2) '(past plural _x)))

(defun past_form(&optional dummy)
  "Used for irregular part-tens forms"
  (list '(past _x _y))) ; where _x is number variable, and _y is person variable

(defun participle(&optional dummy)
  (list '(participle)))

(defun progressive(&optional dummy)
  (list '(progressive)))

(defun past_participle(&optional dummy)
  (list '(past_participle)))

(defun rootV(&optional dummy)
  (list '(root)))
 
(defun regularN(suffix)
  ; morphology for regular nouns, such as "hand"
  (cond((null suffix) (list '(sing)))
       ((equal suffix "es")(list '(plural)))
       ((equal suffix "s")(list '(plural)))))

(defun irregularN1(suffix)
  (cond((null suffix)(list '(sing)))
       ((equal suffix "es")(list 'plural))))

(defun irregularAdj-normal(&optional dummy)
  (list '(normal)))

(defun irregularAdj-comp(&optional dummy)
  (list '(comparative)))

(defun irregularAdj-super(&optional dummy)
  (list '(superlative)))
  
(defun plural_N(&optional dummy)
  ; for irregular plural nouns, e.g. "teeth", and called by regularN, irregularN1
  (list '(plural)))

(defun regularAdj(suffix)
  ; for adjectives and their comparative suffixes "er" and "est"
  (cond((null suffix)(list '(normal)))
       ((equal suffix "er")(list '(comparative)))
       ((equal suffix "est")(list '(superlative)))))

; Low level fns for manipulating strings

(defun member_lis(lis lis-lis)
  "rtns t iff lis is lis-lis, lis can be a string too"
  (dolist (itm lis-lis)
    (if (equal lis itm)(return lis))))
                      
(defun last=pen(word)
  "if last = pen letter in word rtns word with last chopped off, e.g. chopp => chop"
  (let ((rev_word (reverse_string word)))
    (if (and (not (null_string word))(equal (head_string rev_word)(head_string (tail_string rev_word))))
        (cutoff (head_string rev_word) word))))

(defun end (word string)
  ; rtns T iff word end_match str, e.g. (end "fred" "ed")
  (end_match (reverse_string word)(reverse_string string)))

(defun end_match (rev-string rev-target)
  "matches reverse string with reverse target"
  (cond((null_string rev-target) t)
       ((equal (head_string rev-string)(head_string rev-target))
           (end_match (tail_string rev-string)(tail_string rev-target)))))

(defun replace_str(old-str new-str word)
  ; replaces old-str in word with new-str e.g. (replace_str "i" "y" "tri") => "try"
  (add_to_end (cutoff old-str word) new-str))

(defun add_to_end(word str)
  "adds string to end of word"
  (concatenate 'string word str))

(defun cutoff(str word)
  ; cuts off str from end of word, e.g. (cutoff "n't" "don't") => "do"
  (reverse_string (cut_rev str (reverse_string word))))

(defun cut_rev (str rev_word)
  ;cuts off string from word, e.g. (cut_rev "gni" "gninip") => nip"
  (cond((null_string str) rev_word)
       (t (cut_rev (tail_string str) (tail_string rev_word)))))
 
(defun push_back(word)
  "puts a word back onto front of *input*, used after cutting off inflection that's a morpheme"
  (push (concatenate 'string word) *input*))

(defun reverse_string(string)
  "reverses a string"
  (if (null_string string) 
      ""
     (concatenate 'string (reverse_string (tail_string string))(head_string string))))

(defun null_string(string)
  ; rtns t if string equals ""
  (equal (length string) 0))
 
(defun head_string(string)
  "rtns head of string, but if string empty rtns it unchanged"
  (if (null_string string) 
      string
     (string (char string 0))))

(defun tail_string (string)
  "rtns tail of string, but if string is empty rtns it unchanged"
  (if (null_string string) 
      string
     (subseq string 1)))

; Testing inputs, testing lists of words, and some test lists

(defun test_fun(fn lis)
  "prints result of applying fn to each member of lis"
  (prog((counter 0)(output nil))
    loop
    (setf counter (+ counter 1))
    (cond((null lis)(return t))
         (t (terpri)(princ counter)(princ ". ")(princ (car lis))(princ " ")
            (setf output (funcall fn (car lis)))
            (dolist (item output)
              (print item))
            (terpri)
            (setf lis (cdr lis))
            (go loop)))))

; test words
(setf *test-pl-words* '("hands" "hands'" "bathes" "tries" "knives" "taxes" "kisses" "fizzes" "smashes" "elapses"))
(setf *test-sing-words* '("hand" "hand's" "doesn't" "bathe" "try" "knife" "tax" "kiss" "fizz" "smash" "elapse"))

;;; This is a test list of words for morphological analysis and it can be used with (test_m test_lis)
(setf *test-lis* (list "am" "ate" "amn't" "daisy" "daisy's" "daisies" "daisies'"
               "hand"  "hands" "hands'" "handed" "handing"
               "bathe" "bathes" "bathed" "bathing" 
               "wash"  "washes" "washed" "washing" 
               "tax"   "taxes"  "taxed"  "taxing"
               "kiss"  "kisses" "kissed" "kissing" 
               "fizz"  "fizzes" "fizzed" "fizzing"
               "knife" "knife's" "knives" "knives'" "knifes" "knifed" "knifing"
               "try" "tries" "tried" "trying"
               "play"  "plays"  "played" "playing"
               "speed" "speeds" "speeded" "speeding"
               "flee"  "flees"  "fled" "fleeing"
               "whinge" "whinges" "whinged" "whingeing"
               "sue" "sues" "sued" "sueing"
               "smooth" "smoother" "smoothest" "smooths" "smoothed" "smoothing"
               "smash" "smashes" "smashed" "smashing"
               "call" "calls" "called" "calling"
               "libel" "libels" "libeled" "libelled" "libeling" "libelling"
               "chop" "chops" "chopped" "chopping"
               "leave" "leaves" "left" "leaving"
               "lead" "leads" "led" "leading"
               "evade" "evades" "evaded" "evading"
               "hurl" "hurls" "hurled" "hurling"
               "crawl" "crawls" "crawled" "crawling"
               "curve" "curves" "curved" "curving"
               "barge" "barges" "barged" "barging"
               "back"  "backs"  "backed" "backing"
               "tall" "taller" "tallest" 
               "short" "big" "bigger" "thin" "thinner" 
               "mad" "maddest" "tiny" "tiniest" "silliest"
               "friendlier" "cutest" "nicer" "largest" "gentler" 
               "wiser" "whitest" "good" "better" "best" 
               "cut" "cuts" "cutting"
               "come" "comes" "came" "coming")) 

; Obtaining and testing an input sentence, and allowing #'push_back to return word to *input*

; Global for holding an input sentence
(setf *input* nil)

(defun test_input()
  ; reads in a sentence from user and applies parse_word to each word, in case of "isn't" pushes "is" back
  ; onto *input*
  (setf *input* (start_up))
  (prog(word output)
    loop
    (setf word (first *input*))
    (setf *input* (cdr *input*))
    (cond((null word)
            (dolist (item output)
              (terpri)
              (print item))(return t))
         (t (push (parse_word word) output)
            (go loop)))))

(defun start_up()
  "reads in sentence from terminal, includes punctuation"
  (princ "Please type in a sentence or string of words with no puncuation or quotation marks.")(terpri)
  (setf *input* (convert_string (read-line)))) 

(defun convert_string(string)
  "converts a string of words into a list of strings even if irregular number of spaces in string"
  (prog(output word)
    loop
    (setf word (first (next_wd string "")))
    (setf rest-string (second (next_wd string "")))
    (cond((equal word "")(setf string rest-string)(go loop)) 
         (t 
            (push word output)
            (if (equal (setf string rest-string) "")
                (return (reverse output))
              (go loop))))))

(defun next_wd(input word)
  ; (next_wd "don't argue" "") => ("don't" "argue")
  (cond ((or (equal " " (head_string input))(equal "" (head_string input)))
                   (list word (tail_string input)))
        (t (next_wd (tail_string input)(concatenate 'string word (head_string input))))))

; Make a word, given its semantics and syntactic category

; This component of the program is solely to explore the possibility of generating words with inflections appropriate
; to their meanings and syntactic categories.  A task for the future is to extend it to the full lexicon.
; makes plural nouns and (v sing 3)
(defun make_word(word suffixes)
  (let (inflection entry-new-word entry-word-end new-end output)
    (dolist (entry suffixes)
      (setf inflection     (find_item 'inflection entry))
      (setf entry-new-end  (find_item 'new-end entry))
      (setf entry-word-end (find_item 'word-end entry))
      (dolist (new-end entry-new-end output)
        (cond((and new-end entry-word-end (end word entry-word-end))
                 (print (list 'first new-end entry-word-end))
                 (setf output (add_to_end (replace_str entry-word-end new-end word) inflection)))  
             ((and new-end (end word new-end))
                 (print (list 'second new-end word))
                 (setf output (add_to_end word inflection)))))                                     
      (if (and (null entry-new-end)(null entry-word-end)(null output))                             
           (setf output (add_to_end word inflection))))
    output))

; (produce_word '(N sing) '(sem hand3)) => 
(defun produce_word(syn-category semantics)
  (let* ((lexeme (newfindword semantics *lexicon*))   
         (word (caar lexeme)))                        
  (cond((equal syn-category '(N sing))(caar lexeme))
       ((equal syn-category '(N plural)) (add_N_plural_inflection word)))))
 
(defun suffix_regularV(cat)
  ; given subcategory of verb rtns suffix, e.g. (suffix_regularV '(PAST_FORM)) =>"ed"
  (let (output)
    (dolist (itm *regular-verbs* output)
      (cond((member_lis cat itm)(setf output (car itm)))))))

;;; A simple data structure, which can be used in either direction
(setf *regular-verbs* '(
      ( nil  (presentV_1st)(presentV_2nd)(present_plural))
      ( "s"  (presentV_3rd))
      ( "ed" (past_form)(participle))
      ("ing" (progressive))))
 
;;; Given semantics and relevant subcategories rtns appropriate word + suffix
(defun newfindword (semantics lex)
  (let ((lexeme (car lex)) )
    (cond((null lex) nil)
         ((semantics_in semantics (car lex))(cons (car lex)(newfindword semantics (cdr lex))))
         (t (newfindword semantics (cdr lex))))))

(defun semantics_in(semantics lexeme)
  "rtns rest of lexeme after matching semantics with it, or nil"
 (cond((null lexeme) nil)
      ((equal semantics (caar lexeme)) lexeme)
      (t (semantics_in semantics (cdr lexeme)))))

; ---------------------------------------------------------------------------------
; Section 4.6: Inflectional morphology
; ---------------------------------------------------------------------------------

(defvar Aab (parse '(All A are B)))
(defvar Iab (parse '(Some A are B)))
(defvar Eab (parse '(No A are B)))
(defvar Oab (parse '(Some A are not B)))
(defvar Aba (parse '(All B are A)))
(defvar Iba (parse '(Some B are A)))
(defvar Eba (parse '(No B are A)))
(defvar Oba (parse '(Some B are not A)))
(defvar Acb (parse '(All C are B)))
(defvar Icb (parse '(Some C are B)))
(defvar Ecb (parse '(No C are B)))
(defvar Ocb (parse '(Some C are not B)))
(defvar Abc (parse '(All B are C)))
(defvar Ibc (parse '(Some B are C)))
(defvar Ebc (parse '(No B are C)))
(defvar Obc (parse '(Some B are not C)))

(defvar Aac (parse '(All A are C)))
(defvar Iac (parse '(Some A are C)))
(defvar Eac (parse '(No A are C)))
(defvar Oac (parse '(Some A are not C)))
(defvar Aca (parse '(All C are A)))
(defvar Ica (parse '(Some C are A)))
(defvar Eca (parse '(No C are A)))
(defvar Oca (parse '(Some C are not A)))

(defvar Mab (parse '(Most A are B)))
(defvar Ma-b (parse '(Most A are not B)))
(defvar Mba (parse '(Most B are A)))
(defvar Mb-a (parse '(Most B are not A)))
(defvar Mbc (parse '(Most B are C)))
(defvar Mb-c (parse '(Most B are not C)))
(defvar Mcb (parse '(Most C are B)))
(defvar Mc-b (parse '(Most C are not B)))

(defvar x-is-A (parse '(X is an A)))
(defvar x-isnot-A (parse '(X is not an A)))
(defvar x-is-B (parse '(X is a B)))
(defvar x-isnot-B (parse '(X is not a B)))

(defvar aBb (parse '(A happened before B)))
(defvar aBc2 (parse '(A happened before C)))
(defvar aBd (parse '(A happened before D)))
(defvar aBe (parse '(A happened before E)))
(defvar bBa (parse '(B happened before A)))
(defvar bBc (parse '(B happened before C)))
(defvar bBd (parse '(B happened before D)))
(defvar bBe (parse '(B happened before E)))
(defvar cBa (parse '(C happened before A)))
(defvar cBb (parse '(C happened before B)))
(defvar cBd (parse '(C happened before D)))
(defvar cBe (parse '(C happened before E)))
(defvar dBa (parse '(D happened before A)))
(defvar dBb (parse '(D happened before B)))
(defvar dBc (parse '(D happened before C)))
(defvar dBe (parse '(D happened before E)))
(defvar eBa2 (parse '(E happened before A)))
(defvar eBb (parse '(E happened before B)))
(defvar eBc2 (parse '(E happened before C)))
(defvar eBd (parse '(E happened before D)))

(defvar aAb2 (parse '(A happened after B)))
(defvar aAc2 (parse '(A happened after C)))
(defvar aAd (parse '(A happened after D)))
(defvar aAe (parse '(A happened after E)))
(defvar bAa (parse '(B happened after A)))
(defvar bAc (parse '(B happened after C)))
(defvar bAd (parse '(B happened after D)))
(defvar bAe (parse '(B happened after E)))
(defvar cAa (parse '(C happened after A)))
(defvar cAb (parse '(C happened after B)))
(defvar cAd (parse '(C happened after D)))
(defvar cAe (parse '(C happened after E)))
(defvar dAa (parse '(D happened after A)))
(defvar dAb (parse '(D happened after B)))
(defvar dAc (parse '(D happened after C)))
(defvar dAe (parse '(D happened after E)))
(defvar eAa (parse '(E happened after A)))
(defvar eAb2 (parse '(E happened after B)))
(defvar eAc2 (parse '(E happened after C)))
(defvar eAd (parse '(E happened after D)))

(defvar aDb (parse '(A happened during B)))
(defvar aDc (parse '(A happened during C)))
(defvar aDd (parse '(A happened during D)))
(defvar aDe (parse '(A happened during E)))
(defvar bDa (parse '(B happened during A)))
(defvar bDc (parse '(B happened during C)))
(defvar bDd (parse '(B happened during D)))
(defvar bDe (parse '(B happened during E)))
(defvar cDa (parse '(C happened during A)))
(defvar cDb (parse '(C happened during B)))
(defvar cDd (parse '(C happened during D)))
(defvar cDe (parse '(C happened during E)))
(defvar dDa (parse '(D happened during A)))
(defvar dDb (parse '(D happened during B)))
(defvar dDc (parse '(D happened during C)))
(defvar dDe (parse '(D happened during E)))
(defvar eDa (parse '(E happened during A)))
(defvar eDb (parse '(E happened during B)))
(defvar eDc (parse '(E happened during C)))
(defvar eDd (parse '(E happened during D)))

(defvar aWb (parse '(A happened while B)))
(defvar aWc (parse '(A happened while C)))
(defvar aWd (parse '(A happened while D)))
(defvar aWe (parse '(A happened while E)))
(defvar bWa (parse '(B happened while A)))
(defvar bWc (parse '(B happened while C)))
(defvar bWd (parse '(B happened while D)))
(defvar bWe (parse '(B happened while E)))
(defvar cWa (parse '(C happened while A)))
(defvar cWb (parse '(C happened while B)))
(defvar cWd (parse '(C happened while D)))
(defvar cWe (parse '(C happened while E)))
(defvar dWa (parse '(D happened while A)))
(defvar dWb (parse '(D happened while B)))
(defvar dWc (parse '(D happened while C)))
(defvar dWe (parse '(D happened while E)))
(defvar eWa (parse '(E happened while A)))
(defvar eWb (parse '(E happened while B)))
(defvar eWc (parse '(E happened while C)))
(defvar eWd (parse '(E happened while D)))