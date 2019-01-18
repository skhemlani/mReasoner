; ---------------------------------------------------------------------------------
; Part 8: Form conclusions
; ---------------------------------------------------------------------------------

#|
Suppose heuristics yields Eac Eca, which the the first model corroborates, 
but a subsequent model falsifies each of them:
First model                      Second model
   A  -B                            A  -B   C
   A  -B                            A  -B   C
   A  -B                            A  -B   C        Form conclusions should yield as plausible: NVC
       B   C                            B   C              scanning from a - c
       B   C                            B   C        But scanning c-a, it should yield the VALID but
       B   C                            B   C            implausible: Oca
    . . . {T28}                     . . . {T28}

form-conclusion, minimal assumption version:
in effect, it knows that Eac and Eca are conclusions about what is possible,
but not necessary.  Hence, it should look for a weaker conclusion.  In syllogisms, the only
weaker conclusions are Oac and Oca.  In terms of quant settings, the shift in parameter
settings is to set numprop to current-cardinality minus 1, and to setf predicate to '(are not).
Likewise, if current conclusions are Aac and Aca, then weaker conclusions also set numprop
to current-cardinality minus 1; and change boundary conditions must change from ((= cardinality)), 
to ((< cardinality)(> 0)).
Finally, validate this conclusion in modelset. 
 |#

#| 
 form-conclusion tries to replace existing old-concl with new weaker one, if possible
 form-conclusion
    find-subject
    find-object
    validate
    verbalize-intens -- puts intension into a verbal conclusion
        find-subject - API, and all below are, except for form-concl
        find-object
        recover-quant-intens
        recover-card
        recover-card-arg -- new fn in API
        recover-numprop
        form-concl 
    weaken-conclusion -- tries to weaken old-concl
        recover-quant-intens
        recover-card
        recover-card-arg
        recover-numprop
        reset-cardinality -- resets numprop and predicate (if nec)
            recover-quant-intens
            recover-card-arg
        negate-predicate-intens
A separate function is:
form-possible-conclusions
    find-subject
    find-object 
    validate
    form-conclus -- see below
|#

(defun form-weaker-conclusions (conclusions models)
  (if (weaken-conclusions?)
      (progn
        (trc "System 2" (format nil "Weakening falsified conclusion(s) ~{~A~#[~:;, ~]~}" (mapcar #'abbreviate conclusions)))
        (let* ((weaker-conclusions (mapcan #'(lambda (c) (form-weaker-conclusion c models)) conclusions))
               (weaker-conclusions (remove-duplicates weaker-conclusions :test #'equals))
               (weaker-conclusions (remove-if-not #'(lambda (c) (validate c models)) weaker-conclusions)))
          weaker-conclusions))
    (when conclusions (list *nvc*))))

(defun form-weaker-conclusion (conclusion models)
  (let (;(swapped (swap-terms-in-conclusion conclusion))
        (weaker (weaken-conclusion conclusion)))
    (when weaker
      (append (list weaker) (form-weaker-conclusion weaker models))
)))

(defmethod swap-terms-in-conclusion ((conclusion q-intension))
  "Merely takes a conclusion and creates a new one by swapping its terms around;
   validates against the models and returns it."
  (let* ((new-conclusion (copy-class-instance conclusion))
         (subject (subject new-conclusion))
         (object (object new-conclusion)))
    (setf (subject new-conclusion) object)
    (setf (object new-conclusion) subject)
    new-conclusion))

(defmethod weaken-conclusion ((conclusion q-intension))
  "if cardinality is mutable, i.e., prefaced with '? setf mutable to t
   if null.mutable or numprop is already < cardinality (indicating 'some assertion) rtns nil
   elseif T T as final parameters of quantifier resets cardinality (minus 1), which also resets T nil
   elseif nil T resets cardinality and negates predicate

   weakens a conclusion 
    (weaken-conclusion '((((? 3) (> 2)) (? 3) ((= CARDINALITY)) NIL T) (A) (INCLUDE (A) (C))))"
  (let* ((mutable (equalp '? (first (first (cardinality conclusion)))))
         (cardinality (cardinality-value conclusion))
         weakened)
    (when (and mutable (not (or (is-some conclusion) (is-some-not conclusion))))
      (cond
       ((is-all conclusion)
        (setf weakened (parse `(Some ,(first (subject conclusion)) are ,(first (object conclusion))))))
       ((is-none conclusion)
        (setf weakened (parse `(Some ,(first (subject conclusion)) are not ,(first (object conclusion)))))))
      weakened)))

#|
0. Intensions of premises yield subj and obj of conclusion, i.e., end terms. Max has written this fn.
1. NOT IMPLEMENTED: Intensions of premises reveal whether model should be treated as 
   proportional, numerical, or neither (e.g., syllogism):-
   If the second.boundary constraint in intension of premise contains no number or a number < 1,
   non-numerical quantifier (all, some, no, most).
   Elseif it contains number >= 3, a numerical quantifier (e.g., at least 3)
   Else [it contains, e.g. (/ 2 3)], a proportional quantifier (e.g., at most 2/3).
   Take the strongest quantifier that occurs in the intensions of the premises, i.e.,
      proportional overrules numerical overrules non-numerical.  It determines the sort to be used in
   the conclusion. 
2. Model yields card.subj, polarity of obj, and numprop of quantifier,
   i.e., number of objs in indivs containing subjs.
3. Relation between card and numbprop, and polarity, is matched
   against each relevant quantifier in the lexicon, and adjusted to fit the actual values
   in the model. At present, a subset of the lexicon -- just dealing with the four moods
   of syllogistic conclusion -- is included in the code below.
4. For each resulting quantifier, and for relation-term and arg in predicate, a conclusion
   is listed. The relation (are vs are not) is recovered from the model but in a simplistic
   way.
|#

#|
form-conclus          - subj obj intensions and model rtns a conclusion
                           AT PRESENT INTENSIONS PLAY NO ROLE, AND SYLLOGISTIC
                           CONCLUSION IS TAKEN FOR GRANTED
   find-referent-individuals-in-model - rtns all indivs in model with a given proprty API
   negate-property       - negates a property
   find-relation-in-indivs - uses model to rtn predicate for conclusion
   form-concl            - interfaces parameter values from model with lexicon
      match-features-lex - goes through global lexicon looking for match
         match-features  - tries to match values with a lexical entry
            recover-det-features - gets features for determiner from lexicon
            update-boundary      - if pos, alters 'cardinality in lex-entry to match that in model
            numprop-bounds       - applies numprop-bound to each lis in boundary-features
               numprop-bound |   - interpolating numprop-in-model into boundary
                                   feature and evaluting it.
|#
