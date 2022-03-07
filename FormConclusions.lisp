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

(defun form-weaker-conclusions (conclusions models)
  (if (weaken-conclusions?)
      (progn
        (setf conclusions (remove-duplicates conclusions :test #'equals))
        (trc "System 2" (format nil "Weakening falsified conclusion(s) ~{~A~#[~:;, ~]~}" (mapcar #'abbreviate conclusions)))
        (let* ((weaker-conclusions (mapcan #'(lambda (c) (form-weaker-conclusion c models)) conclusions))
               (weaker-conclusions (remove-duplicates weaker-conclusions :test #'equals))
               (weaker-conclusions (remove-if-not #'(lambda (c) (validate c models)) weaker-conclusions)))
          weaker-conclusions))
    (when conclusions (list *nvc*))))

(defun form-weaker-conclusion (conclusion models)
  (let* ((swapped (swap-terms-in-conclusion conclusion))
         (weaker1 (weaken-conclusion conclusion))
         (weaker2 (weaken-conclusion swapped)))
    (when weaker1
      (append (list weaker1 weaker2)
              (form-weaker-conclusion weaker1 models)
              (form-weaker-conclusion weaker2 models)))))

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
  "when the cardinality is mutable + when the conclusion is not already weak itself,
   (as in the case of Aab and Mab, weaken conclusion to existential quantifiers"
  (let* ((mutable (equalp '? (first (first (cardinality conclusion)))))
         weakened)
    (when (and mutable (not (or (is-some conclusion) (is-some-not conclusion))))
      (cond
       ((or (is-all conclusion) (is-most conclusion))
        (setf weakened (parse `(Some ,(subject conclusion) are ,(object conclusion)))))
       ((or (is-none conclusion) (is-most-not conclusion))
        (setf weakened (parse `(Some ,(subject conclusion) are not ,(object conclusion))))))
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