; ---------------------------------------------------------------------------------
; Part 8: Form conclusions
; ---------------------------------------------------------------------------------

; Section 8.1: Heuristics for scanning model to form initial conclusions
; Section 8.2: Forming weaker conclusions after initial ones falsified

; ---------------------------------------------------------------------------------
; Section 8.1: Heuristics for scanning model to form initial conclusions
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
; Section 8.2: Forming weaker conclusions after initial ones falsified
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