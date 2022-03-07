; ---------------------------------------------------------------------------------
; Part 6: Scanning heuristics
; ---------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------
; Section 6.1: Heuristics for scanning model to form initial conclusions
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