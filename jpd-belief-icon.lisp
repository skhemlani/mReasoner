; ---------------------------------------------------------------------------------
; Part 10: Intuitive probabilities of unique events and their compounds
; ---------------------------------------------------------------------------------

#| Evaluate +mReasoner.lisp first, which is a prerequisite for this program to work.
   There are two ways to use this component of mReasoner. 
     1. You can call (interact), which solicits information from you about 
   probabilities, and illustrates how the program infers probabilities. 
     2. The program has a small knowledge-base, which can be used to illustrate
   its operations.  Evaluate one of the following calls, which illustrate conjunctions,
   disjunctions, and conditional probabilities:          Typical 'rational' JPD's
     (compound-problem problem-1 problem-2 2-given-1 3)  .36 .31 .04  .28
     (compound-problem problem-2 problem-1 1-given-2 3)  .32 .11 .35  .23
     (compound-problem problem-5 problem-1 1-given-5 3)  .52 .28 .1   .1
     (compound-problem problem-5 problem-1 1-given-5 1)  .12 .66 .52 -.30
     (compound-problem problem-5 problem-1 1-given-5 2)  .47 .33 .20  .00
     (compound-problem problem-5 problem-6 6-given-5 1)  .18 .62 .24 -.04
     (compound-problem problem-6 problem-5 5-given-6 2)  .12 .25 .62  .00
     (compound-problem problem-7 problem-8 8-given-7 3)  .10 .28 .28  .33
     (compound-problem problem-8 problem-7 7-given-8 1)  .14 .25 .25  .35|#

#|  
A set of functions that use mental models of assertions to infer
the probabilities of unique events and their compounds, such as
conjunctions, disjunctions, and conditional probabilities. They
work either on knowledge elicited from the user or from a small
knowledge base in the program.

Section 10.1:  High-level functions for user interface
Section 10.2:  Interface functions for getting information from usersabout individual problems   
Section 10.3:  Interface functions for getting information from users about relations between events 
Section 10.4:  Low level interface functions
Section 10.5:  Monte-carlo simulations of probabilities -- probably drop from public version!
Section 10.6:  Classes of problems and instances of them
Section 10.7:  The main program for compound events
Section 10.8:  System 1 constructs icons
Section 10.9:  System 1 computes intuitive probability from a quantified model
Section 10.10: System 1 does non-recusive arithmetic, i.e., no numbers or counting
Section 10.11: System 2 for assigning numbers to intuitive probabilities
Section 10.12: Feedback about actual JPD from program's and user's estimates
Section 10.13: Functions for printing outputs
|#

;-----------------------------------------------------------------------------------------------------
; Section 10.1: High-level functions for user interface
;-----------------------------------------------------------------------------------------------------

(defun interact()
  "High level function for user interactions"
  (let ((connective (give-instructions))(problemA (get-problem)) problemB A-relates-pB)
    (princ "You are going to have to think about the probability of another event.")
    (setf problemB     (get-problem)
          A-relates-pB (get-logic-of-relation problemA problemB connective)      
          A-relates-pB (get-relation A-relates-pB)
          A-relates-pB (compound-problem problemA problemB A-relates-pB connective))
    A-relates-pB))

(defun give-instructions()
    (princ "  You're going to estimate some probabilities of events that may or may not happen,")
    (terpri)
    (princ "  such as, Hillary Clinton running for the Presidency of the USA, or, Greece becoming solvent.") 
    (terpri)
    (princ "  You will choose the topics, and so choose topics that you have some views about.")
    (terpri)
    (princ "  You going to estimate two probabilities, and then a combination of them, such as:")(terpri)
    (princ "  their conjunction, A and B; their disjunction, A or B or both;")(terpri)
    (princ "  or, the conditional probability of B assuming that A occurs.")(terpri)
    (princ "  When you're ready, please choose one of these three options by typing 1, 2, or 3:")(terpri)
    (princ "  1: the probability of A and B.")(terpri)
    (princ "  2: the probability of A or B, or both.")(terpri)
    (princ "  3: the probability of B assuming that A occurs.")(terpri)
    (get-option))

(defun get-option()
   (let (input)
     (princ " ")(setf input (first (read-line-no-punct)))
     (cond((or (equal input 1)(equal input 2)(equal input 3))
                  input)
         (t    (princ "Please try again. Type 1, 2, or 3: ")
               (get-option)))))

(defun get-problem()
  "gets probem & elicits all the info for it such as 'Hillary Clinton is elected President'"
  (let* ((problem  (make-instance 'p-problem)) 
         (un-topic  (get-and-check-topic))
         (un-pquest (get-problem-statement un-topic))
         (un-predicate (get-predicate un-topic un-pquest)))
    (setf (p-topic problem)(hyphenate un-topic)
          (p-question problem)(append (p-topic problem) '(is) (hyphenate un-predicate))
          problem (get-slots 'member-k-1 'quantif-k-1 problem))
    (princ "You're going to have to add another piece of evidece. So:")(terpri)
    (setf problem (get-slots 'member-k-2 'quantif-k-2 problem)
          (user-probability problem)(get-and-check-probability un-pquest))
    problem))

;-----------------------------------------------------------------------------------------------------
; Section 10.2: Interface functions for getting information about individual problems from users
;-----------------------------------------------------------------------------------------------------

(defun get-and-check-topic()
  "gets a name or singular noun phrase"
  (let(topic)
    (terpri)
    (print-sentence '(Who or what is the topic of your probability estimate) "?")(terpri)
    (print-sentence '(Please type the name of a person or a topic in the singular) ":")(terpri)
    (setf topic (read-line-no-punct))
    (cond((> (length topic) 3)
            (terpri)
            (princ "That's a bit of a long description. ") 
            (princ "Try to type something akin to 'The US economy' or 'Mayor Bloomberg'.")
            (terpri)(princ "So: ")
            (get-and-check-topic))
         (t topic))))

(defun get-problem-statement(topic)
  "gets predicate to make statement about topic that is p-question"
  (let (statement)
    (print-sentence '(What is the probability about this topic that you want to estimate) "?")
    (print-sentence '(Please complete the statement) ".")(terpri)
    (print-sentence topic " ")(princ "is ")
    (setf statement (append topic '(is) (read-line-no-punct)))
    (if (check-assertion statement)
        statement
      (get-problem-statement topic))))

(defun get-predicate (topic sentence)
  "rtns predicate off sentence if it begins with full and correct topic; otherwise, nil"     
  (cond((null topic)(rest sentence))
       ((equal (first topic)(first sentence))(get-predicate (rest topic)(rest sentence)))
       (t (princ "You didn't state your topic in full.")(terpri))))

(defun get-and-check-probability(statement)
  "reads in probability estimate and checks that it's a number from 0 through 100"
  (let((probability 0))
  (princ "Please state your estimate of the probability that")(print-sentence statement ".")
  (terpri)
  (princ "Make your estimate as a percentage ranging from impossible, i.e., 0 to certain, i.e., 100.")
  (terpri)
  (setf probability (first (read-line-no-punct)))
  (cond((and (and (numberp probability)(>= probability 0)(<= probability 100))
             (not (and (> probability 0)(< probability 1))))
          probability)
       (t (princ "Your estimate should be a whole number from 0 to 100. So, please try again.")(terpri)
          (get-and-check-probability statement)))))

(defun get-slots (member-slot quant-slot problem)
  (let* ((topic (p-topic problem))
         (membership-predicate (get-membership topic))
         (question-predicate (hyphenate (get-predicate topic (p-question problem)))))
  (setf (slot-value problem member-slot)(append (p-topic problem) '(is) (hyphenate membership-predicate)))
  (setf (slot-value problem quant-slot)
        (get-quantified-evidence (p-question problem) question-predicate membership-predicate))
  problem))

;-----------------------------------------------------------------------------------------------------
; Section 10.3: Interface functions for getting information from user about relations between events
;-----------------------------------------------------------------------------------------------------

(defun get-logic-of-relation(problem-a problem-b connective)
  "sets up instances of relational-knowledge and gets user to fill in truth table of a and b possibilities"
  (let* (a-relates-to-pB response (question-a (p-question problem-a))(question-b (p-question problem-b)))
    (setf a-relates-to-pB (make-instance 'relational-knowledge
                                         :a-quest question-a
                                         :b-quest question-b
                                         :b-top   (p-topic problem-b)
                                         :connect    connective
                                         :userpa  (user-probability problem-a)
                                         :userpb  (user-probability problem-b)))
    (terpri)(princ "Now, you are going to assess the relation, if any, between the two cases of interest.")
    (setf response (get-response question-a question-b 'necessarily))
    (cond((or (equal response 'n)(equal response 'no))
             (setf response (get-response question-a question-b 'possibly))
             (cond((or (equal response 'y)(equal response 'yes))
                     (setf (abpossible  a-relates-to-pB) t)
                     (setf (a-bpossible a-relates-to-pB) t)))) ; if r is 'n then all nil defaults hold still
         (t  (setf (abpossible  a-relates-to-pB) t)))          ; default setting of a-b remains nil
    (setf question-a (append '(it is not the case that) question-a))
    (setf response (get-response question-a question-b 'necessarily))
    (cond((or (equal response 'n)(equal response 'no))
             (setf response (get-response question-a question-b 'possibly))
             (cond((or (equal response 'y)(equal response 'yes))
                     (setf (-abpossible  a-relates-to-pB) t)
                     (setf (-a-bpossible a-relates-to-pB) t))))  ; if r is 'n then all nil defaults hold still
         (t  (setf (-abpossible  a-relates-to-pB) t)))           ; default seting of -a-b remains nil 
    a-relates-to-pB))

(defun get-relation(a-relates-pB)
  "solitics info to add bga-member and bga-quantified to a-relates-pB, or rtns it unchanged"
  (let* ( membership-predicate response quantifier
         (topic-b    (b-topic    a-relates-pB))
         (question-a (a-question a-relates-pB))
         (question-b (b-question a-relates-pB))
         (question-predicate (get-predicate topic-b question-b)))
  (princ "Now, you're going to provide some further knowledge.")(terpri)
  (princ "Suppose that")(print-sentence question-a ".")(terpri) 
  (princ "Does this situation affect the probability that")
  (print-sentence question-b "?")
  (setf response (check-yes-or-no))                             
  (cond((or (equal response 'yes)(equal response 'y)) 
         (setf membership-predicate (get-membership topic-b)
              (bgivena-member a-relates-pB)(append (hyphenate topic-b) '(is) (hyphenate membership-predicate))
               quantifier (get-quantified-evidence question-b question-predicate membership-predicate)
              (bgivena-quantified a-relates-pB)
                 (append (list(first quantifier))(hyphenate membership-predicate) '(are)(hyphenate question-predicate))
              (b-question a-relates-pB)(append (hyphenate topic-b) '(is) (hyphenate question-predicate))
              (user-pAcompoundB a-relates-pB) 
                  (get-estimate-of-compound question-a question-b (connective a-relates-pB)))))
  a-relates-pB))

(defun get-estimate-of-compound(question-a question-b connective)
  "gets user's estimate of probability of compound"
  (let (sentence)
    (cond((= connective 1)(setf sentence (append question-a '(and) question-b)))
         ((= connective 2)(setf sentence (append question-a '(or)  question-b)))
         ((= connective 3)(princ "Assuming that")(print-sentence question-a ". ")
                          (terpri)(setf sentence question-b)))
    (get-and-check-probability sentence)))

(defun get-response(question-a question-b modality)
  "gets response to cases where modality is set to necessarily or to possibly"
  (let (response)
  (terpri)(princ "Suppose that")(print-sentence question-a ".")(terpri)
  (princ " Is it ")(princ modality)(princ " the case that in this situation")
  (print-sentence question-b "?")
  (check-yes-or-no)))

;-----------------------------------------------------------------------------------------------------
; Section 10.4: Low level interface functions
;-----------------------------------------------------------------------------------------------------

(defun get-membership(topic)
  (let (membership)
    (princ "Please complete the following sentence in a way that is relevant to your judgment.")
    (terpri)
    (print-sentence (append topic (list 'is)) " ")(setf membership (read-line-no-punct))
    (cond((null (check-assertion membership))
            (print-sentence topic " ")(princ "is ")(setf membership (read-line-no-punct))))
    (if (or (equal (first membership) 'a)(equal (first membership) 'an))
           (rest membership)
      membership)))

(defun get-quantified-evidence (question question-predicate membership-predicate)
  "solicits quantified evidence relevant to membership, e.g.
       --- world-famous are elected President => (Some people who are)"
  (let (quantifier)
     ;  (terpri)
   (princ "Please fill in the start of the following hyphenated assertion so that it relates to the probability of: ") 
   (terpri)(print-sentence question ".")(terpri)
   (princ "___________ ")(print-string (append '(that are) membership-predicate '(are) question-predicate))
   (terpri)
   (princ "Your completion can contain several words, as long as it starts with one of the following words: ")
   (terpri)(princ "All, Most, Many, Some, Few, or No. ")(terpri)
   (princ "But there's no need to retype the rest of the sentence.")(terpri)
   (setf quantifier (check-evidence))
   (append (list (first quantifier)) (hyphenate membership-predicate) '(are) question-predicate)))

(defun check-assertion(sentence)
  "checks that assertion is statement and that it does not refer to probability"
   (cond((or (equal (first sentence) 'what)
             (equal (first sentence) 'how))
          (princ "Please don't ask a question but make an assertion. Try again.")(terpri))
        ((or (member 'probability sentence)
             (member 'probably sentence)
             (member 'likely sentence)
             (member 'chances sentence)
             (member 'chance sentence))
           (princ "Please don't refer to probability explicitly. Just try again with a direct assertion.")
           (terpri))
        (t t)))

(defun check-evidence()
  "gets evidence and checks that it is in appropriate format - DELETED call to check-assertion?"
  (let ((evidence (read-line-no-punct))) 
  (cond ((and (= (length evidence) 1)(equal (first evidence) 'no)) nil)
        ((or (member 'not evidence)(member 't evidence))
          (princ "Please avoid negation in your evidence apart from 'No' as first word. Try again.")
          (terpri)
          (check-evidence))
        ((not (setf evidence (check-quantifier evidence)))
          (princ "Please try again and start your evidence with 'All', 'Most', 'Many', 'Some', 'Few', or 'No'.")
          (terpri)
          (check-evidence))
       (t evidence))))

(defun check-quantifier(sentence)
  (let ((word (first sentence)))
  (cond((is-quantifier word) sentence)
       ((equal word 'many)(cons 'some (rest sentence)))
       ((equal word 'none)(cons 'no (rest sentence))))))

(defun is-quantifier(word)
  "checks whether word is a quantifier"
  (or (equal word 'all)(equal word 'most)(equal word 'some)(equal word 'few)(equal word 'no)))

(defun check-yes-or-no()
  "rtns yes, y, no or n response"
  (let (response) 
    (princ " Yes or No?")(terpri)    
    (setf response (first (read-line-no-punct)))
    (cond((or (equal response 'yes)(equal response 'y)(equal response 'no)(equal response 'n))
             response)
         (t (princ "Please respond yes or no; so try again.")(check-yes-or-no)))))

(defun hyphenate(lis)
  "converts lis into a string with hyphens, and then converts it back into a list of a single atom"
  (list (read-from-string (hyphen lis))))

(defun hyphen(lis)
  "converts lis into a string with hypens between the items"
  (if (null (rest lis))
      (symbol-name (first lis))
    (concatenate 'string (symbol-name (first lis)) "-" (hyphen (rest lis)))))

(defun read-line-no-punct(&optional in-char)
  "reads in line but replaces punctuation marks with spaces"
  (let ((input (read-line)))
    (cond( in-char (setf input (concatenate 'string (string in-char) input))))
    (read-from-string
     (concatenate 'string "(" (substitute-if #\space #'punctuation-p
                                          input)
               ")"))))

(defun punctuation-p(char) 
  ; finds any punctuation mark
  (find char ".,;:'!?%#()\\\""))

;-----------------------------------------------------------------------------------------------------
; Section 10.5: Monte-carlo simulations of probabilities
;-----------------------------------------------------------------------------------------------------

#|
To illustrate how conjunction fallacies arise from 'split the difference'
and to allow Monte Carlo simulations, 
    call (conjunction-probs 10).
To switch heuristics on, provide a second parameter t:
         (conjunction-probs 10 t)
The fn generates and prints n instances of pA, pB, and pA&B that splits
the difference except when heuristics are on.
|#

(defun conjunction-probs(n &optional heuristics)
  ; if heuristics called with t, then simple heuristics kick in
  ; rtns Monte Carlo simulation of 'split the difference' for pA&B
  (let ((pA&B nil)(jitter (random 5)))
    (dotimes (count n)
      (setf pA (random 101))
      (setf pB (random 101))
      (cond((and heuristics (setf pA&B (heuristics-fn pA pB)))
               (print (list pA pB pA&B)))
           (t (setf pA&B (+ (/ (+ pA pB) 2.0) jitter))
              (if (> pA&B 100)
                  (setf pA&B 100))
                    (print (list pA pB pA&B)))))))

(defun heuristics-fn(pA pB)
; rtns simple values for pA&B when pA, pB = 1 or 0
  (cond((= pA 100) pB)
       ((= pB 100) pA)
       ((= pA 0) 0)
       ((= pB 0) 0)))

;----------------------------------------------------------------------------------
; Section 10.6: Classes of problems and instances of them
;----------------------------------------------------------------------------------

(defclass p-problem()
; defines class of problems consisting of a question about a probability,
; and two pairs of relevant knowledge items, each containing a membership
; assertion and a quantifier assertion
  ((p-question       :accessor p-question       :initarg :pquest)
   (p-topic          :accessor p-topic          :initarg :ptop  :initform nil)
   (member-k-1       :accessor member-k-1       :initarg :memb-k-1)
   (quantif-k-1      :accessor quantif-k-1      :initarg :quant-k-1)
   (member-k-2       :accessor member-k-2       :initarg :memb-k-2)
   (quantif-k-2      :accessor quantif-k-2      :initarg :quant-k-2)
   (user-probability :accessor user-probability :initarg :u-prob :initform nil)))

(defclass relational-knowledge()
  ((a-question          :accessor a-question         :initarg :a-quest)
   (b-question          :accessor b-question         :initarg :b-quest)
   (a-topic             :accessor a-topic            :initarg :a-top          :initform nil)
   (b-topic             :accessor b-topic            :initarg :b-top          :initform nil)
   (connective          :accessor connective         :initarg :connect        :initform nil)
   (abpossible          :accessor abpossible         :initarg :abpos          :initform nil)
   (a-bpossible         :accessor a-bpossible        :initarg :a-bpos         :initform nil)
   (-abpossible         :accessor -abpossible        :initarg :-abpos         :initform nil)
   (-a-bpossible        :accessor -a-bpossible       :initarg :-a-bpos        :initform nil)
   (bgivena-member      :accessor bgivena-member     :initarg :bga-member     :initform nil)
   (bgivena-quantified  :accessor bgivena-quantified :initarg :bga-quantified :initform nil)
   (bgivena-relation    :accessor bgivena-relation   :initarg :bga-relation   :initform nil)
   (program-pA          :accessor program-pA         :initarg :p-pa)
   (program-pB          :accessor program-pB         :initarg :p-pb)
   (program-AcompoundB  :accessor program-AcompoundB :initarg :p-pacb         :initform 50.0)
   (program-ArationalB  :accessor program-ArationalB :initarg :ratpaCb        :initform nil)
   (user-pA             :accessor user-pA            :initarg :userpa         :initform nil)
   (user-pB             :accessor user-pB            :initarg :userpb)
   (user-pAcompoundB    :accessor user-pAcompoundB   :initarg :userpaCb)))

(setf problem-1 (make-instance 'p-problem
                               :pquest    '(Apple is a profitable-company)
                               :memb-k-1  '(Apple is a well-managed-firm)
                               :quant-k-1 '(Most well-managed-firm are profitable-company)
                               :memb-k-2  '(Apple is an innovator)
                               :quant-k-2 '(Some innovator are profitable-company)))

(setf problem-2 (make-instance 'p-problem
                               :pquest    '(Microsoft is not a profitable-company)
                               :memb-k-1  '(Microsoft is a poorly-managed-firm)
                               :quant-k-1 '(Most poorly-managed-firm are not profitable-company)
                               :memb-k-2  '(Microsoft is a big-company)
                               :quant-k-2 '(All big-company are profitable-company)))

; For sensible results, the subject of b-quest equal subject of bga-member, and predicate of 
; bga-member equal head phrase of subject of bga-quantified
(setf 2-given-1 (make-instance 'relational-knowledge
                               :a-quest            '(Apple is a profitable-company)
                               :b-quest            '(Microsoft is not a profitable-company)
                               :abpos                t 
                               :a-bpos               t
                               :-abpos               t
                               :-a-bpos              t
                               :bga-member     '(Microsoft is company-with-profitable-rival)
                               :bga-quantified '(Most company-with-profitable-rival are not profitable-company)
                               :bga-relation     nil)) 

(setf 1-given-2 (make-instance 'relational-knowledge                      
                               :a-quest            '(Microsoft is not a profitable-company)             
                               :b-quest            '(Apple is a profitable-company)
                               :abpos                t 
                               :a-bpos               t
                               :-abpos               t
                               :-a-bpos              t
                               :bga-member         '(Apple is company-with-poor-rival)
                               :bga-quantified     '(most company-with-poor-rival are profitable-company)
                               :bga-relation       nil)) 


(setf problem-3 (make-instance 'p-problem
                               :pquest    '(Apple is a profitable-company)
                               :memb-k-1  '(Apple is a well-managed-firm)
                               :quant-k-1 '(Most well-managed-firm are profitable-company)
                               :memb-k-2  '(Apple is a without-CEO-company)
                               :quant-k-2 '(Most without-CEO-company are not profitable-company)))

(setf problem-4 (make-instance 'p-problem
                               :pquest    '(Apple is not a profitable-company)
                               :memb-k-1  '(Apple is a well-managed-firm)
                               :quant-k-1 '(Most well-managed-firm are profitable-company)
                               :memb-k-2  '(Apple is an innovator)
                               :quant-k-2 '(Some innovator are profitable-company)))

(setf problem-5 (make-instance 'p-problem      
                               :pquest    '(Hillary is elected-president)
                               :memb-k-1  '(Hillary is a famous-politician)
                               :quant-k-1 '(Most famous-politician are elected-president)
                               :memb-k-2  '(Hillary is a skilled-politician)
                               :quant-k-2 '(Most skilled-politician are elected-president)))

(setf problem-6 (make-instance 'p-problem
                               :pquest    '(Biden is elected-president)
                               :memb-k-1  '(Biden is a warm-politician)
                               :quant-k-1 '(Some warm-politician are elected-president)
                               :memb-k-2  '(Biden is a gaffe-ridden-speaker)
                               :quant-k-2 '(Most gaffe-ridden-speaker are not elected-president)))

; (compound-problem problem-5 problem-1 3 1-given-5) because bga-member is nil
(setf 1-given-5 (make-instance 'relational-knowledge
                               :a-quest          '(Apple is a profitable-company)
                               :b-quest          '(Hillary is elected-president)
                               :a-top            '(Apple)
                               :b-top            '(Hillary)
                               :abpos               t 
                               :a-bpos              t
                               :-abpos              t
                               :-a-bpos             t
                               :bga-member         nil
                               :bga-quantified     nil
                               :bga-relation       nil))

; The next two examples illustrates cases in which a logical relation holds between a-quest and b-quest
(setf 5-given-6 (make-instance 'relational-knowledge                      
                               :a-quest            '(Hillary is elected-president)             
                               :b-quest            '(Biden is elected-president)
                               :abpos                nil 
                               :a-bpos               t
                               :-abpos               t
                               :-a-bpos              t
                               :bga-member         nil
                               :bga-quantified     nil
                               :bga-relation       nil)) 

(setf 6-given-5 (make-instance 'relational-knowledge
                               :a-quest            '(Biden is elected-president)             
                               :b-quest            '(Hillary is elected-president)           
                               :abpos                nil 
                               :a-bpos               t
                               :-abpos               t
                               :-a-bpos              t
                               :bga-member         nil
                               :bga-quantified     nil
                               :bga-relation       nil)) 

(setf problem-7 (make-instance 'p-problem
                               :pquest    '(Tactical-nuclear-weapon is used)
                               :memb-k-1  '(Tactical-nuclear-weapon is available-weapon)
                               :quant-k-1 '(Most available-weapon are used)
                               :memb-k-2  '(Tactical-nuclear-weapon is untested-weapon)
                               :quant-k-2 '(No untested-weapon are used)))

(setf problem-8 (make-instance 'p-problem
                               :pquest    '(society is less-terrorist)
                               :memb-k-1  '(Society is better-off) 
                               :quant-k-1 '(Most better-off are less-terrorist)
                               :memb-k-2  '(Islamic-terrorism is unpopular)
                               :quant-k-2 '(Some unpopular are less-terrorist)))

;  (compound-problem problem-7 problem-8 3 8-given-7)
(setf 8-given-7 (make-instance 'relational-knowledge
                               :a-quest            '(Tactical-nuclear-weapon is used)             
                               :b-quest            '(Society is less-terrorist)           
                               :abpos                t 
                               :a-bpos               t
                               :-abpos               t
                               :-a-bpos              t
                               :bga-member         '(Society is suffering-nuclear-weapons)
                               :bga-quantified     '(Most suffering-nuclear-weapons are not less-terrorist)
                               :bga-relation       nil)) 

; (compound-problem problem-8 problem-7 3 7-given-8)
(setf 7-given-8 (make-instance 'relational-knowledge
                               :a-quest             '(Society is less-terrorist)            
                               :b-quest             '(Tactical-nuclear-weapon is used)                     
                               :abpos                t 
                               :a-bpos               t
                               :-abpos               t
                               :-a-bpos              t
                               :bga-member         '(Tactical-nuclear-weapon is creator-of-disaster)
                               :bga-quantified     '(Most creator-of-disaster are not used)
                               :bga-relation       nil)) 

;-----------------------------------------------------------------------------------------------------
; Section 10.7: The main program for compound events
;-----------------------------------------------------------------------------------------------------

#| 
Estimates of probabilities of individual problems, pA, from i-jpd calls. Infers relation if any between
A and pB, and if everything is possible then calls appropriate function to estimate p(A and B), p(A or B),
or p(B|A) -- the latter uses the information about relation to modify p(B), or gets it from the user
for user's own problems.  It makes a logical check of the possibilities from the user, and provides
feedback in case of inconsistency. |# 
(defun compound-problem(problem-A problem-B A-relation-pB &optional connective)
  "evaluate-relation calls fn for compound"
  (terpri)(princ "mReasoner's analysis of the probabilities based on the given knowledge:-")
  (let ((a-icon (i-jpd problem-A))(b-icon (i-jpd problem-B))  
        (ab-icon-value (evaluate-relation A-relation-pB)))    
    (if (null connective)   
        (connective (connective A-relation-pB))
      (setf (connective A-relation-pB) connective))
    (setf (program-pa a-relation-pB)(prog-probability a-icon)
          (program-pb a-relation-pB)(prog-probability b-icon))
    (cond((= connective 1)
            (terpri)(terpri)
            (princ "Now, mR will compute the probability of the CONJUNCTION of the two main assertions.")
            (setf (program-AcompoundB A-relation-pB)(conjunction a-icon b-icon)
                  (program-ArationalB A-relation-pB)(multiply-conjunction a-icon b-icon)))
         ((= connective 2)
            (terpri)(terpri)
            (princ "Now, mR will compute the probability of the DISJUNCTION of the two main assertions.")
            (setf (program-AcompoundB A-relation-pB)(conjunction a-icon b-icon)
                  (program-ArationalB A-relation-pB)(disjunction a-icon b-icon))) 
         ((= connective 3)
            (terpri)(terpri)(princ "Now, mR will compute the probability that")
            (print-string (p-question problem-B))(princ "given that")
            (print-sentence (p-question problem-A) ".")(terpri)
            (if (symbolp ab-icon-value)(setf ab-icon-value (anchor-value b-icon))) ; in case ab-icon-value is T
            (setf (program-AcompoundB A-relation-pB)(conditional-probability ab-icon-value b-icon))
            (setf (program-ArationalB A-relation-pB) nil))
         (t (terpri)(princ "Alas, you have selected a non-existent compound relation.")))
    (feedback A-relation-pB)
    a-relation-pB))

(defmethod evaluate-relation((r-knowledge relational-knowledge))
  "calls jpd-step-one to rtn belief icon"
  (let ((b-prop (b-question r-knowledge))(memberbga (bgivena-member r-knowledge))
        (quantified-assn (bgivena-quantified r-knowledge)))
    (if (and memberbga quantified-assn)
        (jpd-step-one (parse b-prop)(parse memberbga) quantified-assn)
      'error)))

(defun conjunction (a-icon b-icon)
    "updates a-icon with pointer from b-icon, splits the difference, and makes numerical estimate too" 
    (let (numeral)
    (terpri)(princ "Most people split the difference, and the two degrees of belief on single icon are: ")
    (setf (anchor-value a-icon)(update-anchor a-icon (anchor-value b-icon)))
    (terpri)(terpri)(i-print-pointer (anchor-value a-icon))(terpri)
    (setf (anchor-value a-icon)(p-averages (anchor-value a-icon)))
    (princ "The result is: ")(terpri)
    (i-print-pointer (anchor-value a-icon))
    (terpri)(princ "A corresponding numerical estimate is ")
    (princ (setf numeral (numerical-probability (anchor-value a-icon))))
    numeral))

(defun multiply-conjunction(a-icon b-icon)
  "p-multiplies of p(A) by p(B)"
  (let (ab-icon numeral)
    (setf ab-icon (make-instance 'icon
                                 :belief nil
                                 :anchor nil
                                 :pointer nil))
    (terpri)
    (princ "A more rational estimate multiplies the probabilities, and is ")
    (setf (anchor-value ab-icon) 
        (p-divides (p-multiplies (anchor-value a-icon)(anchor-value b-icon)) standard-icon))
    (princ (i-intuitive-probability ab-icon))(princ ".")
    (terpri)(princ "A corresponding numerical estimate is ")  
    (princ (setf numeral (numerical-probability (anchor-value ab-icon))))
    numeral))

(defun disjunction (a-icon b-icon)
  "adds up probabilities to a maximum of certainty"
  (let (ab-icon numeral)
    (setf ab-icon (make-instance 'icon
                                 :belief nil
                                 :anchor nil
                                 :pointer nil))
    (terpri)(princ "A more rational estimate adds the probabilities, and is ")
    (setf (anchor-value ab-icon) 
        (p-adds (anchor-value a-icon)(anchor-value b-icon)))
    (princ (i-intuitive-probability ab-icon))(princ ".")
    (terpri)(princ "A corresponding numerical estimate is ")  
    (princ (setf numeral (numerical-probability (anchor-value ab-icon))))
    numeral))

(defun conditional-probability (ab-icon-value b-icon)
  "uses value of ab-icon to adjust value of b-icon"
  (let ((b-val (anchor-value b-icon)) numeral)
  (cond((> (length ab-icon-value)(length b-val))
                 (princ "The model above shows that the given condition increases the probability:")
                 (setf (anchor-value b-icon)(p-adds b-val '(_))))
       ((< (length ab-icon-value)(length b-val))
                 (princ "The model above shows that the given condition decreases the probability:")
                 (setf (anchor-value b-icon)(p-subtracts b-val '(-))))
       (t (princ "No knowledge shows that the given condition affects the probability:")))
  (terpri)(i-print-pointer b-val)(terpri)
  (princ "Namely, it's ")
  (princ (i-intuitive-probability b-icon))(princ ".")
  (terpri)(princ "A corresponding numerical estimate is ")  
  (princ (setf numeral (numerical-probability (anchor-value b-icon))))
   numeral))

;----------------------------------------------------------------------------------
; Section 10.8: System 1 constructs icons
;----------------------------------------------------------------------------------

#| To run this code, evaluate, e.g.:
    (jpd problem-1)
Four sorts of relations between probability question and quantified knowledge, at present: 
1. Probability(a is B)?, where a is an A, and knowledge is (quant A is B),
which is the preceding example.
2. Probability(a is not B)?, where a is an A, and knowledge is (quant A is not B)
3. Probability(a is B)?, where a is an A, and knowledge is (quant A is not B)
4. Probability(a is not B)?, where a is an A, and knowledge is (quant A is B).
 
The top-level function, jpd, takes as input a problem, which is 
an instance of a class.  Implemented, so far, for problems concerning the probability
of a single atomic proposition, A, or its negation (not A). In due course, problems 
will be more complicated, including: p(A & B), p(A / B).  
|#

(defparameter standard-icon '(_ _ _ _ _ _ _ _))

;defines class of icons that have two pointers - anchor and pointer-2
(defclass icon()
  ((belief-content      :accessor belief-content   :initarg :belief)
   (anchor              :accessor anchor-value     :initarg :anchor)
   (pointer             :accessor pointer-value    :initarg :pointer)
   (prog-probability    :accessor prog-probability :initarg :prob)))

(defmethod i-jpd ((problem p-problem))
  "computes intuitive jpd for probability of atomic proposition"
  (let ((prob-intens-A (parse (p-question problem)))
        (icon-1 nil)
        (member-k-1-intens (parse (member-k-1 problem)))
        (member-k-2-intens (parse (member-k-2 problem)))
        (anch nil)(pointer nil))
    (terpri)(terpri)(princ "What is the probability that")(print-sentence (p-question problem) "?")
    (setf icon-1 (make-instance 'icon
                          :belief  prob-intens-A
                          :anchor  nil
                          :pointer nil
                          :prob    nil))
    (setf (anchor-value icon-1) (jpd-step-one prob-intens-A member-k-1-intens (quantif-k-1 problem)))
    (i-print-intermediate icon-1 (quantif-k-1 problem))
    (setf pointer (jpd-step-one prob-intens-A member-k-2-intens (quantif-k-2 problem)))
    (setf (anchor-value icon-1) (update-anchor icon-1 pointer))
    (i-print-intermediate icon-1 (quantif-k-2 problem)) 
    (setf (anchor-value icon-1)(p-averages (anchor-value icon-1)))
    (i-print-intermediate icon-1)
    (setf numeral (numerical-probability (anchor-value icon-1))
          (prog-probability icon-1) numeral)
    (terpri)(princ "Numerical estimate is ")(princ numeral)
    icon-1))

(defun update-anchor(icon pointer)
  "takes anchor such as (- -) and pointer (- - - -) and rtns update icon of both"
  (let ((anchor (anchor-value icon)))
    (if (p-longer anchor pointer)
        (up-anch anchor pointer 'v)
      (up-anch pointer anchor '^))))   

(defun up-anch(longer shorter mark)
  "inserts mark ('^ or 'v) in icon in position of pointer or anchor"
   (cond((null longer) nil)
        ((null (rest shorter))(cons mark (rest longer)))
        (t (cons (first longer)(up-anch (rest longer)(rest shorter) mark)))))
  
#| 
Probabilities of single propositions: a is a B, and a is not a B.  
Four cases:            
1. Both affirmative:  p(a is B)? (Quant A are B) Example:
     (jpd-step-one (parse '(Apple is a profitable-company))
                   (parse '(Apple is a well-managed-firm))
                   '(most well-managed-firm are profitable-company))
     calls extract-probability => (_ _ _ _ _ _)  
2. Double negative
     p(a is not B)? (quant A are not B) Example: 
     (jpd-step-one (parse '(Apple is not a profitable-company))
                   (parse '(Apple is a poorly-managed-firm))
                   '(most poorly-managed-firm are not profitable-company))
     calls extract-probability (negate probability-object) => (_ _ _ _ _ _)
3. Negative quant:    
      p(a is B)(quant A are not B) Example:
      (jpd-step-one (parse '(Apple is a profitable-company))
                    (parse '(Apple is a poorly-managed-firm))
                    '(most poorly-managed-firm are not profitable-company))
      calls comp-probability (negate probability-object) => (_ _)
4. Negative p-intens:
      p(a is not B)(Quant A are B) Example:
      (jpd-step-one (parse '(Apple is not a profitable-company))
                    (parse '(Apple is a well-managed-firm))
                    '(most well-managed-firm are profitable-company))
      calls comp-probability => (_ _) |#
(defun jpd-step-one (probability-intension k-member-intension quantified-assertion)
  (let* ((probability-object (object probability-intension))
         (p-predicate-polarity (negative-intension probability-intension))
         (k-member-object (object k-member-intension))
         (icon nil)
         (quantified-intension (parse quantified-assertion))
         (q-predicate-polarity (negative-intension quantified-intension))
         (quantified-model (first (build-model quantified-intension))))
    (terpri)(princ "Knowledge yields the model of the following possibilities: ")(terpri)
    (print-model quantified-model)(terpri)
    (cond((and
            (equal (subject probability-intension)(subject k-member-intension))
            (equal k-member-object (subject quantified-intension)))
          (if q-predicate-polarity                                   ; sets object to negative
              (setf probability-object (negate probability-object)))
          (if (or (and (not p-predicate-polarity)(not q-predicate-polarity)) ; the four cases
                  (and p-predicate-polarity q-predicate-polarity))
              (first (extract-probability k-member-object probability-object quantified-model))
            (first (comp-probability k-member-object probability-object quantified-model)))))))

#| To cope with quantified knowledge assertions containing negations, such as, 
      Most A are not B.
      Fred is A.
The negative-intension triggers a call to comp-probability, which
asssesses probability of not-B in model of the quantified assertion,
and returns the complementary probability for p(B). |#
(defun comp-probability (k-member-object probability-object quantified-model)
  (let* ((comp-prob (extract-probability k-member-object probability-object quantified-model))
         (numerator (first comp-prob))
         (denominator (second comp-prob)))
    (list (p-subtracts denominator numerator) denominator)))

;-------------------------------------------------------------------------------------------
; Section 10.9: System 1 computes intuitive probability from a quantified model
;-------------------------------------------------------------------------------------------

#|
In model, for each individual satisfying object-member and object-prob, add '_ to 
numerator-icon and add '_ to denominator-icon; for each individual satisfying object-member 
but not object-prop add '_ to denominator-icon: '((_ _ _) (_ _ _ _)).  Return as icon 
with denominator of 8: (_ _ _ _ _ _)
To ensure that extract-probability applies only to q-models (quantified models)
it's a method 
                       object of            object of           model of
                       membership-assertion probability         quantified-assertion
                       in knowledge         question            in knowledge    
                            |                   |                     |   
(extract-probability '(well-managed-firms) '(profitable-company) q-mod-test)
    => ((_ _ _ _ _ _) (_ _ _ _ _ _ _ _)) |#
(defmethod extract-probability (object-member object-prob (model q-model))
  (let((numerator-icon nil)(denominator-icon nil))
    (loop for indiv in (individuals model)
           do (if (memberlis object-member indiv)
                  (if (memberlis object-prob indiv)
                      (setf numerator-icon (cons '_ numerator-icon))
                    (setf denominator-icon (cons '_ denominator-icon)))))
  (setf denominator-icon (append numerator-icon denominator-icon))
    (p-ratio-conversion numerator-icon denominator-icon)))

; The verbal ordinal scale from impossible to certain
(setf i-standard-scale '(impossible almost-impossible highly-improbable improbable 
                     as-likely-as-not probable highly-probable almost-certain certain))

(defmethod i-intuitive-probability ((b-icon icon))
  "retrieves val-lis from icon and rtns its intuitive probility up to position of any marker"
  (let ((val-lis (anchor-value b-icon))(mark-lis nil)
        (probs-lis i-standard-scale))
   (if (or (setf mark-lis (left-member 'v val-lis))
           (setf mark-lis (left-member '^ val-lis)))
       (setf val-lis mark-lis))
   (intuit-prob val-lis)))

(defun intuit-prob(val-lis)
  "uses val-lis to measure off standard-scale to return appropriate verbal label"
  (let ((probs-lis nil))
    (setf probs-lis i-standard-scale)
   (s1-dolist (itm val-lis)
     (setf probs-lis (rest probs-lis)))
   (first probs-lis)))

;--------------------------------------------------------------------------------
; Section 10.10: System 1 does non-recursive arithmetic, i.e., no numbers or counting
;--------------------------------------------------------------------------------

#| Primitive arithmetic is less powerful than a finite-state automaton. It uses
'prisoner's notation', e.g., '(_ _ _ _ _ _), and no counting or other standard
arithmetic operations. No loop of operations, carried out by s1-dolist, can 
iterate for more than *max-iterations*, currently set to 8. 
  There are three basic functions:
 extract-probability [from mental model] calls:
  p-ratio-conversion     -- converts ratio to one with base = standard icon, called by
                              extract-probability
     p-divides           -- divides larger magnitude by smaller
         p-longer        -- rtns t if first is longer or equal in length to second
         p-subtracts     -- subtracts shorter icon from longer, i.e., no neg numbers  
     p-multiplies        -- multiplies two magnitudes
         p-adds          -- two icons with a maximum length of the standard icon
         p-subtracts     -- see above

i-jpd, and conjunction, call:   
 p-averages              -- moves two pointers together by joining or removing a grain
     p-near-equal        -- rtns t if two are equal or longer if within one grain of equal
         p-longer        -- see above         

or calls:
 conjunction            -- see above
 disjunction
   p-adds               -- see above

cond prob calls:
  p-adds                -- if given condition increases probability
  p-subtracts           -- if given condition decreases probability
                        -- otherwise rtns pB without change.
 |#

; A wrapper macro for limiting number of operations in dolist to *max-iterations*
(defmacro s1-dolist ((var list &optional result) &body body)
  `(let ((iterations 0))
     (block loop
       (progn
         (mapc #'(lambda (,var) (if (>= iterations *max-iterations*)
                                    (return-from loop ,result)
                                  (progn (incf iterations) ,@body)))
               ,list)
         (let ((,var nil))
           ,result)))))

;;; A global variable setting limit on s1-dolist, so s1-dolist on a list
;;; greater than 8 will iterate only up to 8
(setf *max-iterations* 8)

#| Primitive conversion of ratio to base = standard icon, called by
   extract-probability, e.g.
; (p-ratio-conversion '(_ _ _) '(_ _ _ _)) =>
; ((_ _ _ _ _ _) (_ _ _ _ _ _ _ _)) where standard is (_ _ _ _ _ _ _ _) |#
(defun p-ratio-conversion(icon-1 icon-2)
  (let ((factor (p-divides standard-icon icon-2)))
    (list (p-multiplies icon-1 factor)(p-multiplies icon-2 factor))))

(defun p-averages(icon)
  "moves rh end of icon and pointer towards each other until they meet of differ by one grain"
  (let (outicon (left-icon (left-member 'v icon))(right-icon (left-member '^ icon)))
   (if (null left-icon)
       (setf short-icon (n-replace right-icon))  ; eg short-icon '(_ _ _) icon =  '(_ _ ^ _)
     (setf short-icon (n-replace left-icon)))    ;               '(_ _ _) icon =  '(_ _ v _)
   (setf icon (n-replace icon))
     (s1-dolist (i icon outicon)
       (cond((p-near-equal short-icon icon)(setf outicon short-icon))
            (t (setf short-icon (cons '_ short-icon))
               (setf icon (rest icon)))))
     outicon))

; if icon is (v _ _ _) then take split between (v) (_ _ _ _)
(defun left-member(atm lis)
  "if member atm lis then rtns lis on left of atm"
  (reverse (member atm (reverse lis))))

(defun n-replace(lis)
  "non-destructive replaces any pointer with '_"
  (let ((outlis nil))
  (s1-dolist (i lis outlis)
    (if (not (equal i '_))
        (setf outlis (cons '_ outlis))
      (setf outlis (cons i outlis))))
   outlis))

(defun p-multiplies (icon-1 icon-2)
  "multiplication based on addition and subtraction"
  (let ((out-icon nil))
    (s1-dolist (ico-num icon-1 icon-2)
      (cond((not (null icon-1))
               (setf out-icon (p-adds icon-2 out-icon))
               (setf icon-1 (p-subtracts icon-1 '(-))))))
    out-icon))

(defun p-divides (numerator denominator)
; progressively p-subtracts denominator from numerator, adding _ to output
; each time, until numerator is shorter than denominator. Division by zero
; returns unchanged denominator, i.e., treated as division by 1
; (p-divides '(_ _ _ _) '(_ _ _) => (_)
  (let ((out-icon nil))
    (s1-dolist (ico-num numerator out-icon)
        (cond ((not (or (null numerator)
                       (and (p-longer denominator numerator)(null (p-longer numerator denominator)))))
               (setf numerator (p-subtracts numerator denominator))
               (setf out-icon (cons '_ out-icon)))))
    out-icon))

(defun p-adds(icon-1 icon-2)
  "adds two icons together but does not exceed standard-icon, for excl disjunctions"
  (let ((out-ico icon-2))
    (s1-dolist (ico icon-1 out-ico)
      (setf out-ico (cons ico out-ico)))
  (if (p-subtracts standard-icon out-ico)
      out-ico
    standard-icon)))

(defun p-subtracts(icon-1 icon-2)
  "rtns icon-1 minus icon-2, if icon-1 =< icon-2 rts nil
   i.e., there are no negative numbers in p-subtracts"
  (s1-dolist (ico icon-2)(setf icon-1 (rest icon-1)))
  icon-1)

(defun p-near-equal(icon-1 icon-2)
  "rtns t if equal, the longer icon if they differ by one grain, nil if larger difference"
  (cond((and (p-longer icon-1 icon-2)
             (p-longer icon-2 icon-1))           t)
       ((and (p-longer icon-1 icon-2)
             (p-longer (cons '_ icon-2) icon-1)) icon-1)
       ((and (p-longer (cons '_ icon-1) icon-2)
             (p-longer icon-2 icon-1))           icon-2)))
   
(defun p-longer(icon-1 icon-2)
  "rtns T iff icon-1 is longer than or equal to icon-2 up to 8 iterations"
  (s1-dolist (ico icon-1)
    (if (equal ico (first icon-2))
        (setf icon-2 (rest icon-2))))
  (if icon-2
      nil
    t))
 
;------------------------------------------------------------------------------
; Section 10.11: System 2 for assigning numbers to intuitive probabilities
;------------------------------------------------------------------------------

#| returns a percentage probability based on the icon with component of small
   amount of random noise |#

(defun numerical-probability(icon)
  "returns numerical value for an icon plus a small component of random noise"
  (let* ((num (list-length icon)))
    (cond((= num 0) 0)
         ((= num (length standard-icon)) 100.0)
         (t (noise (* (/ num (length standard-icon)) 100.0))))))

(defun noise(num)
  "introduces small % of noise always added to the % probability"
  (let ((ran (random 6)))
    (+ num ran)))

;------------------------------------------------------------------------------
; Section 10.12: Feedback about actual JPD from program's and user's estimates
;------------------------------------------------------------------------------

(defmethod feedback((a-relates-b relational-knowledge))
  "gives feedback about jpd and its violations, if any"
  (let ((A (a-question a-relates-b))(B (b-question a-relates-b))
        (connective  (connective a-relates-b))
        (user-prob-A (user-pA a-relates-b))
        (prog-prob-A (program-pA a-relates-b))
        (ab   (abpossible a-relates-b))
        (a-b  (a-bpossible a-relates-b))
        (-ab  (-abpossible a-relates-b))
        (-a-b (-a-bpossible a-relates-b)))
  (terpri)(princ "The estimates were of three probabilities:")(terpri)
  (princ " A:")(print-sentence A ".")(terpri)
  (princ " B:")(print-sentence B ".")(terpri)
  (princ " And their compound: ")
  (cond((= connective 1)(princ "A and B."))
       ((= connective 2)(princ "A or B."))
       ((= connective 3)(princ "Assuming A; the probability of B.")))
  (terpri)
  (cond( user-prob-A
         (princ "Here's an analysis of your estimates.")
         (feed user-prob-A (user-pB a-relates-b)(user-pAcompoundB a-relates-b) connective))) 
  (princ "Here's analysis of the program's estimates.")
  (if prog-prob-A
       (feed prog-prob-A
            (program-pB a-relates-b)
            (program-AcompoundB a-relates-b)
             connective
            (program-ArationalB a-relates-b))
    (princ "For some reason, the program made no estimates."))
  (feedback-from-logic ab a-b -ab -a-b)  t))
                           
(defun feed(pA pB pAcompoundB connective &optional pArationalB)
  "calls assess-probabilities for either user's or program's estimates, latter 
   may include a more rational estimate of the compound"
  (setf pA (to-decimal pA) pB (to-decimal pB) pAcompoundB (to-decimal pAcompoundB))
  (terpri)(princ "Converting from percentages, the estimates were:")(terpri) 
  (princ " Probability of A: ")(princ pA)(terpri)
  (princ " Probability of B: ")(princ pB)(terpri)
  (princ " Probability of compound: ")(princ pAcompoundB)(terpri)
  (assess-probabilities pA pB pAcompoundB connective)
  (cond ( pArationalB
          (setf pArationalB (to-decimal pArationalB))
          (princ " The program's more rational probability of compound: ")
          (princ pArationalB)(terpri)
          (assess-probabilities pA pB pArationalB connective))))
 
(defun to-decimal(percent)
  (/ percent 100.0))

(defun assess-probabilities(pA pB pAcompoundB connective)
  "assesses values and violations of the jpd"
  (let (output n-of-violations)
    (cond((= connective 1)(setf output (p-of-conjunction pA pB pAcompoundB)))
         ((= connective 2)(setf output (p-of-disjunction pA pB pAcompoundB)))
         ((= connective 3)(setf output (conditional-p    pA pB pAcompoundB))))
    (setf n-of-violations (frequency-of-neg-nums output))
    (princ "These estimates yield")
    (cond((= n-of-violations 0)
            (princ " a consistent joint probability distribution:"))
         ((= n-of-violations 1)
            (princ " one violation -- a negative probability -- in the joint probability distribution:"))
         ((= n-of-violations 2)
            (Princ " two violations -- negative probabilities -- in the joint probability distribution:")))
    (terpri)
    (princ " pA&B   = ")(format t "~$" (first  output))(terpri)
    (princ " pA&-B  = ")(format t "~$" (second output))(terpri)
    (princ " p-A&B  = ")(format t "~$" (third  output))(terpri)
    (princ " p-A&-B = ")(format t "~$" (fourth output))(terpri)
    output))

(defun feedback-from-logic(ab a-b -ab -a-b)
  "Prints status of probabilities from set of possibilities concerning relation, rtns t only for
   subsequent computation of probabilities"
  (terpri)(princ "The responses to the questions, if any, about necessity and possibility ")
    (cond((and ab a-b -ab -a-b)(princ "are consistent with your judgments"))
         ((and ab a-b -ab     )(princ "imply that p-A&-B = 0"))
         ((and ab     -ab -a-b)(princ "imply that pA&-B = 0"))
         ((and ab a-b     -a-b)(princ "imply that p-A&B = 0"))
         ((and    a-b -ab -a-b)(princ "imply that pA&B = 0"))
         ((and ab a-b         )(princ "imply that pA = 1.0"))
         ((and ab     -ab     )(princ "imply that pB = 1.0"))
         ((and ab         -a-b)(princ "imply that pA&B or else p-A&-B = 1.0"))
         ((and    a-b -ab     )(princ "imply that pA&-B or else p-A&B = 1.0"))
         ((and    a-b     -a-b)(princ "imply that pB = 0"))
         ((and        -ab -a-b)(princ "imply that pA = 0"))
         (     ab              (princ "imply that pA&B = 1.0"))
         (        a-b          (princ "imply that pA&-B = 1.0"))
         (            -ab      (princ "imply that p-A&B = 1.0"))
         (                -a-b (princ "imply that p-A&-B = 1.0"))
         (t                    (princ "imply that nothing is possible."))))

(defun frequency-of-neg-nums(lis)
  "counts frequency of negative numbers in list of jpd values"
  (cond((null lis) 0)
       ((< (first lis) 0.0)(+ 1 (frequency-of-neg-nums (rest lis))))
       (t (frequency-of-neg-nums (rest lis)))))

(defun p-of-conjunction (pA pB pA&B)    
  "Computes jpd from the three probabilities;  list the jpd in the order below"
  (list pA&B                                                   ; pA&B             
        (- pA pA&B)                                            ; pA&-B
        (- pB pA&B)                                            ; p-A&B
        (- 1.0 (+ pA pB (- pA&B)))))                           ; p-A&-B

(defun p-of-disjunction (pA pB pAvB)
  "Computes jpd from the three probabilities"
  (let*((p-A&-B (- 1 pAvB))(p-A&B (- (- 1 pA) p-A&-B))
        (pA&B (- pB p-A&B))(pA&-B (- pA pA&B)))
    (list pA&B pA&-B p-A&B p-A&-B)))

(defun conditional-p (pA pB pB/A)
  "Computes jpd from the three probabilities"
  (list
   (* pA pB/A)                                                 ; pA&B
   (- pA (* pA pB/A))                                          ; pA&-B
   (- pB (* pA pB/A))                                          ; p-A&B
   (- 1 (+ (* pA pB/A)(- pA (* pA pB/A))(- pB (* pA pB/A)))))) ; p-A&-B 

;--------------------------------------------------------------------------------
; Section 10.13: Functions for printing outputs
;--------------------------------------------------------------------------------

(defun i-print-intermediate (icon &optional quantif-k)
  "prints state of play after second piece of evidence before its amalgamation"
  (terpri)
  (cond( quantif-k
         (princ "which implies an intuitive probability shown by the icon or the pointer: ")
         (terpri)(i-print-pointer (anchor-value icon))(terpri)
         (princ "Namely, it's "))
       (t (princ "So, splitting the difference between the pointer and icon yields:")
          (terpri)(terpri)(i-print-pointer (anchor-value icon))(terpri)
          (princ "And it's ")))
  (princ (i-intuitive-probability icon))(princ ".") t)

(defun print-icon(icon)
  (let ((standard standard-icon))
    (princ "   ")(princ "|")
    (s1-dolist (ico icon)(princ '_)(setf standard (rest standard)))
    (s1-dolist (ico standard)(princ " "))
    (princ "|")(princ " ")))

(defun i-print-pointer(icon)
  "prints icon including pointer"
  (let ((standard standard-icon))
    (princ "   ")(princ "|")
    (s1-dolist (ico icon)(princ ico)(setf standard (rest standard)))
    (s1-dolist (ico standard)(princ " "))
    (princ "|")(princ " ")))

(defun print-string (lis)
  "prints list as a string"
  (princ (concatenate 'string (atm-to-string (car lis))(conversion (cdr lis)))))

(defun print-sentence(lis punct)
  "prints lis as sentence with initial capital letter and final punctuation, such as ?, :, or ' '"
  (princ (concatenate 'string (string-capitalize (atm-to-string (first lis)))
                      (conversion-punct (rest lis) punct)))) 

(defun conversion-punct(lis &optional punct)
  "conversts a list into a string with, or without, final punctuation"
  (cond((null lis) punct)
       (t (concatenate 'string (atm-to-string (car lis))(conversion-punct (cdr lis) punct)))))

(defun conversion(lis)
  "converts list into a string"
  (cond((null lis) " ")
       (t (concatenate 'string (atm-to-string (car lis))(conversion (cdr lis))))))
                        
(defun atm-to-string(atm)
  "turn atom into a string with space at end"
  (concatenate 'string " " (string-downcase (symbol-name atm))))

; END OF FILE




