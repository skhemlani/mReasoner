; ---------------------------------------------------------------------------------
; Part 11: System diagnostics
; ---------------------------------------------------------------------------------

; Section 11.1: System diagnostics
; Section 11.2: Syllogism diagnostics
; Section 11.3: Immediate inference diagnostics
; Section 11.4: Consistency diagnostics
; Section 11.5: Running experiments and parameter searching
; Section 11.6: Pre-defined experiments

; ---------------------------------------------------------------------------------
; Section 11.1: System diagnostics
; ---------------------------------------------------------------------------------

(defun run-diagnostics ()
  (when (stochastic-enabled?)
    (format t "~50@<Disabled stochastic mode.~>~%")
    (disable-stochastic-mode))
  (format t "~50@<Checking syllogism predictions...~>")
  (format t "~10<~A~>~%" (run-syllogism-diagnostics))
  (format t "~50@<Checking immediate inference predictions...~>")
  (format t "~10<~A~>~%" (run-immediate-inference-diagnostics))
  (format t "~50@<Checking immediate inference most predictions ...~>")
  (format t "~10<~A~>~%" (run-immediate-inference-most-diagnostics))
  (format t "~50@<Checking consistency predictions...~>")
  (format t "~10<~A~>~%" (run-consistency-diagnostics))
  (when (stochastic-enabled?)
    (format t "~50@<Re-enabled stochastic mode.~>~%")
    (enable-stochastic-mode)))

; ---------------------------------------------------------------------------------
; Section 11.2: Syllogism diagnostics
; ---------------------------------------------------------------------------------

(defvar +syl-preds-v0.5+
  '(("Aac") ("Iac" "NVC") ("Eac") ("Oac" "NVC") ("Iac") ("Iac" "NVC") ("Eac" "Oac" "NVC")
    ("Oac" "NVC") ("Eac" "NVC") ("Eac" "NVC") ("Eac" "NVC") ("Oac" "NVC") ("Oac" "NVC")
    ("Oac" "NVC") ("Oac" "NVC") ("Oac" "NVC") ("Aac" "Aca" "Iac" "NVC") ("Iac" "Ica")
    ("Eca" "Eac" "Oac" "NVC") ("Oca" "NVC") ("Ica" "Iac" "NVC") ("Ica" "Iac" "NVC")
    ("Eca" "Eac" "Oac" "NVC") ("Oca" "NVC") ("Eac" "Eca") ("Eca" "Eac" "Oca" "NVC")
    ("Eca" "Eac" "NVC") ("Oca" "NVC") ("Oca" "NVC") ("Oca" "NVC") ("Oca" "NVC") ("Oca" "NVC")
    ("Aac" "NVC") ("Ica" "Iac" "NVC") ("Eac" "Eca") ("Oca") ("Iac" "Ica" "NVC") ("Iac" "NVC")
    ("Eca" "Eac" "Oac" "NVC") ("Oca" "NVC") ("Eca" "Eac") ("Eac" "Eca" "Oca" "NVC") ("Eac" "NVC")
    ("Oca" "NVC") ("Oac") ("Oac" "NVC") ("Oac" "NVC") ("Oac" "NVC") ("Aac" "Iac" "NVC") ("Ica" "Iac")
    ("Eac" "Eca" "Oac" "NVC") ("Oac") ("Ica" "Iac") ("Iac" "NVC") ("Eac" "Oac" "NVC") ("Oac" "NVC")
    ("Eac" "Eca" "Oca" "NVC") ("Eac" "NVC") ("Eac" "NVC") ("Oac" "NVC") ("Oca") ("Oca" "NVC")
    ("Oca" "NVC") ("Oac" "NVC")))

(defun run-syllogism-diagnostics (&key (v nil))
  "Compares current model's predictions with predictions of this model (as of 3-25-2011), when
   syllogistic reasoning component was finalized. The fn checks to make sure that all current
   predictions are the same as the previous model's, and when they're not, prints an error."
  (when v
    (format t "---------------------------------------------------------------------------------------------------------~%~
                 ~11,A~23,A~23,A~14,A~%~
                 ---------------------------------------------------------------------------------------------------------~%"
            "Syllogism" "Current predictions" "3-25-11 Predictions" "Diagnostic"))
  (let ((i 0)
        (ab (list Aab Iab Eab Oab))
        (ba (list Aba Iba Eba Oba))
        (bc (list Abc Ibc Ebc Obc))
        (cb (list Acb Icb Ecb Ocb))
        major minor)
    (dotimes (fig 4)
      (case (+ fig 1)
        (1 (setf major ab) (setf minor bc))
        (2 (setf major ba) (setf minor cb))
        (3 (setf major ab) (setf minor cb))
        (4 (setf major ba) (setf minor bc)))
      (dolist (p1 major)
        (dolist (p2 minor)
          (when (not (or (diagnose-syllogism (list p1 p2) (nth i +syl-preds-v0.5+) :v v)
                         t))
              (return-from run-syllogism-diagnostics "[FAILED]"))
          (incf i))))
    (when (not v) "[OK]")))

(defun diagnose-syllogism (premises old-predictions &key (v t))
  (let* ((premise1      (first premises))
         (premise2      (second premises))
         (heuristic-conclusions (what-follows? premises))
         (final-conclusions     (what-follows? premises :system2 t))
         counterexamples predictions new-matches-old-p)

    (setf predictions (remove-duplicates (append heuristic-conclusions final-conclusions) :test #'equals))
    (setf predictions (mapcar #'abbreviate predictions))

    (if (or (not final-conclusions)
            (set-difference final-conclusions heuristic-conclusions :test #'equals))
        (setf predictions (append predictions (list "NVC"))))

    ; (when (equal old-predictions '("Oac")) (setf predictions '("Mca" "Eca" "Xac")))
    ; Uncomment this line to see how the diagnostic

    (setf new-matches-old-p (not (append (set-difference predictions old-predictions :test #'string-equal)
                                         (set-difference old-predictions predictions :test #'string-equal))))

    (if v
        (format t "~3,A~8,A~23,A~23,A~14,A~%"
                (abbreviate premise1)
                (abbreviate premise2)
                (format nil "~{~A~#[~:;, ~]~}" predictions)
                (format nil "~{~A~#[~:;, ~]~}" old-predictions)
                (if new-matches-old-p "" "Error")))
    new-matches-old-p))

; ---------------------------------------------------------------------------------
; Section 11.3: Immediate inference diagnostics
; ---------------------------------------------------------------------------------

(defvar *imm-preds-v0.9*
  '(("Y" "Y") ("Y" "Y") ("N" "N") ("N" "N") ("N" "Y") ("Y" "Y") ("N" "N") ("N" "Y")
    ("N" "N") ("N" "N") ("Y" "Y") ("Y" "Y") ("N" "N") ("N" "Y") ("N" "Y") ("Y" "Y")
    ("N" "Y") ("Y" "Y") ("N" "N") ("N" "Y") ("N" "Y") ("Y" "Y") ("N" "N") ("N" "Y")
    ("N" "N") ("N" "N") ("Y" "Y") ("Y" "Y") ("N" "Y") ("N" "Y") ("N" "Y") ("N" "Y")))

(defun run-immediate-inference-diagnostics (&key (v nil))
  "Compares current model's predictions with predictions of this model (as of 3-25-2011), when
   syllogistic reasoning component was finalized. The fn checks to make sure that all current
   predictions are the same as the previous model's, and when they're not, prints an error."
  (when v
    (format t "--------------------------------------------------------------------------~%~
                 ~15,A~15,A~7,A~7,A~20,A~20,A~%~
                 --------------------------------------------------------------------------~%"
            "Premise" "Conclusion" "Nec?" "Poss?" "06-03-11 Preds" "Diagnostic"))
  (let ((i 0)
        (ab (list Aab Iab Eab Oab))
        (ba (list Aba Iba Eba Oba))
        major minor)
    (dotimes (fig 2)
      (case (+ fig 1)
        (1 (setf major ab) (setf minor ab))
        (2 (setf major ab) (setf minor ba)))
      (dolist (p1 major)
        (dolist (p2 minor)
          (when (not (diagnose-immediate (list p1 p2) (nth i *imm-preds-v0.9*) :v v))
              (return-from run-immediate-inference-diagnostics "[FAILED]"))
          (incf i))))
    (when (not v) "[OK]")))

(defvar *imm-most-preds-v0.99*
  '(("Y" "Y") ("N" "N") ("N" "N") ("N" "N") ("N" "N") ("Y" "Y") ("N" "N") ("N" "N") ("N" "N")
    ("N" "N") ("Y" "Y") ("N" "N") ("N" "N") ("N" "N") ("N" "N") ("Y" "Y") ("N" "Y") ("N" "Y")
    ("N" "N") ("N" "Y") ("N" "Y") ("N" "Y") ("N" "N") ("N" "Y") ("N" "N") ("N" "N") ("Y" "Y")
    ("N" "N") ("N" "Y") ("N" "Y") ("N" "N") ("N" "Y")))

(defun run-immediate-inference-most-diagnostics (&key (v nil))
  "Compares current model's predictions with predictions of this model (as of 2017-06-28), when
   syllogistic reasoning component was finalized. The fn checks to make sure that all current
   predictions are the same as the previous model's, and when they're not, prints an error."
  (when v
    (format t "--------------------------------------------------------------------------~%~
                 ~15,A~15,A~7,A~7,A~20,A~20,A~%~
                 --------------------------------------------------------------------------~%"
            "Premise" "Conclusion" "Nec?" "Poss?" "2017-06-28 Preds" "Diagnostic"))
  (let ((i 0)
        (ab (list Aab Mab Eab Ma-b))
        (ba (list Aba Mba Eba Mb-a))
        major minor)
    (dotimes (fig 2)
      (case (+ fig 1)
        (1 (setf major ab) (setf minor ab))
        (2 (setf major ab) (setf minor ba)))
      (dolist (p1 major)
        (dolist (p2 minor)
          (when (not (diagnose-immediate (list p1 p2) (nth i *imm-most-preds-v0.99*)  :v v))
            (return-from run-immediate-inference-most-diagnostics "[FAILED]"))
          (incf i))))
    (when (not v) "[OK]")))

(defun diagnose-immediate (intensions old-predictions &key (v nil))
  "Fn for diagnosing immediate inference performance for both necessary and possibility tasks.
   If premise = conclusion, true (no need to build model)
   If conclusion is possible given premise, test if conclusion is necessary (engaging sys 2)
   Else necessary is nil and test possible (engaging sys 2)"
  (let* ((intensions  (if (typep (first intensions) 'intension) intensions
                        (mapcar #'parse intensions)))
         (premise     (first intensions))
         (conclusion  (second intensions))
;         (models      (build-model (first intensions)))
         (ab-concl    (abbreviate conclusion))
         nec pos)
   (cond
     ((equals premise conclusion)
      (setf nec "Y") (setf pos "Y"))
     ((possible? (list conclusion) :given (list premise))
      (setf pos "Y")
      (if (necessary? (list conclusion) :given (list premise) :system2 t)
          (setf nec "Y")
        (setf nec "N")))
     (t
      (setf nec "N")
      (if (possible? (list conclusion) :given (list premise) :system2 t)
          (setf pos "Y")
        (setf pos "N"))))
   (if v
       (format t "~15,A~15,A~7,A~7,A~20,A~20,A~%" (abbreviate premise) (abbreviate conclusion)
               nec pos (format nil "~A / ~A" (first old-predictions) (second old-predictions))
               (if (equalp (list nec pos) old-predictions) "" "Error")))
   (equalp (list nec pos) old-predictions)))

; ---------------------------------------------------------------------------------
; Section 11.4: Consistency diagnostics
; ---------------------------------------------------------------------------------

(defvar *imm-con-preds-v0.9*
  '("Y" "Y" "N" "N" "Y" "Y" "N" "Y" "N" "N" "Y" "Y" "N" "Y" "Y" "Y" "Y" "Y" "N"
        "Y" "Y" "Y" "N" "Y" "Y" "Y" "Y" "Y" "Y" "Y" "Y" "Y"))

(defun run-consistency-diagnostics (&key (v nil))
  "Compares current model's predictions with predictions of this model (as of 3-25-2011), when
   syllogistic reasoning component was finalized. The fn checks to make sure that all current
   predictions are the same as the previous model's, and when they're not, prints an error."
  (let ((i 0))
    (when v
      (format t "------------------------------------------------------------~%~
                 ~15,A~15,A~20,A~15,A~%~
                 ------------------------------------------------------------~%"
              "Assertions" "Consistent?" "10-16-12 Preds" "Diagnostic"))
    (dolist (figure (list (list Aab Iab Eab Oab)
                        (list Aba Iba Eba Oba)))
      (dolist (q1 (list Aab Iab Eab Oab))
        (dolist (q2 figure)
          (when (not (diagnose-consistency (list q1 q2) (nth i *imm-con-preds-v0.9*) :v v))
            (return-from run-consistency-diagnostics "[FAILED]"))
          (incf i))))
    (when (not v) "[OK]")))

(setf *temp* nil)

(defun diagnose-consistency (intensions old-prediction &key (v nil))
  "fn for drawing 'consistency' conclusions"
  (let* (model consistency)
    (handler-case
        (setf model (first (consistent? intensions :system2 t))))
    (setf consistency (if model "Y" "N"))
    (setf *temp* (append *temp* (list consistency)))
    (if v
        (format t "~3,A,~11,A~15,A~20,A~15,A~%" (abbreviate (first intensions)) (abbreviate (second intensions))
                consistency old-prediction (if (equalp consistency old-prediction) "" "!")))
    (equalp consistency old-prediction)
t))

; ---------------------------------------------------------------------------------
; Section 11.5: Running experiments and parameter searching
; ---------------------------------------------------------------------------------

(defparameter *parameters* (let ((parameters nil))
                             (dolist (l '(2.0 2.5 3.0 3.5 4.0 4.5 5.0))
                               (dolist (e '(0.0 0.2 0.4 0.6 0.8 1.0))
                                 (dolist (s '(0.0 0.2 0.4 0.6 0.8 1.0))
                                   (dolist (o '(0.0 0.2 0.4 0.6 0.8 1.0))
                                     (push (list l e s o) parameters)))))
                             parameters))

(defun export-synthetic-data (data &key (directory nil) (parameter-string nil))
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    
    #+lispworks (when (not directory) (setf directory (capi:prompt-for-directory "Save data file here:")))
  
    (when directory
      (setf pathname (merge-pathnames directory (if (null parameter-string)
                                                    (format nil "mReasonerData ~A-~A-~A.csv" year month date)
                                                  (format nil "mReasonerData ~A-~A-~A ~A.csv" year month date parameter-string))))
      (with-open-file (output pathname
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format output "Subject,ProblemNumber,Response,InitialModel,FinalModel,Distance,~
                        Premises,Task,Conclusions,Stochastic,Lambda,Epsilon,Sigma,Omega,Category,Accuracy,Latency~%")
        (dolist (datum (reverse data))
          (format output "~{~a~^,~}~%" datum))))))

(defun parameter-search (problems &key (directory nil) (N 100) (parameters *parameters*) (verbose t))
  (let ((l +lambda+) (e +epsilon+) (s +sigma+) (o +omega+)
        experiments)
    (if (listp parameters)
        (setf experiments (length parameters))
      (progn
        (setf experiments parameters)
        (setf parameters (randomize *parameters*))))
    #+lispworks (when (not directory) (setf directory (capi:prompt-for-directory "Save data file here:")))
    ;(format t "~%Exporting data to: ~A~%" directory)
    (dotimes (i experiments)
      (run-experiment problems :N N :verbose verbose :parameters (nth i parameters) :status (list (1+ i) experiments))
      (export-synthetic-data *synthetic-data* :directory directory :parameter-string (format nil "l~A-e~A-s~A-o~A" +lambda+ +epsilon+ +sigma+ +omega+)))
 
    (setf +lambda+ l +epsilon+ e +sigma+ s +omega+ o)))


;; process-data creates a thread which watches for data being
;; sent to its mailbox and then "processes" them.
#+lispworks (defun process-data (ostream)
  (let ((mailbox))
    (mp:process-run-function
     "Process data" ()
     (lambda ()
       ;; Create a mailbox
       (setf mailbox (mp:make-mailbox))
       (loop
        ;; Wait for someone to write to the mailbox
        (let ((datum (mp:mailbox-read mailbox
                                      "Waiting for data to process"
                                      5)))
          ;; "Process" the result
          (if datum
              (format ostream "~&Processing ~a.~%" datum)
            ;; Looks like everyone else went away. Terminate self.
            (return))))))
    ;; See the Slightly Tougher Exercise below...
    (mp:process-wait "Waiting for mailbox to exist."
                     (lambda () mailbox))
    ;; Return mailbox so that others can share it.
    mailbox))


(defun run-experiment (problems &key (N 20) (verbose t) (parameters nil) (status nil))
  (setf *synthetic-data* nil)
  (when verbose
    (format t "~%--------------------------~%~
               Experiment ~A~%~
               --------------------------~%~
               Problems:            ~5d~%~
               Simulated subjects:  ~5d~%~%" (if status (format nil "(~A of ~A)" (first status) (second status)) "") (length problems) N))

  (dotimes (subject N)
    (dotimes (problem (length problems))
      ; ---  single-threaded  ---
      (when parameters
        (setf *stochastic* t
              +lambda+ (first parameters)
              +epsilon+ (second parameters)
              +sigma+ (third parameters)
              +omega+ (fourth parameters)))
      (run-problem-and-collect-output (nth problem problems) parameters problem subject N)
      ; ---  multi-threaded   ---
      ;(mp:process-run-function (format t "mR launch: S~A-P~A~%" subject problem) `(:mailbox ,*mailbox*) 'run-problem-and-collect-output (nth problem problems) parameters problem subject N)
 
       #+lispworks (when *mr-interface*
                     (show-message
                      (synthetic-data-panel-accessor *mR-interface*)
                      (format nil "Running participant ~A of ~A..." subject N))
                     (when (equal 0 (mod subject (/ N 10)))
                       (capi:apply-in-pane-process (experiment-progress-accessor *mR-interface*)
                                                   #'(setf capi:range-slug-start)
                                                   (* (/ subject N) 100)
                                                   (experiment-progress-accessor *mR-interface*))))))
  (when verbose
    (format t "Parameter settings:~%~
               ~T           lambda  = ~4d~%~
               ~T           epsilon = ~4d~%~
               ~T           sigma   = ~4d~%~
               ~T           omega   = ~4d~%~
               --------------------------~%"  +lambda+ +epsilon+ +sigma+ +omega+)))

(defun run-problem-and-collect-output (problem-list parameters problem subject N)
  (let ((*tracer*     (copy-class-instance (reset-tracer)))
        (*stochastic* (if parameters t (nth 3 problem-list)))
        (+lambda+     (if parameters (nth 0 parameters) (nth 4 problem-list)))
        (+epsilon+    (if parameters (nth 1 parameters) (nth 5 problem-list)))
        (+sigma+      (if parameters (nth 2 parameters) (nth 6 problem-list)))
        (+omega+      (if parameters (nth 3 parameters) (nth 7 problem-list)))
        (data-point   (make-list 17 :initial-element "NA")))

    (run-problem problem-list)
;    (format t "mR collect: S~A-P~A~%" subject problem)

    (setf (nth 0  data-point) (format nil "S~A" (1+ subject)))
    (setf (nth 1  data-point) (format nil "P~A" (1+ problem)))
    (setf (nth 2  data-point) (response *tracer*))
    (when (initial-model *tracer*) (setf (nth 3 data-point) (serialize-model (initial-model *tracer*))))
    (when (final-model *tracer*) (setf (nth 4 data-point) (serialize-model (final-model *tracer*))))
    (if (and (initial-model *tracer*) (final-model *tracer*))
        (setf (nth 5 data-point) (distance (initial-model *tracer*) (final-model *tracer*)))
      (setf (nth 5 data-point) 0))
    (setf (nth 6  data-point) (format nil "~{~A~#[~:; ~]~}" (mapcar #'abbreviate (nth 0 problem-list))))
    (setf (nth 7  data-point) (nth 1 problem-list))
    (setf (nth 8  data-point) (if (nth 2 problem-list)
                                  (format nil "~{~A~#[~:;  ~]~}" (mapcar #'abbreviate (nth 2 problem-list))) "NA"))
    (setf (nth 9  data-point) (if (nth 3 problem-list) "Yes" "No"))
    (setf (nth 10 data-point) +lambda+)
    (setf (nth 11 data-point) +epsilon+)
    (setf (nth 12 data-point) +sigma+)
    (setf (nth 13 data-point) +omega+)
    (setf (nth 14 data-point) (nth 8  problem-list))
    (setf (nth 15 data-point) (nth 9  problem-list))
    (setf (nth 16 data-point) (nth 10 problem-list))
    ; ---  single-threaded  ---
    (push data-point *synthetic-data*)
    ; ---  multi--threaded  ---
    ;(mp:mailbox-send (mp:process-mailbox (mp:get-current-process)) data-point)

    ))

(defun run-problem (problem &key (verbose nil))
  (when verbose
    (format t "lambda = ~A epsilon = ~A sigma = ~A omega = ~A~%" +lambda+ +epsilon+ +sigma+ +omega+))
  
  (let* ((premise-intensions (nth 0 problem))
         (task (nth 1 problem))
         (conclusions-intensions (nth 2 problem)))
    (cond
     ((string-equal "What follows?" task) (what-follows? premise-intensions))
     ((string-equal "Is it possible that..." task) (possible? conclusions-intensions :given premise-intensions))
     ((string-equal "Is it necessary that..." task) (necessary? conclusions-intensions :given premise-intensions))
     ((string-equal "Can all of those statements be true at the same time?" task) (consistent? premise-intensions)))))
  
; ---------------------------------------------------------------------------------
; Section 11.6: Pre-defined experiments
; ---------------------------------------------------------------------------------

(defparameter *immediate-inference-exp1-newstead-griggs-1983*
  `(((,Aab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "Zero model"     100 nil T)
    ((,Aab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "Multiple model"  67 nil T)
    ((,Aab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "One model"       73 nil T)
    ((,Aab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       65 nil T)
    ((,Aab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "One model"      100 nil T)
    ((,Aab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "One model"       92 nil T)
    ((,Aab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "One model"       98 nil T)
    ((,Aab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "One model"       58 nil T)
    
    ((,Iab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "One model"      100 nil T)
    ((,Iab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "One model"       90 nil T)
    ((,Iab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "Zero model"     100 nil T)
    ((,Iab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       69 nil T)
    ((,Iab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "One model"      100 nil T)
    ((,Iab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "One model"       90 nil T)
    ((,Iab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "Multiple model"   6 nil T)
    ((,Iab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "Multiple model"  37 nil T)

    ((,Eab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "One model"      100 nil T)
    ((,Eab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "One model"       98 nil T)
    ((,Eab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "One model"       98 nil T)
    ((,Eab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       79 nil T)
    ((,Eab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "Zero model"      98 nil T)
    ((,Eab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "One model"       54 nil T)
    ((,Eab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "One model"       69 nil T)
    ((,Eab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "One model"       54 nil T)
    
    ((,Oab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "One model"      100 nil T)
    ((,Oab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "One model"       87 nil T)
    ((,Oab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "Multiple model"  17 nil T)
    ((,Oab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       35 nil T)
    ((,Oab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "Multiple model"  98 nil T)
    ((,Oab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "Multiple model"  85 nil T)
    ((,Oab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "Zero model"      98 nil T)
    ((,Oab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "Multiple model"  35 nil T)))

(defparameter *immediate-inference-exp2-newstead-griggs-1983*
  `(((,Aab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "Zero model"      98 nil T)
    ((,Aab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "Multiple model"  63 nil T)
    ((,Aab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "One model"       56 nil T)
    ((,Aab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       79 nil T)
    ((,Aab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "One model"      100 nil T)
    ((,Aab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "One model"       90 nil T)
    ((,Aab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "One model"       97 nil T)
    ((,Aab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "One model"       43 nil T)
    
    ((,Iab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "One model"       83 nil T)
    ((,Iab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "One model"       90 nil T)
    ((,Iab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "Zero model"      98 nil T)
    ((,Iab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       90 nil T)
    ((,Iab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "One model"       98 nil T)
    ((,Iab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "One model"       92 nil T)
    ((,Iab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "Multiple model"   3 nil T)
    ((,Iab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "Multiple model"  91 nil T)

    ((,Eab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "One model"      100 nil T)
    ((,Eab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "One model"       95 nil T)
    ((,Eab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "One model"       92 nil T)
    ((,Eab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       67 nil T)
    ((,Eab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "Zero model"      94 nil T)
    ((,Eab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "One model"       59 nil T)
    ((,Eab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "One model"       59 nil T)
    ((,Eab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "One model"       47 nil T)
    
    ((,Oab) "Is it necessary that..." (,Aab) T 4.0 0.3 0.4 1.0 "One model"       98 nil T)
    ((,Oab) "Is it necessary that..." (,Aba) T 4.0 0.3 0.4 1.0 "One model"       89 nil T)
    ((,Oab) "Is it necessary that..." (,Iab) T 4.0 0.3 0.4 1.0 "Multiple model"   8 nil T)
    ((,Oab) "Is it necessary that..." (,Iba) T 4.0 0.3 0.4 1.0 "One model"       12 nil T)
    ((,Oab) "Is it necessary that..." (,Eab) T 4.0 0.3 0.4 1.0 "Multiple model"  87 nil T)
    ((,Oab) "Is it necessary that..." (,Eba) T 4.0 0.3 0.4 1.0 "Multiple model"  83 nil T)
    ((,Oab) "Is it necessary that..." (,Oab) T 4.0 0.3 0.4 1.0 "Zero model"      98 nil T)
    ((,Oab) "Is it necessary that..." (,Oba) T 4.0 0.3 0.4 1.0 "Multiple model"  19 nil T)))

(defparameter *immediate-inference-exp1-khemlani-et-al-2015*
  `(((,Aab) "Is it possible that..." (,Aab) T 3.8 0.2 0.6 1.0 "Zero model"     100 3.98 T)
    ((,Aab) "Is it possible that..." (,Aba) T 3.8 0.2 0.6 1.0 "One model"       88 5.25 T)
    ((,Aab) "Is it possible that..." (,Iab) T 3.8 0.2 0.6 1.0 "One model"       96 5.80 T)
    ((,Aab) "Is it possible that..." (,Iba) T 3.8 0.2 0.6 1.0 "One model"       96 6.01 T)
    ((,Aab) "Is it possible that..." (,Eab) T 3.8 0.2 0.6 1.0 "One model"       96 3.60 T)
    ((,Aab) "Is it possible that..." (,Eba) T 3.8 0.2 0.6 1.0 "One model"       81 6.71 T)
    ((,Aab) "Is it possible that..." (,Oab) T 3.8 0.2 0.6 1.0 "One model"       92 4.89 T)
    ((,Aab) "Is it possible that..." (,Oba) T 3.8 0.2 0.6 1.0 "Multiple model"  58 5.70 T)
    
    ((,Iab) "Is it possible that..." (,Aab) T 3.8 0.2 0.6 1.0 "Multiple model"  69 5.67 T)
    ((,Iab) "Is it possible that..." (,Aba) T 3.8 0.2 0.6 1.0 "Multiple model"  73 5.32 T)
    ((,Iab) "Is it possible that..." (,Iab) T 3.8 0.2 0.6 1.0 "Zero model"     100 4.19 T)
    ((,Iab) "Is it possible that..." (,Iba) T 3.8 0.2 0.6 1.0 "One model"      100 4.75 T)
    ((,Iab) "Is it possible that..." (,Eab) T 3.8 0.2 0.6 1.0 "One model"       96 4.35 T)
    ((,Iab) "Is it possible that..." (,Eba) T 3.8 0.2 0.6 1.0 "One model"       96 5.71 T)
    ((,Iab) "Is it possible that..." (,Oab) T 3.8 0.2 0.6 1.0 "One model"       96 4.85 T)
    ((,Iab) "Is it possible that..." (,Oba) T 3.8 0.2 0.6 1.0 "One model"       96 5.91 T)
    
    ((,Eab) "Is it possible that..." (,Aab) T 3.8 0.2 0.6 1.0 "One model"      100 4.13 T)
    ((,Eab) "Is it possible that..." (,Aba) T 3.8 0.2 0.6 1.0 "One model"       85 5.95 T)
    ((,Eab) "Is it possible that..." (,Iab) T 3.8 0.2 0.6 1.0 "One model"       92 5.56 T)
    ((,Eab) "Is it possible that..." (,Iba) T 3.8 0.2 0.6 1.0 "One model"       73 5.88 T)
    ((,Eab) "Is it possible that..." (,Eab) T 3.8 0.2 0.6 1.0 "Zero model"      96 4.35 T)
    ((,Eab) "Is it possible that..." (,Eba) T 3.8 0.2 0.6 1.0 "One model"       92 6.60 T)
    ((,Eab) "Is it possible that..." (,Oab) T 3.8 0.2 0.6 1.0 "One model"       77 5.56 T)
    ((,Eab) "Is it possible that..." (,Oba) T 3.8 0.2 0.6 1.0 "One model"       85 8.71 T)
    
    ((,Oab) "Is it possible that..." (,Aab) T 3.8 0.2 0.6 1.0 "One model"       85 5.60 T)
    ((,Oab) "Is it possible that..." (,Aba) T 3.8 0.2 0.6 1.0 "Multiple model"  38 6.63 T)
    ((,Oab) "Is it possible that..." (,Iab) T 3.8 0.2 0.6 1.0 "Multiple model"  92 4.72 T)
    ((,Oab) "Is it possible that..." (,Iba) T 3.8 0.2 0.6 1.0 "Multiple model"  92 5.84 T)
    ((,Oab) "Is it possible that..." (,Eab) T 3.8 0.2 0.6 1.0 "One model"       62 6.40 T)
    ((,Oab) "Is it possible that..." (,Eba) T 3.8 0.2 0.6 1.0 "One model"       38 7.62 T)
    ((,Oab) "Is it possible that..." (,Oab) T 3.8 0.2 0.6 1.0 "Zero model"      96 4.59 T)
    ((,Oab) "Is it possible that..." (,Oba) T 3.8 0.2 0.6 1.0 "One model"       92 5.95 T)))

(defparameter *immediate-inference-exp2-khemlani-et-al-2015*
  `(((,Aab) "Is it possible that..." (,Aab)   T 3.8 0.4 0.4 1.0 "Zero model"      98  5.72 T)
    ((,Aab) "Is it possible that..." (,Aba)   T 3.8 0.4 0.4 1.0 "One model"       78 11.14 T)
    ((,Aab) "Is it possible that..." (,Mab)   T 3.8 0.4 0.4 1.0 "One model"       63  8.48 T)
    ((,Aab) "Is it possible that..." (,Mba)   T 3.8 0.4 0.4 1.0 "Multiple model"  80  8.22 T)
    ((,Aab) "Is it possible that..." (,Eab)   T 3.8 0.4 0.4 1.0 "One model"       93  6.78 T)
    ((,Aab) "Is it possible that..." (,Eba)   T 3.8 0.4 0.4 1.0 "One model"       93  9.71 T)
    ((,Aab) "Is it possible that..." (,Ma-b)  T 3.8 0.4 0.4 1.0 "One model"       95 11.33 T)
    ((,Aab) "Is it possible that..." (,Mb-a)  T 3.8 0.4 0.4 1.0 "Multiple model"  40 10.28 T)
    
    ((,Mab) "Is it possible that..." (,Aab)   T 3.8 0.4 0.4 1.0 "One model"       40  8.20 T)
    ((,Mab) "Is it possible that..." (,Aba)   T 3.8 0.4 0.4 1.0 "One model"       55  8.83 T)
    ((,Mab) "Is it possible that..." (,Mab)   T 3.8 0.4 0.4 1.0 "Zero model"     100  6.46 T)
    ((,Mab) "Is it possible that..." (,Mba)   T 3.8 0.4 0.4 1.0 "One model"       90  8.13 T)
    ((,Mab) "Is it possible that..." (,Eab)   T 3.8 0.4 0.4 1.0 "One model"       90  7.87 T)
    ((,Mab) "Is it possible that..." (,Eba)   T 3.8 0.4 0.4 1.0 "One model"       85  8.57 T)
    ((,Mab) "Is it possible that..." (,Ma-b)  T 3.8 0.4 0.4 1.0 "One model"       93  6.20 T)
    ((,Mab) "Is it possible that..." (,Mb-a)  T 3.8 0.4 0.4 1.0 "Multiple model"  40 15.69 T)
    
    ((,Eab) "Is it possible that..." (,Aab)   T 3.8 0.4 0.4 1.0 "One model"       98  5.75 T)
    ((,Eab) "Is it possible that..." (,Aba)   T 3.8 0.4 0.4 1.0 "One model"       85 12.19 T)
    ((,Eab) "Is it possible that..." (,Mab)   T 3.8 0.4 0.4 1.0 "One model"       95  7.21 T)
    ((,Eab) "Is it possible that..." (,Mba)   T 3.8 0.4 0.4 1.0 "One model"       93 11.28 T)
    ((,Eab) "Is it possible that..." (,Eab)   T 3.8 0.4 0.4 1.0 "Zero model"     100  6.60 T)
    ((,Eab) "Is it possible that..." (,Eba)   T 3.8 0.4 0.4 1.0 "One model"       98 19.74 T)
    ((,Eab) "Is it possible that..." (,Ma-b)  T 3.8 0.4 0.4 1.0 "One model"       40 11.57 T)
    ((,Eab) "Is it possible that..." (,Mb-a)  T 3.8 0.4 0.4 1.0 "One model"       83  9.82 T)
    
    ((,Ma-b) "Is it possible that..." (,Aab)  T 3.8 0.4 0.4 1.0 "One model"       90  8.80 T)
    ((,Ma-b) "Is it possible that..." (,Aba)  T 3.8 0.4 0.4 1.0 "One model"       38 11.21 T)
    ((,Ma-b) "Is it possible that..." (,Mab)  T 3.8 0.4 0.4 1.0 "One model"       95  7.62 T)
    ((,Ma-b) "Is it possible that..." (,Mba)  T 3.8 0.4 0.4 1.0 "Multiple model"  40  8.21 T)
    ((,Ma-b) "Is it possible that..." (,Eab)  T 3.8 0.4 0.4 1.0 "One model"       38 11.69 T)
    ((,Ma-b) "Is it possible that..." (,Eba)  T 3.8 0.4 0.4 1.0 "One model"       53 19.91 T)
    ((,Ma-b) "Is it possible that..." (,Ma-b) T 3.8 0.4 0.4 1.0 "Zero model"     100  7.25 T)
    ((,Ma-b) "Is it possible that..." (,Mb-a) T 3.8 0.4 0.4 1.0 "Multiple model"  93 10.93 T)))

(defparameter *immediate-inference-exp3-khemlani-et-al-2015*
  `(((,Aab ,Aab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Zero model"     100  4.98 T)
    ((,Aab ,Aba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"      100  5.62 T)
    ((,Aab ,Iab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       79  5.85 T)
    ((,Aab ,Iba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       83  7.56 T)
    ((,Aab ,Eab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"      100  4.86 T)
    ((,Aab ,Eba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       96  8.06 T)
    ((,Aab ,Oab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       96  8.24 T)
    ((,Aab ,Oba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Multiple model"  54  9.30 T)
    
    ((,Iab ,Aab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Multiple model"  88  5.63 T)
    ((,Iab ,Aba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Multiple model"  88  9.37 T)
    ((,Iab ,Iab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Zero model"     100  5.77 T)
    ((,Iab ,Iba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"      100  6.89 T)
    ((,Iab ,Eab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"      100  5.93 T)
    ((,Iab ,Eba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       83  8.66 T)
    ((,Iab ,Oab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"      100  6.92 T)
    ((,Iab ,Oba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       96  7.49 T)
    
    ((,Eab ,Aab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"      100  5.42 T)
    ((,Eab ,Aba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       88  9.33 T)
    ((,Eab ,Iab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       96  6.28 T)
    ((,Eab ,Iba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       79  8.47 T)
    ((,Eab ,Eab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Zero model"      96  4.58 T)
    ((,Eab ,Eba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       96  5.90 T)
    ((,Eab ,Oab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       63 13.35 T)
    ((,Eab ,Oba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       83  8.80 T)
    
    ((,Oab ,Aab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       96  7.92 T)
    ((,Oab ,Aba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Multiple model"  25 12.88 T)
    ((,Oab ,Iab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Multiple model"  96  8.18 T)
    ((,Oab ,Iba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Multiple model"  96 10.48 T)
    ((,Oab ,Eab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       75  9.42 T)
    ((,Oab ,Eba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"       71 11.93 T)
    ((,Oab ,Oab) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "Zero model"     100  6.34 T)
    ((,Oab ,Oba) "Can all of those statements be true at the same time?" () T 3.5 0.6 0.7 1.0 "One model"      100  7.78 T)))

(defparameter *khemlani-&-johnson-laird-2012*
  `(((,Aab ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "AA1" 88 NIL T)
    ((,Aba ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "AA2" 54 NIL T)
    ((,Aba ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "AA4" 16 NIL T)
    ((,Aba ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "AI2" 90 NIL T)
    ((,Aba ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "AI4" 83 NIL T)
    ((,Aab ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "AE1" 87 NIL T)
    ((,Aba ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "AE2"  1 NIL T)
    ((,Aab ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "AE3" 81 NIL T)
    ((,Aba ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "AE4"  8 NIL T)
    ((,Aab ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "AO3" 40 NIL T)
    ((,Aba ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "AO4" 54 NIL T)
    ((,Iab ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "IA1" 88 NIL T)
    ((,Iba ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "IA4" 81 NIL T)
    ((,Iab ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "IE1" 44 NIL T)
    ((,Iba ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "IE2" 13 NIL T)
    ((,Iab ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "IE3" 20 NIL T)
    ((,Iba ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "IE4" 28 NIL T)
    ((,Eab ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "EA1"  3 NIL T)
    ((,Eba ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "EA2" 78 NIL T)
    ((,Eab ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "EA3" 80 NIL T)
    ((,Eba ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "EA4"  9 NIL T)
    ((,Eab ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "EI1"  8 NIL T)
    ((,Eba ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "EI2" 37 NIL T)
    ((,Eab ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "EI3" 21 NIL T)
    ((,Eba ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "EI4" 15 NIL T)
    ((,Oab ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "OA3" 36 NIL T)
    ((,Oba ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "OA4" 42 NIL T)

    ((,Aab ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "AA3" 31 NIL T)
    ((,Aab ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "AI1" 16 NIL T)
    ((,Aab ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "AI3" 37 NIL T)
    ((,Aab ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "AO1" 14 NIL T)
    ((,Aba ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "AO2" 17 NIL T)
    ((,Iba ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "IA2" 12 NIL T)
    ((,Iab ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "IA3" 28 NIL T)
    ((,Iab ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "II1" 33 NIL T)
    ((,Iba ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "II2" 30 NIL T)
    ((,Iab ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "II3" 51 NIL T)
    ((,Iba ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "II4" 61 NIL T)
    ((,Iab ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "IO1" 33 NIL T)
    ((,Iba ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "IO2" 49 NIL T)
    ((,Iab ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "IO3" 53 NIL T)
    ((,Iba ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "IO4" 54 NIL T)
    ((,Eab ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "EE1" 44 NIL T)
    ((,Eba ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "EE2" 44 NIL T)
    ((,Eab ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "EE3" 76 NIL T)
    ((,Eba ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "EE4" 66 NIL T)
    ((,Eab ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "EO1" 28 NIL T)
    ((,Eba ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "EO2" 47 NIL T)
    ((,Eab ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "EO3" 49 NIL T)
    ((,Eba ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "EO4" 57 NIL T)
    ((,Oab ,Abc) "What follows?" () T 3.0 0.45 0.5 1.0 "OA1" 20 NIL T)
    ((,Oba ,Acb) "What follows?" () T 3.0 0.45 0.5 1.0 "OA2" 13 NIL T)
    ((,Oab ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "OI1" 36 NIL T)
    ((,Oba ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "OI2" 31 NIL T)
    ((,Oab ,Icb) "What follows?" () T 3.0 0.45 0.5 1.0 "OI3" 49 NIL T)
    ((,Oba ,Ibc) "What follows?" () T 3.0 0.45 0.5 1.0 "OI4" 47 NIL T)
    ((,Oab ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "OE1" 37 NIL T)
    ((,Oba ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "OE2" 51 NIL T)
    ((,Oab ,Ecb) "What follows?" () T 3.0 0.45 0.5 1.0 "OE3" 47 NIL T)
    ((,Oba ,Ebc) "What follows?" () T 3.0 0.45 0.5 1.0 "OE4" 49 NIL T)
    ((,Oab ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "OO1" 37 NIL T)
    ((,Oba ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "OO2" 42 NIL T)
    ((,Oab ,Ocb) "What follows?" () T 3.0 0.45 0.5 1.0 "OO3" 64 NIL T)
    ((,Oba ,Obc) "What follows?" () T 3.0 0.45 0.5 1.0 "OO4" 66 NIL T)))

(defparameter *khemlani-lotstein-&-johnson-laird-2014*
  `(((,x-is-A    ,Aab) "What follows?" () T 5.5 0.0 0.58 1.0 "1 - One model"      90  NIL T)
    ((,x-isnot-A ,Aab) "What follows?" () T 5.5 0.0 0.58 1.0 "5 - Multiple model" 79  NIL T)
    ((,x-is-A    ,Aba) "What follows?" () T 5.5 0.0 0.58 1.0 "2 - Multiple model" 81  NIL T)
    ((,x-isnot-A ,Aba) "What follows?" () T 5.5 0.0 0.58 1.0 "6 - One model"      50  NIL T)
    ((,x-is-A    ,Eab) "What follows?" () T 5.5 0.0 0.58 1.0 "3 - One model"      95  NIL T)
    ((,x-isnot-A ,Eab) "What follows?" () T 5.5 0.0 0.58 1.0 "7 - Multiple model" 87  NIL T)
    ((,x-is-A    ,Eba) "What follows?" () T 5.5 0.0 0.58 1.0 "4 - One model"      95  NIL T)
    ((,x-isnot-A ,Eba) "What follows?" () T 5.5 0.0 0.58 1.0 "8 - Multiple model" 82  NIL T)))

(defparameter *mascarenhas-&-koralus-2016*
  `(((,Iab    ,x-is-A)    "Is it necessary that..." (,x-is-B)  T 5.5 0.0 0.58 1.0 "P9"       00  NIL T)
    ((,x-is-A ,Iab)       "Is it necessary that..." (,x-is-B)  T 5.5 0.0 0.58 1.0 "P10"      00  NIL T)
    ((,Aab    ,x-is-A)    "Is it necessary that..." (,x-is-B)  T 5.5 0.0 0.58 1.0 "P11"      00  NIL T)
    ((,X-is-A ,Aab)       "Is it necessary that..." (,x-is-B)  T 5.5 0.0 0.58 1.0 "P12"      00  NIL T)
    ((,x-is-A ,x-isnot-B) "Is it necessary that..." (,Oab)     T 5.5 0.0 0.58 1.0 "P13"      00  NIL T)
    ((,Iab    ,x-isnot-A) "Is it necessary that..." (,x-is-B)  T 5.5 0.0 0.58 1.0 "P14"      00  NIL T)
    ((,x-isnot-A ,Iab)    "Is it necessary that..." (,x-is-B)  T 5.5 0.0 0.58 1.0 "P15"      00  NIL T)))

(defparameter *temporal-inference-exp1&2-khemlani-et-al-in-prep*
  `(((,wBx ,xBy ,zWw) "Is it necessary that..." (,zBx) T 5.5 0.5 0.0 1.0 "1 - One model"      75  NIL T)
    ((,wBx ,yAx ,zWw) "Is it necessary that..." (,zBy) T 5.5 0.5 0.0 1.0 "2 - One model"      79  NIL T)
    ((,wAx ,xAy ,zWx) "Is it necessary that..." (,zAy) T 5.5 0.5 0.0 1.0 "3 - One model"      61  NIL T)
    ((,wAx ,xAy ,zWx) "Is it necessary that..." (,wAz) T 5.5 0.5 0.0 1.0 "4 - One model"      64  NIL T)
    ((,wBx ,yBx ,zWw) "Is it necessary that..." (,yBz) T 5.5 0.5 0.0 1.0 "5 - Multiple model" 39  NIL T)
    ((,wBx ,xAy ,zWw) "Is it necessary that..." (,yAz) T 5.5 0.5 0.0 1.0 "6 - Multiple model" 39  NIL T)
    ((,xAw ,yBx ,wWz) "Is it necessary that..." (,zAy) T 5.5 0.5 0.0 1.0 "7 - Multiple model" 36  NIL T)
    ((,xAw ,xAy ,wWz) "Is it necessary that..." (,zBy) T 5.5 0.5 0.0 1.0 "8 - Multiple model" 32  NIL T)))

(defparameter *durational-reasoning-exp1-kelly-et-al-2020*
   `(((,xDy ,yBz ,xBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,xDy ,zDx ,zDy) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,xDy ,yDz ,xDz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,xDy ,zBy ,zBx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,xDy ,xDz ,zDy) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,xDy ,zDy ,zDx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,xDy ,zBx ,zDy) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,xDy ,zDy ,xBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,xDy ,yBz ,zDx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,xDy ,zDx ,zBy) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,xDy ,yDz ,xBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,xDy ,zBy ,xBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,xDy ,zBx ,yDz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)
     ((,xDy ,xDz ,zBy) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)
     ((,xDy ,xDz ,zBx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)
     ((,xDy ,xBz ,zBy) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)))

(defparameter *durational-reasoning-exp2&3-kelly-et-al-2020*
   `(((,wDx ,xDy ,yBz ,wBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,wDx ,yDw ,xBz ,yDx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,wDx ,yBx ,zDw ,yBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,wDx ,xBy ,zDy ,wBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Consistent" 00  NIL T)
     ((,wDx ,yDx ,zDy ,zBw) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,wDx ,wDy ,wBz ,yDx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,wDx ,yBw ,zBy ,yDx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,wDx ,wBy ,zDx ,yDz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Consistent" 00  NIL T)
     ((,wDx ,xDy ,yDz ,zBw) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,wDx ,xBy ,yBz ,wDz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,wDx ,yBx ,zDy ,zDw) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,wDx ,yDw ,zBx ,wBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "One model - Inconsistent" 00  NIL T)
     ((,wDx ,wBy ,xDz ,yBz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)
     ((,wDx ,yBw ,zDy ,zDw) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)
     ((,wDx ,wDy ,yBz ,zBx) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)
     ((,wDx ,xDy ,zBw ,yDz) "Can all of those statements be true at the same time?" () T 4.5 0.5 0.4 1.0 "Multiple models - Inconsistent" 00  NIL T)))

(defparameter *wang-koralus-khemlani-2019*
   `(((,Min-ba ,Min-bc) "Is it necessary that..." (,Maj-a-c) T 4.5 0.5 0.4 1.0 "1 - One model - P7"       70  NIL T)
     ((,Maj-ab ,Abc)    "Is it necessary that..." (,Maj-ac)  T 4.5 0.5 0.4 1.0 "2 - One model - P13"      72  NIL T)
   ;  ((,Maj-ab ,Maj-cb) "Is it necessary that..." (,Aac)     T 4.5 0.5 0.4 1.0 "3 - One model - P24"      55  NIL T)
   ;  ((,Min-ab ,Min-bc) "Is it necessary that..." (,Aac)     T 4.5 0.5 0.4 1.0 "4 - One model - P20"      63  NIL T)
   ;  ((,Maj-ab ,Maj-cb) "Is it necessary that..." (,Maj-ac)  T 4.5 0.5 0.4 1.0 "5 - Multiple model - P12" 54  NIL T)
     ((,Maj-ba ,Maj-bc) "Is it necessary that..." (,Maj-ac)  T 4.5 0.5 0.4 1.0 "6 - Multiple model - P9"  43  NIL T)
     ((,Min-ab ,Maj-bc) "Is it necessary that..." (,Min-ac)  T 4.5 0.5 0.4 1.0 "7 - Multiple model - P3"  45  NIL T)
     ((,Maj-ab ,Min-bc) "Is it necessary that..." (,Min-ac)  T 4.5 0.5 0.4 1.0 "8 - Multiple model - P10" 41  NIL T)
     ((,Min-ba ,Min-bc) "Is it necessary that..." (,Min-ac)  T 4.5 0.5 0.4 1.0 "9 - Multiple model - P4"  51  NIL T)))

#|

(setf wLx (parse '(W is to the left of X)))
(setf xLy (parse '(X is to the left of Y)))
(setf zAw (parse '(Z is above W)))
(setf wRx (parse '(W is to the right of X)))
(setf xRy (parse '(X is to the right of Y)))
(setf zAx (parse '(Z is above X)))
(setf yLx (parse '(Y is to the left of X)))
(setf xRw (parse '(X is to the right of W)))
(setf wAx (parse '(W is above X)))
(setf zLx (parse '(Z is to the left of X)))
(setf zLy (parse '(Z is to the left of Y)))
(setf zRy (parse '(Z is to the right of Y)))
(setf wRz (parse '(W is to the right of Z)))
(setf yLz (parse '(Y is to the left of Z)))
(setf yRz (parse '(Y is to the right of Z)))
(setf zRx (parse '(Z is to the right of X)))
(setf wAz (parse '(W is above Z)))

(defparameter *spatial-inference-in-prep*
  `(((,wLx ,xLy ,zAw) "Is it necessary that..." (,zLx) NIL 5.5 0.5 0.0 1.0 "1 - One model"      00  NIL T)
    ((,wLx ,yLx ,zAw) "Is it necessary that..." (,zLy) NIL 5.5 0.5 0.0 1.0 "2 - One model"      00  NIL T)
    ((,wAx ,xRy ,zAx) "Is it necessary that..." (,zRy) NIL 5.5 0.5 0.0 1.0 "3 - One model"      00  NIL T)
    ((,wAx ,xRy ,zAx) "Is it necessary that..." (,wRz) NIL 5.5 0.5 0.0 1.0 "4 - One model"      00  NIL T)
    ((,wLx ,yLx ,zAw) "Is it necessary that..." (,yLz) NIL 5.5 0.5 0.0 1.0 "5 - Multiple model" 00  NIL T)
    ((,wLx ,xRy ,zAw) "Is it necessary that..." (,yRz) NIL 5.5 0.5 0.0 1.0 "6 - Multiple model" 00  NIL T)
    ((,xRw ,yLx ,wAz) "Is it necessary that..." (,zRx) NIL 5.5 0.5 0.0 1.0 "7 - Multiple model" 00  NIL T)
    ((,xRw ,xRy ,wAz) "Is it necessary that..." (,zLy) NIL 5.5 0.5 0.0 1.0 "8 - Multiple model" 00  NIL T)))

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