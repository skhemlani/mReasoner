; ---------------------------------------------------------------------------------
; mReasoner is a unified computational implementation of the mental model theory of
; thinking and reasoning.
; ---------------------------------------------------------------------------------
; Contact: Sangeet Khemlani (sunny.khemlani@nrl.navy.mil)
;          Navy Center for Applied Research in Artificial Intelligence
;          Naval Research Laboratory
;
;          Phil Johnson-Laird (phil@princeton.edu)
;          Department of Psychology
;          Princeton University
; ---------------------------------------------------------------------------------
;
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
;
; ---------------------------------------------------------------------------------
; Long-term plan of development:
; ---------------------------------------------------------------------------------
; Version Expressivity                 Inferential tasks                         
; ------- ---------------------------- --------------------------------------------
;   1.0   Connectives and quantifiers  Inference, possibility, consistency, GUI
;   2.0   Temporality and causality    Probability, modulation
;   3.0   Spatial relations            Command-line interoperability
;   4.0   Deontic and epistemic verbs  Explanations
; ---------------------------------------------------------------------------------

(format t "~A[~A]~A~%"
        (make-string 40 :initial-element #\-)
        (format nil "~30:@<~A~>" "Loading mReasoner...")
        (make-string 40 :initial-element #\-))

; ---------------------------------------------------------------------------------
; Loading code
; ---------------------------------------------------------------------------------

(defparameter *system-name* "mReasoner")
(defparameter *version*     "2.0")

(defun file-path (file)
  (let (pathname)
    #+lispworks (setf pathname (current-pathname file))
    #+ccl       (setf pathname (merge-pathnames file (or *load-pathname* *loading-file-source-file*)))
    #+clisp     (setf pathname (merge-pathnames file (or *load-pathname* *loading-file-source-file*)))
    #+sbcl      (setf pathname (merge-pathnames (sb-unix:posix-getcwd/) file))
    pathname))

#-:jsown (when (probe-file (file-path "jsown-master/load.lisp"))
               (load (file-path "jsown-master/load.lisp")))

#+lispworks (progn 
              ; Get revision information from SVN
              (let ((string-stream (make-string-output-stream)) output position revision-number)
                (system:call-system-showing-output (format nil "cd \"~A\"; svn info" 
                                                           (directory-namestring (current-pathname)))
                                                   :prefix "" :show-cmd nil :output-stream string-stream)
                (setf output (split-sequence (format nil ":MSP~%") (get-output-stream-string string-stream)))
                (setf position (position "Last Changed Rev" output :test #'string-equal))
                (when (numberp position) (setf revision-number (nth (+ 1 position) output)))
                (when revision-number
                  (setf revision-number (parse-integer revision-number :junk-allowed t))
                  (setf *version* (format nil "~A.r~A" *version* revision-number))))

              (setf system:*stack-overflow-behaviour* nil) ; Suppress stack overflow warnings SSK 2015-02-05

              (defsystem "mReasoner"
                ()
                :members ("Classes.lisp"
                          "Utilities.lisp"
                          "API.lisp"
                          "Parser.lisp"
                          "Observer.lisp"
                          "Builder.lisp"       
                          "Scanner.lisp"
                          "FormConclusions.lisp"
                          "Counterexamples.lisp"
                          "HighLevel.lisp"
                          "Diagnostics.lisp"
                          "jpd-belief-icon.lisp" ;; FIX !!! SSK 4/19/12
                          "UserInterface.lisp"
                          )
                :rules  ((:in-order-to :compile :all
                          (:requires (:load :previous)))))
              
              (lispworks:load-system "mReasoner" :force t))

#+(or ccl mcl openmcl cmu sbcl clisp ecl scl allegro)
(progn
  (load (file-path "Classes.lisp"))
  (load (file-path "Utilities.lisp"))
  (load (file-path "API.lisp"))
  (load (file-path "Parser.lisp"))
  (load (file-path "Observer.lisp"))
  (load (file-path "Builder.lisp"))
  (load (file-path "Scanner.lisp"))
  (load (file-path "FormConclusions.lisp"))
  (load (file-path "Counterexamples.lisp"))
  (load (file-path "HighLevel.lisp"))
  (load (file-path "Diagnostics.lisp"))
  (load (file-path "jpd-belief-icon.lisp")))

; ---------------------------------------------------------------------------------
; Global variables and related functions
; ---------------------------------------------------------------------------------

(defparameter *mR-interface* nil
  "Global variable used to access GUI interface")

(defparameter *exhaustive-search-depth* 7
  "Global variable that controls the combinatoric depth of the exhaustic search
   algorithms defined in Counterexamples.lisp")

(defparameter *stochastic* t
  "Parameter that enables or disables stochastic mode of the system.")

(defparameter *build-attempts* 1000
  "Parameter that establishes the number of attempts to build a stochastic model
   before the system stops trying.")

(defparameter *synthetic-data* nil
  "Parameter that holds synthetic data from multiple inferences.")

(defparameter +sigma+ 0.0
  "Parameter that control the execution of system 2 processes (i.e., sigma = search)
   across the system as a whole. When sigma = 0, system 2 processes are never called.
   When sigma = 1, system 2 processes are always called. When sigma = .6, there is a 60%
   chance that system 2 processes will be called.")

(defparameter +lambda+ 4.0
  "Parameter that controls the size of models. Lambda denotes the lambda parameter Poisson
   distribution. By default, lambda = 4. To set a model's size, the system will sample from a
   left-truncated Poisson distribution with lambda parameter = 4. The system will sample from
   the distribution and use that sample as the size (cardinality) of the model.")

(defparameter +epsilon+ 0.0
  "Parameter that controls the construction of canonical models, i.e., models whose individuals
   are specified in the intensions, i.e., epsilon = error. When epsilon = 0, the system always
   builds canonical models. When epsilon = 1, every time the system needs to build a token in a
   model, it will do so with respect to the intensional constraints alone. When epsilon = .6, 
   every time the system needs to build a token in a model, there will be a 60% chance that the
   system will draw from canonical tokens.")

(defparameter +omega+ 0.0
  "Parameter that controls whether individuals weaken their initial conclusion after finding a
   counterexample or whether they simply report 'NVC' once a counterexample is found. When
   omega = 1.0, weakening always occurs; when omega = 0.0, individuals always report 'NVC' when
   a counterexample is found. When omega = 0.6, there's a 60% chance that weakening will occur.")

(defparameter +kappa+ 0.0
  "Parameter that controls whether spatial observations are compressed to eliminate spaces between
   them or else retain their absolute positioning in a provided grid. When kappa = 0.0, no spatial
   compression occurs; when kappa = 1.0, compression always occurs; when kappa = 0.6, there's a
   60% chance of compression.")

(defun reset-system-parameters ()
  (setf *stochastic* nil)
  (setf +lambda+ 4.0)
  (setf +sigma+ 0.0)
  (setf +epsilon+ 0.0)
  (setf +omega+ 0.0)
  (setf +kappa+ 0.0))

(defun set-system-parameters (&key lambda sigma epsilon omega kappa)
  (setf *stochastic* t)
  (setf +lambda+ lambda)
  (setf +sigma+ sigma)
  (setf +epsilon+ epsilon)
  (setf +omega+ omega)
  (setf +kappa+ kappa))

(defun stochastic-enabled? ()
  *stochastic*)

(defun enable-stochastic-mode ()
  (setf *stochastic* t))

(defun disable-stochastic-mode ()
  (setf *stochastic* nil))

(defun generate-size ()
  (let ((size nil)
        (candidate-size nil)
        (left-truncate '(0 1)))
    (while (not size)
      (setf candidate-size (poisson-random-number +lambda+))
      (when (not (member candidate-size left-truncate))
        (setf size candidate-size)))
    size))

(defun build-canonical? ()
  (< (random 1.0) (- 1 +epsilon+)))

(defun system2-enabled? ()
  (< (random 1.0) +sigma+))

(defun weaken-conclusions? ()
  (< (random 1.0) +omega+))

(defun spatial-compression? ()
  (< (random 1.0) +kappa+))

(format t "~A[~A]~A~%"
        (make-string 40 :initial-element #\-)
        (format nil "~30:@<~A ~A~>" "Loaded mReasoner" *version*)
        (make-string 40 :initial-element #\-))