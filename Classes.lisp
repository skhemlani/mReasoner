; ---------------------------------------------------------------------------------
; Part 1: Classes
; ---------------------------------------------------------------------------------
; 
; Class definitions for models and intensions
; 
; Section 1.1: General class copy functions
; Section 1.2: Classes for conditions
; Section 1.3: Classes for intensions
; Section 1.4: Classes for models
; Section 1.5: Class for JSON output
; ---------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------
; Section 1.1: General class copy functions
; ---------------------------------------------------------------------------------

(defun copy-class-instance (instance)
  "Constructs new copy of an instance of a class"
;  (describe instance)
  (let* ((class (class-of instance))
         (slots (class-slots class))
         (new-instance (make-instance class)))
    (loop for slot in slots do
          (let ((slot-val (slot-value instance (slot-definition-name slot))))
            (cond
             ((null slot-val)
              (setf (slot-value new-instance (slot-definition-name slot))
                    nil))
             ((typep slot-val 'intension)
              (setf (slot-value new-instance (slot-definition-name slot))
                    (copy-class-instance slot-val)))
             ((and (listp slot-val)
                   (some #'(lambda (x) (typep x 'intension)) slot-val))
              (setf (slot-value new-instance (slot-definition-name slot))
                    (copy-instance-list slot-val)))
             ((and (listp slot-val)
                   (some #'(lambda (x) (typep x 'model)) slot-val))
              (setf (slot-value new-instance (slot-definition-name slot))
                    (copy-instance-list slot-val)))
             ((listp slot-val)
               (setf (slot-value new-instance (slot-definition-name slot))
                     (copy-tree slot-val)))
             ((or (symbolp slot-val) (numberp slot-val))
               (setf (slot-value new-instance (slot-definition-name slot))
                     slot-val)))))
    new-instance))

(defun copy-instance-list (instance-list)
  "Copy list of instances of classes"
  (let* ((new-instance-list nil))
    (dolist (el instance-list)
      (push (copy-class-instance el) new-instance-list))
    new-instance-list))

; ---------------------------------------------------------------------------------
; Section 1.2: Classes for conditions
; ---------------------------------------------------------------------------------

(define-condition parser-error (error)
  ((text :initarg :text :reader text)))

(define-condition consistency-error (error)
  ((text :initarg :text :reader text)))

(define-condition ambiguity-warning (warning)
  ((text :initarg :text :reader text)))

(define-condition false-heuristic-conclusion-error (error)
  ((text :initarg :text :reader text)))

; ---------------------------------------------------------------------------------
; Section 1.3: Classes for intensions
; ---------------------------------------------------------------------------------

(defclass intension ()
  ((first-argument :accessor first-argument :initarg :first-argument)
   (second-argument :accessor second-argument :initarg :second-argument)
   (active :accessor active :initarg :active :initform t))
  (:documentation "Parent class for all types of intensions that contains a slot
                   for whether or not the intension is active in the model. By
                   default, all intensions are active"))

(defclass null-intension ()
  ((description  :accessor description  :initarg :desc)
   (abbreviation :accessor abbreviation :initarg :abbr))
  (:documentation "Intension for special case assertions, e.g., 'no valid conclusion', 'impossible', etc."))

(defconstant *nvc* (make-instance 'null-intension :desc "No valid conclusion" :abbr "NVC"))

(defclass q-intension (intension)
  ((first-argument  :accessor subject     :initarg :subj)
   (second-argument :accessor object      :initarg :obj)
   (cardinality     :accessor cardinality :initarg :card)
   (numprop         :accessor numprop     :initarg :np)
   (boundary        :accessor boundary    :initarg :bnd)
   (polarity        :accessor polarity    :initarg :pol)
   (footnotes       :accessor footnotes   :initarg :fn)
   (relation        :accessor relation    :initarg :rel))
  (:documentation "Intension for quantified assertions"))

(defclass t-intension (intension)
  ((first-argument        :accessor subject               :initarg :subj)
   (second-argument       :accessor object                :initarg :obj)
   (precedence            :accessor precedence            :initarg :prec)
   (start-time            :accessor start-time            :initarg :start)
   (end-time              :accessor end-time              :initarg :end)
   (reference-time        :accessor reference-time        :initarg :ref)
   (relation-to-utterance :accessor relation-to-utterance :initarg :ut))
  (:documentation "Intension for temporal assertions"))

(defclass sp-intension (intension)
  ((first-argument           :accessor subject           :initarg :subj)
   (second-argument          :accessor object            :initarg :obj)
   (first-argument-modifier  :accessor subject-modifier  :initarg :subj-mod)
   (second-argument-modifier :accessor object-modifier   :initarg :obj-mod)
   (relation                 :accessor spatial-relation  :initarg :rel)
   (dimension                :accessor spatial-dimension :initarg :dim)
   (template                 :accessor spatial-template  :initarg :temp)
   (distance                 :accessor spatial-distance  :initarg :dist))
  (:documentation "Intension for spatial assertions"))

(defclass s-intension (intension)
  ((first-argument  :accessor first-clause  :initarg :first-clause)
   (second-argument :accessor second-clause :initarg :second-clause)
   (both            :accessor both          :initarg :both)
   (first-only      :accessor first-only    :initarg :first-only)
   (second-only     :accessor second-only   :initarg :second-only)
   (neither         :accessor neither       :initarg :neither))
  (:documentation "Intension for sentences and sentential connectives"))

(defclass c-intension (s-intension)
  ((temporal-constraint  :accessor constraint  :initarg :constraint))
  (:documentation "Intension for causal connectives"))

(defclass e-intension (intension)
  ((first-argument  :accessor agent     :initarg :agent)
   (affirms         :accessor affirms   :initarg :affirms)
   (negates         :accessor negates   :initarg :negates)
   (factivity       :accessor factivity :initarg :factivity)
   (second-argument :accessor clause    :initarg :clause))
  (:documentation "Intension for epistemic verbs"))

; ---------------------------------------------------------------------------------
; Section 1.4: Classes for models
; ---------------------------------------------------------------------------------

(defclass model ()
  ((entities :accessor entities :initarg :ents)
   (capacity :accessor capacity :initarg :capacity :initform 'infinity)
   (footnote :accessor footnote :initarg :fn))
  (:documentation "Parent class for all sorts of iconic models"))

(defclass q-model (model)
  ((entities :accessor individuals :initarg :indivs))
  (:documentation "Quantified model in which all instances are individuals"))

(defclass s-model (model)
  ((entities :accessor possibilities :initarg :poss))
  (:documentation "Sentential model in which all instances are conjunctions of atomic
                   sentences, i.e., terms that stand in place of propositions"))

(defclass t-model (model)
  ((entities :accessor moments :initarg :moments))
  (:documentation "Temporal model in which all instances are moments, i.e., episodic markers of
                   past, present, future points in time along a linear timeline"))

(defclass sp-model (model)
  ((entities :accessor things :initarg :things)
   (dimensions :accessor dimensions :initarg :dims))
  (:documentation "Spatial model in which all instances are things (objects)."))

(defclass null-model (model)
  ((conflict :accessor conflict :initarg :conf))
  (:documentation "Null model that is constructed when model building yields a conflict"))

(defgeneric equals (entity-1 entity-2))

; ---------------------------------------------------------------------------------
; Section 1.5: Class for JSON output
; ---------------------------------------------------------------------------------

(defclass json-output ()
  ((description   :accessor description   :initarg :desc  :initform nil)
   (validation    :accessor validation    :initarg :val   :initform nil)
   (model-string  :accessor model-string  :initarg :print :initform nil)
   (model         :accessor model         :initarg :mod   :initform nil)
   (warning       :accessor warn-log      :initarg :war   :initform nil)
   (error         :accessor error-log     :initarg :err   :initform nil))
  (:documentation "Class for JSON output"))

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