; ---------------------------------------------------------------------------------
; Part 5: Observer
; ---------------------------------------------------------------------------------
;
; This module receives input in the form of a JSON file. The file can specify
; either the spatial layout of an observation, or else a set of premises. In either
; case, the system constructs a model. The JSON file can also specify a task to
; perform on the model constructed:
;
;      i. It can merely return the model (observe)
;     ii. Given an instruction, e.g., "what is left of the X", it can return a
;         model that isolates only those items that are left of the X (focus-on)
;    iii. Given one or more conclusions, it can validate whether the conclusion holds
;         in the perceptual model
;     iv. It can generate a description of perceptual model
;
; ---------------------------------------------------------------------------------
; Section 5.1: JSON input processing + task execution
; Section 5.2: Focus task
; Section 5.3: Validate observation task
; Seciton 5.4: Generate description task
; ---------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------
; Section 5.1: JSON input processing + task execution
; ---------------------------------------------------------------------------------

(defun print-header ()
  (format t "---------------------------------------------------------------~%~
	     mReasoner version ~A (~A)                                      ~%~
             ASTEREX system for spatial reasoning                           ~%~
	     Copyright (C) 2022 by S. Khemlani and P.N. Johnson-Laird       ~%~
                                                                            ~%~
             This software is licensed under a Creative Commons Attribution-~%~
             NonCommercial-ShareAlike 4.0 International License. For more   ~%~
             information on this license, please visit:                     ~%~
                                                                            ~%~
                    http://creativecommons.org/licenses/by-nc-sa/4.0/       ~%~
             ---------------------------------------------------------------~%"
                *version* "2022-05-20"))

(defun process-input (&key (input-file nil) (output-file nil))
  "High level fn to take observation (in the form of a JSON file) + task, and output
   the result; if input file isn't given, default to mReasoner REPL.

   This function serves as high-level function for the delivery script in Deliver.lisp"
  (initialize-tracer)
  (let* ((input-file  (if input-file input-file (find-argument "--input")))
         (output-file (if output-file output-file (find-argument "--output"))))
    (if input-file
        (process-json-input-file input-file output-file)
      (progn
        (print-header)
        (mreasoner-repl)))))

(defun process-json-input-file (input-file &optional output-file)
  "Read input file, parse it into JSON object, send it to observe-and-comprehend;
   if output-file, write JSON to file"
  (handler-case
      (let* ((input-json (jsown:parse (read-file-to-string input-file)))
             (output-json (observe-or-comprehend input-json)))
        (if (and output-file output-json)
            (write-to-json (to-json-object output-json) output-file)))
    (parser-error ()
      (let ((error-output (make-instance 'json-output)))
        (setf (error-log error-output) "Parser error")
        (write-to-json (to-json-object error-output) output-file)))
    (error ()
      (let ((error-output (make-instance 'json-output)))
        (setf (error-log error-output) "Unspecified error")
        (write-to-json (to-json-object error-output) output-file)))))

(defun observe-or-comprehend (json &key (print-model t))
  "If JSON object includes 'observations' key, run process-observation;
   if it includes 'premises' key, run process-premises;
   Print the observed or interpreted model and save it to output object;
   Then send json + model to proces-task"
  (let ((output (make-instance 'json-output))
        model)
    (handler-case
        (if (equals (first json) :obj)
            (progn
              
              (cond
               ((some #'(lambda (x) (equals "observations" x)) (mapcar #'first (rest json)))
                (setf model (process-observation json)))
               ((some #'(lambda (x) (equals "premises" x)) (mapcar #'first (rest json)))
                ;(parse-premises-to-intensions json)  ;;;;; do we need this? (ssk - 2022-06-05)
                (setf model (first (interpret (process-premises json))))))

              (when print-model (print-model model) (format t "~%"))
              
              (setf (model output) model)

              (process-task json model output))
          (progn
            (format t "Error: improperly formatted JSON file.~%")
            (setf (error-log output) "Improperly formatted JSON file.")))
      (consistency-error ()
        (format t "Inconsistent relations; unable to build model.~%")
        (setf (error-log output) "Inconsistent relations; unable to build model.")))))

(defun process-observation (json-observation)
  "Constructs spatial model from observations JSON file"
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

(defun process-premises (json)
  (let ((premises (get-json-premises json)))
    (mapcar #'parse-premise-to-intension premises)))

(defun process-task (json model output)
  (let* ((type (if (footnote model) :premises :observations))
         (obj  (rest json))
         (task (position "task" obj :test #'equals :key #'first))
         (task (when task (read-from-string (rest (nth task obj)))))
         (args (position "arguments" obj :test #'equals :key #'first))
         (args (when args (rest (nth args obj))))
         relations)

    (dolist (relation args)
      (push (parse (format nil "~{~a~^ ~}" relation)) relations))
    (setf relations (reverse relations))

    (case type
      (:premises     nil)
      (:observations (when (not task) (setf task 'observe))))
    (case task
      (observe
       (setf (model output)        model)
       (setf (model-string output) (serialize-model model)))
      (focus-on
       (setf (model output)        (focus-on (first relations) model))
       (setf (model-string output) (split-sequence (format nil "~%") (serialize-model (model output)))))
      (validate
       (setf (validation output)  (validate-observation type relations model)))
      (describe
       (setf (description output) (generate-description model))))

    (setf *output* output)

    output))

(defun get-json-premises (json-object)
  (let ((obj (rest json-object)))
    (mapcar #'rest (rest (nth (position "premises" obj :test #'equals :key #'first) obj)))))

(defun parse-premise-to-intension (json-premise)
  (let* ((obj1 (read-from-string (rest (assoc "firstObject" json-premise :test 'equal))))
         (rel  (convert-to-relation (rest (assoc "relation" json-premise :test 'equal))))
         (obj2 (read-from-string (rest (assoc "secondObject" json-premise :test 'equal)))))
    (parse (flatten `(,obj1 ,rel ,obj2)))))

(defun convert-to-relation (relation)
  (cond
   ((string= relation "leftOf")               '(is to the left of))
   ((string= relation "is to the left of")    '(is to the left of))
   ((string= relation "rightOf")              '(is to the right of))
   ((string= relation "is to the right of")   '(is to the right of))
   ((string= relation "behind")               '(is behind))
   ((string= relation "is behind")            '(is behind))
   ((string= relation "inFrontOf")            '(is in front of))
   ((string= relation "is in front of")       '(is in front of))
   ((string= relation "above")                '(is above))
   ((string= relation "is above")             '(is above))
   ((string= relation "below")                '(is below))
   ((string= relation "is below")             '(is below))))

; ---------------------------------------------------------------------------------
; Section 5.2: Focus task
; ---------------------------------------------------------------------------------

(defmethod focus-on ((directive sp-intension) (observation sp-model))
  (let* ((duplicates (duplicate-entities observation))
         (observations (build-dummy-sp-models duplicates observation))
         focus)
    (loop for o in observations
          do
            (let* ((entities (flatten (entities o)))
                   (foc (copy-class-instance o))
                   (validation (apply-directive-to-all-entities (copy-class-instance directive)
                                                                entities)))
              (loop for v in validation
                    for e in entities
                    if (not (validate-model v o))
                      do
                        (setf foc (find-and-replace-all e nil foc)))
              (setf focus foc)))
    
    (setf (things focus) (resolve-dummy-sp-models duplicates (things focus)))
    focus))

(defun apply-directive-to-all-entities (directive entities)
  (let ((directive-copy (copy-class-instance directive)))
    (loop for e in entities
          collect (if (equals 'WHAT (first-argument directive))
                      (progn
                        (setf (first-argument directive-copy) e)
                        (setf (spatial-template directive-copy)
                              (sp-find-and-replace 'WHAT e (spatial-template directive)))
                        (copy-class-instance directive-copy))
                    (error "Improperly formed focus query.")))))

; ---------------------------------------------------------------------------------
; Section 5.3: Validate observation task
; ---------------------------------------------------------------------------------

(defun validate-observation (type relations observation)
  (let (validation validation-output)
    (case type
      (:premises     nil)
      (:observations
       (format t "~45@<Queried relation~>~15<Holds in model~>~%~
                     ------------------------------------------------------------~%")
       (dolist (relation relations)
         (handler-case
             (setf validation-output (if (validate (parse relation) (list observation)) "YES" "NO"))
           (parser-error () (setf validation-output "ERROR")))
         (format t "~45@<~A.~>~15<~A~>~%" relation validation-output)
         (push (list relation validation-output) validation))
       (format t "~%")
       (reverse validation)))))

; ---------------------------------------------------------------------------------
; Section 5.4: Generate description task
; ---------------------------------------------------------------------------------

(defmethod generate-description ((observation sp-model))
  "Constructions description of spatial model"
  (format t "")
  (let ((description
         (format nil "~{~#[None~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;,~%~]~}~]~:}"
                 (mapcar #'(lambda (x) (to-english x)) (pre-filter-relations (find-all-relations observation))))))
    (format t "Description~%-----------~%~A.~%~%" description)
    description))

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

(defmethod find-all-relations ((observation sp-model))
  (let* ((things       (flatten (things observation)))
         (combinations (combinations-with-replacement things 2))
         (combinations (remove-if #'(lambda (x) (equals (first x) (second x))) combinations))
         (relations    (mapcar #'(lambda (x) (funcall #'test-1d-relations observation (first x) (second x))) combinations))
         (relations    (flatten relations)))
    relations))

(defun pre-filter-relations (relations)
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

