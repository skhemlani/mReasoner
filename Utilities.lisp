; ---------------------------------------------------------------------------------
; Part 2: Utilities
; ---------------------------------------------------------------------------------

; Section 2.1: Miscellaneous utility functions
; Section 2.2: Printing functions
; Section 2.3: CCL compatibility patches

; ---------------------------------------------------------------------------------
; Section 2.1: Miscellaneous utility functions
; ---------------------------------------------------------------------------------

(defmacro while (test &rest body)
  "Repeat body while test is true."
  (list* 'loop
         (list 'unless test '(return nil))
         body))

(defun flatten (lis)
  "Takes a nested list and makes in into a single-level list"
  (declare (list lis))
  (labels ((rec (lis acc)
             (cond ((null lis) acc)
                   ((atom lis) (cons lis acc))
                   (t (rec (car lis) (rec (cdr lis) acc))))))
    (rec lis nil)))

(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) 
  lst)

(defun insert-at (item list index &key (append nil))
  "Inserts item into list (of lists) at position index; if :append is t,
   then it appends the item to the end of list at position index"
  (cond
    ((< index 0)                     (append item (copy-list list)))
    ((and (= index 0)
          (or (equals append :after)
              (equals append t)))    (cons (append (copy-list (first list)) item) (rest list)))
    ((and (= index 0)
          (equals append :before))   (cons (append item (copy-list (first list))) (rest list)))
    ((and (= index 0) (not append))  (cons item list))
    ((endp list)                     (list item))
    (t (cons (first list)            (insert-at item (rest list) (1- index) :append append)))))

(defun remove-from (item list-of-lists)
  "Removes an item from a list of lists, and deletes nils produced if necessary"
  (let* ((list-of-lists (mapcar #'(lambda (x) (remove item x :test #'equals)) list-of-lists))
         (list-of-lists (remove-if #'null list-of-lists)))
    list-of-lists))

(defun all-positions (needle haystack)
  (loop
    for element in haystack 
    and position from 0
     when (equals element needle)
      collect position))

(defun all-permutations (lst &optional (remain lst))
  "Rtns list of all possible permutations of lst.
   Adapted from code posted by Orm Finnendahl on Stackoverflow.com."
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

(defun randomize (list)
  "Randomizes the order of elements in a list"
  (let ((n (length list))
        (result nil))
    (setf list (copy-tree list))
    (dotimes (i n result)
      (let ((data (nth (random (- n i)) list)))
        (push data result)
        (setf list (delete data list))))))

(defun depth (lis-of-lis)
  "rtns depth of embedding of lists, so can be used to
   distinguish between a single model as opposed to a modelset, e.g.,
   (depth '( ( ((A)(B)) )( ((B)(C)) ))) => 4, i.e., a modelset"
  (when (listp lis-of-lis) (setf lis-of-lis (remove-if #'null lis-of-lis)))
  (cond((or (null lis-of-lis) (atom lis-of-lis)) 0)
       ((listp lis-of-lis) (+ 1 (depth (first lis-of-lis))))))

(defun replace-nth-from-list  (list n elem)
  ;"Given a list, replaces the nth element and returns the list.
  ;(replace-nth-from-list (list 1 2 3) 1 4)
  ;=>(1 4 3)"
  (cond
    ((null list) ())
    (t (append (subseq list 0 n) (list elem) (subseq list (+ 1 n)(length list))))))

(defun transpose-list (list-of-lists)
  "Transposes a two-dimensional list of lists, e.g.,
   (transpose-list '((A B) (C D) (E F) (G H))) => ((A C E G) (B D F H))"
  (let* ((new-list nil))
    (dotimes (i (length (first list-of-lists)))
      (push (mapcar #'(lambda (x) (nth i x)) list-of-lists) new-list))
    (reverse new-list)))

(defun split (string &optional max (ws '(#\Space #\Tab)))
  "Split `string' along whitespace as defined by the sequence `ws'.
   Whitespace which causes a split is elided from the result.  The whole
   string will be split, unless `max' is provided, in which case the
   string will be split into `max' tokens at most, the last one
   containing the whole rest of the given `string', if any."
  (flet ((is-ws (char) (find char ws)))
    (nreverse
     (let ((list nil) (start 0) (words 0) end)
       (loop
        (when (and max (>= words (1- max)))
          (return (cons (subseq string start) list)))
        (setf end (position-if #'is-ws string :start start))
        (push (subseq string start end) list)
        (incf words)
        (unless end (return list))
        (setf start (1+ end)))))))
 
(defun extract-all-individuals (individuals)
  "removes list structure, '(((A)(B))((B)(- C))) => ((A)(B)(B)(- C))"
  (if (null individuals) 
      nil
    (append (first individuals)(extract-all-individuals (rest individuals)))))

(defun factorial (n)
  (if (< n 2)
      1
    (* n (factorial (- n 1)))))

(defun poisson-density (n lambda)
  "Adapted from the cl-math library developed by Gerald Roylance and repackaged by
   Scott Brown."
  (when (> n 34) (setf n 34)) ;; SSK fix for floating-point-overflow error in CCL
  (cond ((> lambda 0.0)
	 (/ (* (expt lambda n) (exp (- lambda)))
	    (float (factorial n))))
	((= lambda 0.0)
	 (cond ((= n 0) 1.0)
	       (t       0.0)))
	((< lambda 0.0) (ERROR "Negative lambda to POISSON-DENSITY"))))

(defun poisson-random-number (lambda)
  "Adapted from the cl-math library developed by Gerald Roylance and repackaged by
   Scott Brown."
  (do ((u (random 1.0))
       (p 0.0)
       (i 0 (1+ i)))
      ((progn (setq p (+ p (poisson-density i lambda)))
         (< u p))
       i)
    (declare (fixnum i)
	     (float u p))))

(defun read-file-to-string (file-path)
  (with-open-file (file-stream file-path)
    (let ((file-contents (make-string (file-length file-stream))))
      (read-sequence file-contents file-stream)
      file-contents)))

; ---------------------------------------------------------------------------------
; Section 2.2: Printing functions
; ---------------------------------------------------------------------------------

; General model printing ----------------------------------------------------------

(defun print-models (modelset)
  "Prints set of models"
  (let
      ((template (find-properties-in-modelset modelset))
       (output (make-array 0 
                           :element-type 'character 
                           :adjustable t 
                           :fill-pointer 0)))
    (dolist (model modelset)
      (setf (fill-pointer output) 0)
      (print-model model :template template :output output)
      (let*
          ((footnotes (footnote model))
           (abbreviation (format nil "~{~a~^, ~}"   ;concatenate all footnote abbreviations into one, comma-separated list
                                 (loop for footnote in footnotes
                                       collect (abbreviate footnote))))
           (model-print-lines (split-sequence (format nil "~%") output))
           (midpoint (floor (/ (length model-print-lines) 2)))
           (length-of-longest (length (first (sort (copy-list model-print-lines)
                                                   #'(lambda(x y) (> (length x) (length y)))))))
           (padded-output (mapcar #'(lambda(x)
                                       (concatenate 'string x (format nil "~vT" (- length-of-longest (- (length x) 1)))))
                                  model-print-lines))
           (line-with-abbreviation (concatenate 'string (elt padded-output midpoint) "   " abbreviation))
           (padded-w-abbrev (substitute line-with-abbreviation (elt padded-output midpoint) padded-output)))
        (format T "~{~a~% ~}" padded-w-abbrev)))))

(defun stringify-model (model)
  "Converts model into a simple-string"
  (let
      ((template (find-properties-in-modelset (list model)))
       (output (make-array 0 
                           :element-type 'character 
                           :adjustable t 
                           :fill-pointer 0)))
    (print-model model :template template :output output)
    (coerce output 'simple-string)))

; Printing for q-models ----------------------------------------------------------

(defmethod print-model ((model q-model) &key (template nil) (output nil) (separator "~%"))
"prints a q-model applies print-indiv to each individual, making new line 
first, print-indiv prints properties in order of template"
  (let* ((indivs (individuals model)) 
         (template (or template (find-properties indivs))))
    (dolist (indiv indivs)
      (format (or output t) separator)
      (print-indiv template indiv output))))

#| (print-indiv '((a)(b)(c)(d)) '((d)(- b))) => -B   D |#     
(defun print-indiv (template indiv &optional output)
  "prints each individual in order of template,
   where template = template rtnd by find-properties, and indiv is car.models
   Tail recursion on indiv 
   if car.indiv = car.template prints car.indiv and recurse
   elseif car.template matches property in indiv, reorders indiv by shifting car.indiv to 
     tail of indiv, recurse
   else print no. of spaces required for car.template, which isn't in indiv, recurse"
    (cond ((null indiv) nil)
          ((equal (affirm (first indiv)) (first template))
           (print-property (first indiv) output) 
           (print-indiv (rest template)(rest indiv) output))
          ((match-property (first template)(affirm-indiv indiv))
           (print-indiv template (append (rest indiv)(list (first indiv))) output))
          (t (print-spaces-for-property (first template) output)
             (print-indiv (rest template) indiv output))))

#| (print-property '(- heavy)) => -HEAVY
(setf t22 '(((a))))(print-property '(t22)) |#
(defun print-property (property &optional output)
  "prints a property, length 1 is affirmative, 2 is negative"
  (cond ((eq (first property) '-)
         (format (or output t) "~A¬~A" (print-spaces 2) (cadr property)))  
        (t 
         (format (or output t) "~A~A" (print-spaces 3) (first property)))))

(defun print-spaces-for-property (property &optional output)
  "print no of spaces = length of property in template + constant of 3
   to allow for space+negation. Itm is from template so always affirmative"
  (format (or output t) "~A"
          (print-spaces (+ 3 (length (symbol-name (first property)))))))   
       
#| (find-properties '(((a)) ((- b)) ((- a)(- c)) ((b)(d)))) => ((a)(b)(c)(d)) |#
(defun find-properties (model)
"makes list of all affirmative properties in a model"
  (let ((lis-of-properties))
    (dolist (indiv model)
      (dolist (property indiv lis-of-properties)
      (if (not(match-property (affirm property) lis-of-properties))
          (setf lis-of-properties (append lis-of-properties (list (affirm property)))))))
    lis-of-properties))

(defun find-properties-in-modelset (models)
"make a list of all affirmative properties in a set of models"
(let
    ((list-of-properties))
  (dolist (model models list-of-properties)
    (let*
        ((entities (case (class-name (class-of model))
                     (q-model (individuals model))
                     (s-model (list (possibilities model)))))
         (properties (find-properties entities))
         (new-properties (reverse (set-difference properties list-of-properties :test #'property-equal))))
      (setf list-of-properties (append list-of-properties new-properties))))))

(defun affirm-indiv (indiv)
  "Changes all properties in individual to affirmative"
  (mapcar #'affirm indiv))

(defun affirm (property)
  "Rtns affirmative property whether or not its negative"
  (if (and (listp property)(eq '- (first property)))(negate-property property)
    property))

(defun print-spaces (number)
  "prints number of spaces" 
  (cond ((<= number 0) "")
        (t  (concatenate 'string
                         (format nil " ")
                         (print-spaces (- number 1))))))

#| (match-property '(b) '((a)(b)(c))) => t, (match '(- b) '((a)(- b)(c))) => t
(match-property '((a)(b)) '( ((a)(b)) c)) => t |#
(defun match-property(prop-1 indiv)
  "Matches property or list of properties to an individual"
  (dolist (property indiv)
    (if (equal prop-1 property)(return t))))  

(defmethod serialize-model ((model q-model))
  "Serialize model, and its constituents below, i.e.,
   - serialize-entities
   - serialize-individual
   - serialize-property
   are recursive functions that create a 'serialized'
   string representation of a model. For instance, the
   model for Aab 

   A   B
   A   B
   A   B   is serialzed as AB|AB|AB|AB|
   A   B

   This function is primarily useful for calculating
   the (Levenshtein) distance between two models. Note
   that you cannot recover a model from its serialized
   representation."
  (format nil "~{| ~a~^~}" (serialize-q-entities (entities model))))

(defun serialize-q-entities (entities)
  (if (null entities) nil
    (append (list (serialize-q-individual (first entities))) (serialize-q-entities (rest entities)))))

(defun serialize-q-individual (individual)
  (if (null individual) ""
    (format nil "~A~A"
            (serialize-q-property (first individual))
            (serialize-q-individual (rest individual)))))

(defun serialize-q-property (property)
  (if (equalp (first property) '-)
      (format nil "-~A " (second property))
    (format nil "~A " (first property))))

; Printing for sp-models ----------------------------------------------------------

(defmethod serialize-model ((model sp-model))
  "Serialize model for sp-models"
  (let ((entities (entities model))
        transposed-entities)
    (dotimes (i (length (dimensions model)))
      (push (mapcar #'(lambda (x) (nth i x)) entities) transposed-entities)) 
    
    (format nil "~{~a~^ ~}" (serialize-sp-grid transposed-entities))))

(defun serialize-sp-grid (grid)
  (if (null grid) nil
    (append (list (serialize-sp-column (first grid))) (serialize-sp-grid (rest grid)))))

(defun serialize-sp-column (column)
  (if (null column) ""
    (format nil "[ ~A ] ~A+"
            (serialize-sp-cell (first column))
            (serialize-sp-column (rest column)))))

(defun serialize-sp-cell (cell)
  (format nil "~{~a~^ ~}" cell))

; Printing for t-models ----------------------------------------------------------

(defmethod serialize-model ((model t-model))
  "Serialize model for t-models"
  (format nil "~{~a~^~}" (serialize-t-entities (entities model))))

(defun serialize-t-entities (entities)
  (if (null entities) nil
    (append (list (serialize-t-individual (first entities))) (serialize-t-entities (rest entities)))))

(defun serialize-t-individual (individual)
  (if (null individual) ""
    (format nil "~A~A"
            (serialize-t-property (first individual))
            (serialize-t-individual (rest individual)))))

(defun serialize-t-property (property)
  (cond ((equalp (second property) 'start)
         (format nil "[~A " (first property)))
        ((equalp (second property) 'end)
         (format nil " ~A]" (first property)))
        (t
         (format nil " ~A " (first property)))))

#|
(setf t1 (make-instance 't-model :moments '(((A)) ((B))))) ; FAIL

(setf t2 (make-instance 't-model :moments '(((A start)) ((A end)) ((B start)) ((B end)))))

(setf t3 (make-instance 't-model :moments '(((A start)) ((A end)) ((B start)) ((B end)) ((C))))) ; FAIL

(setf t4 (make-instance 't-model :moments '(((A) (B))))) ; FAIL

(setf t5 (make-instance 't-model :moments '(((A start) (B start)) ((A end) (B end)))))

(setf t6 (make-instance 't-model :moments '(((A start)) ((B start)) ((B end)) ((A end)))))

(setf t7 (make-instance 't-model :moments '(((A start)) ((B start)) ((C start)) ((C end)) ((B end)) ((A end)))))

(setf t8 (make-instance 't-model :moments '(((A start)) ((B start)) ((A end)) ((B end)))))

(setf t9 (make-instance 't-model :moments '(((A start)) ((B start))  ((A end) (B end)))))

(setf t10 (make-instance 't-model :moments '(((A start) (B start))  ((B end))  ((A end)))))

(setf t11 (make-instance 't-model :moments '(((A START) (B START)) ((B END)) ((F START)) ((C)) ((F END))  ((D)) ((E START)) ((A END)) ((E END)) )))
|#

(defun find-events (events)
  "Find all events in model, both punctate and durational"
  (let* ((moments (flatten (mapcar #'(lambda (x) (mapcar #'first x)) events)))
         (template (remove-duplicates moments :test #'equal :from-end t)))
    template))

(defun pad-label (label padding)
  "Tacks on spaces (given by padding) to the end of a string"
  (format nil (format nil "~A~A~A~A~A" "~" padding "@<" label "~>")))

(defun start-event-p (event-marker)
  "Checks whether a particular event marker demarcates the start
   of an event, i.e., if it is of the pattern (_ START)"
  (equal 'start (second event-marker)))

(defun end-event-p (event-marker)
  "Checks whether a particular event marker demarcates the end
   of an event, i.e., if it is of the pattern (_ END)"
  (equal 'end (second event-marker)))

(defun establish-buffer (marker events)
  (let ((buffer " ") event-marker)
    (dotimes (e (length events))
        (setf event-marker (first (remove-if-not #'(lambda (x) (member (nth e events) x)) marker)))
        (setf buffer-length (length (format nil "~A" (first event-marker))))
        (when (and (start-event-p event-marker) (> buffer-length (length buffer)))
          (setf buffer (make-string buffer-length :initial-element #\Space))))
    buffer))

(defun splice-tracks (track1 track2 position)
  "Given two string tracks, e.g., '[A  ]       ' and '       [B  ]',
   returns a track that includes the strings by cutting the second string
   into the first at the specified position to yield, e.g., '[A  ]  [B  ]'"
  (concatenate 'string (subseq track1 0 position) (subseq track2 position)))

(defun pretty-print-t-model (tracks &key (template nil) (output nil) (separator "~%"))
  (let* ((d-event-main-track (position-if #'(lambda (x) (position "[" x :test #'string-equal)) tracks))
         (p-event-main-track (position-if #'(lambda (x) (not (position "[" x :test #'string-equal))) tracks)))
    (dotimes (i (length tracks))
      (when (and (not (equal i d-event-main-track))
                 (not (equal i p-event-main-track)))
        (let* ((start-of-d-event (position "[" (nth i tracks) :test #'string-equal))
               (start-of-event-name (position-if #'(lambda (x) (not (string-equal " " x))) (nth i tracks)))
               (end-of-event-name (position-if #'(lambda (x) (not (string-equal " " x))) (nth i tracks) :from-end t)))
          (if start-of-d-event
              (when (< (position "]" (nth d-event-main-track tracks) :test #'string-equal :from-end t) start-of-d-event)
                (setf (nth d-event-main-track tracks)
                      (splice-tracks (nth d-event-main-track tracks)
                                     (nth i tracks)
                                     start-of-d-event))
                (setf (nth i tracks) ""))
            (when (< (position-if #'(lambda (x) (not (string-equal " " x))) (nth p-event-main-track tracks) :from-end t)
                     start-of-event-name)
              (setf (nth p-event-main-track tracks)
                    (splice-tracks (nth p-event-main-track tracks)
                                   (nth i tracks)
                                   end-of-event-name))
              (setf (nth i tracks) "")))))))
  (setf tracks (remove-if #'(lambda (x) (equal 0 (length x))) tracks))
  (mapcar #'(lambda (x)
              (format (or output t) separator)
              (format (or output t) "~A" x)) tracks))

(defmethod print-model ((model t-model) &key (template nil) (output nil) (separator "~%"))
  (let* ((events   (find-events (moments model)))
         (tracks   (mapcar #'(lambda (x) (make-string 0)) events))
         event-marker buffer buffer-length)
    (dolist (marker (moments model))
      (setf buffer (establish-buffer marker events))
      (dolist (event-marker marker)
        (let* ((event-track (nth (position (first event-marker) events) tracks)))
          (cond
           ((equal (length event-marker) 1)
            (setf event-track (format nil "~A ~A" event-track (pad-label (first event-marker) (length buffer)))))
           ((start-event-p event-marker)
            (setf event-track (format nil "~A[~A" event-track (pad-label (first event-marker) (length buffer)))))
           ((end-event-p event-marker)
            (setf event-track (format nil "~A~A]" event-track buffer)))
           (t
            (setf event-track (format nil "~A~A " event-track buffer))))
          (setf (nth (position (first event-marker) events) tracks) event-track)))
      (dolist (event events)
        (when (not (member event (mapcar #'first marker) :test #'equal))
          (setf (nth (position event events) tracks)
                (format nil "~A~A " (nth (position event events) tracks) buffer))))) 
    (pretty-print-t-model tracks :output output :separator separator)
    nil))

; Printing for sp-models ---------------------------------------------------------

(defun print-thing-or-place (thing)
  (if (= 1 (length thing))
      (symbol-name (first thing))
    (format nil "~{~a~^+~}" (mapcar #'symbol-name thing))))

(defmethod print-model ((model sp-model) &key (template nil) (output nil) (separator "~%"))
  (case (length (dimensions model))
    (1 (format (or output t) "~{~a~^  ~}" (mapcar #'print-thing-or-place (things model))))
    (2 (print-2d-model model :template template :output output :separator separator))
    (3 (error "print-model is not implemented for 3D models yet."))))

(defmethod print-2d-model ((model sp-model) &key (template nil) (output nil) (separator "~%")) ;; 2d models only!
  (let* ((things       (reverse (transpose-list (things model))))
         (columns      (length (first things)))
         (rows         (length things))
         (print-width  15)
         thing-string column-string)
    (dotimes (i rows)
      (setf column-string "")
      (dotimes (j columns)
        (setf thing-string (print-thing-or-place (nth j (nth i things))))
        (if (equals thing-string "NIL")
            (setf thing-string "")
          (when (> (length thing-string) print-width) (setf thing-string (format nil "~A." (subseq thing-string 0 (1- print-width))))))
        (setf column-string (format nil "~A~17@<~A~>" column-string thing-string)))
      (format (or output t) (format nil "~A~A" column-string separator)))))

; Printing for s-models ----------------------------------------------------------

(defmethod print-model ((model s-model) &key (template nil) (output nil) (separator "~%") (print-footnotes nil))
  "prints a set of models properly aligned"
  (let* ((possibilities (remove-duplicates (possibilities model) :test #'equals))
         (footnotes (mapcar #'footnote (possibilities model)))
         (template (make-print-template (mapcar #'possibilities possibilities))))
    (format (or output t) "~%Model of: ~{~a~^, ~}~%" (mapcar #'abbreviate (footnote model)))
    (dotimes (i (length possibilities))
      (format (or output t) separator)
      (print-and-apply-template template
                                (possibilities (nth i possibilities))
                                (when print-footnotes (nth i footnotes))
                                output))))

(defun make-print-template (models)
  "lists all literals in models, making them affirmative, and ignoring footnotes, then adds one if there is one 
   in models, treating all bound variables as though they were the same."
  (affirm-indiv (find-properties models)))

(defun print-and-apply-template (template model footnote &optional (output nil))
  "uses template to print items in model with appropriate separations"
  (let ((lead-in 4))
    (cond ((null model)
           (format (or output t) "~A" (print-spaces (+ lead-in (reduce #'+ (mapcar #'(lambda (x) (+ 4 (length x))) (mapcar #'symbol-name (mapcar #'first template)))))))
           (print-footnote footnote output)
           nil)
          ((null template)
           (print-possibility (first model) output)
           (print-and-apply-template template (rest model) footnote output))    
          ((equal (affirm (first template)) (affirm (first model)))                        ; first item in model = first in template
           (print-possibility (first model) output)
           (print-and-apply-template (rest template) (rest model) footnote output))
          ((match (affirm (first template)) (affirm-indiv (find-properties (list model)))) ; first item in model != first in template
           (print-and-apply-template template (append-mods (rest model) (list(first model))) footnote output))
          (t  (format (or output t) "~A" (print-spaces (+ lead-in (length (symbol-name (caar template))))))
              (print-and-apply-template (rest template) model footnote output)))))

(defun print-footnote (footnote &optional (output nil))
  (when footnote (format (or output t) "{ ~{~a~^, ~} }" (mapcar #'abbreviate footnote))))

(defun print-possibility (item &optional (output nil))
  "prints single prop e.g. '(a1) '(- a1) or '(t121) which is a gentemp variable"
(cond((null item) t)
     ((eq (first item) '-)
      (format (or output t) "~A¬ ~A" (print-spaces 2) (second item)))
     (t
      (format (or output t) "~A~A"  (print-spaces 4) (first item)))))

(defun findatms (mods &optional lis)
  "rtns a list of each atom, once only, in the order that they occur in mods, treatingatm and its negation as same"
  (if (null mods) 
      (reverse lis)
    (findatms (cdr mods) (append (fi-atms (car mods) lis)))))

(defun fi-atms(mod lis)
  "rtns literals in mod with those in lis, and rtns each once only"
  (cond((null mod) lis)
       ((or (match (car mod) lis)
            (match (negate-property (car mod)) lis)) (fi-atms (cdr mod) lis))
       (t (fi-atms (cdr mod) (cons (car mod) lis)))))

(defun footnotep(literal)
  (boundp (car (last literal))))

(defun modelize(var)
  "for sys 1, turns variable into set of models; for sys 2 evaluates the variable"
  (list(list(list var))))

(defun rem-atom (atom model)
  "remove atom from a model"
  (cond ((null model) nil)
        ((equal atom (car model)) (cdr model))
        (t (cons (car model) (rem-atom atom (cdr model))))))

(defun append-mods(models1 models2)
  "makes single set of models from two sets"
  (cond((eq models1 t)
         (cond((eq models2 t) t)
              ((eq models2 nil) t)
              (t (append (list t) models2))))
     ((eq models2 t)(append models1 (list t)))
     (t (append models1 models2))))

; ---------------------------------------------------------------------------------
; Section 2.3: CCL compatibility patches
; ---------------------------------------------------------------------------------

#+ccl (defun split-sequence (delimiter seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (test nil test-supplied) (test-not nil test-not-supplied) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by delimiter.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (nconc (when test-supplied 
                             (list :test test))
                           (when test-not-supplied 
                             (list :test-not test-not))
                           (when key-supplied 
                             (list :key key)))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position delimiter seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position delimiter seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

#+ccl (defun split-sequence-if (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
predicate.

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
        (other-keys (when key-supplied 
		      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if predicate seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if predicate seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

#+ccl (defun split-sequence-if-not (predicate seq &key (count nil) (remove-empty-subseqs nil) (from-end nil) (start 0) (end nil) (key nil key-supplied))
  "Return a list of subsequences in seq delimited by items satisfying
(CL:COMPLEMENT predicate).

If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE-IF-NOT.  In particular,
the behaviour of :from-end is possibly different from other versions
of this function; :from-end values of NIL and T are equivalent unless
:count is supplied. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped."
  (let ((len (length seq))
	(other-keys (when key-supplied 
		      (list :key key))))
    (unless end (setq end len))
    (if from-end
        (loop for right = end then left
              for left = (max (or (apply #'position-if-not predicate seq 
					 :end right
					 :from-end t
					 other-keys)
				  -1)
			      (1- start))
              unless (and (= right (1+ left))
                          remove-empty-subseqs) ; empty subseq we don't want
              if (and count (>= nr-elts count))
              ;; We can't take any more. Return now.
              return (values (nreverse subseqs) right)
              else 
              collect (subseq seq (1+ left) right) into subseqs
              and sum 1 into nr-elts
              until (< left start)
              finally (return (values (nreverse subseqs) (1+ left))))
      (loop for left = start then (+ right 1)
            for right = (min (or (apply #'position-if-not predicate seq 
					:start left
					other-keys)
				 len)
			     end)
            unless (and (= right left) 
                        remove-empty-subseqs) ; empty subseq we don't want
            if (and count (>= nr-elts count))
            ;; We can't take any more. Return now.
            return (values subseqs left)
            else
            collect (subseq seq left right) into subseqs
            and sum 1 into nr-elts
            until (>= right end)
            finally (return (values subseqs right))))))

;;; clean deprecation

#+ccl (defun partition (&rest args)
  (apply #'split-sequence args))

#+ccl (defun partition-if (&rest args)
  (apply #'split-sequence-if args))

#+ccl (defun partition-if-not (&rest args)
  (apply #'split-sequence-if-not args))

#+ccl (define-compiler-macro partition (&whole form &rest args)
  (declare (ignore args))
  (warn "PARTITION is deprecated; use SPLIT-SEQUENCE instead.")
  form)

#+ccl (define-compiler-macro partition-if (&whole form &rest args)
  (declare (ignore args))
  (warn "PARTITION-IF is deprecated; use SPLIT-SEQUENCE-IF instead.")
  form)

#+ccl (define-compiler-macro partition-if-not (&whole form &rest args)
  (declare (ignore args))
  (warn "PARTITION-IF-NOT is deprecated; use SPLIT-SEQUENCE-IF-NOT instead")
  form)

#+ccl (pushnew :split-sequence *features*)
