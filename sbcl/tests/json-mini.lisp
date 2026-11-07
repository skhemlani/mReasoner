; ---------------------------------------------------------------------------------
; Minimal JSON writer used by the reference generator. It does not depend on jsown.
;
; Representation:
;   (:obj ("key" . value) ...)  object
;   list                        array
;   string / number             as is
;   :true :false :null          literals
;   :empty                      empty array (use json-arr)
; ---------------------------------------------------------------------------------

(defun json-escape (s stream)
  (write-char #\" stream)
  (loop for c across s
        do (case c
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (t (if (< (char-code c) 32)
                    (format stream "\\u~4,'0X" (char-code c))
                    (write-char c stream)))))
  (write-char #\" stream))

(defun json-write (x stream &optional (indent 0))
  (let ((pad (make-string (+ indent 2) :initial-element #\Space))
        (end (make-string indent :initial-element #\Space)))
    (cond
      ((eq x :empty) (write-string "[]" stream))
      ((eq x :true) (write-string "true" stream))
      ((eq x :false) (write-string "false" stream))
      ((or (eq x :null) (null x)) (write-string "null" stream))
      ((stringp x) (json-escape x stream))
      ((integerp x) (format stream "~D" x))
      ((numberp x) (format stream "~F" (float x 1.0d0)))
      ((symbolp x) (json-escape (string x) stream))
      ((and (consp x) (eq (first x) :obj))
       (if (null (rest x))
           (write-string "{}" stream)
           (progn
             (format stream "{~%")
             (loop for (pair . more) on (rest x)
                   do (write-string pad stream)
                      (json-escape (car pair) stream)
                      (write-string ": " stream)
                      (json-write (cdr pair) stream (+ indent 2))
                      (when more (write-string "," stream))
                      (terpri stream))
             (write-string end stream)
             (write-string "}" stream))))
      ((consp x)
       (format stream "[~%")
       (loop for (item . more) on x
             do (write-string pad stream)
                (json-write item stream (+ indent 2))
                (when more (write-string "," stream))
                (terpri stream))
       (write-string end stream)
       (write-string "]" stream))
      (t (json-escape (prin1-to-string x) stream)))))

(defun json-arr (list)
  "Forces an empty list to be written as [] and not as null."
  (or list :empty))

(defun json-to-file (x path)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :external-format :utf-8)
    (json-write x out)
    (terpri out))
  path)

(defun env (name default)
  (or (sb-posix:getenv name) default))

(defmacro capture (&body body)
  "Runs BODY capturing what it prints. Returns (values text error)."
  (let ((vals (gensym)) (txt (gensym)) (err (gensym)))
    `(let (,vals ,err)
       (let ((,txt (with-output-to-string (*standard-output*)
                     (handler-case (setf ,vals (multiple-value-list (progn ,@body)))
                       (error (e) (setf ,err (err-str e)))))))
         (list ,vals ,txt ,err)))))

(defun err-str (e)
  "Text of an error without memory addresses ({1002662C23}) or line breaks,
   so that the same error reads the same in every run."
  (let* ((s (princ-to-string e))
         (out (with-output-to-string (o)
                (loop with i = 0
                      while (< i (length s))
                      do (let ((c (char s i)))
                           (if (char= c #\{)
                               (let ((j (position #\} s :start i)))
                                 (if (and j (every (lambda (ch) (digit-char-p ch 16))
                                                   (subseq s (1+ i) j)))
                                     (progn (write-string "{...}" o) (setf i (1+ j)))
                                     (progn (write-char c o) (incf i))))
                               (progn (write-char c o) (incf i))))))))
    (string-trim " " (format nil "~{~A~^ ~}"
                             (let ((parts nil) (start 0))
                               (loop for k from 0 to (length out)
                                     do (when (or (= k (length out))
                                                  (member (char out k) '(#\Space #\Newline #\Tab)))
                                          (when (> k start) (push (subseq out start k) parts))
                                          (setf start (1+ k))))
                               (nreverse parts))))))
