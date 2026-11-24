; ---------------------------------------------------------------------------------
; Part 0: Patches
; ---------------------------------------------------------------------------------

; Section 0.1: CCL compatibility patches
; Section 0.2: SBCL compatibility patches

; ---------------------------------------------------------------------------------
; Section 0.1: CCL compatibility patches (from Dan Bothell's ACT-R codebase)
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

; ---------------------------------------------------------------------------------
; Section 2.2: SBCL compatibility patches (by Juan Manuel Goya (2026-11-07 email)
; ---------------------------------------------------------------------------------

#+sbcl (setf sb-impl::*default-external-format* :latin-1)

#+sbcl (defparameter *mr-root*
         (make-pathname :name nil :type nil
                        :defaults (or *load-truename* *default-pathname-defaults*)))

; LispWorks exposes the MOP functions in CL-USER; in SBCL they live in SB-MOP
#+sbcl (import '(sb-mop:class-slots sb-mop:class-direct-slots sb-mop:slot-definition-name))

; Utilities.lisp calls IGNORE as a function (LispWorks tolerates it)
#+sbcl (sb-ext:unlock-package :common-lisp)
#+sbcl (setf (fdefinition 'ignore) (lambda (&rest args) (declare (cl:ignore args)) nil))
#+sbcl (sb-ext:lock-package :common-lisp)

; CL-USER in SBCL inherits symbols from SB-EXT whose names mReasoner redefines
#+sbcl (shadow '("PROCESS-INPUT"))

; jsown is loaded first, with absolute paths
#+sbcl (dolist (f '("packages" "accessors" "reader" "writer"))
         (load (merge-pathnames (format nil "jsown-master/~A.lisp" f) *mr-root*)))
#+sbcl (pushnew :jsown *features*)

; Minimal split-sequence: the delimiter can be a character or a string of
; separator characters (LispWorks semantics)
#+sbcl (defun split-sequence (delimiter seq &key (remove-empty-subseqs nil) &allow-other-keys)
         (let ((test (if (characterp delimiter)
                         (lambda (c) (char= c delimiter))
                       (lambda (c) (find c delimiter))))
               (out nil) (start 0))
           (loop for i from 0 to (length seq)
                 do (when (or (= i (length seq)) (funcall test (elt seq i)))
                      (let ((piece (subseq seq start i)))
                        (unless (and remove-empty-subseqs (zerop (length piece)))
                          (push piece out)))
                      (setf start (1+ i))))
           (nreverse out)))

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