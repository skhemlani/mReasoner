; ---------------------------------------------------------------------------------
; mReasoner loader for SBCL. It does not modify any original file.
;
; Usage (from the repository folder):
;   sbcl --load load-sbcl.lisp
; or from SLIME / Portacle:
;   (load "/path/to/mReasoner/load-sbcl.lisp")
;
; It works around the problems that prevent loading +mReasoner.lisp as is in SBCL:
;   1. API.lisp and Utilities.lisp are ISO-8859-1, not UTF-8.
;   2. FILE-PATH for SBCL passes the arguments of MERGE-PATHNAMES in reverse order
;      and depends on the current directory, so jsown-master/load.lisp is never found.
;   3. SPLIT-SEQUENCE only exists in LispWorks (native) and in CCL (patch), and
;      PARSE of a string only splits it into words under those two implementations.
;   4. A few LispWorks-only details (MOP symbols, IGNORE called as a function,
;      a name clash with SB-EXT, two helpers with only #+lispworks / #+ccl branches).
;
; Tested with SBCL 2.2.9. Not tested on LispWorks.
; ---------------------------------------------------------------------------------

(setf sb-impl::*default-external-format* :latin-1)

(defparameter *mr-root*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

; LispWorks exposes the MOP functions in CL-USER; in SBCL they live in SB-MOP
(import '(sb-mop:class-slots sb-mop:class-direct-slots sb-mop:slot-definition-name))

; Utilities.lisp calls IGNORE as a function (LispWorks tolerates it)
(sb-ext:unlock-package :common-lisp)
(setf (fdefinition 'ignore) (lambda (&rest args) (declare (cl:ignore args)) nil))
(sb-ext:lock-package :common-lisp)

; CL-USER in SBCL inherits symbols from SB-EXT whose names mReasoner redefines
(shadow '("PROCESS-INPUT"))

; jsown is loaded first, with absolute paths
(dolist (f '("packages" "accessors" "reader" "writer"))
  (load (merge-pathnames (format nil "jsown-master/~A.lisp" f) *mr-root*)))
(pushnew :jsown *features*)

; Minimal split-sequence: the delimiter can be a character or a string of
; separator characters (LispWorks semantics)
(defun split-sequence (delimiter seq &key (remove-empty-subseqs nil) &allow-other-keys)
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

; FILE-PATH uses the working directory of the process: move to the repository
(require :sb-posix)
(sb-posix:chdir *mr-root*)
(let ((*default-pathname-defaults* *mr-root*))
  (load (merge-pathnames "+mReasoner.lisp" *mr-root*)))

; PARSE of strings: split on spaces in SBCL too
(defmethod parse ((words string))
  (parse (mapcar #'read-from-string
                 (split-sequence #\Space words :remove-empty-subseqs t))))

;; Spatial helpers that only have #+lispworks / #+ccl branches in API.lisp
(defun get-thing-from-enumerated-duplicate (enumerated-duplicate-thing)
  (read-from-string
   (first (split-sequence #\# (format nil "~A" enumerated-duplicate-thing)))))

(defun get-enumeration-from-enumerated-duplicate (enumerated-duplicate-thing)
  (let ((parts (split-sequence #\# (format nil "~A" enumerated-duplicate-thing))))
    (when (> (length parts) 1)
      (read-from-string (second parts)))))

(format t "~%mReasoner ready in SBCL. Example: (print-model (first (interpret '(\"All A are B\" \"Some B are C\"))))~%")
