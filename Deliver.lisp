; ---------------------------------------------------------------------------------
; Part X: Delivery functions
; ---------------------------------------------------------------------------------

; Section 1: Deliver Mac app with GUI (LispWorks)
; Section 2: Deliver standalone executable with REPL (LispWorks)
; Section 3: Deliver standalone executable for Syllogism Challenge (CCL)
; Section 4: Deliver standalone executable for CMRAS system (CCL)

#+lispworks (in-package "CL-USER")
#+lispworks (load-all-patches)
#+lispworks (load (current-pathname "+mReasoner.lisp" nil))
#+ccl (load "~/Repository/Models/mReasoner/Code/+mReasoner.lisp")

; ---------------------------------------------------------------------------------
; Section 1: Deliver Mac app with GUI (LispWorks)
; ---------------------------------------------------------------------------------

#+lispworks (deliver 'start-ui #+:cocoa (create-macos-application-bundle "/Users/skhemlani/Desktop/mReasoner.app")
                     #-:cocoa "mReasoner"
                     0 :interface :capi)
#+lispworks (quit)

; ---------------------------------------------------------------------------------
; Section 2: Deliver standalone executable with REPL (LispWorks)
; ---------------------------------------------------------------------------------

#|
(defun main ()
	(if (> (length (get-command-line-arguments)) 0)
		(let ((input (find-argument "-i"))
		      (output (find-argument "-o")))
                  (write-to-json (read-events-and-execute-query input) output))
          (progn
            (print-header)
            (mreasoner-repl))))

(deliver 'main "mReasoner" 0)
|#

; ---------------------------------------------------------------------------------
; Section 3: Deliver standalone executable for Syllogism Challenge (CCL)
; ---------------------------------------------------------------------------------

; #+ccl (save-application "~/mReasoner-bin" :toplevel-function #'main :prepend-kernel t)

; ---------------------------------------------------------------------------------
; Section 4: Deliver standalone executable for CMRAS system (CCL)
; ---------------------------------------------------------------------------------

#+ccl (save-application "~/Desktop/mReasoner-spatial" :toplevel-function #'process-input :prepend-kernel t)
