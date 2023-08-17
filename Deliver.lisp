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

; #+lispworks (deliver 'start-ui #+:cocoa (create-macos-application-bundle "/Users/skhemlani/Desktop/mReasoner.app")
;                      #-:cocoa "mReasoner"
;                      0 :interface :capi)
; #+lispworks (quit)

; ---------------------------------------------------------------------------------
; Section 2: Deliver standalone executable with REPL (LispWorks)
; ---------------------------------------------------------------------------------

; (defun main ()
;	(if (> (length (get-command-line-arguments)) 0)
;		(let ((input (find-argument "-i"))
;		      (output (find-argument "-o")))
;                  (write-to-json (read-events-and-execute-query input) output))
;          (progn
;            (print-header)
;            (mreasoner-repl))))
;
;(deliver 'main "mReasoner" 0)

; ---------------------------------------------------------------------------------
; Section 3: Deliver standalone executable for Syllogism Challenge (CCL)
; ---------------------------------------------------------------------------------

; #+ccl (save-application "~/mReasoner-bin" :toplevel-function #'main :prepend-kernel t)

; ---------------------------------------------------------------------------------
; Section 4: Deliver standalone executable for CMRAS system (CCL)
; ---------------------------------------------------------------------------------

#+ccl (save-application "~/Desktop/mReasoner-spatial" :toplevel-function #'process-input :prepend-kernel t)

; ---------------------------------------------------------------------------------
; Section 3: Deliver standalone executable for Syllogism Challenge (CCL)
; ---------------------------------------------------------------------------------

(deliver 'process-input "mReasoner-spatial" 0)

; #+ccl (save-application "~/mReasoner-bin" :toplevel-function #'main :prepend-kernel t)

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