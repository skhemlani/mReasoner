; ---------------------------------------------------------------------------------
; Part 12: User interface
; ---------------------------------------------------------------------------------

; Section 12.1:  Interface definition
; Section 12.2:  Intension library
; Section 12.3:  Premise setup
; Section 12.4:  Problem setup
; Section 12.5:  Drag and drop
; Section 12.6:  Select task and clear premises
; Section 12.7:  Model display
; Section 12.8:  Console interaction
; Section 12.9:  Draw conclusion and setup experiment
; Section 12.10: Load experiment
; Section 12.11: Run experiment
; Section 12.12: Tracer
; Section 12.13: Launch interface

; ---------------------------------------------------------------------------------
; Section 12.1: Interface definition
; ---------------------------------------------------------------------------------

(capi:define-interface mR ()
  ()
 (:panes

  ; Intension library tab ------------------------------------------------------------

  (filter-intension-library
   capi:filtering-layout
   :change-callback 'filter-library
   :reader filter-intension-library)

  (intension-library-panel
   capi:multi-column-list-panel
   :items *intension-library*
   :interaction :extended-selection
   :columns '((:title "Intension" :adjust :left :visible-min-width (character 10))
              (:title "Label" :adjust :left :visible-min-width (character 2)))
   :column-function #'(lambda (x)  (list (second x) (first x)))
   :drag-callback 'drag-from-intension-library
   :action-callback 'add-selected-to-premises
   :pane-menu (make-instance 'capi:menu
                             :items `("Add selected to premises"
                                      "Set selected to conclusion"
                                      "View intensional semantics")
                             :callback 'intension-library-popup-menu)
   :visible-min-height 400
   :visible-min-width 200
   :visible-max-width nil
   :horizontal-scroll t
   :accessor intension-library-panel-accessor)

  ; Model tab ------------------------------------------------------------------------

  (clear-button capi:push-button
                :text "Clear Model"
                :callback-type :none
                :callback #'(lambda () (clear-pinboard-objects model-pinboard))) 

  ; Intension tab --------------------------------------------------------------------
  (intension-string-pane capi:display-pane
                         :enabled nil
                         :text "Aab"
                         :background :gray87
                         :accessor intension-string-pane-accessor)
  (intension-panel capi:multi-column-list-panel
                           :items (clos:class-slots (class-of Aab))
                           :columns '((:title "Parameter" :adjust :left :visible-min-width (character 2))
                                      (:title "Value" :adjust :left :visible-min-width (character 5)))
                           :column-function 'get-intension-parameter-value
                           :accessor intension-panel-accessor)


  ; Problem tab -------------------------------------------------------------------
  (premise-selection-panel
    capi:list-panel
    :items nil
    :print-function (lambda (x) (format nil "~A" (second x)))
    :drag-callback 'drag-from-premise-selection
    :drop-callback 'drop-to-premise-selection
    :visible-min-height '(character 8)
    :visible-max-width nil
    :vertical-scroll nil
    :accessor premise-selection-panel-accessor)
  (inference-option-pane
   capi:option-pane
   :separator-item :separator
   :visible-min-width 200
   :accessor inference-option-pane-accessor
   :selection-callback 'select-inference-type
   :callback-type :data
   :items '("What follows?"
            "Is it possible that..."
            "Is it necessary that..."
            "Can all of those statements be true at the same time?"))
  (inference-evaluation-pane capi:list-panel
                             :title "?"
                             :title-position :right
                             :print-function (lambda (x) (format nil "~A" (second x))) 
                             :visible-max-height '(:character 1)
                             :visible-max-width nil
                             :vertical-scroll nil
                             :horizontal-scroll nil
;                            :drop-callback 'drop-to-inference-evaluation-selection
                             :accessor inference-evaluation-pane-accessor)
  (draw-conclusion-button capi:push-button
                          :text "Draw Conclusion"
                          :callback-type :none
                          :accessor draw-conclusion-button-accessor
                          :callback 'draw-conclusion)
  (add-to-experiment-button capi:push-button
                            :text "Add to Experiment"
                            :callback-type :none
                            :accessor add-to-experiment-button-accessor
                            :callback 'add-to-experiment)

  ; Experiment tab ----------------------------------------------------------------

  (experiment-selection-pane capi:option-pane
                             :separator-item :separator
                             :visible-min-width 200
                             :callback-type :data
                             :test-function #'string-equal
                             :enabled-positions '(0 6 7 8 9 10 11 12 13 16 17)
                             :items '("User defined"
                                      "Save..."
                                      "Load saved..."
                                      :separator
                                      "Quantificational Reasoning"
                                      "Consistency (Experiment 3; Ragni, Khemlani, & Johnson-Laird, 2014)"
                                      "Immediate inferences (Experiment 1; Khemlani, Lotstein, Trafton, & Johnson-Laird, 2015)"
                                      "Immediate inferences (Experiment 2; Khemlani, Lotstein, Trafton, & Johnson-Laird, 2015)"
                                      "Immediate inferences (Experiment 3; Khemlani, Lotstein, Trafton, & Johnson-Laird, 2015)"
                                      "Immediate inferences (Experiment 1; Newstead & Griggs, 1983)"
                                      "Immediate inferences (Experiment 2; Newstead & Griggs, 1983)"
                                      "Set membership inferences (Khemlani & Johnson-Laird, 2014)"
                                      "Set membership inferences (Mascarenhas & Koralus, 2016)"
                                      "Syllogisms meta-analysis (Khemlani & Johnson-Laird, 2012)"
                                      :separator
                                      "Temporal reasoning"
                                      "Temporal inferences (Experiments 1 and 2; Khemlani, Harrison, & Trafton, in preparation)"
                                      "Durational inferences (Experiment 1; Kelly, Khemlani, & Johnson-Laird, 2020"
                                      "Temporal inferences (Experiment 1; Schaeken, Johnson-Laird, & d'Ydewalle, 1996)"
                                      "Temporal inferences (Experiment 2; Schaeken, Johnson-Laird, & d'Ydewalle, 1996)")
                             :selection-callback 'load-experiment
                             :accessor experiment-selection-pane-accessor)
  (problem-list-panel capi:multi-column-list-panel
                      :title-position :frame
                      :column-function 'format-experiment-problem-list
                      :columns `((:title "Premises (initial)" :adjust :left :width 60)
                                 (:title "Task" :adjust :left :width 120)
                                 (:title "Premises (additional)" :adjust :left :width 60)
                                 (:title "Stochastic?" :adjust :left :width 100)
                                 (:title ,(format nil "~A" #\u+03bb) :adjust :left :width 50)
                                 (:title ,(format nil "~A" #\u+025b) :adjust :left :width 50)
                                 (:title ,(format nil "~A" #\u+03c3) :adjust :left :width 50)
                                 (:title ,(format nil "~A" #\u+03c9) :adjust :left :width 50)
                                 (:title "Category" :adjust :left :width 100)
                                 (:title "Accuracy" :adjust :left :width 100)
                                 (:title "Latency" :adjust :left :width 100))
                      :pane-menu (make-instance 'capi:menu
                                                :items `("Clear"
                                                         "Enable/disable stochastic system"
                                                         ,(format nil "Change ~A parameter..." #\u+03bb)
                                                         ,(format nil "Change ~A parameter..." #\u+025b)
                                                         ,(format nil "Change ~A parameter..." #\u+03c3)
                                                         ,(format nil "Change ~A parameter..." #\u+03c9)
                                                         "Assign category label..."
                                                         "Delete")
                                                :callback 'problem-list-popup-menu)
                      :interaction :extended-selection
                      :auto-reset-column-widths nil
                      :accessor problem-list-panel-accessor)
  (synthetic-data-participants-pane capi:option-pane
                                    :title "Run experiment for n participants:"
                                    :title-position :left 
                                    :visible-max-width 80
                                    :callback-type :data
                                    :accessor synthetic-data-participants-pane-accessor
                                    :items '(1 5 10 50 100 500 1000))
  (run-experiment-button capi:push-button
                         :text "Run"
                         :callback-type :none
                         :accessor run-experiment-button-accessor
                         :callback 'prepare-interface-and-run-experiment)
  (stop-experiment-button capi:push-button
                         :text "Stop"
                         :callback-type :none
                         :accessor stop-experiment-button-accessor
                         :callback 'stop-experiment)
  (experiment-progress capi:progress-bar
                       :start 0
                       :end 100
                       :slug-start 0
                       :enabled nil
                       :accessor experiment-progress-accessor)

  ; Premise tab -------------------------------------------------------------------
#|  (premise-input-pane capi:text-input-pane
                      :title "Premise:"
                      :title-position :left
                      :accessor premise-input-pane-accessor)
  (premise-label-input-pane capi:text-input-pane
                            :title "Label:"
                            :title-position :left
                            :accessor premise-label-input-pane-accessor)
  (add-to-problem-button capi:push-button
                      :text "Add to problem"
                      :callback-type :none
                      :callback 'add-to-problem-button-callback) |#
  (clear-premises-button capi:push-button
                         :text "Clear Premises"
                         :callback-type :none
                         :callback 'clear-premises)

  ; Parameter settings tab ------------------------------------------------------------
  (sigma-parameter-slider capi:slider :start 0 :end 100
                          :callback 'sigma-parameter-slider-callback
                          :tick-frequency 10
                          :title (format nil "System 2 (~A) = 0.00" #\u+03c3)
                          :title-position :top
                          :accessor sigma-parameter-slider-accessor)
  (omega-parameter-slider capi:slider :start 0 :end 100
                            :callback 'omega-parameter-slider-callback
                            :slug-start 100
                            :tick-frequency 10
                            :title (format nil "Weaken conclusions (~A)  = 1.00" #\u+03c9)
                            :title-position :top
                            :accessor omega-parameter-slider-accessor)
  (stochastic-button capi:check-button
                     :selection-callback 'stochastic-button-callback
                     :retract-callback 'stochastic-button-callback
                     :selected nil
                     :text "Stochastic model building"
                     :accessor stochastic-button-accessor)
  (lambda-parameter-slider capi:slider :start 1 :end 80
                           :enabled nil
                           :callback 'lambda-parameter-slider-callback
                           :slug-start 40
                           :tick-frequency 10
                           :title (format nil "Size (~A) = 4.00" #\u+03bb)
                           :title-position :top
                           :accessor lambda-parameter-slider-accessor)
  (epsilon-parameter-slider capi:slider :start 0 :end 100
                            :enabled nil
                            :callback 'epsilon-parameter-slider-callback
                            :tick-frequency 10
                            :title (format nil "Deviation from canonicality (~A) = 0.00" #\u+025b)
                            :title-position :top
                            :accessor epsilon-parameter-slider-accessor)
  (defaults-button capi:push-button
                   :text "Restore defaults"
                   :callback-type :none
                   :accessor defaults-button-accessor
                   :callback 'set-defaults)

  ; Trace tab ------------------------------------------------------------------------
  (trace-panel capi:multi-column-list-panel
               :columns '((:title "Step" :adjust :left :width 50)
                          (:title "System" :adjust :left :width 100)
                          (:title "Description" :adjust :left :width 400)
                          (:title "Runtime" :adjust :left :width 50))
               :column-function #'(lambda (x) (reverse (rest (reverse x))))
               :font (gp:make-font-description
                       :family "Monaco" 
                       :size 11
                       :weight :medium)
               :color-function 'highlight-trace-model
               :auto-reset-column-widths nil
               :action-callback 'add-pinboard-object
               :accessor trace-panel-accessor)

  ; Synthetic data tab ---------------------------------------------------------------
  (synthetic-data-panel capi:multi-column-list-panel
                        :columns `((:title "Subject" :adjust :left :width 30)
                                   (:title "Problem #" :adjust :left :width 50)
                                   (:title "Response" :adjust :left :width 120)
                                   (:title "Initial Model" :adjust :left :width 120)
                                   (:title "Final Model" :adjust :left :width 120)
                                   (:title "Distance" :adjust :left :width 70)
                                   (:title "Premises (initial)" :adjust :left :width 100)
                                   (:title "Task" :adjust :left :width 120)
                                   (:title "Premises (additional)" :adjust :left :width 100)
                                   (:title "Stochastic?" :adjust :left :width 100)
                                   (:title ,(format nil "~A" #\u+03bb) :adjust :left :width 40)
                                   (:title ,(format nil "~A" #\u+025b) :adjust :left :width 40)
                                   (:title ,(format nil "~A" #\u+03c3) :adjust :left :width 40)
                                   (:title ,(format nil "~A" #\u+03c9) :adjust :left :width 40)
                                   (:title "Category" :adjust :left :width 100)
                                   (:title "Accuracy" :adjust :left :width 100)
                                   (:title "Latency" :adjust :left :width 100))
                        :pane-menu (make-instance 'capi:menu :items '("Export as CSV file...")
                                                  :callback #'(lambda (x y) (export-synthetic-data *synthetic-data*)))
                        :sort-descriptions *mR-multi-column-test-sorting-types*
                        :header-args (list 
                                      :print-function 'string-capitalize
                                      :selection-callback :sort ;;; "magic" callback tells it to use the sort descriptions
                                      )
                        :font (gp:make-font-description
                               :family "Monaco" 
                               :size 10)
                        :auto-reset-column-widths nil
                        :accessor synthetic-data-panel-accessor)
 #| (experiment-time-string-pane capi:rich-text-pane
                               :visible-max-height '(:character 1)
                               :enabled nil
                               :visible-border nil
                               :text ""
                               :background :gray87
                               :paragraph-format '(:alignment :right)
                               :accessor experiment-time-string-pane-accessor)|#

  ; Console tab ----------------------------------------------------------------------
  (command-line-pane capi:listener-pane
                     :background :gray90
                     :accessor command-line-pane)

  ; Message pane ---------------------------------------------------------------------
   (message-pane
    capi:title-pane
    :title-position :right
    :title-adjust :right
    :visible-max-width nil))
 (:layouts
   (main-layout capi:column-layout '(left-and-right-layout message-pane) :internal-border 10)
   (left-and-right-layout capi:row-layout '(left-ui :divider right-ui)
                :x-ratios '(1 nil 4))

   ; Left side of UI
   (left-ui capi:column-layout
            '(intension-library-layout :divider model-intension-layout)
            :y-ratios '(5 nil 2))
   (intension-library-layout capi:tab-layout ()
                 :items '(("Library" intension-library-column))
                 :print-function 'car
                 :visible-child-function 'second)
   (intension-library-column capi:column-layout '(intension-library-panel 
                                                  filter-intension-library)
                             :internal-border 10
                             :x-adjust :centre)
   (model-intension-layout capi:tab-layout ()
                 :items '(("Model" model-column) ("Intension" intension-column))
                 :print-function 'car
                 :visible-child-function 'second
                 :accessor model-intension-layout-accessor)
   (model-column capi:column-layout '(model-pinboard model-buttons)
                 :x-adjust :centre)
   (model-buttons capi:row-layout '(clear-button))
   (model-pinboard capi:pinboard-layout nil
                   :background :gray90
                   :font (gp:make-font-description
                               :family "Monaco" 
                               :size 14)
                   :draw-with-buffer t
                   :input-model '(((:button-1 :press)   start-pinboard-object-move)
                                  ((:button-1 :motion)  drag-pinboard-object)
                                  ((:button-1 :release) finish-pinboard-object-move)
                                  (:post-menu model-popup-menu))
                   :accessor model-pinboard-accessor)
   (intension-column capi:column-layout '(intension-string-pane intension-panel)
                     :internal-border 10
                     :x-adjust :centre
                     :accessor intension-column-accessor)

   ; Right side of UI
   (right-ui capi:column-layout
            '(input-layout :divider output-layout)
            :y-ratios '(1 nil 1))
   (input-layout capi:tab-layout ()
                 :items '(("Problem" problem-layout) ("Experiment" experiment-layout))
                 :print-function 'car
                 :visible-child-function 'second
                 :accessor input-layout-accessor)
   (problem-layout capi:row-layout '(problem-inference-layout problem-parameters-layout)
                 :internal-border 5
                 :x-ratios '(1 1))
   (problem-parameters-layout capi:column-layout '(stochastic-button 
                                                   lambda-parameter-slider 
                                                   epsilon-parameter-slider
                                                   sigma-parameter-slider 
                                                   omega-parameter-slider
                                                   nil
                                                   ; defaults-button SSK !!! 2016-08-03
                                                   )
                 :title "Parameters"
                 :title-position :frame
                 :x-adjust :centre
                 :internal-border 10
                 :x-adjust :left)
   (problem-inference-layout capi:column-layout '(premise-selection-panel 
                                                  clear-premises-button 
                                                  nil 
                                                  inference-option-pane 
                                                  problem-buttons-layout)
                 :title "Premises and task" 
                 :title-position :frame 
                 :internal-border 10 
                 :x-adjust :centre
                 :accessor problem-inference-layout-accessor)
   (problem-buttons-layout capi:row-layout '(draw-conclusion-button 
                                             add-to-experiment-button)
                 :internal-border 10 
                 :x-ratios '(1 1))
   (experiment-layout capi:column-layout '(experiment-selection-pane 
                                           problem-list-panel 
                                           experiment-generate-layout)
                  :internal-border 10 
                  :x-adjust :left)
   (experiment-generate-layout capi:row-layout '(synthetic-data-participants-pane 
                                                 run-experiment-button)
                               :y-adjust :centre
                               :accessor experiment-generate-layout-accessor)
   (synthetic-layout capi:column-layout '(synthetic-data-panel)
                  :internal-border 10 
                  :x-adjust :left)
   (output-layout capi:tab-layout ()
                 :items '(("Trace" trace-panel) 
                          ("Synthetic Data" synthetic-layout)
                          ("Debug" command-line-pane))
                 :print-function 'first
                 :visible-child-function 'second
                 :accessor output-layout-accessor))
   (:menus 
     (file-menu "File"
                (("Export synthetic data..."
                  :callback #'(lambda (x) (export-synthetic-data *synthetic-data*))
                  :callback-type :item)))
     (problem-menu "Problem"
                (("Add selected to premises" 
                  :callback 'add-selected-to-premises)
                 ("Set selected to conclusion"
                  :callback 'set-selected-to-conclusion)
                 ("Clear premises"
                  :callback #'(lambda (x) (clear-premises))
                  :callback-type :item))))
   (:menu-bar file-menu problem-menu)
 (:default-initargs :title (format nil "~A ~A" *system-name* *version*)
  :visible-min-height 400
  :visible-min-width 700
  :best-height 800
  :best-width 1000
  :create-callback #'(lambda (interface)
                       (reset-system-parameters)
                       (reset-tracer)
                       (enable-tracer))
  :destroy-callback #'(lambda (interface)
                        (setf *mR-interface* nil)
                        (reset-tracer))
  :auto-menus t
  :message-area t
  :window-styles (list :internal-borderless :movable-by-window-background)
  :display-state :normal))

; ---------------------------------------------------------------------------------
; Section 12.2: Intension library
; ---------------------------------------------------------------------------------

(defvar *mR-multi-column-test-sorting-types*
  (list
   (capi:make-sorting-description :type "Subject"
                                  :key 'first
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type "Problem #"
                                  :key 'second
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type "Response"
                                  :key 'third
                                  :sort 'string-lessp
                                  :reverse-sort 'string-greaterp)
   (capi:make-sorting-description :type "Distance"
                                  :key 'sixth
                                  :sort '>
                                  :reverse-sort '<)
   ))

(defparameter *intension-library*
  (list
   ; Set-membership intensions
   ; -------------------------------------------------------------
   (list "X-is-A" "X is an A" x-is-A)
   (list "X-isnot-A" "X is not an A" x-isnot-A)
   (list "X-is-B" "X is a B" x-is-B)
   (list "X-isnot-B" "X is not a B" x-isnot-B)
   ; Quantificational intensions (including "most" and "most_not")
   ; -------------------------------------------------------------
   (list "Aab" "All A are B" Aab)
   (list "Aba" "All B are A" Aba)
   (list "Abc" "All B are C" Abc)
   (list "Acb" "All C are B" Acb)
   (list "Iab" "Some A are B" Iab)
   (list "Iba" "Some B are A" Iba)
   (list "Ibc" "Some B are C" Ibc)
   (list "Icb" "Some C are B" Icb)
   (list "Eab" "No A are B" Eab)
   (list "Eba" "No B are A" Eba)
   (list "Ebc" "No B are C" Ebc)
   (list "Ecb" "No C are B" Ecb)
   (list "Oab" "Some A are not B" Oab)
   (list "Oba" "Some B are not A" Oba)
   (list "Obc" "Some B are not C" Obc)
   (list "Ocb" "Some C are not B" Ocb)
   (list "Mab" "Most A are B" Mab)
   (list "Mba" "Most B are A" Mba)
   (list "Mbc" "Most B are C" Mbc)
   (list "Mcb" "Most C are B" Mcb)
   (list "Ma-b" "Most A are not B" Ma-b)
   (list "Mb-a" "Most B are not A" Mb-a)
   (list "Mb-c" "Most B are not C" Mb-c)
   (list "Mc-b" "Most C are not B" Mc-b)
   ; Temporal intensions
   ; -------------------------------------------------------------
#|   (list "aBb" "A happened before B" aBb)
   (list "aBc" "A happened before C" aBc2)
   (list "aBd" "A happened before D" aBd)
   (list "aBe" "A happened before E" aBe)
   (list "bBa" "B happened before A" bBa)
   (list "bBc" "B happened before C" bBc)
   (list "bBd" "B happened before D" bBd)
   (list "bBe" "B happened before E" bBe)
   (list "cBa" "C happened before A" cBa)
   (list "cBb" "C happened before B" cBb)
   (list "cBd" "C happened before D" cBd)
   (list "cBe" "C happened before E" cBe)
   (list "dBa" "D happened before A" dBa)
   (list "dBb" "D happened before B" dBb)
   (list "dBc" "D happened before C" dBc)
   (list "dBe" "D happened before E" dBe)
   (list "eBa" "E happened before A" eBa2)
   (list "eBb" "E happened before B" eBb)
   (list "eBc" "E happened before C" eBc2)
   (list "eBd" "E happened before D" eBd)
   (list "aAb" "A happened after B" aAb2)
   (list "aAc" "A happened after C" aAc2)
   (list "aAd" "A happened after D" aAd)
   (list "aAe" "A happened after E" aAe)
   (list "bAa" "B happened after A" bAa)
   (list "bAc" "B happened after C" bAc)
   (list "bAd" "B happened after D" bAd)
   (list "bAe" "B happened after E" bAe)
   (list "cAa" "C happened after A" cAa)
   (list "cAb" "C happened after B" cAb)
   (list "cAd" "C happened after D" cAd)
   (list "cAe" "C happened after E" cAe)
   (list "dAa" "D happened after A" dAa)
   (list "dAb" "D happened after B" dAb)
   (list "dAc" "D happened after C" dAc)
   (list "dAe" "D happened after E" dAe)
   (list "eAa" "E happened after A" eAa)
   (list "eAb" "E happened after B" eAb2)
   (list "eAc" "E happened after C" eAc2)
   (list "eAd" "E happened after D" eAd)
   (list "aDb" "A happened during B" aDb)
   (list "aDc" "A happened during C" aDc)
   (list "aDd" "A happened during D" aDd)
   (list "aDe" "A happened during E" aDe)
   (list "bDa" "B happened during A" bDa)
   (list "bDc" "B happened during C" bDc)
   (list "bDd" "B happened during D" bDd)
   (list "bDe" "B happened during E" bDe)
   (list "cDa" "C happened during A" cDa)
   (list "cDb" "C happened during B" cDb)
   (list "cDd" "C happened during D" cDd)
   (list "cDe" "C happened during E" cDe)
   (list "dDa" "D happened during A" dDa)
   (list "dDb" "D happened during B" dDb)
   (list "dDc" "D happened during C" dDc)
   (list "dDe" "D happened during E" dDe)
   (list "eDa" "E happened during A" eDa)
   (list "eDb" "E happened during B" eDb)
   (list "eDc" "E happened during C" eDc)
   (list "eDd" "E happened during D" eDd)
   (list "aWb" "A happened while B" aWb)
   (list "aWc" "A happened while C" aWc)
   (list "aWd" "A happened while D" aWd)
   (list "aWe" "A happened while E" aWe)
   (list "bWa" "B happened while A" bWa)
   (list "bWc" "B happened while C" bWc)
   (list "bWd" "B happened while D" bWd)
   (list "bWe" "B happened while E" bWe)
   (list "cWa" "C happened while A" cWa)
   (list "cWb" "C happened while B" cWb)
   (list "cWd" "C happened while D" cWd)
   (list "cWe" "C happened while E" cWe)
   (list "dWa" "D happened while A" dWa)
   (list "dWb" "D happened while B" dWb)
   (list "dWc" "D happened while C" dWc)
   (list "dWe" "D happened while E" dWe)
   (list "eWa" "E happened while A" eWa)
   (list "eWb" "E happened while B" eWb)
   (list "eWc" "E happened while C" eWc)
   (list "eWd" "E happened while D" eWd)|#
))

(defun get-intension-parameter-value (slot)
  (let ((intension (intern (format nil "~:@(~a~)" (capi:display-pane-text (intension-string-pane-accessor *mR-interface*))))))
    (list (clos:SLOT-DEFINITION-NAME slot)
          (eval `(,(clos:SLOT-DEFINITION-NAME slot) ,intension)))))

(defun refresh-intension-library ()
  (setf (capi:collection-items (intension-library-panel-accessor *mR-interface*)) *intension-library*))

(defun add-to-intension-library (label string intension)
  (setf *intension-library*
        (append *intension-library* (list (list label string intension))))
  (refresh-intension-library))

(defun filter-library (interface)
  (let* ((intensions *intension-library*)
         (filtered-intensions
          (multiple-value-bind (regexp excludep)
              (capi:filtering-layout-match-object-and-exclude-p
               (filter-intension-library interface)
               nil)
            (if regexp
                (loop for i in intensions
                      when (if (find-regexp-in-string
                                regexp
                                (format nil "~{~a~^, ~}" i))
                               (not excludep)
                             excludep)
                      collect i)
              intensions))))
    (setf (capi:collection-items
           (intension-library-panel-accessor interface))
          filtered-intensions))
  )

; ---------------------------------------------------------------------------------
; Section 12.3: Premise setup
; ---------------------------------------------------------------------------------

#|

(defun add-premise ()
  (let* ((premise-pane (premise-input-pane-accessor *mR-interface*))
        (label-pane (premise-label-input-pane-accessor *mR-interface*))
        (label-string (format nil "~:@(~a~)" (capi:text-input-pane-text label-pane)))
        (premise-tokens (mapcar #'read-from-string (split (capi:text-input-pane-text premise-pane))))
        intension-label intension)
    (if (find-symbol label-string)
        (capi:display-message "Label already exists, please choose an alternative.")
      (handler-case
          (progn
            (setf intension (parse premise-tokens))
            (setf intension-label (intern label-string))
            (set intension-label intension)
            (add-to-intension-library (capi:text-input-pane-text label-pane) (capi:text-input-pane-text premise-pane) intension)
            (update-intension-panel (list (capi:text-input-pane-text label-pane)))
            (setf (capi:text-input-pane-text label-pane) "")
            (setf (capi:text-input-pane-text premise-pane) ""))
        (error ()
          (capi:display-message "Syntax error, please check syntax and try again."))))))

|#

(defun switch-model-intension-tab (column)
  (capi:apply-in-pane-process 
   (model-intension-layout-accessor *mR-interface*) #'(setf capi:tab-layout-visible-child)
   column (model-intension-layout-accessor *mR-interface*)))

(defun view-intensional-semantics (&rest args)
  (let ((intension-label-string (first (first (coerce (capi:choice-selected-items (intension-library-panel-accessor *mR-interface*)) 'list)))))
    (setf (capi:display-pane-text (intension-string-pane-accessor *mR-interface*)) intension-label-string)
    (setf (capi:collection-items (intension-panel-accessor *mR-interface*))
          (clos:class-slots (class-of (eval (intern (format nil "~:@(~a~)" intension-label-string))))))
    (setf (capi:choice-selection (model-intension-layout-accessor *mR-interface*)) 1)))

; ---------------------------------------------------------------------------------
; Section 12.4: Problem setup
; ---------------------------------------------------------------------------------

(defun sigma-parameter-slider-callback (pane position movement)
  (setf +sigma+ (float (/ position 100)))
  (setf (capi:titled-object-title pane) (format nil "System 2 (~A) = ~,2F" #\u+03c3 (/ position 100))))

(defun stochastic-button-callback (value interface)
  (setf *stochastic* (capi:button-selected (stochastic-button-accessor interface)))
  (setf (capi:simple-pane-enabled (lambda-parameter-slider-accessor interface)) *stochastic*)
  (setf (capi:simple-pane-enabled (epsilon-parameter-slider-accessor interface)) *stochastic*))

(defun lambda-parameter-slider-callback (pane position movement)
  (setf +lambda+ (float (/ position 10)))
  (setf (capi:titled-object-title pane) (format nil "Size (~A) = ~,2F" #\u+03bb (/ position 10))))

(defun epsilon-parameter-slider-callback (pane position movement)
  (setf +epsilon+ (float (/ position 100)))
  (setf (capi:titled-object-title pane) (format nil "Deviation from canonicality (~A) = ~,2F" #\u+025b (/ position 100))))

(defun omega-parameter-slider-callback (pane position movement)
  (setf +omega+ (float (/ position 100)))
  (setf (capi:titled-object-title pane) (format nil "Weaken conclusions (~A) = ~,2F" #\u+03c9 (/ position 100))))

(defun add-selected-to-premises (pane interface)
  (declare (ignore pane))
  (setf (capi:collection-items (premise-selection-panel-accessor interface))
        (coerce (append (coerce (capi:collection-items (premise-selection-panel-accessor interface)) 'list)
                        (coerce (capi:choice-selected-items (intension-library-panel-accessor interface)) 'list)) 'vector)))

(defun set-selected-to-conclusion (pane interface)
  (declare (ignore pane))
  (let ((selected (capi:choice-selected-items (intension-library-panel-accessor interface))))
    (if (> (length selected) 1)
        (capi:display-message "Error: The conclusion can only be set to one intension.")
      (setf (capi:collection-items (inference-evaluation-pane-accessor interface))
            selected))))

; ---------------------------------------------------------------------------------
; Section 12.5: Drag and drop
; ---------------------------------------------------------------------------------

(defun drag-from-intension-library (pane indices)
  (show-message
   pane
   (format nil "Dragging intension panel items ~S." indices))
   (and indices
       (list :string (format nil "~{~A~^~%~}"
                             (loop for index in indices
                                   collect
                                   (capi:get-collection-item pane index)))
             :intension-library indices)))

#|(defun drag-from-intension-library (pane indices)
  (drag-and-drop-choices-show-message
   pane
   (format nil "Dragging exchangeable items ~S." indices))
  (when indices
    (list :intension-library indices)))
|#

(defun drag-from-premise-selection (pane indices)
  (show-message
   pane
   (format nil "Dragging premise selection items ~S." indices))
  (and indices
       (list :intension-library indices)))

(defun drop-to-premise-selection (pane drop-object stage)
 (case stage
    (:formats
     (setf *temp* drop-object)
     (setf (capi:drop-object-drop-effect drop-object) :copy)
     (capi:set-drop-object-supported-formats drop-object '(:intension-library)))
    (:drag
     (when (and (capi:drop-object-provides-format drop-object :intension-library)
                (capi:drop-object-allows-drop-effect-p drop-object :copy))
       (multiple-value-bind (index placement)
           (capi:drop-object-collection-index drop-object)
           (let ((new-placement (if (eq placement :item) :above placement)))
             (setf (capi:drop-object-collection-index drop-object)
                   (values index new-placement)))
           (setf (capi:drop-object-drop-effect drop-object) :copy))))
    (:drop
     (let ((wanted-format :intension-library))
       (when (and (capi:drop-object-provides-format drop-object wanted-format)
                  (capi:drop-object-allows-drop-effect-p drop-object :copy))
         (multiple-value-bind (dropped-index placement)
             (capi:drop-object-collection-index drop-object)
           (show-message
            pane
            (format nil "Dropped ~A rearrangeable item ~D: ~a"
                    (string-downcase placement) dropped-index 
                    (capi:get-collection-item pane dropped-index)))
           (let* ((dragged-indices (capi:drop-object-get-object
                                    drop-object pane wanted-format))
                  (source-pane (slot-value (capi:top-level-interface pane) 'drag-list))       ;;; MODIFY THIS LINE
                  (dragged-items (loop for index in dragged-indices
                                       collect (capi:get-collection-item source-pane index)))
                  (new-items (list-panel-modified-items
                              pane
                              :add-items dragged-items
                              :add-index dropped-index
                              :add-placement placement)))
             (setf (capi:collection-items pane) new-items)
             (setf (capi:drop-object-drop-effect drop-object) :move))))))))

(defun list-panel-modified-items (pane &key remove-indices
                                       add-items add-index (add-placement :above))
  (let ((item-count (capi:count-collection-items pane)))
    (if (zerop item-count)
        add-items
      (if add-index
          (let ((adjusted-add-index (if (eq add-placement :above)
                                        add-index
                                      (1+ add-index))))
            (loop for index from 0 to item-count
                  if (= index adjusted-add-index)
                  append add-items
                  unless (or (member index remove-indices)
                             (= index item-count))
                  collect (capi:get-collection-item pane index)))
        (loop for index from 0 below item-count
              unless (member index remove-indices)
              collect (capi:get-collection-item pane index))))))

(defun show-message (pane title)
  (let ((message-pane (slot-value (capi:element-interface pane) 'message-pane)))
    (setf (capi:title-pane-text message-pane) title)))

; ---------------------------------------------------------------------------------
; Section 12.6: Select task and clear premises
; ---------------------------------------------------------------------------------

(defun select-inference-type (data)
  (setf (capi:layout-description (problem-inference-layout-accessor *mR-interface*))
        (if (or (string-equal "Is it possible that..." data)
                (string-equal "Is it necessary that..." data))
            '(premise-selection-panel clear-premises-button nil inference-option-pane inference-evaluation-pane problem-buttons-layout)
          (progn
            (setf (capi:collection-items (inference-evaluation-pane-accessor *mR-interface*)) nil)
            '(premise-selection-panel clear-premises-button nil inference-option-pane problem-buttons-layout)))))

(defun clear-premises ()
  (setf (capi:collection-items (premise-selection-panel-accessor *mR-interface*)) nil)
  (setf (capi:collection-items (inference-evaluation-pane-accessor *mR-interface*)) nil))

; ---------------------------------------------------------------------------------
; Section 12.7: Model display
; ---------------------------------------------------------------------------------

(defun highlight-trace-model (lp item state)
  (declare (ignore lp))
  (when (first (last item)) :blue))

(defclass text-object (capi:pinboard-object)
  ((entity :accessor entity :initform nil :initarg :entity)
   (foreground :accessor foreground :initform nil :initarg :foreground)
   (background :accessor background :initform nil :initarg :background)
   (filled :accessor filled :initform nil :initarg :filled))
  (:default-initargs
   :visible-min-width 30
   :visible-min-height 30))

(defmethod capi:draw-pinboard-object (pinboard (text text-object) &key)
  (capi:with-geometry text
    (let ((entities (split-sequence (format nil "~%") (entity text)))
          (foreground (or (foreground text) (capi:simple-pane-foreground pinboard)))
          (background (or (background text) (capi:simple-pane-background pinboard)))
          (filled (filled text))
          (y 0))
      (dolist (e entities)
        (gp:draw-x-y-adjusted-string pinboard
                                     e
                                     capi:%x%
                                     (+ y capi:%y%)
                                     :y-adjust :top
                                     :foreground (if filled background foreground)
                                     :background (if filled foreground background)
                                     :block (filled text))
        (incf y 20)))))

(defmethod draw-object-outline (pinboard (text text-object) x1 y1 x2 y2)
  (multiple-value-bind
      (x y width height)
      (x-y-width-and-height x1 y1 x2 y2)
    (with-xor (pinboard)
      (gp:draw-rectangle pinboard x y width height))))

(defun make-new-pinboard-object (string)
  (make-instance
   'text-object
   :entity string
   :foreground :grey90
   :background :black
   :filled t))

(defun add-pinboard-object (item interface)
  (when (first (last item))
    (let* ((model (first (last item)))
           (model (if (listp model) (first model) model))
           (output (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
           (temp   (print-model model :output output))
           (pinboard (model-pinboard-accessor *mR-interface*))
           (new-object (make-new-pinboard-object output)))
      (clear-pinboard-objects pinboard)
      (capi:manipulate-pinboard pinboard new-object :add-top)
      (setf (capi:pinboard-pane-position new-object) 
            (values 30 5))
      (setf (capi:pinboard-pane-size new-object)
            (values 100 100)))
    (setf (capi:choice-selection (model-intension-layout-accessor *mR-interface*)) 0)))

(defstruct drag-status
  object
  x-offset
  y-offset)

(defvar *pinboard-drag-status* nil)

(defun start-pinboard-object-move (layout x y)
  (let ((object (capi:pinboard-object-at-position layout x y)))
    (setq *pinboard-drag-status*
          (when object
            (capi:with-geometry object
              (make-drag-status
               :object object
               :x-offset (- x capi:%x%)
               :y-offset (- y capi:%y%)))))))

(defun drag-pinboard-object (layout x y)
  (declare (ignore layout))
  (when *pinboard-drag-status*
    (let ((object (drag-status-object *pinboard-drag-status*)))
      (let ((new-x (- x (drag-status-x-offset *pinboard-drag-status*)))
            (new-y (- y (drag-status-y-offset *pinboard-drag-status*))))
        (setf (capi:pinboard-pane-position object) (values new-x new-y))))))

(defun finish-pinboard-object-move (layout x y)
  (declare (ignore layout x y))
  (setq *pinboard-drag-status* nil))

(defun clear-pinboard-objects (pinboard)
  (setf (capi:layout-description pinboard) nil))

(defun model-popup-menu (pinboard x y &optional gspec)
  (capi:display-popup-menu (make-instance 'capi:menu :items '("Minimize" "Save" "Update" "View footnotes" ))
                            :owner pinboard :x x :y y))


; ---------------------------------------------------------------------------------
; Section 12.8: Console interaction
; ---------------------------------------------------------------------------------

(defun send-keys-to-pane-aux (pane string newline-p)
  (loop for char across string
        do (capi:call-editor pane char))
  (if newline-p
      (capi:call-editor pane #\Return)))

(defun send-keys-to-pane (pane string newline-p)
  (capi:apply-in-pane-process pane 
                              'send-keys-to-pane-aux
                              pane string newline-p))

; ---------------------------------------------------------------------------------
; Section 12.9: Draw conclusion and setup experiment
; ---------------------------------------------------------------------------------

(defun set-global-parameters-from-interface ()
  (let ((sigma      (capi:range-slug-start (sigma-parameter-slider-accessor *mR-interface*)))
        (stochastic (capi:button-selected (stochastic-button-accessor *mR-interface*)))
        (lambda     (capi:range-slug-start (lambda-parameter-slider-accessor *mR-interface*)))
        (epsilon    (capi:range-slug-start (epsilon-parameter-slider-accessor *mR-interface*)))
        (omega      (capi:range-slug-start (omega-parameter-slider-accessor *mR-interface*))))
    (setf +sigma+ (float (/ sigma 100)))
    (setf *stochastic* stochastic)
    (if stochastic
        (progn
          (setf +lambda+ (float (/ lambda 10)))
          (setf +epsilon+ (float (/ epsilon 100)))
          (setf +omega+ (float (/ omega 100)))))))

(defun draw-conclusion ()
  (reset-tracer)
  (clear-pinboard-objects (model-pinboard-accessor *mR-interface*))
  (let* ((premises (coerce (capi:collection-items (premise-selection-panel-accessor *mR-interface*)) 'list))
         (premise-intensions (mapcar #'third premises))
         (conclusions (coerce (capi:collection-items (inference-evaluation-pane-accessor *mR-interface*)) 'list))
         (conclusions-intensions (mapcar #'third conclusions))
         (task (capi:choice-selected-item (inference-option-pane-accessor *mR-interface*))))
    (set-global-parameters-from-interface)
    (cond
     ((string-equal "What follows?" task) (what-follows? premise-intensions))
     ((string-equal "Is it possible that..." task) (possible? conclusions-intensions :given premise-intensions))
     ((string-equal "Is it necessary that..." task) (necessary? conclusions-intensions :given premise-intensions))
     ((string-equal "Can all of those statements be true at the same time?" task) (consistent? premise-intensions)))
    (setf (capi:collection-items (trace-panel-accessor *mR-interface*)) (reverse (trace-output *tracer*)))
    (setf (capi:choice-selection (output-layout-accessor *mR-interface*)) 0)))

(defparameter *experiment-problems* nil)

(defun format-experiment-problem-list (problem-list)
  "Format multi-column-list-panel values for experiment problems"
  (list (format nil "~{~A~#[~:; ~]~}" (mapcar #'abbreviate (nth 0 problem-list)))
        (nth 1 problem-list)
        (if (nth 2 problem-list) (format nil "~{~A~#[~:;  ~]~}" (mapcar #'abbreviate (nth 2 problem-list))) "--")
        (if (nth 3 problem-list) "Yes" "No")
        (nth 4 problem-list) (nth 5 problem-list) (nth 6 problem-list)
        (if (nth 7 problem-list) (nth 7 problem-list) "--")
       (nth 8 problem-list) (nth 9 problem-list) (nth 10 problem-list)))

(defun add-to-experiment ()
  (let* ((premises (coerce (capi:collection-items (premise-selection-panel-accessor *mR-interface*)) 'list))
         (premise-intensions (mapcar #'third premises))
         (conclusions (coerce (capi:collection-items (inference-evaluation-pane-accessor *mR-interface*)) 'list))
         (conclusions-intensions (mapcar #'third conclusions))
         (task (capi:choice-selected-item (inference-option-pane-accessor *mR-interface*))))
  (setf (capi:choice-selection (input-layout-accessor *mR-interface*)) 1)
  (push (list premise-intensions
              task
              conclusions-intensions
              (stochastic-enabled?)
              +lambda+ +epsilon+ +sigma+ +omega+
              nil nil t) *experiment-problems*)
  (modify-problems)
  (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) (reverse *experiment-problems*))
  (clear-premises)))

(defun clear-problem-list-panel ()
  (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) nil)
  (if (find "User defined" (capi:collection-items (experiment-selection-pane-accessor *mr-interface*)) :test #'string-equal)
      (capi:apply-in-pane-process
       (experiment-selection-pane-accessor *mr-interface*)
       #'(setf capi:choice-selected-item) "User defined" (experiment-selection-pane-accessor *mr-interface*))
    (capi:apply-in-pane-process
       (experiment-selection-pane-accessor *mr-interface*)
       #'(setf capi:choice-selected-item) "User defined [unsaved]" (experiment-selection-pane-accessor *mr-interface*)))
  (setf *experiment-problems* nil))
                    
(defun intension-library-popup-menu (data interface)
  (cond
   ((string-equal data "Add selected to premises")   (add-selected-to-premises (intension-library-panel-accessor *mR-interface*) *mR-interface*))
   ((string-equal data "Set selected to conclusion") (set-selected-to-conclusion (intension-library-panel-accessor *mR-interface*) *mR-interface*))
   ((string-equal data "View intensional semantics") (view-intensional-semantics))
))

(defun problem-list-popup-menu (data interface)
  (cond
   ((string-equal data "Clear")                                        (clear-problem-list-panel))
   ((string-equal data "Enable/disable stochastic system")             (enable-stochastic-system))
   ((string-equal data (format nil "Change ~A parameter..." #\u+03bb)) (adjust-lambda-parameter))
   ((string-equal data (format nil "Change ~A parameter..." #\u+025b)) (adjust-epsilon-parameter))
   ((string-equal data (format nil "Change ~A parameter..." #\u+03c3)) (adjust-sigma-parameter))
   ((string-equal data (format nil "Change ~A parameter..." #\u+03c9)) (adjust-omega-parameter))
   ((string-equal data "Assign category label...")                     (assign-category-label))
   ((string-equal data "Delete")                                       (delete-problems))
))

(defun enable-stochastic-system ()
  (let ((stochastic (capi:prompt-for-confirmation "Enable stochastic system for selected problems?")))
    (modify-problems)
    (mapcar #'(lambda (x) (setf (nth 3 x) stochastic)) (capi:choice-selected-items (problem-list-panel-accessor *mR-interface*)))
    (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*)))

(defun adjust-lambda-parameter ()
  (let ((lambda (capi:prompt-for-number (format nil "Change ~A parameter of selected problems to:" #\u+03bb :min 0.1 :max 8.0))))
    (when lambda
      (modify-problems)
      (mapcar #'(lambda (x) (setf (nth 4 x) lambda)) (capi:choice-selected-items (problem-list-panel-accessor *mR-interface*)))
      (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*))))

(defun adjust-epsilon-parameter ()
  (let ((epsilon (capi:prompt-for-number (format nil "Change ~A parameter of selected problems to:" #\u+025b :min 0.0 :max 1.0))))
    (when epsilon
      (modify-problems)
      (mapcar #'(lambda (x) (setf (nth 5 x) epsilon)) (capi:choice-selected-items (problem-list-panel-accessor *mR-interface*)))
      (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*))))

(defun adjust-sigma-parameter ()
  (let ((sigma (capi:prompt-for-number (format nil "Change ~A parameter of selected problems to:" #\u+03c3) :min 0.0 :max 1.0)))
    (when sigma
      (modify-problems)
      (mapcar #'(lambda (x) (setf (nth 6 x) sigma)) (capi:choice-selected-items (problem-list-panel-accessor *mR-interface*)))
      (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*))))

(defun adjust-omega-parameter ()
  (let ((omega (capi:prompt-for-number (format nil "Change ~A parameter of selected problems to:" #\u+03c9) :min 0.0 :max 1.0)))
    (when omega
      (modify-problems)
      (mapcar #'(lambda (x) (setf (nth 7 x) omega)) (capi:choice-selected-items (problem-list-panel-accessor *mR-interface*)))
      (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*))))

(defun assign-category-label ()
  (let ((category (capi:prompt-for-string "Assign a category label (e.g., control, experimental) for selected problems:")))
    (when category
      (modify-problems)
      (mapcar #'(lambda (x) (setf (nth 8 x) category)) (capi:choice-selected-items (problem-list-panel-accessor *mR-interface*)))
      (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*))))

;(defun delete-problems ()
;  (mapcar #'(lambda (x) (setf (nth 10 x) nil)) (capi:choice-selected-items (problem-list-panel-accessor *mR-interface*)))
;  (setf *experiment-problems* (delete-if #'(lambda (x) (not (nth 9 x))) *experiment-problems*))
;  (let ((problem-number (length *experiment-problems*)))
;    (dolist (problem *experiment-problems*) (setf (nth 0 problem) problem-number) (decf problem-number))
;    (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*)))

(defun modify-problems ()
  (setf (capi:collection-items (experiment-selection-pane-accessor *mr-interface*))
        (substitute "User defined [unsaved]" "User defined" (capi:collection-items (experiment-selection-pane-accessor *mr-interface*))
                    :test #'string-equal)))

; ---------------------------------------------------------------------------------
; Section 12.10: Load experiment
; ---------------------------------------------------------------------------------

(defun load-experiment (exp)
  "Loads pre-defined experiments (defined in Diagnostics.lisp, Section 11.4)"
  (setf *experiment-problems*
        (copy-tree
         (cond
          ((string= exp "User defined") (capi:collection-items (problem-list-panel-accessor *mR-interface*)))
          ((string= exp "Immediate inferences (Experiment 1; Khemlani, Lotstein, Trafton, & Johnson-Laird, 2015)") *immediate-inference-exp1-khemlani-et-al-2015*)
          ((string= exp "Immediate inferences (Experiment 2; Khemlani, Lotstein, Trafton, & Johnson-Laird, 2015)") *immediate-inference-exp2-khemlani-et-al-2015*)
          ((string= exp "Immediate inferences (Experiment 3; Khemlani, Lotstein, Trafton, & Johnson-Laird, 2015)") *immediate-inference-exp3-khemlani-et-al-2015*)
          ((string= exp "Immediate inferences (Experiment 1; Newstead & Griggs, 1983)") *immediate-inference-exp1-newstead-griggs-1983*)
          ((string= exp "Immediate inferences (Experiment 2; Newstead & Griggs, 1983)") *immediate-inference-exp2-newstead-griggs-1983*)
          ((string= exp "Set membership inferences (Khemlani & Johnson-Laird, 2014)") *khemlani-&-johnson-laird-2014*)
          ((string= exp "Set membership inferences (Mascarenhas & Koralus, 2016)") *mascarenhas-&-koralus-2016*)
          ((string= exp "Syllogisms meta-analysis (Khemlani & Johnson-Laird, 2012)") *khemlani-&-johnson-laird-2012*)
          ((string= exp "Temporal inferences (Experiments 1 and 2; Khemlani, Harrison, & Trafton, in preparation)") *temporal-inference-exp1&2-khemlani-et-al-in-prep*)
          ((string= exp "Durational inferences (Experiment 1; Kelly, Khemlani, & Johnson-Laird, 2020") *durational-reasoning-exp1-kelly-et-al-2020*)
          )))
  (setf (capi:collection-items (problem-list-panel-accessor *mR-interface*)) *experiment-problems*))

; ---------------------------------------------------------------------------------
; Section 12.11: Run experiment
; ---------------------------------------------------------------------------------

(defparameter *experiment-process* nil)

(defun enable-interface-controls (enabled)
  (setf (capi:button-enabled (draw-conclusion-button-accessor *mR-interface*)) enabled)
  (setf (capi:button-enabled (add-to-experiment-button-accessor *mR-interface*)) enabled)
  (setf (capi:button-enabled (stochastic-button-accessor *mR-interface*)) enabled)
  (setf (capi:simple-pane-enabled (sigma-parameter-slider-accessor *mR-interface*)) enabled)
  (setf (capi:simple-pane-enabled (lambda-parameter-slider-accessor *mR-interface*)) enabled)
  (setf (capi:simple-pane-enabled (epsilon-parameter-slider-accessor *mR-interface*)) enabled))

(defun cleanup-experiment ()
  (capi:execute-with-interface *mR-interface*
                               #'(lambda ()
                                   (enable-interface-controls t)
                                   (setf (capi:layout-description (experiment-generate-layout-accessor *mR-interface*))
                                         (list (synthetic-data-participants-pane-accessor *mR-interface*) (run-experiment-button-accessor *mR-interface*)))))
  (set-global-parameters-from-interface))

(defun stop-experiment ()
  (mp:process-stop *experiment-process*)
  (cleanup-experiment))

(defun prepare-interface-and-run-experiment ()
  (enable-interface-controls nil)
  (setf (capi:layout-description (experiment-generate-layout-accessor *mR-interface*))
        '())
  (setf (capi:layout-description (experiment-generate-layout-accessor *mR-interface*))
        '(experiment-progress stop-experiment-button))
  (setf (capi:choice-selection (output-layout-accessor *mR-interface*)) 1)

  (setf *experiment-process* (mp:process-run-function "Experiment process" nil
                                                        #'(lambda () (run-experiment-process)))))

(defun run-experiment-process ()
  (let ((num-subjects (capi:choice-selected-item (synthetic-data-participants-pane-accessor *mR-interface*)))
        (run-time (get-universal-time))
        (mail nil))

    (run-experiment *experiment-problems* :N num-subjects :verbose nil :parameters nil :status nil)

    (show-message (synthetic-data-panel-accessor *mR-interface*)
                  (format nil "Experiment completed in ~A minutes." (float (/ (- (get-universal-time) run-time) 60))))
    ; ---  multi-threaded  ---
    ;(while (setf mail (mp:mailbox-read *mailbox* :timeout 0.01))
    ;       (format t "~A~%" mail)
    ;       (push mail *synthetic-data*))
    (setf (capi:collection-items (synthetic-data-panel-accessor *mR-interface*)) (reverse *synthetic-data*))
    (cleanup-experiment)))

; ---------------------------------------------------------------------------------
; Section 12.12: Tracer
; ---------------------------------------------------------------------------------

(defun get-tracer-output ()
  (let ((tracer (reverse (trace-output *tracer*))))
    (list (mapcar #'first tracer)
          (mapcar #'second tracer)
          (mapcar #'third tracer)
          (mapcar #'fourth tracer))))

; ---------------------------------------------------------------------------------
; Section 12.13: Launch interface
; ---------------------------------------------------------------------------------

(defun start-ui ()
  (setf *prompt* "~%mReasoner [~A] ~D~[~:;~:* : ~D~] > ")
  (setf *mR-interface* (capi:contain (make-instance 'mR)))
  (setf (capi:simple-pane-enabled (lambda-parameter-slider-accessor *mR-interface*)) *stochastic*)
  (setf (capi:simple-pane-enabled (epsilon-parameter-slider-accessor *mR-interface*)) *stochastic*)
  (setf (capi:choice-selection (model-intension-layout-accessor *mR-interface*)) 0))

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