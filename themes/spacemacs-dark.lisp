;; A spacemacs-dark theme for the next-browser minibuffer
(in-package :next-user)

(defvar *my-minibuffer-style* (cl-css:css
			       '((* :font-family "DejaVu Sans Mono"
				  :color "#b2b2b2")
				 (body :border-top "1px solid dimgray"
				  :background-color "#292b2e"
				  :margin "0"
				  :padding "0 6px")
				 ("#container" :display "flex"
				  :flex-flow "column"
				  :height "100%")
				 ("#input" :padding "6px 0"
				  :border-bottom "solid 1px lightgray")
				 ("#completions" :flex-grow "1"
				  :overflow-y "auto"
				  :overflow-x "auto")
				 ("#cursor" :background-color "gray"
				  :color "white")
				 ("#prompt" :padding-right "4px"
				  :font-weight "bold"
				  :color "#4f97d7")
				 (ul :list-style "none"
				  :padding "0"
				  :margin "0")
				 (li :padding "2px")
				 (.marked :background-color "darkgray"
				  :font-weight "bold"
				  :color "white")
				 ;; .selected must be set _after_ .marked so
				 ;; that it overrides its attributes since the
				 ;; candidate can be both marked and selected.
				 (.selected :background-color "#444155"
				  :font-weight "bold"))))

(defmethod minibuffer-line-style ((minibuffer minibuffer))
  (cl-css:css
   `((* :font-size ,(minibuffer-font-size minibuffer)
        :line-height ,(minibuffer-line-height minibuffer)
	:background-color "#292b2e"))))
