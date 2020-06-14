;; A spacemacs-light theme for the nyxt-browser minibuffer
(in-package :nyxt-user)

(defvar *my-minibuffer-style* (cl-css:css
			       '((* :font-family "DejaVu Sans Mono"
				  :color "#655370")
				 (body :border-top "1px solid dimgray"
				  :background-color "#fbf8ef"
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
				  :color "#3a81c3")
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
				 (.selected :background-color "#d3d3e7"
				  :font-weight "bold"))))

(defmethod minibuffer-line-style ((minibuffer minibuffer))
  (cl-css:css
   `((* :font-size ,(minibuffer-font-size minibuffer)
        :line-height ,(minibuffer-line-height minibuffer)
	:background-color "#fbf8ef"))))
