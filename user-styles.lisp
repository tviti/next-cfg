;; A nyxt-browser mode for injecting CSS stylesheets into webviews, e.g. to
;; achieve a "dark-theme".
;;
;; To use this file, first (load ...) it somewhere in your nyxt config
;; (e.g. init.lisp). Then, call load-stylesheet on the .CSS file you'd like to
;; associate with the user-style-mode. This will load the CSS file's contents
;; into the global *user-style* variable, which will be applied (upon page load
;; completion) to any buffer that has user-style-mode active. To disable the
;; stylesheet in a buffer, just disable the mode.

;; TODO: Can we override the default white buffer background, so that we don't
;; get blinded temporarily while pages are still loading?
(uiop:define-package :nyxt/user-style-mode
    (:use :common-lisp :nyxt)
  (:export :*user-style* :%load-stylesheet :%add-to-default-modes))
(in-package :nyxt/user-style-mode)

(defvar *user-style* ""
  "String valued stylesheet to inject on page load.

The user stylesheet is loaded into a global var, so that we don't have to
re-load it every time the injection function(s) are invoked.")

(define-mode user-style-mode ()
  "Dummy mode that page load methods can be dispatched on."
  ((keymap-schemes :initform nil)))

(defun inject-stylesheet (str buffer)
  "Inject the CSS stylesheet STR, into the BUFFER."
  (ffi-buffer-evaluate-javascript
   buffer
   (ps:ps (let ((str (ps:lisp str))
		(node (ps:chain document (create-element "style"))))
	    (setf (ps:chain node inner-h-t-m-l) str)
	    (ps:chain document body (append-child node))))))

(defun %load-stylesheet (css-path)
  "Load the contents of the CSS file at path, into the global user-style var."
  (log:info "Setting *user-style* from ~a" css-path)
  (with-open-file (s css-path :direction :input)
    (let ((contents (make-string (file-length s))))
      (read-sequence contents s)
      (setf nyxt/user-style-mode:*user-style* contents))))

(defun load-stylesheet-filter (input)
  "TODO: Completion filter."
  (list input))

(defun %add-to-default-modes (buffer)
  (pushnew 'user-style-mode (default-modes buffer)))

(in-package :nyxt)

(defun activate-global-style (&optional (browser *browser*))
  "Add `user-style-mode' to the `default-modes' of new buffers, as well as the
`modes' of existing buffers. The `last-active-buffer' will also be reloaded."
  (mapcar (lambda (buffer)
	    (funcall (sym (mode-command 'nyxt/user-style-mode:user-style-mode))
		     :buffer buffer :activate t))
	  (alexandria:hash-table-values (buffers browser)))
  (with-accessors ((active-buffer last-active-buffer)
		   (hook buffer-before-make-hook)) browser
    (hooks:add-hook hook (make-handler-buffer
			  #'nyxt/user-style-mode:%add-to-default-modes))
    (funcall (sym (mode-command 'nyxt/user-style-mode:user-style-mode))
	     :buffer active-buffer :activate t)
    (reload-current-buffer active-buffer)))

(defun deactivate-global-style (&optional (browser *browser*))
  "Remove `user-style-mode' from current and new buffers. The
`last-active-buffer' will also be reloaded."
  (mapcar (lambda (buffer)
	    (funcall (sym (mode-command 'nyxt/user-style-mode:user-style-mode))
		     :buffer buffer :activate nil))
	  (alexandria:hash-table-values (buffers browser)))
  (hooks:remove-hook (buffer-before-make-hook browser)
		     'nyxt/user-style-mode:%add-to-default-modes)
    (reload-current-buffer (last-active-buffer browser)))
	
(define-command toggle-global-style (&optional (browser *browser*))
  "Toggle using `user-style-mode' in new buffers."
  (with-accessors ((handlers hooks:handlers)) (buffer-before-make-hook browser)
    (if (member 'nyxt/user-style-mode:%add-to-default-modes
		(mapcar #'hooks:name handlers))
	(deactivate-global-style browser)
	(activate-global-style browser))))

(define-command load-stylesheet (&optional (css-path))
  "Prompt for a stylesheet path CSS-PATH, then load the contents of the css file
into the global `*user-style*' var."
  (if css-path
      (nyxt/user-style-mode:%load-stylesheet css-path)
      (with-result (css-path (read-from-minibuffer
			      (make-instance 'minibuffer
					     :input-prompt "Stylesheet:"
					     :empty-complete-immediate t
					     :completion-function #'nyxt/user-style-mode::load-stylesheet-filter)))
	(nyxt/user-style-mode:%load-stylesheet css-path))))

(in-package :nyxt)
(defmethod on-signal-load-finished ((mode nyxt/user-style-mode:user-style-mode) url)
  "Inject the user's stylesheet upon page load completion."
  (nyxt/user-style-mode::inject-stylesheet nyxt/user-style-mode:*user-style* (current-buffer))
  url)
