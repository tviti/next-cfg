;; A next-browser mode for injecting CSS stylesheets into webviews, e.g. to
;; achieve a "dark-theme".
;;
;; To use this file, first (load ...) it somewhere in your next config
;; (e.g. init.lisp). Then, call load-stylesheet on the .CSS file you'd like to
;; associate with the user-style-mode. This will load the CSS file's contents
;; into the global *user-style* variable, which will be applied (upon page load
;; completion) to any buffer that has user-style-mode active. To disable the
;; stylesheet in a buffer, just disable the mode.

;; TODO: Can we override the default white buffer background, so that we don't
;; get blinded temporarily while pages are still loading?
(uiop:define-package :next/user-style-mode
    (:use :common-lisp :next)
  (:export :*user-style* :%load-stylesheet :%add-to-default-modes))
(in-package :next/user-style-mode)

;; The user stylesheet is loaded into a global var, so that we don't have to
;; re-load it every time the injection function(s) are invoked.
(defvar *user-style* ""
  "String valued stylesheet to inject on page load.")

;; Create a dummy mode so that we can dispatch the page load methods on it.
(define-mode user-style-mode ()
  ""
  ((keymap-schemes :initform nil)))

(defun inject-stylesheet (str buffer)
  "Inject the CSS stylesheet STR, into the BUFFER."
  (rpc-buffer-evaluate-javascript
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
      (setf next/user-style-mode:*user-style* contents))))

(defun load-stylesheet-filter (input)
  "TODO: Completion filter."
  (list input))

(defun %add-to-default-modes (buffer)
  (pushnew 'user-style-mode (default-modes buffer)))

(in-package :next)

(defun activate-global-style (&optional (interface *interface*))
  "Add `user-style-mode' to the `default-modes' of new buffers."
  (with-accessors ((active-buffer last-active-buffer)
		   (hook buffer-make-hook)) interface
    (next-hooks:add-hook hook (make-handler-buffer
			       #'next/user-style-mode:%add-to-default-modes))
    (funcall (sym (mode-command 'next/user-style-mode:user-style-mode))
	     :buffer active-buffer :activate t)
    (reload-current-buffer active-buffer)))

(defun deactivate-global-style (&optional (interface *interface*))
  "Remove `user-style-mode' from the `default-modes' of new buffers."
  (with-accessors ((active-buffer last-active-buffer)) interface
    (next-hooks:remove-hook (buffer-make-hook interface)
			    'next/user-style-mode:%add-to-default-modes)
    (funcall (sym (mode-command 'next/user-style-mode:user-style-mode))
	     :buffer active-buffer :activate nil)
    (reload-current-buffer active-buffer)))
	
(define-command toggle-global-style (&optional (interface *interface*))
  "Toggle using `user-style-mode' in new buffers."
  (with-accessors ((handlers next-hooks:handlers)) (buffer-make-hook interface)
    (if (member 'next/user-style-mode:%add-to-default-modes
		(mapcar #'next-hooks:name handlers))
	(deactivate-global-style interface)
	(activate-global-style interface))))

(define-command load-stylesheet (&optional (css-path))
  "Prompt for a stylesheet path CSS-PATH, then load the contents of the css file
into the global `*user-style*' var."
  (if css-path
      (next/user-style-mode:%load-stylesheet css-path)
      (with-result (css-path (read-from-minibuffer
			      (make-instance 'minibuffer
					     :input-prompt "Stylesheet:"
					     :empty-complete-immediate t
					     :completion-function #'next/user-style-mode::load-stylesheet-filter)))
	(next/user-style-mode:%load-stylesheet css-path))))

(defmethod did-finish-navigation ((mode next/user-style-mode:user-style-mode) url)
  "Inject the user's stylesheet upon page load completion."
  (next/user-style-mode::inject-stylesheet next/user-style-mode:*user-style* (current-buffer))
  url)
