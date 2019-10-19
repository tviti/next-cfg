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

(in-package :next)

;; The user stylesheet is loaded into a global var, so that we don't have to
;; re-load it every time the injection function(s) are invoked.
(defvar *user-style* ""
  "String valued stylesheet to inject on page load.")

;; Create a dummy mode so that we can dispatch the page load methods on it.
(define-mode user-style-mode ()
  ""
  ())

(defun inject-stylesheet (str buffer)
  "Inject the CSS stylesheet contained in str, into the buffer buffer."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (let ((str (ps:lisp str))
		(node (ps:chain document (create-element "style"))))
	    (setf (ps:chain node inner-h-t-m-l) str)
	    (ps:chain document body (append-child node))))))

(defun load-stylesheet (css-path)
  "Load the contents of the CSS file at path, into the global user-style var."
  (with-open-file (s css-path :direction :input)
    (let ((contents (make-string (file-length s))))
      (read-sequence contents s)
      (setf *user-style* contents))))

(defmethod did-finish-navigation ((mode user-style-mode) url)
  "Inject the user's stylesheet upon page load completion."
  (inject-stylesheet *user-style* (current-buffer))
  url)
