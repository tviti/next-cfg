(in-package :nyxt-user)

;; Setup the search engine "shortcut" IDs
(defvar *my-search-engines* nil)
(setf *my-search-engines*
      (list
       '("gmt" "https://docs.generic-mapping-tools.org/6.0/search.html?q=~a&check_keywords=yes&area=default" "https://docs.generic-mapping-tools.org/6.0")
       '("gmt-docs" "https://docs.generic-mapping-tools.org/6.0/~a.html" "https://docs.generic-mapping-tools.org/6.0")
       '("google" "https://www.google.com/search?q=~a" "https://www.google.com/")
       '("quickdocs" "http://quickdocs.org/search?q=~a" "http://quickdocs.org/")
       '("wiki" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org/")
       '("define" "https://en.wiktionary.org/w/index.php?search=~a" "https://en.wiktionary.org/")
       '("python3" "https://docs.python.org/3/search.html?q=~a" "https://docs.python.org/3")
       '("doi" "https://dx.doi.org/~a" "https://dx.doi.org/")))

;;
;; vsc-mode configuration
;;
(setf nyxt/vcs:*vcs-projects-roots* '("~/Source"))
(setf nyxt/vcs:*vcs-usernames-alist* '(("github.com" . "tviti")))

;;
;; Misc. hook handlers
;;
(defun old-reddit-handler (request-data)
  (let* ((url (url request-data))
         (uri (quri:uri url)))
    (setf (url request-data)
          (if (search "reddit.com" (quri:uri-host uri))
              (progn
                (setf (quri:uri-host uri) "old.reddit.com")
                (let ((new-url (quri:render-uri uri)))
                  (log:info "Switching to old Reddit: ~a" new-url)
                  new-url))
              url)))
  request-data)

;;
;; Keyboard macros
;;
(define-command spoof-escape-key ()
  "Spoof an ESCAPE keypress."
  (let ((bound-value (keymap:lookup-key "escape" (current-keymaps))))
    (when bound-value (funcall-safely bound-value))))

;; (define-command open-home-dir ()
;;   "Open my home directory in a browser window (useful for viewing html exports
;; e.g. from org-mode or an Rmarkdown doc)."
;;   (let ((url (concatenate 'string "file://"
;;                        (directory-namestring (truename "~/")))))
;;     (set-url url)))

;;
;; Emacs integration
;;
;; TODO: Use a better tempfile name (something more... random)
(defun edit-str-with-emacs (str tempfile)
  "Dump the contents of str to the temporary file tempfile, then open tempfile
in Emacs for editing. Note that this call is synchronous!"
  ;; Dump the cell's contents to a tempfile
  (with-open-file (s tempfile :direction :output :if-exists :supersede)
    ;; Replace \n with literal newlines
    (format s "~a" str))
  ;; Open an emacs buffer pointed at the file
  (uiop:run-program `("emacsclient" ,tempfile) :output :string)
  ;; Read the file contents back in
  (with-open-file (s tempfile :direction :input)
    (let ((contents (make-string (file-length s))))
      (read-sequence contents s)
      contents)))

(defun edit-in-emacs-callback (retval buffer
                               &optional (tempfile "/tmp/nyxt-tmp.txt"))
  (let ((output (edit-str-with-emacs retval tempfile)))
    (ffi-buffer-evaluate-javascript
     buffer
     (ps:ps (setf (ps:@ document active-element value) (ps:lisp output))))))

(define-command edit-in-emacs ()
  "Open a new emacs frame using the `emacsclient' mechanism, and place the value
  of the currently selected input element in a temporary buffer. Upon exiting
  using the <C-x #> keybinding, the text will be placed in the nyxt-buffer's
  active input element."
  (with-result (ext (read-from-minibuffer
                     (make-instance 'minibuffer
                                    :input-prompt "temp-file ext (default .txt):")))
    (let ((tempfile (format nil "/tmp/nyxt-tmp~a" (if ext ext ".txt"))))
      (ffi-buffer-evaluate-javascript
       (current-buffer)
       (ps:ps (ps:@ document active-element value))
       :callback (lambda (retval)
                   (edit-in-emacs-callback
                    retval (current-buffer) tempfile))))))

(define-command org-capture (&optional (buffer (current-buffer)))
  "Org-capture current page. Stolen from Ambrevar's dotfiles repo."
  (eval-in-emacs
   `(org-link-set-parameters
     "nyxt"
     :store (lambda ()
              (org-store-link-props
               :type "nyxt"
               :link ,(url buffer)
               :description ,(title buffer))))
   `(org-capture)))

(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'. Stolen from Ambrevar's dotfiles repo."
  (let ((s-exps-string (cl-ppcre:regex-replace-all
                        ;; Discard the package prefix.
                        "nyxt-user::?"
                        (write-to-string
                         `(progn ,@s-exps) :case :downcase)
                        "")))
    (log:debug "Sending to Emacs: ~s" s-exps-string)
    (ignore-errors (uiop:run-program
                    (list "emacsclient" "--eval" s-exps-string)))))

;;
;; User defined CSS styles
;;
;; Load the user-styles "mode"
(load (merge-pathnames "user-styles.lisp" *load-truename*))

;; Load a stylesheet
(defparameter *user-style-path*
  (uiop:xdg-config-home "nyxt/user-styles/Global-Dark-Style/data/themes/global-dark-style.css"))

(load-stylesheet *user-style-path*)

;;
;; Drop all of my customizations into various modes
;;
(defvar *my-override-map* (make-keymap "my-override-map")
  "My override keymap.")
(define-key *my-override-map*
    "C-[" 'spoof-escape-key
    "C-g" 'spoof-escape-key)

(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
    "C-x r b" 'nyxt/web-mode::set-url-from-bookmark
    "C-c '" 'edit-in-emacs
    "C-c s-O" 'org-capture
    "p" 'nyxt/web-mode:paste
    "P" 'nyxt/web-mode:paste-from-ring
    "Z Z" 'quit-after-clearing-session
    "C-x C-c" 'quit-after-clearing-session)

(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-scheme :initform (keymap:make-scheme
                             scheme:emacs *my-keymap*
                             scheme:vi-normal *my-keymap*))))

;;
;; Buffer customizations
;;
(define-configuration buffer
    ((override-map *my-override-map*)
     (default-modes (append '(my-mode vi-normal-mode blocker-mode)
			    %slot-default))
     (request-resource-hook
      (reduce #'hooks:add-hook
	      (mapcar #'make-handler-resource (list #'old-reddit-handler))
	      :initial-value %slot-default))))

(define-configuration browser
  ((search-engines (append (mapcar (lambda (x)
				     (make-instance 'search-engine
						    :shortcut (first x)
						    :search-url (second x)
						    :fallback-url (third x)))
				   *my-search-engines*)
                           %slot-default))))

;;
;; Minibuffer customizations
;;
(load (merge-pathnames "./themes/spacemacs-light.lisp" *load-truename*))
;; (load (merge-pathnames "./themes/spacemacs-dark.lisp" *load-truename*))
(define-configuration minibuffer
  ((minibuffer-style *my-minibuffer-style*)))
