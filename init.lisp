(in-package :next-user)

;; Setup the search engine "shortcut" IDs
(defvar *my-search-engines* nil)
(setf *my-search-engines*
      (list
       '("google" "https://www.google.com/search?q=~a" "https://www.google.com/")
       '("quickdocs" "http://quickdocs.org/search?q=~a" "http://quickdocs.org/")
       '("wiki" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org/")
       '("define" "https://en.wiktionary.org/w/index.php?search=~a" "https://en.wiktionary.org/")
       '("python3" "https://docs.python.org/3/search.html?q=~a" "https://docs.python.org/3")
       '("doi" "https://dx.doi.org/~a" "https://dx.doi.org/")))

;;
;; vsc-mode configuration
;;
(setf next/vcs:*vcs-projects-roots* '("~/Source"))
(setf next/vcs:*vcs-usernames-alist* '(("github.com" . "tviti")))

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
;; Commands for bookmark-db management. These commands all assume that the
;; ssh-key for origin/master has already been added to the ssh-agent, hence
;; obviating the need for any username/password entry!
;;
;; TODO: This should probably get broken out into a package...
;; (defun is-git-repo (path)
;;   "Returns path/.git if path contains a .git dir (and is hence assumed to be a
;;   git repo). Returns nil otherwise."
;;   (uiop:directory-exists-p (merge-pathnames path ".git")))

;; (defun bookmark-db-dir ()
;;   "Return path to the directory containing the active bookmark-db file."
;;   (uiop:pathname-directory-pathname (bookmarks-path *browser*)))

;; (defun bookmark-db-git-cmd (cmd-list)
;;   "Run `git cmd-list` with the dir containing the current bookmark-db as repo."
;;   (let* ((git-cmd "git")
;;       (db-dir (bookmark-db-dir))
;;       (git-dir-opt (format nil "--git-dir=~a.git" db-dir))
;;       (git-tree-opt (format nil "--work-tree=~a" db-dir)))
;;     (uiop:run-program (concatenate 'list
;;                                 `(,git-cmd ,git-dir-opt ,git-tree-opt)
;;                                 cmd-list)
;;                    :output :string
;;                    :error-output :output
;;                    :ignore-error-status t)))

;; (defun bookmark-db-commit (msg)
;;   "If the active bookmark db is housed in a repo, then stage updates and commit
;;   the repos current state, with msg as the commit message. Return nil if there
;;   is no repo."
;;   (if (is-git-repo (bookmark-db-dir))
;;       (progn
;;      (print (bookmark-db-git-cmd '("add" "--update")))
;;      (print (bookmark-db-git-cmd `("commit" "-m" ,msg))))
;;       (progn
;;      (print (format nil "No repo at ~a !!!" (bookmark-db-dir)))
;;      'nil)))

;; (defun set-bookmark-db (path)
;;   "Set the current active bookmark-db (i.e. (bookmark-db *browser*) to
;; path. If path lives in a git repo, call `git add path`."
;;   (setf (bookmarks-path *browser*) path)
;;   ;; Wipe the existing bookmark data. Use slot-value for this, since calling
;;   ;; (setf (bookmarks-data *browser*) ...) will also call
;;   ;; bookmarks-store-function (hence wiping the actual file contents!).
;;   (setf (slot-value *browser* 'bookmarks-data) nil)
;;   (next::restore-sexp-bookmarks)
;;   (if (is-git-repo (bookmark-db-dir))
;;       ;; Add to git repo in case the file was just created
;;       (bookmark-db-git-cmd `("add" ,(namestring path)))))

;; (defun query-file-path (start-dir &key (prompt-base "Path:"))
;;   "Drop into dir, and then start a minibuffer file query. Returns the path to
;; the selected file. This function is intended to be used in a call to
;; with-result."
;;   ;; TODO: Can this be done w/out mucking w/ a global var?
;;   ;; (setf next/file-manager-mode::*current-directory* start-dir)
;;   (uiop:with-current-directory (start-dir)
;;     (with-result (path (read-from-minibuffer
;; 			(make-minibuffer
;; 			 :default-modes '(next/file-manager-mode::file-manager-mode minibuffer-mode)
;; 			 :input-prompt (format nil "~a~a" prompt-base (file-namestring start-dir))
;; 			 :empty-complete-immediate t
;; 			 :completion-function #'next/file-manager-mode::open-file-from-directory-completion-filter)))
;;       (file path))))

;; (define-command select-bookmark-db ()
;;   "Prompt the user to choose which bookmark database file they would like to
;; use. If the file does not exist, create it, then set it as the active bookmark
;; database. A git add is then performed on the selected file."
;;   (let* ((bookmarks-path (bookmarks-path *browser*))
;; 	 (bookmarks-dir (dirname bookmarks-path)))
;;     (with-result (path (query-file-path bookmarks-dir
;; 					:prompt-base "Bookmark-db file: "))
;;       (if (uiop:directory-pathname-p path)
;;        ;; TODO: This echo statement currently doesn't show anything...
;;        (echo (format nil "~a is a directory! Nothing done!" path))
;;        (progn
;;          ;; Wipe the currently loaded entries
;;          ;; (setf (bookmarks-data *browser*) nil)
;;          (print (set-bookmark-db path))
;;          (bookmark-db-commit (format nil "select-bookmark-db on ~a" path)))))))

;; (define-command bookmark-db-pull ()
;;   "Do a git pull on the bookmark db repo. Return 'nil if there is no repo."
;;   (if (bookmark-db-commit "bookmark-db-pull")
;;       (print (bookmark-db-git-cmd '("pull" "origin" "master")))
;;       'nil))

;; (define-command bookmark-db-push ()
;;   "Do a git push on the bookmark db repo. Return 'nil if there is no repo."
;;   (if (bookmark-db-commit "bookmark-db-push")
;;       (print (bookmark-db-git-cmd '("push" "origin" "master")))
;;       'nil))

;; (defun query-bookmark-db-entry ()
;;   "Ask the user to select an entry from the active bookmark-db. Return the url
;; of the selected entry."
;;   (read-from-minibuffer
;;    (make-minibuffer
;;     :input-prompt "Select bookmark:"
;;     :completion-function (bookmark-completion-filter))))

;; (define-command bookmark-db-cp ()
;;   "Copy a bookmark from the active db to another. The repo state will be
;;   committed before and after the copy operation. Upon completion, the starting
;;   db remains the active one."
;;   (let ((origin-db-path (bookmarks-path *browser*)))
;;     (bookmark-db-commit "bookmark-db-cp start")
;;     (with-result* ((entry (query-bookmark-db-entry))
;;                    (dest-db-path (query-file-path (bookmark-db-dir))))
;;       (set-bookmark-db dest-db-path)
;;       (bookmark-add (url entry)
;;                     :title (title entry)
;;                     :tags (tags entry))
;;       (set-bookmark-db origin-db-path)
;;       (bookmark-db-commit "bookmark-db-cp end"))))

;; (define-command bookmark-db-mv ()
;;   "Move a bookmark from the active db to another. The repo state will be
;;   committed before and after the copy operation. Upon completion, the starting
;;   db remains the active one."
;;   ;; TODO: Severe copy-pasta with bookmark-db-cp
;;   (let ((origin-db-path (bookmarks-path *browser*)))
;;     (bookmark-db-commit "bookmark-db-mv start")
;;     (with-result* ((entry (query-bookmark-db-entry))
;;                    (dest-db-path (query-file-path (bookmark-db-dir))))
;;       ;; This is ripped from the body of (bookmark-delete)
;;       (setf (bookmarks-data *browser*)
;;             (set-difference (bookmarks-data *browser*) (list entry) :test #'equals))
;;       (set-bookmark-db dest-db-path)
;;       (bookmark-add (url entry)
;;                     :title (title entry)
;;                     :tags (tags entry))
;;       (set-bookmark-db origin-db-path)
;;       (bookmark-db-commit "bookmark-db-mv end"))))

;; (define-command edit-bookmark-title ()
;;   "Edit the title of an existing bookmark entry (in the currently active db)."
;;   (with-result* ((entry (read-from-minibuffer
;; 			 (make-instance 'minibuffer
;; 					:input-prompt "Bookmark:"
;; 					:completion-function
;; 					(bookmark-completion-filter))))
;; 		 (title (read-from-minibuffer
;; 			 (make-instance 'minibuffer
;; 					:input-prompt "Title:"
;; 					:completion-function
;; 					(lambda (x) (list x (title entry)))))))
;;     (unless (str:emptyp title)
;;       ;; Delete the old entry. This is ripped from the body of `bookmark-delete'
;;       ;; TODO: copy-pasta
;;       (setf (bookmarks-data *browser*) ;; Remove the entry
;;             (set-difference (bookmarks-data *browser*) (list entry)
;;                             :test #'equals))
;;       ;; Set the new entry's title, then stuff it into the db
;;       (setf (title entry) title)
;;       (push entry (bookmarks-data *browser*)))))

;; (define-command make-buffer-from-bookmark ()
;;   "Open a new tab with url set from a bookmark in the current db."
;;   (let ((buffer (make-buffer)))
;;     (set-current-buffer buffer)
;;     (set-url-from-bookmark)))

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
                               &optional (tempfile "/tmp/next-tmp.txt"))
  (let ((output (edit-str-with-emacs retval tempfile)))
    (ffi-buffer-evaluate-javascript
     buffer
     (ps:ps (setf (ps:@ document active-element value) (ps:lisp output))))))

(define-command edit-in-emacs ()
  "Open a new emacs frame using the `emacsclient' mechanism, and place the value
  of the currently selected input element in a temporary buffer. Upon exiting
  using the <C-x #> keybinding, the text will be placed in the next-buffer's
  active input element."
  (with-result (ext (read-from-minibuffer
                     (make-instance 'minibuffer
                                    :input-prompt "temp-file ext (default .txt):")))
    (let ((tempfile (format nil "/tmp/next-tmp~a" (if ext ext ".txt"))))
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
     "next"
     :store (lambda ()
              (org-store-link-props
               :type "next"
               :link ,(url buffer)
               :description ,(title buffer))))
   `(org-capture)))

(defun eval-in-emacs (&rest s-exps)
  "Evaluate S-exps with `emacsclient'. Stolen from Ambrevar's dotfiles repo."
  (let ((s-exps-string (cl-ppcre:regex-replace-all
                        ;; Discard the package prefix.
                        "next-user::?"
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
  (uiop:xdg-config-home "next/user-styles/Global-Dark-Style/data/themes/global-dark-style.css"))

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
    "C-x r b" 'next/web-mode::set-url-from-bookmark
    "C-c '" 'edit-in-emacs
    "C-c s-O" 'org-capture
    "p" 'next/web-mode:paste
    "P" 'next/web-mode:paste-from-ring
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
;; (load (merge-pathnames "./themes/spacemacs-light.lisp" *load-truename*))
(load (merge-pathnames "./themes/spacemacs-dark.lisp" *load-truename*))
(define-configuration minibuffer
  ((minibuffer-style *my-minibuffer-style*)))
