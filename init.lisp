(in-package :next)

;; Setup the search engine "shortcut" IDs
(setf (get-default 'remote-interface 'search-engines)
      '(("default" . "https://duckduckgo.com/?q=~a")
	("google" . "https://www.google.com/search?q=~a")
	("quickdocs" . "http://quickdocs.org/search?q=~a")
	("wiki" . "https://en.wikipedia.org/w/index.php?search=~a")
	("define" . "https://en.wiktionary.org/w/index.php?search=~a")
	("python3" . "https://docs.python.org/3/search.html?q=~a")
	("doi" . "https://dx.doi.org/~a")))

;;
;; vsc-mode configuration
;;
(setf next/vcs:*vcs-projects-roots* '("~/Source"))
(setf next/vcs:*vcs-usernames-alist* '(("github.com" . "tviti")))

;;
;; Misc. hooks
;;
(defun old-reddit-hook (url)
  "Enforce old reddit. (taken from github.com/vindarel/next-init.lisp)"
  (let* ((uri (quri:uri url)))
    (if (search "www.reddit" (quri:uri-host uri))
        (progn
          (setf (quri:uri-host uri) "old.reddit.com")
          (let ((new-url (quri:render-uri uri)))
            (log:info "Switching to old Reddit: ~a" new-url)
            new-url))
        url)))

;; Unfortunately, if we launch from an application package (e.g. by double
;; clicking Next.app) macOS doesn't seem to properly set the dbus session bus
;; address (that, or I just don't understand HOW it is setting it), so we will
;; instead have to figure out where the bus socket is by querying launchd, then
;; set all the env vars ourselves. NOTE: The following is sbcl specific!
(if (string-equal (software-type) "Darwin")
    (let ((sock_path (string-trim " 
" ;; Trim \n from cmd result (gross, but portable...)
				  (uiop:run-program
				   '("launchctl" "getenv"
				     "DBUS_LAUNCHD_SESSION_BUS_SOCKET")
				   :output :string))))
      (sb-posix:setenv "DBUS_LAUNCHD_SESSION_BUS_SOCKET" sock_path 1)
      (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS"
		       (concatenate 'string "unix:path=" sock_path) 1)))
;; As an alternative, we can also try just create a new session bus, just for
;; next to use (then set the address env var so the port knows where to listen).
;; (let ((sock-addr "unix:path=/tmp/dbus/bus"))
;;   (setq +dbus-launch-command+ (list "dbus-daemon" "--session"
;; 				    (concatenate 'string
;; 						 "--address=" sock-addr)
;; 				    "--fork"))
;;   (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS" sock-addr 1))

;; Launch the port manually
;; (uiop:launch-program '("/usr/bin/env" "python3"
;; 		       "/Users/taylor/common-lisp/next/ports/pyqt-webengine/next-pyqt-webengine.py")
;; 		     :output :interactive)
;; (sleep 2)  ;; Take a breather (otherwise core can't connect for some reason)

(defun my-buffer-completion-filter ()
  (let ((buffers (alexandria:hash-table-values (buffers *interface*)))
        (active-buffer (current-buffer)))
    ;; Make the active buffer the first buffer in the list
    (when (not (equal (first buffers) active-buffer))
      (push active-buffer buffers))
    (lambda (input) (fuzzy-match input buffers))))

(define-command my-delete-buffer ()
  "Delete the buffer via minibuffer input. This is basically identical to the
original implementation, but uses a slightly modified completion function that
makes the active buffer the default deletion (i.e. how it is in Emacs)."
  (with-result (buffers (read-from-minibuffer
			 (make-instance 'minibuffer
					:input-prompt "Kill buffer(s):"
					:multi-selection-p t
					:completion-function (my-buffer-completion-filter))))
    (mapcar #'rpc-buffer-delete buffers)))

(define-command delete-all-buffers ()
  "Delete ALL buffers, EXCEPT for the active buffer. I'd prefer to just delete
ALL of them (even the active buffer), but Next doesn't seem to like it when the
sole active buffer gets deleted."
  (with-result (y-n (read-from-minibuffer
		     (make-instance 'minibuffer
				    :input-prompt "Are you sure you want to kill all buffers (y or n)?")))
    (when (string-equal y-n "y")
      (let* ((active-buffer (current-buffer))
	     (buffers (alexandria:hash-table-values
		       (buffers *interface*)))
	     (bg-buffers (remove active-buffer buffers)))
	(mapcar #'rpc-buffer-delete bg-buffers)))))

;;
;; Hacked together keyboard macros (a hackro?)
;;
(defun spoof-escape-key ()
  "Spoof an ESCAPE keypress. Kind of a dirty hack. The call to
%%push-input-event is copy-pasta from a slime stacktrace after hitting ESCAPE."
  ;; Reset the keystack so we can _really_ spoof a keypress.
  ;; TODO: Possibly problematic if there are multiple `remote-interface' objs.
  (setf (key-chord-stack *interface*) nil)
  (%%push-input-event 16777216 "ESCAPE" '("") -1.0d0 -2.0d0 16777216 "1"))

;;
;; Vim ex style command abbreviations
;;
(defparameter *ex-command-list* '()
  "The list of ex style command abbreviations.")

(defparameter *ex-filter-pairs* '()
  "Input filterfunctions for the ex-commands, that can deal with arguments.")

(defmacro def-ex-command (alias original filter)
  "Create ex style abbreviations for commands. Each abbrev is associated with original,
already existing command, and filter, a minibuffer completion filter."
  `(progn
     (setf (fdefinition ',alias) ,original)
     ;; NOTE: We store the syms always DOWNCASED!
     (pushnew (make-instance 'command
			     :sym (string-downcase (string ',alias))
			     :pkg *package*)
	      *ex-command-list*)
     (pushnew `(,(string-downcase (string ',alias)) ,,filter)
	      *ex-filter-pairs*)))

(defun ex-b-completion-filter (args)
  (funcall (buffer-completion-filter :current-is-last-p t) args))

(defun ex-e-completion-filter (args)
  (let ((completions (funcall (history-completion-filter) args)))
    (push args completions)
    completions))

(def-ex-command b #'switch-buffer #'ex-b-completion-filter)
(def-ex-command e #'set-url-new-buffer #'ex-e-completion-filter)

(define-command ex-insert-candidate (&optional (minibuffer (current-minibuffer)))
  "Insert completion candidate while preserving the ex-cmd on the input buffer."
  (let* ((value (input-buffer minibuffer))
	 (split-cmd (ex-split-cmd value))
	 (ex-cmd (first split-cmd))
	 (candidate (get-candidate minibuffer)))
    (when (and candidate (member ex-cmd (mapcar (lambda (x)
						  (string (sym x)))
						*ex-command-list*)
				 :test #'string=))
      (kill-whole-line minibuffer)
      (insert
       (format nil "~a ~a" ex-cmd candidate) minibuffer))))

(define-mode ex-minibuffer-mode ()
  "Mode for the minibuffer when an ex-command is input (allows for tab
completion of ex-command args)."
  ((keymap-schemes
    :initform
    (let ((map (make-keymap)))
      (define-key :keymap map
	"TAB" #'ex-insert-candidate)
      (list :emacs map
            :vi-normal map
            :vi-insert map)))))

(defun activate-ex-minibuffer-mode (&key (activate t)
				      (minibuffer (current-minibuffer)))
  "Turn on the ex-command minibuffer mode."
  (when minibuffer
    (funcall (sym (mode-command 'ex-minibuffer-mode))
	     :buffer minibuffer :activate activate)))

(defun ex-split-cmd (input)
  ;; For now, we treat only the first space as a delimeter (i.e. all other
  ;; spaces get lumped into the sole argument)
  (ppcre:register-groups-bind (cmd args) ("(.*?)[  ](.*?$)" input)
    (list cmd args)))

(defun ex-command-completion-filter (input)
  "Custom completion function that facilitates argument-passing to ex-commands."
  (let* ((split-cmd (ex-split-cmd input))
	 (ex-cmd (first split-cmd))
	 (ex-args (second split-cmd))
	 (idx (position ex-cmd (mapcar #'first *ex-filter-pairs*)
			:test #'string=)))
    (if idx
	(progn
	  (activate-ex-minibuffer-mode)
	  (funcall (second (nth idx *ex-filter-pairs*)) ex-args))
	(progn ;; Just behave like a normal command entry prompt
	  (activate-ex-minibuffer-mode :activate nil)
	  (command-completion-filter input)))))

;; Handlers for minibuffer entry that dispatch based on the commands type.
(defmethod ex-handler ((buffer buffer))
  (set-current-buffer buffer))

(defmethod ex-handler ((url history-entry))
  (ex-handler (url url)))

(defmethod ex-handler ((url string))
  (let ((buffer (make-buffer)))
    (set-url url :buffer buffer)
    (set-current-buffer buffer))
  (set-url url))

(defmethod ex-handler ((command command))
  (setf (access-time command) (get-internal-real-time))
  (run command))

(define-command execute-command-or-ex ()
  "Execute a command by name. If however the start of the input-buffer is a one
character long ex-command followed by a space, then the rest of the input-buffer
string is treated as an argument line w/ completions (as though the user had
pressed `RETURN' after entering the ex-command's alias)."
  (with-result (result (read-from-minibuffer
			(make-minibuffer
			 :default-modes '(minibuffer-mode)
			 :input-prompt ":"
			 :completion-function 'ex-command-completion-filter)))
    (ex-handler result)))


(define-command open-home-dir ()
  "Open my home directory in a browser window (useful for viewing html exports
e.g. from org-mode or an Rmarkdown doc)."
  (let ((url (concatenate 'string "file://"
			  (directory-namestring (truename "~/")))))
    (set-url url)))

;;
;; Commands for bookmark-db management. These commands all assume that the
;; ssh-key for origin/master has already been added to the ssh-agent, hence
;; obviating the need for any username/password entry!
;;
;; TODO: This should probably get broken out into a package...
(defun is-git-repo (path)
  "Returns path/.git if path contains a .git dir (and is hence assumed to be a
  git repo). Returns nil otherwise."
  (uiop:directory-exists-p (merge-pathnames path ".git")))

(defun bookmark-db-dir ()
  "Return path to the directory containing the active bookmark-db file."
  (uiop:pathname-directory-pathname (bookmark-db-path *interface*)))

(defun bookmark-db-git-cmd (cmd-list)
  "Run `git cmd-list` with the dir containing the current bookmark-db as repo."
  (let* ((git-cmd "git")
	 (db-dir (bookmark-db-dir))
	 (git-dir-opt (format nil "--git-dir=~a.git" db-dir))
	 (git-tree-opt (format nil "--work-tree=~a" db-dir)))
    (uiop:run-program (concatenate 'list
				   `(,git-cmd ,git-dir-opt ,git-tree-opt)
				   cmd-list)
		      :output :string
		      :error-output :output
		      :ignore-error-status t)))

(defun bookmark-db-commit (msg)
  "If the active bookmark db is housed in a repo, then stage updates and commit
  the repos current state, with msg as the commit message. Return nil if there
  is no repo."
  (if (is-git-repo (bookmark-db-dir))
      (progn
	(print (bookmark-db-git-cmd '("add" "--update")))
	(print (bookmark-db-git-cmd `("commit" "-m" ,msg))))
      (progn
	(print (format nil "No repo at ~a !!!" (bookmark-db-dir)))
	'nil)))

(defun set-bookmark-db (path)
  "Set the current active bookmark-db (i.e. (bookmark-db *interface*) to
path. If path lives in a git repo, call `git add path`."
  (setf (bookmarks-path *interface*) path)
  ;; Wipe the existing bookmark data. Use slot-value for this, since calling
  ;; (setf (bookmarks-data *interface*) ...) will also call
  ;; bookmarks-store-function (hence wiping the actual file contents!).
  (setf (slot-value *interface* 'bookmarks-data) nil)
  (restore-sexp-bookmarks)
  (if (is-git-repo (bookmark-db-dir))
      ;; Add to git repo in case the file was just created
      (bookmark-db-git-cmd `("add" ,(namestring path)))))

(defun query-file-path (start-dir
			&key (prompt-base "Path:") callback)
  "Drop into dir, and then start a minibuffer file query. Returns the path to
the selected file. This function is intended to be used in a call to
with-result."
  ;; TODO: Can this be done w/out mucking w/ a global var?
  (setf next/file-manager-mode::*current-directory* start-dir)
  (let ((directory next/file-manager-mode::*current-directory*))
    (read-from-minibuffer
     (make-instance 'minibuffer
		    :callback callback
		    :default-modes '(next/file-manager-mode::file-manager-mode minibuffer-mode)
		    :input-prompt (format nil "~a~a" prompt-base (file-namestring directory))
		    :empty-complete-immediate t
		    :completion-function #'next/file-manager-mode::open-file-from-directory-completion-filter))))

(define-command select-bookmark-db ()
  "Prompt the user to choose which bookmark database file they would like to
use. If the file does not exist, create it, then set it as the active bookmark
database. A git add is then performed on the selected file."
  (let* ((bookmark-db-path (bookmark-db-path *interface*))
	 (bookmark-db-dir (uiop:pathname-directory-pathname bookmark-db-path)))
    (with-result (path (query-file-path bookmark-db-dir
					:prompt-base "Bookmark-db file: "))
      (if (uiop:directory-pathname-p path)
	  ;; TODO: This echo statement currently doesn't show anything...
	  (echo (format nil "~a is a directory! Nothing done!" path))
	  (progn
	    ;; Wipe the currently loaded entries
	    ;; (setf (bookmarks-data *interface*) nil)
	    (print (set-bookmark-db path))
	    (bookmark-db-commit (format nil "select-bookmark-db on ~a" path)))))))

(define-command bookmark-db-pull ()
  "Do a git pull on the bookmark db repo. Return 'nil if there is no repo."
  (if (bookmark-db-commit "bookmark-db-pull")
      (print (bookmark-db-git-cmd '("pull" "origin" "master")))
      'nil))

(define-command bookmark-db-push ()
  "Do a git push on the bookmark db repo. Return 'nil if there is no repo."
  (if (bookmark-db-commit "bookmark-db-push")
      (print (bookmark-db-git-cmd '("push" "origin" "master")))
      'nil))

(defun query-bookmark-db-entry (&key callback)
  "Ask the user to select an entry from the active bookmark-db. Return the url
of the selected entry."
  (read-from-minibuffer
   (make-instance 'minibuffer
		  :input-prompt "Select bookmark:"
		  :completion-function (bookmark-completion-filter)
		  :callback callback)))

(define-command bookmark-db-cp ()
  "Copy a bookmark from the active db to another. The repo state will be
  committed before and after the copy operation. Upon completion, the starting
  db remains the active one."
  (let ((origin-db-path (bookmark-db-path *interface*)))
    (bookmark-db-commit "bookmark-db-cp start")
    (with-result* ((entry (query-bookmark-db-entry))
		   (dest-db-path (query-file-path (bookmark-db-dir))))
      (set-bookmark-db dest-db-path)
      (bookmark-add (url entry)
		    :title (title entry)
		    :tags (tags entry))
      (set-bookmark-db origin-db-path)
      (bookmark-db-commit "bookmark-db-cp end"))))

(define-command bookmark-db-mv ()
  "Move a bookmark from the active db to another. The repo state will be
  committed before and after the copy operation. Upon completion, the starting
  db remains the active one."
  ;; TODO: Severe copy-pasta with bookmark-db-cp
  (let ((origin-db-path (bookmark-db-path *interface*)))
    (bookmark-db-commit "bookmark-db-mv start")
    (with-result* ((entry (query-bookmark-db-entry))
		   (dest-db-path (query-file-path (bookmark-db-dir))))
      ;; This is ripped from the body of (bookmark-delete)
      (setf (bookmarks-data *interface*)
	    (set-difference (bookmarks-data *interface*) (list entry) :test #'equals))
      (set-bookmark-db dest-db-path)
      (bookmark-add (url entry)
		    :title (title entry)
		    :tags (tags entry))
      (set-bookmark-db origin-db-path)
      (bookmark-db-commit "bookmark-db-mv end"))))

(define-command edit-bookmark-title ()
  "Edit the title of an existing bookmark entry (in the currently active db)."
  (with-result* ((entry (read-from-minibuffer
			 (make-instance 'minibuffer
					:input-prompt "Bookmark:"
					:completion-function
					(bookmark-completion-filter))))
		 (title (read-from-minibuffer
			 (make-instance 'minibuffer
					:input-prompt "Title:"
					:completion-function
					(lambda (x) (list x (title entry)))))))
    (unless (str:emptyp title)
      ;; Delete the old entry. This is ripped from the body of `bookmark-delete'
      ;; TODO: copy-pasta
      (setf (bookmarks-data *interface*) ;; Remove the entry
	    (set-difference (bookmarks-data *interface*) (list entry)
			    :test #'equals))
      ;; Set the new entry's title, then stuff it into the db
      (setf (title entry) title)
      (push entry (bookmarks-data *interface*)))))

(define-command make-buffer-from-bookmark ()
  "Open a new tab with url set from a bookmark in the current db."
  (let ((buffer (make-buffer)))
    (set-current-buffer buffer)
    (set-url-from-bookmark)))

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
    (rpc-buffer-evaluate-javascript
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
      (rpc-buffer-evaluate-javascript
       (current-buffer)
       (ps:ps (ps:@ document active-element value))
       :callback (lambda (retval)
		   (edit-in-emacs-callback
		    retval (current-buffer) tempfile))))))

;;
;; User defined CSS styles
;;
;; Load the user-styles "mode"
(load (merge-pathnames "user-styles.lisp" *load-truename*))

;; Load a stylesheet
(defparameter *user-style-path*
  (xdg-config-home "user-styles/Global-Dark-Style/data/themes/global-dark-style.css"))

(load-stylesheet *user-style-path*)

;;
;; Drop all of my customizations into a mode.
;;
(defvar *my-keymap* (make-keymap))
(define-key :keymap *my-keymap*
  "C-c '" #'edit-in-emacs
  "C-c b s" #'select-bookmark-db
  "C-c b m" #'bookmark-db-mv
  "C-c b c" #'bookmark-db-cp
  "C-c b b" #'make-buffer-from-bookmark
  "C-x k" #'my-delete-buffer
  "C-x b" #'switch-buffer
  "C-x r b" #'set-url-from-bookmark
  "M-x" #'execute-command
  "S-x" #'execute-command
  "p" #'next/web-mode:paste
  "P" #'next/web-mode:paste-from-ring
  "Z Z" #'quit-after-clearing-session
  ":" #'execute-command-or-ex
  "C-x C-c" #'quit-after-clearing-session)

(defvar *my-override-map* (make-keymap))
(define-key :keymap *my-override-map*
  "C-[" #'spoof-escape-key)

(define-mode my-mode ()
  ""
  ((keymap-schemes :initform (list :emacs-map *my-keymap*
                                   :vi-normal *my-keymap*))))

;;
;; Buffer customizations
;;
(defclass my-buffer-defaults (buffer)
  ((override-map :initform *my-override-map*)
   (default-modes :initform
       (concatenate 'list
		    '(my-mode vi-normal-mode blocker-mode)
		    (get-default 'buffer 'default-modes)))))
(setf *buffer-class* 'my-buffer-defaults)

;;
;; Minibuffer customizations
;;
(load (merge-pathnames "./themes/spacemacs-light.lisp" *load-truename*))
(defclass my-minibuffer-defaults (minibuffer)
  ((minibuffer-style :initform *my-minibuffer-style*)
   (minibuffer-font-size :initform "14px")))
(setf *minibuffer-class* 'my-minibuffer-defaults)		     
