(in-package :next)

;; Setup the search engine "shortcut" IDs
(setf (get-default 'window 'search-engines)
      '(("default" . "https://duckduckgo.com/?q=~a")
	("google" . "https://www.google.com/search?q=~a")
	("quickdocs" . "https://quickdocs.org/search?q=~a")
	("wiki" . "https://en.wikipedia.org/w/index.php?search=~a")
	("define" . "https://en.wiktionary.org/w/index.php?search=~a")))


;; Unfortunately, if we launch from an application package (e.g. by double
;; clicking Next.app) macOS doesn't seem to properly set the dbus session bus
;; address (that, or I just don't understand HOW it is setting it), so we will
;; instead have to figure out where the bus socket is by querying launchd, then
;; set all the env vars ourselves. NOTE: The following is sbcl specific!
(if (string-equal (software-type) "Darwin")
    (let
	((sock_path (string-trim " 
" ;; Trim the newline from the cmd result (very gross syntax, but portable...)
				 (uiop:run-program
				  '("launchctl" "getenv" "DBUS_LAUNCHD_SESSION_BUS_SOCKET")
				  :output :string))))
      (sb-posix:setenv "DBUS_LAUNCHD_SESSION_BUS_SOCKET" sock_path 1)
      (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS"
		       (concatenate 'string "unix:path=" sock_path) 1)))

;; As an alternative, we can also try just create a new session bus, just for
;; next to use (then set the address env var so the port knows where to listen).
;; (let ((sock-addr "unix:path=/tmp/dbus/bus"))
;;   (setq +dbus-launch-command+ (list "dbus-daemon"
;; 				    "--session"
;; 				    (concatenate 'string "--address=" sock-addr)
;; 				    "--fork"))
;;   (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS" sock-addr 1))

;; Launch the port manually
;; (uiop:launch-program '("/usr/bin/env" "python3"
;; 		       "/Users/taylor/common-lisp/next/ports/pyqt-webengine/next-pyqt-webengine.py")
;; 		     :output :interactive)
;; (sleep 2)  ;; Take a breather (otherwise core can't connect for some reason)

(defun my-buffer-completion-fn ()
  (let ((buffers (alexandria:hash-table-values (buffers *interface*)))
        (active-buffer (active-buffer *interface*)))
    ;; Make the active buffer the first buffer in the list
    (when (not (equal (first buffers)
		      active-buffer))
      (push active-buffer buffers))
    (lambda (input)
      (fuzzy-match input buffers))))

(define-command my-delete-buffer ()
  "Delete the buffer via minibuffer input. This is basically identical to the
original implementatino, but uses a slightly modified completion function that
makes the active buffer the default deletion (which is how Emacs does it)."
  (with-result (buffer (read-from-minibuffer
                        (make-instance 'minibuffer
                                       :input-prompt "Kill buffer:"
                                       :completion-function (my-buffer-completion-fn))))
    (rpc-buffer-delete *interface* buffer)))

;; Make vi-normal the default keybinding scheme
(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; Edit vi-normal a bit to make it more evil
(define-key :scheme :vi-normal
  "C-x k" #'my-delete-buffer)

(define-key :scheme :vi-insert
  "C-[" #'next/vi-mode:vi-normal-mode
  "C-x k" #'my-delete-buffer)

(define-key :mode 'minibuffer-mode
  "C-[" #'cancel-input)

(define-command my-set-url-new-buffer ()
  "This is a hack of the original next command, with a small sleep timer added
because looks like the original tries to set url before buffer creation
finishes."
  (with-result (url (buffer-get-url))
    (let ((history (minibuffer-set-url-history *interface*)))
      (when history
        (ring:insert history url))
      (with-result (url (read-from-minibuffer
                         (make-instance 'minibuffer
                                        :input-prompt "Open URL in new buffer:"
                                        :completion-function 'history-typed-complete
                                        :history history
                                        :empty-complete-immediate t)))
        (let ((buffer (make-buffer)))
	  (set-active-buffer *interface* buffer)
	  (sleep 0.05) ;; our sleep timer hack
          (set-url url :buffer buffer))))))

;; Evil abbreviations for cmds
(defmacro def-cmd-alias (alias original)
  "This doesn't seem like the right way to do this"
  `(progn
     (define-command ,alias ()
       (,original))
     (setf (fdefinition ',alias) #',original)))
(def-cmd-alias b switch-buffer)
(def-cmd-alias e my-set-url-new-buffer)

(define-command open-home-dir ()
  "Open my home directory in a browser window"
  (let ((url (concatenate 'string "file://"
			  (directory-namestring(truename "~/")))))
    (buffer-set-url :url url :buffer (active-buffer *interface*))))

;;
;; Commands for bookmark-db management
;;
;; TODO: Should (select-bookmark-db) add new db to git repo file (if it exists)?
(define-command select-bookmark-db ()
  "Prompt the user to choose which bookmark database file they would like to
use. If the file does not exist, create it, then set it as the active bookmark
database."
  (let ((bookmark-db-path (xdg-data-home)))
    ;; Can this be done w/out setting a global var?
    (setf next/file-manager-mode::*current-directory* bookmark-db-path)
    (with-result (path (read-from-minibuffer
			(make-instance 'minibuffer
				       :default-modes '(next/file-manager-mode::file-manager-mode minibuffer-mode)
				       :input-prompt "path:"
				       :empty-complete-immediate t
				       :completion-function #'next/file-manager-mode::open-file-from-directory-completion-fn)))
      (if (uiop:directory-pathname-p path)
	  ;; TODO: This echo statement currently doesn't show anything...
	  (echo (format nil "~a is a directory! Nothing done!" path))
	  (progn
	    (ensure-file-exists path #'%initialize-bookmark-db)
	    (setf (bookmark-db-path (rpc-window-active *interface*)) path))))))

;; TODO: construct db-dir from bookmark-db-path global
;; TODO: allow user to specify remote and branch
;; TODO: display command output in minibuffer
;; TODO: password prompts
(defun bookmark-db-git-cmd (cmd-list)
  (let* ((git-cmd "git")
	 (db-dir (directory-namestring (xdg-data-home)))
	 (git-dir-cmd (concatenate 'string "--git-dir=" db-dir ".git"))
	 (git-tree-cmd (concatenate 'string "--work-tree=" db-dir)))
    (uiop:run-program (concatenate 'list
				   `(,git-cmd ,git-dir-cmd ,git-tree-cmd)
				   cmd-list)
		      :output :string
		      :error-output :output
		      :ignore-error-status t)))

(define-command bookmark-db-pull ()
  "Do a git pull on the bookmark db repo. Assumes that (xdg-data-home) has been
setup as a repo for your bookmarks!"
  (print (bookmark-db-git-cmd '("add" "--update")))
  (print (bookmark-db-git-cmd '("commit" "-m" "pre-pull")))
  (print (bookmark-db-git-cmd '("pull" "origin" "master"))))

(define-command bookmark-db-push ()
  "Do a git push on the bookmark db repo. Assumes that (xdg-data-home) has been
setup as a repo for your bookmarks!"
  (print (bookmark-db-git-cmd '("add" "--update")))
  (print (bookmark-db-git-cmd '("commit" "-m" "bookmark db update")))
  (print (bookmark-db-git-cmd '("push" "origin" "master"))))

;; (add-to-default-list (lambda ()
;; 		       (setf (get-default 'buffer 'box-style)
;; 			     (with-open-file (stream "~/.config/next/GitHub-Dark/github-dark.user.css")
;; 			       (let ((contents (make-string (file-length stream))))
;; 				 (read-sequence contents stream)
;; 				 contents))))
;; 		     'buffer 'load-hook)
		      
;; Enable blocker mode in new browsers
(add-to-default-list 'blocker-mode 'buffer 'default-modes)
