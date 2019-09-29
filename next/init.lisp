(in-package :next)

;; Setup the search engine "shortcut" IDs
(setf (get-default 'remote-interface 'search-engines)
      '(("default" . "https://duckduckgo.com/?q=~a")
	("google" . "https://www.google.com/search?q=~a")
	("quickdocs" . "http://quickdocs.org/search?q=~a")
	("wiki" . "https://en.wikipedia.org/w/index.php?search=~a")
	("define" . "https://en.wiktionary.org/w/index.php?search=~a")
	("python3" . "https://docs.python.org/3/search.html?q=~a")))


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

(defun my-buffer-completion-fn ()
  (let ((buffers (alexandria:hash-table-values (buffers *interface*)))
        (active-buffer (active-buffer *interface*)))
    ;; Make the active buffer the first buffer in the list
    (when (not (equal (first buffers) active-buffer))
      (push active-buffer buffers))
    (lambda (input) (fuzzy-match input buffers))))

(define-command my-delete-buffer ()
  "Delete the buffer via minibuffer input. This is basically identical to the
original implementation, but uses a slightly modified completion function that
makes the active buffer the default deletion (i.e. how it is in Emacs)."
  (with-result (buffer (read-from-minibuffer
                        (make-instance 'minibuffer
                                       :input-prompt "Kill buffer:"
                                       :completion-function (my-buffer-completion-fn))))
    (rpc-buffer-delete *interface* buffer)))

(define-command delete-all-buffers ()
  "Delete ALL buffers, EXCEPT for the active buffer. I'd prefer to just delete
ALL of them (even the active buffer), but Next doesn't seem to like it when the
sole active buffer gets deleted."
  (with-result (y-n (read-from-minibuffer
		     (make-instance 'minibuffer
				    :input-prompt "Are you sure you want to kill all buffers (y or n)?")))
    (if (string-equal y-n "y")
	(let* ((active-buffer (active-buffer *interface*))
	       (b-list (alexandria:hash-table-values (buffers *interface*)))
	       (b-list-bg (remove active-buffer b-list)))
	  (mapcar (lambda (b) (rpc-buffer-delete *interface* b)) b-list-bg)))))

;; Make vi-normal the default keybinding scheme
(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; Edit keybindings a bit to make them more consistent with Emacs + evil-mode
(define-key :scheme :vi-normal
  "C-x k" #'my-delete-buffer)

(define-key :scheme :vi-insert
  "C-[" #'next/vi-mode:vi-normal-mode
  "C-x k" #'my-delete-buffer)

(define-key :mode 'minibuffer-mode
  "C-[" #'cancel-input)

;; Create ex abbreviations for cmds. I'm aware that my def-cmd-alias macro is a
;; dirty hack, and would appreciate advice for a better way to do this.
(defmacro def-cmd-alias (alias original)
  "This doesn't seem like the right way to do this"
  `(progn
     (define-command ,alias () (,original))
     (setf (fdefinition ',alias) #',original)))
;; Create the abreviations
(def-cmd-alias b switch-buffer)
(def-cmd-alias e set-url-new-buffer)

(define-command open-home-dir ()
  "Open my home directory in a browser window (useful for viewing html exports
e.g. from org-mode or an Rmarkdown doc)."
  (let ((url (concatenate 'string "file://"
			  (directory-namestring (truename "~/")))))
    (set-url url :buffer (active-buffer *interface*))))

;;
;; Commands for bookmark-db management. These commands all assume that the
;; ssh-key for origin/master has already been added to the ssh-agent, hence
;; obviating the need for any username/password entry!
;;
;; TODO: construct db-dir from bookmark-db-path global
;; TODO: allow user to specify remote and branch
;; TODO: display command output in minibuffer
;; TODO: password prompts
;; TODO: select-bookmark-db should glob for .db files
;;
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

(define-command select-bookmark-db ()
  "Prompt the user to choose which bookmark database file they would like to
use. If the file does not exist, create it, then set it as the active bookmark
database. A git add is then performed on the selected file."
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
	    (setf (bookmark-db-path *interface*) path)
	    ;; Add to git repo in case the file was just created
	    (print (bookmark-db-git-cmd `("add" ,(namestring path)))))))))

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
		      
;; Enable blocker mode in new browsers
(add-to-default-list 'blocker-mode 'buffer 'default-modes)
