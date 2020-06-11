;;
;; Commands for bookmark-db management. These commands all assume that the
;; ssh-key for origin/master has already been added to the ssh-agent, hence
;; obviating the need for any username/password entry!
;;
;; TODO: This should probably get broken out into a package...
(in-package :next)

(defun is-git-repo (path)
  "Returns path/.git if path contains a .git dir (and is hence assumed to be a
  git repo). Returns nil otherwise."
  (uiop:directory-exists-p (merge-pathnames path ".git")))

(defun bookmark-db-dir ()
  "Return path to the directory containing the active bookmark-db file."
  (uiop:pathname-directory-pathname (expand-path (bookmarks-path *browser*))))

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
  "Set the current active bookmark-db (i.e. (bookmark-db *browser*) to
path. If path lives in a git repo, call `git add path`."
  (setf (bookmarks-path *browser*) path)
  ;; Wipe the existing bookmark data. Use slot-value for this, since calling
  ;; (setf (bookmarks-data *browser*) ...) will also call
  ;; bookmarks-store-function (hence wiping the actual file contents!).
  (setf (slot-value *browser* 'next::bookmarks-data) nil)
  (next::restore-sexp-bookmarks)
  (if (is-git-repo (bookmark-db-dir))
      ;; Add to git repo in case the file was just created
      (bookmark-db-git-cmd `("add" ,(namestring path)))))

(defun query-file-path (start-dir &key (prompt-base "Path:"))
  "Drop into dir, and then start a minibuffer file query. Returns the path to
the selected file. This function is intended to be used in a call to
with-result."
  ;; TODO: Can this be done w/out mucking w/ a global var?
  ;; (setf next/file-manager-mode::*current-directory* start-dir)
  (uiop:with-current-directory (start-dir)
    (with-result (path (read-from-minibuffer
			(make-minibuffer
			 :default-modes '(next/file-manager-mode::file-manager-mode minibuffer-mode)
			 :input-prompt (format nil "~a~a" prompt-base (file-namestring start-dir))
			 :empty-complete-immediate t
			 :completion-function #'next/file-manager-mode::open-file-from-directory-completion-filter)))
      (expand-path path))))

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

