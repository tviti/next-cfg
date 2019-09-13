(in-package :next)

;; Setup the search engine "shortcut" IDs
(setf (get-default 'window 'search-engines)
      '(("default" . "https://www.google.com/search?q=~a")
	("duck" . "https://duckduckgo.com/?q=~a")
	("quickdocs" . "https://quickdocs.org/search?q=~a")
	("wiki" . "https://en.wikipedia.org/w/index.php?search=~a")
	("define" . "https://en.wiktionary.org/w/index.php?search=~a"))


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

(define-command my-delete-buffer ()
  "Just ask for confirmation, since it's too easy to accidentally kill a buffer
with a keybinding"
  (with-result (y-n (read-from-minibuffer
		     (make-instance 'minibuffer
				    :input-prompt "Delete current buffer (y or n)?:")))
    (if (string-equal y-n "y")
	(delete-current-buffer))))

;; Make vi-normal the default keybinding scheme
(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; Edit vi-normal a bit to make it more evil
(define-key :scheme :vi-normal
  "C-x k" #'my-delete-buffer)

(define-key :scheme :vi-insert
  "C-[" #'next/vi-mode:vi-normal-mode
  "C-x k" #'delete-buffer)

(define-key :mode 'minibuffer-mode
  "C-[" #'cancel-input)

;; Evil abbreviations for cmds
(defmacro def-cmd-alias (alias original)
  "This doesn't seem like the right way to do this"
  `(progn
     (define-command ,alias ()
       (,original))
     (setf (fdefinition ',alias) #',original)))

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

(def-cmd-alias b switch-buffer)
(def-cmd-alias e my-set-url-new-buffer)

(define-command open-home-dir ()
  "Open my home directory in a browser window"
  (let ((url (concatenate 'string "file://"
			  (directory-namestring(truename "~/")))))
    (buffer-set-url :url url :buffer (active-buffer *interface*))))


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
