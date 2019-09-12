(in-package :next)

;; Setup the sear engine "shortcut" IDs
(setf (get-default 'window 'search-engines)
      '(("default" . "https://www.google.com/search?q=~a")
	("duck" . "https://duckduckgo.com/?q=~a")
	("quickdocs" . "https://quickdocs.org/seawrch?q=~a")
	("wiki" . "https://enwikipedia.org/w/index.php?search=~a")))


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

;; Make vi-normal the default keybinding scheme
(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

;; Edit vi-normal a bit to make it more evil
(define-key :scheme :vi-normal
  "C-x k" (lambda ()
	    (with-result (y-n (read-from-minibuffer
			       (minibuffer *interface*)
			       :input-prompt "Delete current buffer (y or n)?:"))
	      (if (string-equal y-n "y")
		  (delete-current-buffer)))))

(define-key :scheme :vi-insert
  "C-[" #'next/vi-mode:vi-normal-mode
  "C-x k" #'delete-buffer)

;; Evil abbreviations for cmds
(defmacro def-cmd-alias (alias original)
  "This doesn't seem like the right way to do this"
  `(progn
     (define-command ,alias ()
       (,original))
     (setf (fdefinition ',alias) #',original)))

(def-cmd-alias b switch-buffer)
(def-cmd-alias e set-url-new-buffer)

;; WIP
;; (define-command execute-ex-command ()
;;   "Execute a command by name."
;;   (with-result (str (read-from-minibuffer
;; 		     (minibuffer *interface*)
;; 		     :input-prompt "Execute command:"
;; 		     :completion-function 'command-complete))
;;     (break)
;;     (let* ((cmd-seq (split-sequence:split-sequence #\Space str))
;; 	   (command (nth 0 cmd-seq)))
;;       (setf (access-time command) (get-internal-real-time))
;;       (run command))))

;; (define-key :scheme :vi-normal
;;   ";" #'execute-ex-command)

(define-command open-home-dir ()
  "Open my home directory in a browser window"
  (let ((url (concatenate 'string "file://"
			  (directory-namestring(truename "~/")))))
    (buffer-set-url :url url :buffer (active-buffer *interface*))))

(setf (get-default 'port 'path)
      "/home/tviti/common-lisp/next/ports/pyqt-webengine")
