;; Add spork support to ruby-compilation-rspec

;;
;; This module provides interactive functions to start, kill and restart the spork server.
;; In addition, it can hook necessary functions to allow rspec to use spork.
;;

(defvar spork-server-autostart t
  "Set this variable to nil if you don't want spork to be executed automatically")

(defconst spork-server-buffer-name "spork-server"
  "The name of the buffer that runs spork server.
This is the name without the enclosing *s")

(defconst spork-server-buffer-real-name (concat "*" spork-server-buffer-name "*")
  "The visible name of the buffer that runs spork server")

(defun have-spork-p ()
  (executable-find "spork"))

(defun spork-inactive-p ()
  (let* ((spork-buffer (get-buffer spork-server-buffer-real-name))
         (spork-process (and spork-buffer
                             (get-buffer-process spork-buffer))))
    (null spork-process)))

(defun wait-spork-start-or-kill ()
  "Wait for the spork server to become active.
This function also detects if spork needs to be bootstrapped. If the spork
server is started successfully, it reports the port spork listens to."
  (let* ((spork-buffer (get-buffer spork-server-buffer-real-name))
         (proc (get-buffer-process spork-buffer))
         (old-filter (process-filter proc))
         (old-sentinel (process-sentinel proc))
         (output "") spork-terminated spork-port)
    (set-process-filter proc (lambda (proc string)
                               (setf output (concat output string))
                                ;; Chain to the old filter
                               (if old-filter
                                   (funcall old-filter proc string))))
    (set-process-sentinel proc (lambda (proc state)
                                 (unless (eq 'run
                                             (process-status proc))
                                   (setf spork-terminated t))))
    (save-match-data
      (while (cond
              ;; Make sure this project can handle spork
              ((string-match "has not been bootstrapped" output)
               (kill-spork)
               (message "Failed to start spork server. Did you run \"spork --bootstrap\"?")
               nil)                        ; stop loop
              ;; Wait for the server to start - and report its port. I am putting this after the
              ;; bootstrap check, because the message can also appear even when project is not
              ;; bootstrapped for spork.
              ((string-match "listening on \\([0-9]+\\)" output)
               (setq spork-port (match-string 1 output))
               (message (concat "Spork serving at port " spork-port))
               nil)                        ; stop loop
              ;; See if process died
              (spork-terminated
               ;; Put appropriate message if we know why
               (if (string-match "spork is not part of the bundle" output)
                   (message "Failed to start spork server. Did you add spork to project Gemfile?")
                 (message "Spork server died unexpectedly."))
               nil)                        ; stop loop
              ;; If we need to loop, wait for more output
              (t (accept-process-output proc)
                 t))))                 ;continue looping
    (set-process-sentinel proc old-sentinel)
    (set-process-filter proc old-filter)
    spork-port))

(defun start-spork ()
  "Start a spork server, and return the port it is listening as a string.
If the projet is not configured to run spork, the server is killed and nil is returned."
  (interactive)
  (when (and (spork-inactive-p)
             (have-spork-p))
    (message "Starting spork server...")
    (ruby-compilation-do spork-server-buffer-name
                         (list "bundle" "exec" "spork"))
    (wait-spork-start-or-kill)))

(defun kill-spork (&optional buffer-name)
  "If a spork server is running, this function kills both the process and the buffer."
  (interactive)
  (let* ((spork-buffer (get-buffer spork-server-buffer-real-name))
         (proc (get-buffer-process spork-buffer)))
    (when proc
      (delete-process proc))
    (when spork-buffer
      (kill-buffer spork-buffer))))

(defun restart-spork ()
  "Restart a running spork server.
If the server is not running, this function only starts the spork server."
  (interactive)
  (if (not (spork-inactive-p))
      (kill-spork))
  (start-spork))

(defmacro with-spork-if (condition action &rest body)
  "A macro to simplify spork server integration.
CONDITION is evaluated to see if spork is required. If it is not nil, presence
of spork server is checked. If it is present, ACTION is executed before BODY.

If spork-server-autostart is not nil, then the macro
attempts to start spork-server automatically."
  (declare (indent 2))
  `(when ,condition
     (if (and (spork-inactive-p)
              spork-server-autostart)
         (start-spork))
     (if (not (spork-inactive-p))
         ,action)
     ,@body))

(provide 'spork-server)
