;;
;;  Add cucumber feature-running support to rinari
;;
;;  This code targetted at Michael Klishin's cucumber-mode.el which can be found at:
;;
;;     http://github.com/michaelklishin/cucumber.el
;;
;; Author: Mike Dalessio (borrowing heavily from Eric Schulte's ruby-compilation.el)
;; Created: 2009-01-29

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Allow for execution of cucumber features dumping the results into a
;; compilation buffer. Gives the user the ability to jump to errors in
;; source code.
;;
;; The functions you will probably want to use are
;;
;; cucumber-compilation-this-buffer (C-x t)
;; cucumber-compilation-this-scenario (C-x C-t)
;;

;;; TODO:

;;
;; cucumber-compilation-error-regexp could be vastly improved.
;; Do we really need to strip ansi color codes out?
;;

(require 'ansi-color)
(require 'compile)
(require 'inf-ruby)
(require 'which-func)

;; This could be:
;;
;; "cucumber"
;; to run cucumber directly
;;
;; -- or --
;;
;; (list "rake" "cucumber")
;; to run cucumber via rake so the test DB setup is done (which runs
;; cucumber via bundle)
;;
;; -- or --
;;
;; (list "bundle" "exec" "cucumber")
;; to run cucumber via bundle but skip the test DB setup.
;;
;; Not sure how to do this but it would be nice if the three (or more)
;; options could be switched between easily
(defvar cucumber-compilation-executable (list "bundle" "exec" "cucumber")
  "The binary to run the feature scenarios. Override if you use JRuby etc.")

(defvar cucumber-compilation-error-regexp
  "^\\([[:space:]]*\\|.*\\[\\|[^\*].*at \\)\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)[]:)\n]?"
  "regular expression to match errors in cucumber process output")

(defvar cucumber-compilation-error-regexp-alist
  `((,cucumber-compilation-error-regexp 2 3))
  "a version of `compilation-error-regexp-alist' to be used in
  cucumber output (should be used with `make-local-variable')")

(defvar cucumber-compilation-clear-between t
  "Whether to clear the compilation output between runs.")

(defvar cucumber-compilation-reuse-buffers t
  "Whether to re-use the same comint buffer for focussed tests.")

(defadvice cucumber-compilation-do (around cucumber-compilation-do activate)
  "Set default directory to the root of the rails application
  before running cucumber processes."
  (let ((default-directory (or (rinari-root) default-directory)))
    ad-do-it
    (rinari-launch)))

(defun flatten (LIST)
  "flattens LIST"
  (cond
   ((atom LIST) (list LIST))
   ((null (cdr LIST)) (flatten (car LIST)))
   (t (append (flatten (car LIST)) (flatten (cdr LIST))))))

;;;###autoload
(defun cucumber-compilation-this-buffer ()
  "Run the current buffer's scenarios through cucumber."
  (interactive)
  (cucumber-compilation-run (buffer-file-name)))

;;;###autoload
(defun cucumber-compilation-this-scenario ()
  "Run the scenario at point through cucumber."
  (interactive)
  (let ((scenario-name (cucumber-compilation-this-scenario-name))
        (profile-name (cucumber-compilation-profile-name)))
    (pop-to-buffer (cucumber-compilation-do
                    (cucumber-compilation-this-test-buffer-name scenario-name)
                    (flatten
		     (list cucumber-compilation-executable
			   (buffer-file-name)
			   "-p" profile-name
			   "-s" scenario-name))))))

(defun cucumber-compilation-this-test-buffer-name (scenario-name)
  "The name of the buffer in which test-at-point will run."
  (interactive)
  (if cucumber-compilation-reuse-buffers
      (file-name-nondirectory (buffer-file-name))
    (format "cucumber: %s - %s"
            (file-name-nondirectory (buffer-file-name))
            scenario-name)))

;;;###autoload
(defun cucumber-compilation-run (cmd)
  "Run a cucumber process, dumping output to a compilation buffer."
  (interactive)
  (let* ((name (file-name-nondirectory (car (split-string cmd))))
         (profile-name (cucumber-compilation-profile-name))
         (cmdlist (flatten
		   (list cucumber-compilation-executable
			 "-p" profile-name
			 (expand-file-name cmd)))))
    (pop-to-buffer (cucumber-compilation-do name cmdlist))))


(defun cucumber-compilation-do (name cmdlist)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((comp-buffer-name (format "*%s*" name)))
    (unless (comint-check-proc comp-buffer-name)
      (let* ((buffer (apply 'make-comint name (car cmdlist) nil (cdr cmdlist)))
         (proc (get-buffer-process buffer)))
    (save-excursion
      (set-buffer buffer) ;; set buffer local variables and process ornaments
      (set-process-sentinel proc 'cucumber-compilation-sentinel)
          (set-process-filter proc 'cucumber-compilation-insertion-filter)
      (set (make-local-variable 'compilation-error-regexp-alist)
           cucumber-compilation-error-regexp-alist)
      (set (make-local-variable 'kill-buffer-hook)
           (lambda ()
         (let ((orphan-proc (get-buffer-process (buffer-name))))
           (if orphan-proc
               (kill-process orphan-proc)))))
      (compilation-minor-mode t)
      (cucumber-compilation-minor-mode t))))
    comp-buffer-name))

(defun cucumber-compilation-sentinel (proc msg)
  "Notify to changes in process state"
  (message "%s - %s" proc (replace-regexp-in-string "\n" "" msg)))

(defun cucumber-compilation-previous-error-group ()
  "Jump to the start of the previous error group in the current compilation buffer."
  (interactive)
  (compilation-previous-error 1)
  (while (string-match cucumber-compilation-error-regexp (thing-at-point 'line))
    (forward-line -1))
  (forward-line 1) (recenter))

(defun cucumber-compilation-next-error-group ()
  "Jump to the start of the previous error group in the current compilation buffer."
  (interactive)
  (while (string-match cucumber-compilation-error-regexp (thing-at-point 'line))
    (forward-line 1))
  (compilation-next-error 1) (recenter))

(defun cucumber-compilation-insertion-filter (proc string)
  "Insert text to buffer stripping ansi color codes"
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        (goto-char (process-mark proc))
        (insert (ansi-color-apply string))
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun cucumber-compilation-this-scenario-name ()
  "Which scenario are we currently in?"
  (save-excursion
    (search-backward-regexp "\\(?:Scenario:\\) \\(.*\\)")
    (match-string-no-properties 1)))

(defun cucumber-compilation-profile-name ()
  "Tries to find a comment in the file source indicating which cucumber profile to use.
   The comment will be of the format '# profile <profilename>'
   If not found, we'll default to 'default'."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#[[:space:]]*profile:?[[:space:]]+\\(.+\\)" nil t)
        (match-string-no-properties 1)
      "default")))

(defvar cucumber-compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"    'quit-window)
    (define-key map "p"    'previous-error-no-select)
    (define-key map "n"    'next-error-no-select)
    (define-key map "\M-p" 'cucumber-compilation-previous-error-group)
    (define-key map "\M-n" 'cucumber-compilation-next-error-group)
    (define-key map (kbd "C-c C-c") 'comint-interrupt-subjob)
    map)
  "Key map for Cucumber Compilation minor mode.")

(define-minor-mode cucumber-compilation-minor-mode
  "Enable Cucumber Compilation minor mode providing some key-bindings
  for navigating cucumber compilation buffers."
  nil
  " cucumber:comp"
  cucumber-compilation-minor-mode-map
  (when cucumber-compilation-clear-between
    (delete-region (point-min) (point-max))))


;; So we can invoke it easily.
(add-hook 'feature-mode-hook
          '(lambda ()
             (define-key feature-mode-map (kbd "C-x t") 'cucumber-compilation-this-buffer)
             (define-key feature-mode-map (kbd "C-x C-t") 'cucumber-compilation-this-scenario)

	     ;; ensure font lock font-stripping doesn't kill our explicit ansi colorization
	     (setq font-lock-unfontify-region-function 'ansi-color-unfontify-region)
             ))

(provide 'cucumber-mode-compilation)
