(cd (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path "../")
(add-to-list 'load-path "../util/")
(add-to-list 'load-path "../util/jump/")
(add-to-list 'load-path "./")
(require 'rinari)
(require 'elunit)

;; testing rinari-movement
(defsuite rinari-suite nil
  :setup-hook (lambda () )
  :teardown-hook (lambda ()
		   (switch-to-buffer "*Messages*")
		   (message "test completed")))

(deftest rinari-move-test rinari-suite
  ;; test moving from everywhere to everywhere
  (save-excursion
    (let ((max-lisp-eval-depth 2000)
	  (rails-root (format "%s" (concat (file-name-directory
					    (or load-file-name buffer-file-name))
					   "rails-app/"))))
      (cl-flet ((here-to-here (start func end)
			   (let ((default-directory rails-root))
			     ;; go to start
			     (find-file (car start))
			     (goto-char (cdr start))
			     (rinari-launch)
			     ;; run movement function
			     (eval (list func))
			     ;; assert
			     (assert-equal (file-name-nondirectory (car end))
					   (file-name-nondirectory (buffer-file-name)))
			     (assert-equal (cdr end) (point))
			     ;; clean up
			     (kill-buffer (file-name-nondirectory (car end)))
			     (kill-buffer (file-name-nondirectory (car start))))))
	(here-to-here '("test/unit/example_test.rb" . 153)
		      'rinari-find-model
		      '("app/models/example.rb" . 37))
	(here-to-here '("app/controllers/units_controller.rb" . 52)
		      'rinari-find-test
		      '("test/functional/units_controller_test.rb" . 150))
	(here-to-here '("app/controllers/units_controller.rb" . 61)
		      'rinari-find-view
		      '("app/views/units/fall.html.erb" . 1))))))

(deftest rinari-console-test rinari-suite
  ;; testing ability to launch console, server, and a test
  (save-excursion
    (let ((default-directory (format "%s" (concat (file-name-directory
					    (or load-file-name buffer-file-name))
					   "rails-app/"))))
      (rinari-console)
      (assert-equal (buffer-name) "*ruby*")
      (assert-that (get-buffer-process (current-buffer)))
      (kill-buffer (current-buffer)))))

(deftest rinari-server-test rinari-suite
  ;; testing ability to launch console, server, and a test
  (save-excursion
    (let ((default-directory (format "%s" (concat (file-name-directory
					    (or load-file-name buffer-file-name))
					   "rails-app/"))))
      (rinari-web-server)
      (assert-equal (buffer-name) "*server*")
      (assert-that (get-buffer-process (current-buffer)))
      (kill-buffer (current-buffer)))))

(deftest rinari-test-test rinari-suite
  ;; testing ability to launch console, server, and a test
  (save-excursion
    (let ((default-directory (format "%s" (concat (file-name-directory
					    (or load-file-name buffer-file-name))
					   "rails-app/"))))
      (find-file "app/controllers/units_controller.rb")
      (rinari-test)
      (assert-equal (buffer-name) "*units_controller_test.rb*")
      (assert-equal major-mode 'comint-mode)
      (kill-buffer "*units_controller_test.rb*")
      (kill-buffer "units_controller_test.rb")
      (kill-buffer "units_controller.rb"))))

;; TODO: for some reason this doesn't work inside the test body
(add-hook 'rinari-minor-mode-hook '(lambda () (setq hooks-ran t)))

(deftest rinari-hooks rinari-suite
  (save-excursion
    (unwind-protect ;; like ruby's ensure
        (progn
          (let ((default-directory (format "%s" (concat (file-name-directory
                                                         (or load-file-name buffer-file-name))
                                                        "rails-app/"))))
            (find-file "app/controllers/units_controller.rb")
            (assert-that hooks-ran)))
      (kill-buffer "units_controller.rb")
      (setq hooks-ran nil))))

;; (elunit-run-suite (elunit-get-suite 'rinari-movement-suite))
(elunit "rinari-suite")
