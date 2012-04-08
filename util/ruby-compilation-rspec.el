;; Add rspec support to ruby-compilation

;;
;;  rspec will run with the -l <line_number> option, so that we can
;;  run multiple tests in a context if that's where the point happens
;;  to be.
;;

(require 'ruby-compilation)

(add-hook 'ruby-mode-hook (lambda ()
                            (when (and (not (null buffer-file-name)) (string-match "_spec.rb$" buffer-file-name))
                              (set (make-local-variable 'ruby-compilation-executable)
                                   (file-name-nondirectory
                                    (or (executable-find "rspec")
                                        (executable-find "spec"))))
                              (set (make-local-variable 'ruby-compilation-test-name-flag)
                                   "-l"))))

(fset 'ruby-compilation-this-test-name-old
      (symbol-function 'ruby-compilation-this-test-name))

(defun ruby-compilation-this-test-name ()
  (if (string-match "^r?spec$" ruby-compilation-executable)
      (ruby-compilation-this-spec-name)
    (ruby-compilation-this-test-name-old)))

(defun ruby-compilation-this-spec-name ()
  "Return the line number at point"
  (number-to-string (line-number-at-pos)))

(defmacro with-spork-if-rspec (&rest body)
  `(let ((ruby-compilation-executable-args ruby-compilation-executable-args))
     (with-spork-if
         (string-equal ruby-compilation-executable "rspec")
         (setf ruby-compilation-executable-args
               (cons "--drb"
                     ruby-compilation-executable-args))
       ,@body)))

(defun ruby-compilation-rspec-use-spork ()
  (interactive)
  (defadvice ruby-compilation-this-buffer (around
                                           ruby-compilation-this-buffer-spork
                                           activate)
    (with-spork-if-rspec ad-do-it))
  (defadvice ruby-compilation-this-test (around
                                         ruby-compilation-this-test-spork
                                         activate)
    (with-spork-if-rspec ad-do-it)))

(provide 'ruby-compilation-rspec)
