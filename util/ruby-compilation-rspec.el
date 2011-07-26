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
                                   (if (some (lambda (x)
                                               (file-executable-p (expand-file-name "rspec" x)))
                                             exec-path)
                                       "rspec"
                                   "spec"))
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

(provide 'ruby-compilation-rspec)
