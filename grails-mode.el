;;; grails-mode.el
;;
;; Author: Benjamin Cluff
;; Created: 02-Feb-2010
;;
;; Synopsis:
;;   Help when working with grails apps.
;;   * Finding files is greatly simplified (see key bindings)

(require 'project-mode)

(defgroup grails nil
  "Grails mode helps when working with grails apps."
  :prefix "grails-"
  :group 'programming)

(define-minor-mode grails-mode
  "Toggle grails mode.
   With no argument, this command toggles the mode.
   Non-null prefix argument turns on the mode.
   Null prefix argument turns off the mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Grails"
  ;; This mode is best as a global minor mode
  :global t
  ;; The minor mode bindings.
  :keymap
  '(("\M-+gd" . grails-find-domain)
    ("\M-+gs" . grails-find-service)
    ("\M-+gc" . grails-find-controller)
    ([C-f6] . grails-find-domain-for-current)
    ([C-f7] . grails-find-service-for-current)
    ([C-f8] . grails-find-controller-for-current)
    ([C-f9] . grails-find-view-for-controller-action)
    ([C-f10] . grails-find-unit-test-for-current)
    ([C-f11] . grails-run-test-unit-for-current)
    ([C-f12] . grails-run-last-unit-test))
  :group 'grails)

(defcustom grails-default-project-mode-tags-form
  '(;; File name pattern
    "\\.groovy$"
    ;; Regexes that are run for the file name match
    (;; classes
     "^class\s+\\w+"
     ;; members
     "^\\(\s\\{4\\}\\|\t\\)\\w+\s+\\w+[^\r\n$({=]+[({=]?"
     ;; closures defined in method bodies
     "^\\(\s\\{8\\}\\|\t\t\\)\\w+\\.\\w+\s*=\s*{"
     ))
  "Used to generate tags for groovy files"
  :group 'grails)

(defface grails-unit-test-failed
  '((default (:foreground "white" :background "red4" :stipple nil)))
  "Used for when a unit test fails.")

(defface grails-unit-test-passed
  '((default (:foreground "white" :background "green4" :stipple nil)))
  "Used for when a unit test passes.")

;;;###autoload

(defvar *grails-last-unit-test-name* nil) ; TODO: This should be per project

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive commands

(defun grails-find-domain (file-name)
  (interactive "MSearch for Domain: ")
  (grails-find-domain-for file-name))

(defun grails-find-service (file-name)
  (interactive "MSearch for Service: ")
  (grails-find-service-for file-name))

(defun grails-find-controller (file-name)
  (interactive "MSearch for Controller: ")
  (grails-find-controller-for file-name))

(defun grails-find-domain-for-current nil
  (interactive)
  (grails-find-domain-for (buffer-name)))

(defun grails-find-service-for-current nil
  (interactive)
  (grails-find-service-for (buffer-name)))

(defun grails-find-controller-for-current nil
  (interactive)
  (grails-find-controller-for (buffer-name)))

(defun grails-find-unit-test-for-current nil
  (interactive)
  (grails-find-unit-test-for (buffer-name)))

(defun grails-run-test-unit-for-current nil
  (interactive)
  (grails-run-test-unit-for (buffer-name)))

(defun grails-run-last-unit-test nil
  (interactive)
  (when *grails-last-unit-test-name*
    (grails-run-test-unit-for *grails-last-unit-test-name*)))

(defun grails-find-view-for-controller-action nil
  (interactive)
  (when (string-match "Controller\\." (buffer-name))
    (grails-find-view-for-controller-action-at-point (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-interactive functions

(defun grails-find-domain-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((file-name (substring file-name 0
                                (string-match "\\(Service\\|Controller\\|Tests?\\)"
                                              file-name))))
      (let ((file-name (concat file-name ext)))
        (message (concat "Searching for: " file-name))
        (project-im-feeling-lucky-regex file-name)))))

(defun grails-find-service-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((file-name (substring file-name 0
                                (string-match "\\(Service\\|Controller\\|Tests?\\)"
                                              file-name))))
      (let ((file-name (concat file-name "Service" ext)))
        (message (concat "Searching for: " file-name))
        (project-im-feeling-lucky-regex file-name)))))

(defun grails-find-controller-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((file-name (substring file-name 0
                                (string-match "\\(Service\\|Controller\\|Tests?\\)"
                                              file-name))))
      (let ((file-name (concat file-name "Controller" ext)))
        (message (concat "Searching for: " file-name))
        (project-im-feeling-lucky-regex file-name)))))

(defun grails-find-unit-test-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((file-name (concat file-name "Tests?" ext)))
      (message (concat "Searching for: " file-name))
      (project-im-feeling-lucky-regex file-name))))

(defun grails-run-test-unit-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((test-name (substring file-name 0
                                (string-match "Tests?"
                                              file-name)))
          (buf (generate-new-buffer "*grails-unit-test*")))
      (setq *grails-last-unit-test-name* test-name)
      (pop-to-buffer buf)
      (local-set-key "q" 'kill-this-buffer)
      (local-set-key "Q" 'kill-buffer-and-window)
      (cd (project-default-directory (project-current)))
      (let ((proc (start-process-shell-command "grails-unit-test"
                                               buf
                                               "grails" "test-app" test-name "-unit")))
        (set-process-filter proc (lambda (proc str) (grails-unit-test-filter proc str)))
        (process-kill-without-query proc)))))

(defun grails-unit-test-filter (proc test-output)
  (save-excursion
    (let ((buf (process-buffer proc)))
      (set-buffer buf)
      (grails-unit-test-filter-insert-unit-test-results)
      (grails-unit-test-filter-buttonize-groovyc-errors)
      (when (string-match "exited abnormally with code 255" test-output)
        (message (concat "Error: You're probably running 'grails test-app' from the wrong directory")))
      (goto-char (point-max))
      (insert (concat "\n" test-output)))))

(defun grails-unit-test-filter-buttonize-groovyc-errors nil
  "Meant to be called from `GRAILS-UNIT-TEST-FILTER'"
  (when (string-match "\\[groovyc\\][^\r\n]+startup failed, +\\([^\r\n]+\\): +\\([0-9]+\\):" test-output)
    (beginning-of-buffer)
    (insert-button (concat (match-string-no-properties 1 test-output)
                           ":"
                           (match-string-no-properties 2 test-output))
                   'action 'project-file-button-handler)))

(defun grails-unit-test-filter-insert-unit-test-results nil
  "Meant to be called from `GRAILS-UNIT-TEST-FILTER'"
  (when (string-match "Tests \\(FAILED\\|PASSED\\) - view reports" test-output)
    (let ((passed-p (equal "PASSED" (match-string-no-properties 1 test-output))))
      (beginning-of-buffer)
      (insert "\n-----\n")
      (if passed-p
          (insert (propertize "PASSED" 'face 'grails-unit-test-passed))
        (insert (propertize "FAILED" 'face 'grails-unit-test-failed)))
      (insert "\n")
      (insert "Some output files:\n")
      (insert-button "all-tests.html" 'action (lambda (but)
                                                (browse-url (project-append-to-path
                                                             (grails-tests-html-output-dir)
                                                             "all-tests.html"))))
      (dolist (file (grails-tests-list-of-plain-output-files))
        (insert "\n")
        (insert-button file 'action (lambda (but)
                                      (find-file (project-append-to-path
                                                  (grails-tests-plain-output-dir) (button-label but))))))
      (insert "\n-----\n"))))

(defun grails-tests-html-output-dir nil
  (project-append-to-path
   (project-default-directory (project-current))
   '("target" "test-reports"
     "html")))

(defun grails-tests-plain-output-dir nil
  (project-append-to-path
   (project-default-directory (project-current))
   '("target" "test-reports"
     "plain")))

(defun grails-tests-list-of-plain-output-files nil
  (let (result)
    (dolist (file (directory-files (grails-tests-plain-output-dir)))
      (when (not (string-match "^\\." file))
        (setq result (append result (list file)))))
    result))

(defun grails-find-view-for-controller-action-at-point (buf)
  (save-excursion
    (set-buffer buf)
    (when (re-search-backward "def[ 	\n\r]+\\([a-z0-9]+\\)[ 	\n\r]*=[ 	\n\r]*{" (point-min) t)
      (let ((action-name (match-string-no-properties 1))
            (view-dir (downcase (substring (buffer-name) 0 (string-match "Controller\\." (buffer-name))))))
        (let ((file-path (project-append-to-path
                          (project-default-directory (project-current))
                          (list "grails-app" "views"
                                view-dir
                                (concat action-name ".gsp")))))
          (if (file-readable-p file-path)
              (find-file file-path)
            (message (concat "Grails view '" view-dir "/" action-name "' doesn't exist."))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'grails-mode)
