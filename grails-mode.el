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
  '(("\M-+gd" . grails-find-domain) ; TODO
    ("\M-+gs" . grails-find-service) ; TODO
    ("\M-+gc" . grails-find-controller) ; TODO
    ([C-f6] . grails-find-domain-for-current)
    ([C-f7] . grails-find-service-for-current)
    ([C-f8] . grails-find-controller-for-current)
    ;; C-f5 - consider something like grails-find-view-for-controller-action-at-point
    ([C-f10] . grails-find-unit-test-for-current)
    ([C-f11] . grails-run-last-unit-test)
    ([C-f12] . grails-run-test-unit-for-current))
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

;;;###autoload

(defvar *grails-last-unit-test* nil)

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
  (setq *grails-last-unit-test* (buffer-name))
  (grails-run-test-unit-for (buffer-name)))

(defun grails-run-last-unit-test nil
  (interactive)
  (when *grails-last-unit-test*
    (grails-run-test-unit-for *grails-last-unit-test*)))

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
    (let ((file-name (substring file-name 0
                                (string-match "Tests?"
                                              file-name)))
          (buf (generate-new-buffer "*grails-unit-test*")))
      (pop-to-buffer buf)
      (local-set-key "q" 'kill-buffer-and-window)
      (cd (project-search-paths-get-default (project-current)))
      (let ((proc (start-process-shell-command "grails-unit-test"
                                               buf
                                               "grails" "test-app" file-name "-unit")))
        (set-process-filter proc (lambda (proc str) (grails-unit-test-filter proc str)))
        (process-kill-without-query proc)))))

(defun grails-unit-test-filter (proc str)
  (save-excursion
    (when (string-match "Tests FAILED - view reports" str)
      (browse-url (project-append-to-path
                   (project-search-paths-get-default (project-current))
                   '("target" "test-reports"
                     "html" "all-tests.html"))))
    (when (string-match "exited abnormally with code 255" str)
      (message (concat "Error: You're running 'grails test-app' from the wrong directory")))
    (set-buffer (process-buffer proc))
    (goto-char (point-max))
    (insert (concat "\n" str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'grails-mode)
