;;; grails-mode.el
;;
;; Author: Benjamin Cluff
;; Created: 02-Feb-2010
;;
;; Synopsis:
;;   Help when working with grails apps.
;;   * Finding files is greatly simplified (see key bindings)
;;
;; Usage:
;;     (when (require 'project-mode)
;;       (require 'grails-mode)
;;       (let ((project (project-create "foo")))
;;         (project-search-paths-add project "/usr/home/me/proj/foo")
;;         (project-tags-form-set project grails-default-project-mode-tags-form)
;;         (project-search-exclusion-regexes-set project project-search-exclusion-regexes-default)
;;         (project-fuzzy-match-tolerance-set project project-fuzzy-match-tolerance-default)))


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
  '(("\C-cgd" . grails-find-domain) ; TODO
    ("\C-cgs" . grails-find-service) ; TODO
    ("\C-cgc" . grails-find-controller) ; TODO
    ("\C-c\C-gd" . grails-find-domain-for-current)
    ("\C-c\C-gs" . grails-find-service-for-current)
    ("\C-c\C-gc" . grails-find-controller-for-current))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-interactive functions

(defun grails-find-domain-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((file-name (substring file-name 0
                                (string-match "\\(Service\\|Controller\\)"
                                              file-name))))
      (let ((file-name (concat file-name ext)))
        (message (concat "Searching for: " file-name))
        (project-im-feeling-lucky-regex file-name)))))

(defun grails-find-service-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((file-name (substring file-name 0
                                (string-match "\\(Service\\|Controller\\)"
                                              file-name))))
      (let ((file-name (concat file-name "Service" ext)))
        (message (concat "Searching for: " file-name))
        (project-im-feeling-lucky-regex file-name)))))

(defun grails-find-controller-for (file-arg)
  (let ((file-name (project-file-strip-extension file-arg))
        (ext (or (project-file-get-extension file-arg) ".groovy")))
    (let ((file-name (substring file-name 0
                                (string-match "Service"
                                              file-name))))
      (let ((file-name (concat file-name "Controller" ext)))
        (message (concat "Searching for: " file-name))
        (project-im-feeling-lucky-regex file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'grails-mode)
