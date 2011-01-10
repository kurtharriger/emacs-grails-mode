Forked project from http://emacs-grails-mode.googlecode.com/svn/trunk

Extends project-mode to add grails specific functionality such as fast switching between related files and unit testing. 

Download

    * Easy switching between services, domains, controllers, views and tests.
    * Quickly and easily run unit/integration tests
    * Inherits all the functionality of project-mode 

Installation:

    * Put the source code in site-lisp or somewhere else in your LOAD-PATH.
    * Add this to .emacs (may be in AppData/Roaming for Windows users).

      (require 'grails-mode)
      (setq grails-mode t)
      (setq project-mode t)
      (add-to-list 'auto-mode-alist '("\.gsp$" . nxml-mode)) ; Use whatever mode you want for views.
      (project-load-all) ; Loads all saved projects. Recommended, but not required.

    * Dependencies:
          o project-mode is the only dependency. 
    * Suggested groovy-mode:
          o I use groovy-mode.el by Jeremy Rayner. There is also one by Russel Winder. 
    * To evaluate groovy within Emacs I use something I created called groovy-eval-mode 

To see what you can do within grails-mode try:

    * Looking at the "Grails" menu.
    * M-+ C-h
    * Check out the top of the source code grails-mode.el where it lists all the bindings. 

Grails-mode inherits lots of functionality from project-mode

