;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: make.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

;; Deprecate make.lisp
;; -- move build to black-stone.lisp as part of application

(require :asdf)
 
;; We're setting up a clean environment, so we have to specifically tell SBCL where
;; Quicklisp is installed
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Define current working directory
(defvar *this-dir* (pathname (directory-namestring #.(or *compile-file-truename* *load-truename*))))
(defvar *log-dir* (merge-pathnames "logs/" *this-dir*))
 
(ql:quickload '("com.dvlsoft.clon" "black-stone"))


;; Set up some environment variables
(defvar *app-name* #.black-stone:*black-stone-app-name*)
(defvar *app-title* (string-upcase *app-name*))
(defvar *app-version* #.black-stone:*black-stone-version*)
(defvar *app-codename* #.black-stone:*black-stone-version-codename*)
(defvar *app-banner* #.black-stone:*black-stone-app-banner*)
 
;; We need to shadow SBCL's (exit) over CLON's, for sanity
(shadowing-import '(exit) 'cl-user)
(use-package :com.dvlsoft.clon)
(nickname-package)

;; Use ``(defsynopsis)'' to define the command line options for your program and order of
;; elements in your help string
(defsynopsis ()
  (text :contents (format nil "~A: Quantum Common Lisp REPL." *app-title*))
  (group (:header "REPL Options:")
  	(flag :short-name "q" :long-name "quiet-start"
  		  :description "Suppress welcome banner and go straight to the REPL prompt."))
  (group (:header "Immediate exit options:")
    (flag :short-name "h" :long-name "help"
      :description "Print this help and exit.")
    (flag :short-name "v" :long-name "version"
    :description "Print version number and exit.")))

(defun app-version ()
  (format t "~%~A: v~A \"~A\"~%" *app-title* *app-version* *app-codename*))

(defun signal-handler (signal)
  (format t "~A received~%" signal)
  (clon::exit))

; ``(main)'' function for our standalone console program.
(defun main ()
  "Entry point for BLACK-STONE."
  (make-context)
  ;; explicitly launch the help menu if it's called from command-line
  (when (getopt :short-name "h")
    (help)
    (terpri)
    (clon::exit)) ;; since we shadowed CLON's (exit), we need to refer to it explicitly
  (when (getopt :short-name "v")
    (app-version)
    (terpri)
    (clon::exit))
  (if (getopt :short-name "q")
  	  (app-version)
  	  (format t "~%This is ~A v~A \"~A\", ~A~%" *app-title* *app-version* *app-codename* *app-banner*))
  ;; customize prompt and enter BLACK-STONE REPL
  (setf sb-int:*repl-prompt-fun*
        (lambda (stream)
          (format stream "~%~C[31m#[~A::~A]>~C[0m " #\Escape *app-title* (or (first (package-nicknames *package*))(package-name *package*)) #\Escape)))
  (in-package :qcl-user)
  (sb-impl::toplevel-repl nil)
  ;(terpri)
  ;(clon::exit)
  )
 
;; Dump the Lisp image.
(dump #.*app-name* main)

;; EOF
