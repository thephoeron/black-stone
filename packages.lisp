;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: packages.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:black-stone
  (:use :cl)
  (:export #:*black-stone-app-name*
           #:*black-stone-version*
           #:*black-stone-version-codename*
           #:*black-stone-app-banner*))

;; see asdf system definition
(defvar black-stone:*black-stone-app-name*
  #.black-stone-asd:*black-stone-app-name*)

(defvar black-stone:*black-stone-version*
  #.black-stone-asd:*black-stone-version*)

(defvar black-stone:*black-stone-version-codename*
  #.black-stone-asd:*black-stone-version-codename*)

(defvar black-stone:*black-stone-app-banner*
  #.black-stone-asd:*black-stone-app-banner*)

(defpackage #:quantum-common-lisp
  (:nicknames #:qcl)
  (:use :cl :black-stone)
  (:export))

(defpackage #:quantum-common-lisp-user
  (:nicknames #:qcl-user)
  (:use :cl :cl-user :sb-ext :qcl :black-stone)
  (:export))

;; EOF
