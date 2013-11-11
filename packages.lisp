;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: packages.lisp

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:black-stone
    (:nicknames #:qclambda)
    (:use :cl)
    (:export #:*black-stone-version*))

;; see asdf system definition
(defvar black-stone:*black-stone-version*
  #.black-stone-asd::*black-stone-version*)

;; EOF
