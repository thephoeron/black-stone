;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: black-stone.asd

;;;; Copyright (c) 2013 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage black-stone-asd
    (:use :cl :asdf)
    (:export #:*black-stone-version*))

(in-package :black-stone-asd)

(defparameter *black-stone-version* "0.0.1")

(defsystem black-stone
  :version #.*black-stone-version*
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "MIT"
  :description "Specification and Implementation of Quantum Common Lisp, for gate-model quantum computers."
  :serial t
  :depends-on (:gsll
               :cl-ppcre
               :cl-fad
               :let-over-lambda)
  :components ((:file "packages")
               (:module "lib"
                :serial t
                :components ((:file "arithmetic")
                             (:file "decompose")
                             (:file "dynamic-lifting")
                             (:file "gate-decomposition")))
               (:module "qclambda"
               	:serial t
               	:components ((:file "qclambda")))
               (:file "black-stone")))

;; EOF