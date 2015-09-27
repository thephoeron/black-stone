;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*-
;;;; file: black-stone.asd

;;;; Copyright (c) 2013--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage black-stone-asd
    (:use :cl :asdf)
    (:export #:*black-stone-app-name*
             #:*black-stone-version*
             #:*black-stone-version-codename*
             #:*black-stone-app-banner*))

(in-package :black-stone-asd)

(defparameter *black-stone-app-name* "black-stone")
(defparameter *black-stone-version* "0.0.1")
(defparameter *black-stone-version-codename* "Pebble")
(defparameter *black-stone-app-banner*
    (format nil "an implementation of Quantum Common Lisp.~
                ~%More information is available at <http://github.com/thephoeron/black-stone/>.~
                ~%Powered by ~A v~A <http://www.sbcl.org/>.~
        
                ~%~%BLACK-STONE is free software, provided as-is, with absolutely no warranty.~
                ~%Copyright (c) 2013--2015 \"the Phoeron\" Colin J.E. Lupton.  Provided under the~
                ~%MIT License.  See LICENSE in the official repo for more information."
            (lisp-implementation-type)
            (lisp-implementation-version)))

(defsystem black-stone
  :version #.*black-stone-version*
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "MIT"
  :description "BLACK-STONE: Specification and Implementation of Quantum Common Lisp, for classical interface gate-model quantum computers."
  :serial t
  :depends-on (:gsll
               :cl-ppcre
               :cl-fad
               :let-over-lambda
               :cl-isaac)
  :components ((:file "packages")
               (:module "qclambda"
                :serial t
                :components ((:file "qtypes")
                             (:file "circuit")
                             (:file "oracles")
                             (:file "classical")
                             (:file "control")
                             (:file "generic")
                             (:file "internal")
                             (:file "labels")
                             (:file "printing")
                             (:file "qdata")
                             (:file "qobjects")
                             (:file "circ-lifting")
                             (:file "transformer")
                             (:file "qclambda")))
               (:module "lib"
                :serial t
                :components ((:file "arithmetic")
                             (:file "decompose")
                             (:file "decompose-clifford")
                             (:file "decompose-gatebase")
                             (:file "decompose-legacy")
                             (:file "dynamic-lifting")
                             (:file "gate-decomposition")
                             (:file "fp-real")
                             (:file "qft")
                             (:file "qft-add")
                             (:file "q-ram")
                             (:file "ascii-parser")
                             (:file "qureg")
                             (:file "simulation")
                             (:file "simulation-classical")
                             (:file "simulation-clifford")
                             (:file "simulation-quantum")
                             (:file "synthesis")
                             (:file "unboxing")
                             (:file "classical-optimization")))
               (:file "black-stone")))

;; EOF
