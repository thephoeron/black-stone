;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*-
;;;; file: packages.lisp

;;;; Copyright (c) 2013--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:black-stone
  (:use :cl)
  (:export #:*black-stone-app-name*
           #:*black-stone-version*
           #:*black-stone-version-codename*
           #:*black-stone-app-banner*
           ; qtypes.lisp
           #:qubit
           #:quantum-coupler
           #:quantum-system
           ; circuit.lisp
           #:set-address-components
           #:map-qubit-combinations
           #:get-addressed-amplitude
           #:set-addressed-amplitude
           #:matrix-multiply
           #:extract-column
           #:install-column
           #:apply-operator
           #:qc-output-probabilities
           #:multi-qsys-output-probabilities
           #:expected-oracles
           ; oracles.lisp
           #:binary-operator-matrix
           #:oracle
           #:limited-oracle
           ; generic.lisp
           #:qnot
           #:cnot
           #:srn
           #:nand
           #:hadamard
           #:u-theta
           #:cphase
           #:u2
           #:swap
           #:printamps
           #:insp
           ; internal.lisp
           #:end
           #:distance-to-next-unmatched-end
           #:without-if-branch
           #:without-else-branch
           #:force-to
           ; qclambda.lisp
           #:*post-oracle-measurements*
           #:run-qsys
           #:execute-quantum-program
           #:test-quantum-program))

;; see asdf system definition
(defvar black-stone:*black-stone-app-name*
  #.black-stone-asd:*black-stone-app-name*)

(defvar black-stone:*black-stone-version*
  #.black-stone-asd:*black-stone-version*)

(defvar black-stone:*black-stone-version-codename*
  #.black-stone-asd:*black-stone-version-codename*)

(defvar black-stone:*black-stone-app-banner*
  #.black-stone-asd:*black-stone-app-banner*)

;; Shadow relevant Common Lisp symbols with Quantum equivalents
(defpackage #:quantum-common-lisp
  (:nicknames #:qcl)
  (:use :cl :black-stone)
  (:export))

;; Set up a user namespace and shadow functionality from SBCL contrib libs
(defpackage #:quantum-common-lisp-user
  (:nicknames #:qcl-user)
  (:use :cl :cl-user :sb-ext :qcl :black-stone)
  (:export))

;; EOF
