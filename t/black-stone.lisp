;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE-TEST; Base: 10 -*-
;;;; file: t/black-stone.lisp

;;;; Copyright (c) 2013--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage black-stone-test
  (:use cl black-stone prove))

(in-package :black-stone-test)

;; NOTE: To run this test file, execute `(asdf:test-system :black-stone)' in your Lisp.

(plan 4)

(deftest sanity-check
  (pass "PROVE is loaded and ready to go.")
  (ok (= 1 1)
      "Numeric equality: (= 1 1) => T.")
  (is (+ 1 1)
      2
      "Addition: (+ 1 1) => 2.")
  (is (* 2 2)
      4
      "Multiplication: (* 2 2) => 4.")
  (is (mod (+ 10 2) 10)
      2
      "Modulus: (mod (+ 10 2) 10) => 2."))

(deftest qubits
  (is-type (make-instance 'qubit)
           'qubit
           "Qubit created successfully.")
  (is (qubit-state (make-instance 'qubit))
      1.0
      "The State of the Qubit is 1.0.")
  (is (alpha (make-instance 'qubit))
      0.0
      "Alpha: 0.0")
  (is (beta (make-instance 'qubit))
      1.0
      "Beta: 1.0")
  (is (pure-state (make-instance 'qubit))
      #C(0.0 0.7071067932881648)
      "Pure Qubit State: ~#C(0.0 0.71)"))

(deftest quantum-coupler
  (is-type (make-instance 'quantum-coupler)
           'quantum-coupler
           "Quantum Coupler created successfully.")
  (is-type (make-instance 'quantum-coupler :node-a (make-instance 'qubit))
           'quantum-coupler
           "Quantum Coupler with one initial qubit created successfully.")
  (is-type (make-instance 'quantum-coupler :node-a (make-instance 'qubit) :node-b (make-instance 'qubit))
           'quantum-coupler
           "Quantum Coupler with two initial qubits created successfully.")
  (ok (energy (make-instance 'quantum-coupler)))
  (ok (pure-state (node-a (make-instance 'quantum-coupler))))
  (ok (pure-state (node-b (make-instance 'quantum-coupler)))))

(deftest quantum-register
  (is-type (make-instance 'quantum-register)
           'quantum-register
           "Quantum Register created successfully.")
  (is (cardinality (make-instance 'quantum-register))
      8
      "Quantum Register has a cardinality of 8 qubits.")
  (is-type (make-instance 'quantum-register :base-unit 2)
           'quantum-register
           "Quantum Register with ebits base unit created successfully.")
  (is (cardinality (make-instance 'quantum-register :base-unit 2))
      8
      "Quantum Register with ebits base unit has a cardinality of 8 ebits."))

(run-test-all)

;; EOF
