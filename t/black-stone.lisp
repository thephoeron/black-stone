;;;; file: t/black-stone.lisp

(in-package :cl-user)

(defpackage black-stone-test
  (:use cl black-stone prove))

(in-package :black-stone-test)

;; NOTE: To run this test file, execute `(asdf:test-system :bit-smasher)' in your Lisp.

(plan 2)

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
  (is (black-stone::qubit-state (make-instance 'qubit))
      1.0
      "The State of the Qubit is 1.0.")
  (is (black-stone::alpha (make-instance 'qubit))
      0.0
      "Alpha: 0.0")
  (is (black-stone::beta (make-instance 'qubit))
      1.0
      "Beta: 1.0")
  (is (black-stone::pure-state (make-instance 'qubit))
      #C(0.0 0.7071067932881648)
      "Pure Qubit State: ~#C(0.0 0.71)"))

(run-test-all)

;; EOF
