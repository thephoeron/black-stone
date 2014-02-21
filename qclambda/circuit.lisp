;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: circuit.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :black-stone)

;; Quantum Computer Circuit manipulation utilities

(defun set-address-components (qsys count qubits)
  "Sets (amplitude-address qsys) to refer to a particular amplitude, as
indicated by the bits in the integer count."
  (dotimes (i (length qubits))
    (setf (aref (amplitude-address qsys) (nth i qubits))
          (if (logbitp i count) 1 0))))
               
(defun map-qubit-combinations (qsys function qubits)
  "Calls function once for each of the 1/0 combinations of the provided
qubits, with the right-most qubit varying the fastest."
  (setq qubits (reverse qubits))
  (let ((number-of-iterations (expt 2 (length qubits))))
    (dotimes (i number-of-iterations)
      (set-address-components qsys i qubits)
      (funcall function))))

(defun get-addressed-amplitude (qsys)
  "Returns the amplitude currently addressed by (amplitude-address qsys)"
  (let ((numerical-address 0))
    (dotimes (i (number-of-qubits qsys))
      (unless (zerop (aref (amplitude-address qsys) i))
        (incf numerical-address (expt 2 i))))
    (aref (amplitudes qsys) numerical-address)))

(defun set-addressed-amplitude (qsys new-value)
  "Sets the amplitude currently addressed by (amplitude-address qsys) 
to new-value."
  (let ((numerical-address 0))
    (dotimes (i (number-of-qubits qsys))
      (unless (zerop (aref (amplitude-address qsys) i))
        (incf numerical-address (expt 2 i))))
    (setf (aref (amplitudes qsys) numerical-address) new-value)))

(defun matrix-multiply (matrix column)
  "Multiplies the given square matrix by the given column (assumed
to be the right length) and returns the resulting column."
  (let ((matrix-size (car (array-dimensions matrix)))
        (result nil))
    (dotimes (i matrix-size)
      (push (let ((element 0))
              (dotimes (j matrix-size)
                (incf element (* (aref matrix i j) (nth j column))))
              element)
            result))
    (reverse result)))

(defun extract-column (qsys qubits-to-vary)
  "Returns a column from the amplitudes obtained by varying the listed
qubits, with the right-most qubit varying the fastest."
  (let ((col nil))
    (map-qubit-combinations 
     qsys
     #'(lambda () 
         (push (get-addressed-amplitude qsys) col))
     qubits-to-vary)
    (reverse col)))

(defun install-column (qsys column qubits-to-vary)
  "Installs the given column in the amplitude positions obtained by
varying the listed qubits, with the right-most qubit varying the fastest."
  (map-qubit-combinations 
   qsys
   #'(lambda () 
       (set-addressed-amplitude qsys (car column))
       (setq column (cdr column)))
   qubits-to-vary))

(defun apply-operator (qsys operator qubits)
  "Applies the given matrix-form operator to the given qubits."
  (map-qubit-combinations
   qsys
   #'(lambda ()
       ;(format t "~%address:~A" (amplitude-address qsys))
       (let* ((pre-column (extract-column qsys qubits))
              (post-column (matrix-multiply operator pre-column)))
         (install-column qsys post-column qubits)))
   (set-difference (qubit-numbers qsys) qubits))
  qsys)

(defun qc-output-probabilities (qsys qubits)
  "Returns a list of the probabilities for all combinations for the
given qubits, in binary order with the rightmost qubit varying fastest."
  (let ((probabilities nil)
        (other-qubits (set-difference (qubit-numbers qsys) qubits)))
    (map-qubit-combinations
     qsys
     #'(lambda ()
         (push (let ((probability 0))
                 (map-qubit-combinations
                  qsys
                  #'(lambda ()
                      (incf probability 
                            (expt (abs (get-addressed-amplitude qsys)) 2)))
                  other-qubits)
                 probability)
               probabilities))
     qubits)
    (reverse probabilities)))

(defun multi-qsys-output-probabilities (qsys-list qubits)
  "Returns a list of the probabilities for all combinations for the
given qubits, in binary order with the rightmost qubit varying fastest.
This function takes a LIST of quantum systems as input and sums the
results across all systems."
  (let ((probabilities
         (mapcar #'(lambda (qsys) 
                     (qc-output-probabilities qsys qubits))
                 qsys-list)))
    (labels ((add-lists (l1 l2)
               (if (null l1) 
                 nil
                 (cons (+ (first l1) (first l2))
                       (add-lists (rest l1) (rest l2))))))
      (reduce #'add-lists probabilities))))

(defun expected-oracles (qsys-list)
  "Returns the expected number of oracle calls for the given
set of quantum systems."
  (reduce #'+
          (mapcar #'(lambda (qsys)
                      (* (prior-probability qsys)
                         (oracle-count qsys)))
                  qsys-list)))

;; EOF
