;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: oracles.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :black-stone)

;; Oracle Gates

(defun binary-operator-matrix (tt-right-column)
  "Returns a matrix operator for a binary function with the
given tt-right-column as the right column of its truth table."
  (let* ((column-length (length tt-right-column))
         (operator-size (* 2 column-length))
         (matrix (make-array (list operator-size operator-size)
                             :initial-element 0)))
    (dotimes (i column-length)
      (let ((offset (* i 2)))
        (if (zerop (nth i tt-right-column))
          (setf (aref matrix offset offset) 1
                (aref matrix (1+ offset) (1+ offset)) 1)
          (setf (aref matrix offset (1+ offset)) 1
                (aref matrix (1+ offset) offset) 1))))
    matrix))

(defun oracle (qsys tt-right-column &rest qubits)
  "Applies the oracle operator built from tt-right-column, which
is the right column of the corresponding truth table."
  (incf (oracle-count qsys))
  (apply-operator
   qsys
   (binary-operator-matrix tt-right-column)
   qubits))

(defun limited-oracle (qsys max-calls tt-right-column &rest qubits)
  "If (oracle-count qsys) is less than max-calls then this applies 
the oracle operator built from tt-right-column, which is the right 
column of the corresponding truth table. Otherwise this does nothing."
  (if (< (oracle-count qsys) max-calls)
    (progn (incf (oracle-count qsys))
           (apply-operator
            qsys
            (binary-operator-matrix tt-right-column)
            qubits))
    qsys))

;; EOF
