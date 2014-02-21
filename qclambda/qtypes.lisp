;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: qtypes.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :black-stone)

;; We define the quantum data types as CLOS classes

; Basic Quantum Computer system
(defclass quantum-system ()
  ((number-of-qubits :initarg :number-of-qubits :accessor number-of-qubits 
                     :documentation "The Number of Qubits in the quantum computer system.")
   (amplitudes :initarg :amplitudes :initform nil :accessor amplitudes 
               :documentation "An array of amplitudes.")
   (prior-probability :initarg :prior-probability :initform 1 :accessor prior-probability 
                      :documentation "The probability of reaching this system in the first place.")
   (oracle-count :initarg :oracle-count :initform 0 :accessor oracle-count
                 :documentation "The number of oracle calls that have been made in the history of this system.")
   (measurement-history :initarg :measurement-history :initform nil :accessor measurement-history
                        :documentation "A list of measurements and their results in the history of this system.")
   (instruction-history :initarg :instruction-history :initform nil :accessor instruction-history
                        :documentation "A list of all instructions executed in the history of this system.")
   (program :initarg :program :initform nil :accessor program
            :documentation "The program yet to be executed by this system, if it hasn't yet terminated.")
   (qubit-numbers :accessor qubit-numbers
                  :documentation "Convenience slot for all valid qubit indices.")
   (amplitude-address :accessor amplitude-address
                      :documentation "Convenience slot for looping over qubits.")))

(defmethod initialize-instance :after ((qsys quantum-system) &rest args)
  "An initializer for quantum systems."
  (declare (ignore args))
  (let ((num-qubits (number-of-qubits qsys)))
    ; if there are no amplitudes yet, initialize to |00...0>
    (unless (amplitudes qsys)
      (setf (amplitudes qsys)
            (let ((amps (make-array (expt 2 num-qubits) :initial-element 0.0L0)))
              ;; start in zero state
              (setf (aref amps 0) 1.0L0)
              amps)))
    ; initialize list of valid qubit indices
    (setf (qubit-numbers qsys)
          (let ((all nil))
            (dotimes (i num-qubits) (push i all))
            (reverse all)))
    ; initialize address register for amplitudes
    (setf (amplitude-address qsys)
          (make-array num-qubits :initial-element 0))))


;; EOF
