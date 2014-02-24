;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: qtypes.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :black-stone)

;; We define the quantum data types as CLOS classes

;; Basic Qubit
;; -- to be integrated in QUANTUM-SYSTEM
;; -- each QUBIT instance in slot QUBIT-LIST can be treated individually or as part of system
;; -- will cost additional overhead, but necessary for features such as entanglements of qubits
;;    and arbitrary Q-REGs
(defclass qubit ()
  ((alpha :initarg :alpha :initform 0.0L0 :accessor alpha)
   (beta :initarg :beta :initform 1.0L0 :accessor beta)
   (pure-state :initarg :pure-state :initform nil :accessor pure-state)))

(defmethod initialize-instance :after ((qubit qubit) &rest args)
  (declare (ignore args))
  ; |0> + i|1>/(sqrt 2)
  (let* ((i (sqrt -1))
         (state (/ (+ (alpha qubit)
                      (* i (beta qubit)))
                   (sqrt 2))))
    (setf (pure-state qubit) state)))

(defgeneric qubit-state (qubit)
  (:documentation "Linear superposition of basis states"))

;; adds probability amplitudes ALPHA and BETA, returns resulting STATE
(defmethod qubit-state ((qubit qubit) &rest args)
  (declare (ignore args))
  (let ((state (+ (alpha qubit)
                  (beta qubit))))
    state))

;; Basic Coupler
;; -- used by some Quantum systems
(defclass quantum-coupler ()
  ((energy :initarg :energy :initform 0 :accessor energy)
   (node-a :initarg :node-a :initform 0 :accessor node-a)
   (node-b :initarg :node-b :initform 0 :accessor node-b)))

;; Basic Quantum Computer system
;; -- add slot QUBIT-LIST to track list of QUBIT instances
;; -- should probably be implemented as a hash-table to store QUBITs to quantum core grid
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
