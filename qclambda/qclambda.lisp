;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: qclambda.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :black-stone)

;; Top-level functions and external API

(defvar *post-oracle-measurements*) ;*T*

(defun run-qsys (qsys)
  "Takes a quantum system and returns the list of quantum systems that
results from the execution of its program."
  (if (or (null (program qsys))
          (zerop (prior-probability qsys)))
    (list qsys)
    (let ((instruction (first (program qsys))))
      (setf (instruction-history qsys)
            (append (instruction-history qsys) (list instruction)))
      (if (eq (first instruction) 'halt)
        (list qsys)
        (if (eq (first instruction) 'measure)
          ;; it's a measurement so split state and return list of results
          (let* ((measurement-qubit (second instruction))
                 (probabilities (qc-output-probabilities qsys (list measurement-qubit))))
            (append 
             ;; 1 branch
             (run-qsys
              (force-to 1 measurement-qubit
                        (make-instance 'quantum-system
                          :number-of-qubits (number-of-qubits qsys)
                          :amplitudes (copy-seq (amplitudes qsys))
                          :prior-probability (second probabilities)
                          :oracle-count (oracle-count qsys)
                          :measurement-history (append (measurement-history qsys)
                                                       (list (list measurement-qubit
                                                                   'is 1)))
                          :instruction-history (instruction-history qsys)
                          :program (without-else-branch (rest (program qsys))))))
             ;; 0 branch
             (run-qsys
              (force-to 0 measurement-qubit
                        (make-instance 'quantum-system
                          :number-of-qubits (number-of-qubits qsys)
                          :amplitudes (copy-seq (amplitudes qsys))
                          :prior-probability (first probabilities)
                          :oracle-count (oracle-count qsys)
                          :measurement-history (append (measurement-history qsys)
                                                       (list (list measurement-qubit
                                                                   'is 0)))
                          :instruction-history (instruction-history qsys)
                          :program (without-if-branch (rest (program qsys))))))))
          (let ((resulting-sys
                 (apply (first instruction) (cons qsys (rest instruction)))))
            (setf (program resulting-sys) (rest (program resulting-sys)))
            (run-qsys resulting-sys)))))))
           

(defun execute-quantum-program (pgm num-qubits &optional (oracle-tt nil))
  "Executes the provide quantum program with the specified number of qubits
and the provided oracle truth table, returning a list of the resulting
quantum systems."
  (run-qsys (make-instance 'quantum-system
              :number-of-qubits num-qubits
              :program (subst oracle-tt 'ORACLE-TT pgm))))


(defun test-quantum-program (pgm &key num-qubits cases final-measurement-qubits
                                 threshold (inspect nil) (debug 0))
  "The top-level function to evaluate a quantum program relative to a list of 
a list of (oracle value) cases. Returns a list of:
misses max-error average-error max-expected-oracles average-expected-oracles
See documentation for a more complete explanation of the arguments and
return values."
  (let ((misses 0)
        (max-error 0)
        (total-error 0)
        (average-error 0)
        (max-expected-oracles 0)
        (total-expected-oracles 0)
        (average-expected-oracles 0)
        (num-cases (length cases)))
    (dolist (case cases)
      (let* ((resulting-systems (execute-quantum-program
                                 pgm num-qubits (first case)))
             (raw-error (- 1.0 (nth (second case) 
                                    (multi-qsys-output-probabilities
                                     resulting-systems final-measurement-qubits))))
             (expected-oracles (expected-oracles resulting-systems)))
        (if (> raw-error threshold) (incf misses))
        (incf total-error raw-error)
        (when (> raw-error max-error) 
          (setq max-error raw-error))
        (incf total-expected-oracles expected-oracles)
        (when (> expected-oracles max-expected-oracles) 
          (setq max-expected-oracles expected-oracles))
        (when (>= debug 2)
          (format t "~%---~%Case:~A, Error:~,5F" case raw-error))
        (when inspect (inspect resulting-systems))))
    (setq average-error (/ total-error num-cases))
    (setq average-expected-oracles (/ total-expected-oracles num-cases))
    (when (>= debug 1)
      (format t "~%~%Misses:~A" misses)
      (format t "~%Max error:~A" max-error)
      (format t "~%Average error:~A" (float average-error))
      (format t "~%Max expected oracles:~A" max-expected-oracles)
      (format t "~%Average expected oracles:~A" (float average-expected-oracles)))
    (list misses max-error average-error max-expected-oracles average-expected-oracles)))

;; EOF
