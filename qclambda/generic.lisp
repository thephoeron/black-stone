;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: generic.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :black-stone)

;; Generic Gates

(defun qnot (qsys q)
  "Quantum NOT gate"
  (apply-operator qsys 
                  #2A((0 1)
                      (1 0))
                  (list q)))
  

(defun cnot (qsys q1 q2)
  "Quantum Controlled NOT gate"
  (apply-operator qsys 
                  #2A((1 0 0 0)
                      (0 1 0 0)
                      (0 0 0 1)
                      (0 0 1 0))
                  (list q1 q2)))

(defun srn (qsys q)
  "Quantum Square-Root-of-NOT gate"
  (apply-operator
   qsys 
   (make-array '(2 2)
               :initial-contents 
               (list (list (/ 1 (sqrt 2.0L0))  (- (/ 1 (sqrt 2.0L0))))
                     (list (/ 1 (sqrt 2.0L0))  (/ 1 (sqrt 2.0L0)))
                     ))
   (list q)))

(defun nand (qsys q1 q2 q3) 
  "Quantum NAND gate"
  (apply-operator
   qsys 
   (binary-operator-matrix '(1 1 1 0))
   (list q1 q2 q3)))

(defun hadamard (qsys q)
  "Quantum Hadamard gate"
  (apply-operator
   qsys 
   (make-array '(2 2)
              :initial-contents 
              (list (list (/ 1 (sqrt 2.0L0))  (/ 1 (sqrt 2.0L0)))
                    (list (/ 1 (sqrt 2.0L0))  (- (/ 1 (sqrt 2.0L0))))
                    ))
   (list q)))

(defun u-theta (qsys q theta)
  "Quantum U-theta (rotation) gate"
  (apply-operator
   qsys 
   (make-array '(2 2)
               :initial-contents 
               (list (list (cos theta)  (sin theta))
                     (list (- (sin theta))  (cos theta))
                     ))
   (list q)))

(defun cphase (qsys q1 q2 alpha)
  "Quantum conditional phase gate"
  (apply-operator
   qsys 
   (make-array '(4 4)
               :initial-contents 
               (list (list 1 0 0 0)
                     (list 0 1 0 0)
                     (list 0 0 1 0)
                     (list 0 0 0 (exp (* (sqrt -1.0L0) alpha)))
                     ))
   (list q1 q2)))

;; U(2) =  U(phi) * R(theta) * U(psi) * exp(i alpha)I
;; where  U(a) = e^(-ia) 0
;;               0       e^(ia)
;; and    R(a) = cos(a) sin(-a)
;;               sin(a) cos(a)
;; This is all pre-multiplied in the following code

(defun u2 (qsys q phi theta psi alpha)
  "Quantum U2 gate, implemented as:
        e^(i(-phi-psi+alpha))*cos(theta)  e^(i(-phi+psi+alpha))*sin(-theta)
        e^(i(phi-psi+alpha))*sin(theta)   e^(i(phi+psi+alpha))*cos(theta)    "
  (apply-operator
   qsys 
   (let ((i (sqrt -1.0L0)))
     (make-array
      '(2 2)
      :initial-contents 
      (list (list (* (exp (* i (+ (- phi) (- psi) alpha))) (cos theta))
                  (* (exp (* i (+ (- phi) psi alpha))) (sin (- theta))))
            (list (* (exp (* i (+ phi (- psi) alpha))) (sin theta))
                  (* (exp (* i (+ phi psi alpha))) (cos theta)))
            )))
   (list q)))


(defun swap (qsys q1 q2)
  "A quantum gate that swaps the amplitudes for the two specified qubits."
  (apply-operator
   qsys
   (make-array '(4 4)
               :initial-contents 
               (list (list 1 0 0 0)
                     (list 0 0 1 0)
                     (list 0 1 0 0)
                     (list 0 0 0 1)
                     ))
   (list q1 q2)))

(defun printamps (qsys)
  "For use in quantum programs; causes the amplitudes of the executing 
quantum system to be printed."
  (print (amplitudes qsys))
  qsys)

(defun insp (qsys)
  "For use in quantum programs; causes the inspector to be invoked on 
the executing quantum system."
  (inspect qsys)
  qsys)

;; EOF
