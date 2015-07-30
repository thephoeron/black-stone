;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE; Base: 10 -*- file: internal.lisp

;;;; Copyright (c) 2013--2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :black-stone)

;; Internal utilities for measurement and branching

(defun end (qsys)
  "Marks the end of a measurement branch; has no effect when used
in a quantum program in any other context."
  qsys)

(defun distance-to-next-unmatched-end (list &optional
                                            (num-measures 0) (num-ends 0)
                                            (distance-so-far 0))
  "Returns 0 if there is no unmatched (end) in list; otherwise returns
the number of instructions to the next unmatched (end) (counting the (end))."
  (if (null list)
    0
    (if (eq (caar list) 'end)
      (if (zerop num-measures)
        (+ 1 distance-so-far)
        (if (oddp num-ends) ;; then this one closes a measure
          (distance-to-next-unmatched-end (cdr list)
                                          (- num-measures 1) (- num-ends 1)
                                          (+ 1 distance-so-far))
          (distance-to-next-unmatched-end (cdr list)
                                          num-measures (+ num-ends 1)
                                          (+ 1 distance-so-far))))
      (if (eq (caar list) 'measure)
        (distance-to-next-unmatched-end (cdr list)
                                        (+ num-measures 1) num-ends
                                        (+ 1 distance-so-far))
        (distance-to-next-unmatched-end (cdr list)
                                        num-measures num-ends
                                        (+ 1 distance-so-far))))))

(defun without-if-branch (program)
  "Assuming that a MEASURE form has just been removed from the given
program, returns the remainder of the program without the IF (measure-1)
branch."
  (let* ((distance-to-first-unmatched-end
          (distance-to-next-unmatched-end program))
         (distance-from-first-to-second-unmatched-end
          (distance-to-next-unmatched-end
           (nthcdr distance-to-first-unmatched-end program))))
    (if (zerop distance-to-first-unmatched-end)
      ;; it's all the if part
      nil
      ;; there is some else part
      (if (zerop distance-from-first-to-second-unmatched-end)
        ;; the else never ends
        (subseq program distance-to-first-unmatched-end)
        ;; the else does end
        (append (subseq program
                        distance-to-first-unmatched-end
                        (+ distance-to-first-unmatched-end
                           distance-from-first-to-second-unmatched-end
                           -1))
                (subseq program (+ distance-to-first-unmatched-end
                                   distance-from-first-to-second-unmatched-end
                                   )))))))

(defun without-else-branch (program)
  "Assuming that a MEASURE form has just been removed from the given
program, returns the remainder of the program without the ELSE (measure-0)
branch."
  (let* ((distance-to-first-unmatched-end
          (distance-to-next-unmatched-end program))
         (distance-from-first-to-second-unmatched-end
          (distance-to-next-unmatched-end
           (nthcdr distance-to-first-unmatched-end program))))
    (if (zerop distance-to-first-unmatched-end)
      ;; it's all the if part
      program
      ;; there is some else part
      (if (zerop distance-from-first-to-second-unmatched-end)
        ;; the else never ends
        (subseq program 0 (- distance-to-first-unmatched-end 1))
        ;; the else does end
        (append (subseq program 0 (- distance-to-first-unmatched-end 1))
                (subseq program (+ distance-to-first-unmatched-end
                                   distance-from-first-to-second-unmatched-end)))))))

(defun force-to (measured-value qubit qsys)
  "Collapses a quantum system to the provided measured-value for the provided
qubit."
  (map-qubit-combinations
   qsys
   #'(lambda ()
       (let* ((pre-column (extract-column qsys (list qubit)))
              (new-column (case measured-value
                            (0 (list (first pre-column) 0))
                            (1 (list 0 (second pre-column))))))
         (install-column qsys new-column (list qubit))))
   (remove qubit (qubit-numbers qsys)))
  qsys)

;; EOF
