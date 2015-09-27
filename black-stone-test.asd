;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BLACK-STONE-TEST; Base: 10 -*-
;;;; file: black-stone-test.asd

;;;; Copyright (c) 2013--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage black-stone-test-asd
  (:use :cl :asdf))

(in-package :black-stone-test-asd)

(defsystem #:black-stone-test
  :serial t
  :version #.black-stone-asd:*black-stone-version*
  :description "The test code for BLACK-STONE."
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "MIT"
  :depends-on (#:black-stone
               #:prove)
  :components ((:module "t"
                :components
                ((:test-file "black-stone"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

;; EOF
