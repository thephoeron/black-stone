BLACK-STONE
===========

Specification and Implementation of Quantum Common Lisp, for classical interface gate-model quantum computers.

Note: due to an unfortunate naming conflict with Omer's QCL, a quantum programming language based on the syntax of C, we will generally refer to Quantum Common Lisp as (qcλ), or qclambda in the source.

Based on:

* [Quipper, embedded, scalable pure functional language for quantum computation](http://www.mathstat.dal.ca/~selinger/quipper/)
* [Quantum Lambda Calculus](http://www.mathstat.dal.ca/~selinger/papers/#qlambdabook)
* ["A lambda calculus for quantum computation with classical control](http://www.mathstat.dal.ca/~selinger/papers/#qlambda)
* Quantum Processes, Systems & Information -- Schumacher, Westmoreland
* Quantum Computer Science -- Mermin

Installation & Use (Linux/OS X)
-------------------------------

Dump the executable:

    $ CC=gcc sbcl --script make.lisp

Install for all users:

    $ sudo install -v ./black-stone /usr/local/bin

Or if you have a local executable directory on your path:

    $ install -v ./black-stone ~/bin

When running from the terminal, it is recommended to use RLWRAP:

    $ rlwrap black-stone
    ...

    #[BLACK-STONE::QCL-USER]> _

System Requirements
-------------------

Tested on Linux x86_64 and OS X 10.7

* SBCL 1.1.10+
* Quicklisp

Dependencies
------------

Available through Quicklisp:

* GSLL
* CL-PPCRE
* CL-FAD
* CLON (the Command-Line Options Nuker)

Other libraries on GitHub (clone into ~/quicklisp/local-projects/)

* [LET-OVER-LAMBDA](https://github.com/thephoeron/let-over-lambda)
* [CL-ISAAC](https://github.com/thephoeron/cl-isaac)
* [UBER-SHELL](https://github.com/thephoeron/uber-shell)
