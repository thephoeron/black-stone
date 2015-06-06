# BLACK-STONE

[![Build Status](https://travis-ci.org/thephoeron/black-stone.svg)](https://travis-ci.org/thephoeron/black-stone)

Specification and Implementation of Quantum Common Lisp, for classical interface gate-model quantum computers.

Note: due to an unfortunate naming conflict with Omer's QCL, a quantum programming language based on the syntax of C, we will generally refer to Quantum Common Lisp as *(qcλ)*, or `qclambda` in the source.

Based on:

* [Quipper, embedded, scalable pure functional language for quantum computation](http://www.mathstat.dal.ca/~selinger/quipper/)
* [Quantum Lambda Calculus](http://www.mathstat.dal.ca/~selinger/papers/#qlambdabook)
* ["A lambda calculus for quantum computation with classical control](http://www.mathstat.dal.ca/~selinger/papers/#qlambda)
* [QGAME: Quantum Gate And Measurement Emulator](http://faculty.hampshire.edu/lspector/qgame.html)
* [Q-Lisp Project (Quantum Computer Programming Language)](http://www.schloerconsulting.com/quantum-computer-q-lisp-programming-language) (*not to be confused with QLISP CL Extension for parallel processing*)
* [Simulation of Quantum Computations in Lisp](ftp://prog.vub.ac.be/tech_report/2006/vub-prog-tr-06-15.pdf) ftp://prog.vub.ac.be/tech_report/2006/vub-prog-tr-06-15.pdf
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

This will provide you with an improved line-editing environment over the underlying SBCL REPL.

Full shell integration is also provided through UBER-SHELL.

System Requirements
-------------------

Tested on Linux x86_64 and OS X 10.7

* SBCL 1.1.14+
* Quicklisp

Dependencies
------------

Available through Quicklisp:

* LET-OVER-LAMBDA
* CL-ISAAC
* GSLL
* CL-PPCRE
* CL-FAD
* CLON (the Command-Line Options Nuker)

Other libraries on GitHub (clone into ~/quicklisp/local-projects/)

* [UBER-SHELL](https://github.com/thephoeron/uber-shell)
