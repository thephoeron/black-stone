# BLACK-STONE

[![Build Status](https://travis-ci.org/thephoeron/black-stone.svg)](https://travis-ci.org/thephoeron/black-stone)
[![Coverage Status](http://coveralls.io/repos/thephoeron/black-stone/badge.svg?branch=master&service=github)](http://coveralls.io/github/thephoeron/black-stone?branch=master)
[![Quicklisp](http://quickdocs.org/badge/black-stone.svg)](http://quickdocs.org/black-stone/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)
[![Join the chat at https://gitter.im/thephoeron/black-stone](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/thephoeron/black-stone?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Specification and Implementation of Quantum Common Lisp, for classical interface gate-model quantum computers.

Note: due to an unfortunate naming conflict with Omer's QCL, a quantum programming language based on the syntax of C, we will generally refer to Quantum Common Lisp as *(qcλ)*, or `qclambda` in the source.

Documentation available at: http://thephoeron.viewdocs.io/black-stone

## Sources and References

* [Quantum Computation and Quantum Information](), Nielson and Chuang, 2010.
* [Quantum Algorithms via Linear Algebra](), Lipton and Regan, 2015.
* [Quantum Information, Computation, and Communication](), Jones and Jaksch, 2012.
* [Quantum Processes, Systems, and Information](), Schumacher and Westmoreland, 2010.
* [Quantum Computer Science](), Mermin, 2007.
* [Quipper, embedded, scalable pure functional language for quantum computation](http://www.mathstat.dal.ca/~selinger/quipper/), Selinger, 2013.
* [Quantum Lambda Calculus](http://www.mathstat.dal.ca/~selinger/papers/#qlambdabook), Selinger, 2009.
* ["A lambda calculus for quantum computation with classical control](http://www.mathstat.dal.ca/~selinger/papers/#qlambda), Selinger, 2006.
* [QGAME: Quantum Gate And Measurement Emulator](http://faculty.hampshire.edu/lspector/qgame.html)
* [Q-Lisp Project (Quantum Computer Programming Language)](http://www.schloerconsulting.com/quantum-computer-q-lisp-programming-language) (*not to be confused with QLISP CL Extension for parallel processing*)
* [Simulation of Quantum Computations in Lisp](ftp://prog.vub.ac.be/tech_report/2006/vub-prog-tr-06-15.pdf) ftp://prog.vub.ac.be/tech_report/2006/vub-prog-tr-06-15.pdf

## Installation & Use (Linux/OS X)

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

## System Requirements

Tested on Linux x86_64 and OS X 10.7

* SBCL 1.1.14+
* Quicklisp

## Dependencies

Available through Quicklisp:

* LET-OVER-LAMBDA
* CL-ISAAC
* GSLL
* CL-PPCRE
* CL-FAD
* CLON (the Command-Line Options Nuker)

## License

Copyright &copy; 2013&ndash;2015, "the Phoeron" Colin J.E. Lupton.  This project has been released under the MIT License; please see `black-stone/LICENSE` for more information.
