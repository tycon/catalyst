Catalyst
========
A relational specification framework implemented as a 
dependent type system for ML.


To Run
========
Catalyst takes and SML program and its spec, and verifies SML against
the spec. For eg, here is how you run the alpha-conversion example:

    ./catalyze -spec test/utlc.spec test/utlc.sml

To print debug information and intermediate files:

    ./catalyze -verbose -spec test/utlc.spec test/utlc.sml

Catalyst generates following intermediate files:

* utlc.vcs - Pretty printed verification conditions (VCs)
* utlcs.evcs - Elaborated VCs
* catalyst.z3 - VCs encoded in SMT-LIB. Can be passed to Z3 SMT solver.

To print core-ml and A-Normalized core-ml:

    ./catalyze -keep core-ml -spec test/utlc.spec test/utlc.sml

Catalyst needs Z3 dynamic library to run. A compatible version of
libz3.dylib has been provided with the source.

To Compile
============
You need MLton Standard ML compiler.

    mlton make.mlb

Misc
====
The current version of Catalyst is not optimized for performance.
Nevertheless, in our benchmarks, almost 90% of Catalyst's run-time is
spent in MLton's native parseAndElaborate pass. With verbose option
enabled, Catalyst displays time spent in various passes, including
MLton passes. 

