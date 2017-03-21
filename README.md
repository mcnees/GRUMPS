# GRUMPS
GRUMPS: General Relativity Using Multiple Processors and Symmetries

A _Mathematica_ package for coordinate basis calculations of tensors that show up in general relativity and other gravitational theories. Calculations are performed in parallel across all available processors, and make use of symmetries to reduce the number of computations.

GRUMPS is a [_Mathematica_](http://wolfram.com) package for coordinate basis calculations of curvature tensors and related objects. It is designed to be fast and easy to use. Most modern CPUs are multi-core, meaning _Mathematica_ can run multiple kernels simultaneously. GRUMPS takes advantage of this by distributing calculations and simplifications across all available kernels.

GRUMPS is meant to be used as a scratchpad for quick calculations. It is not intended to take the place of more advanced systems. If you are using _Mathematica_ for complicated tensor calculations I strongly recommend the [xAct suite of packages.](http://xact.es/index.html) They have a bit of a learning curve, but they define the state-of-the-art in tensor computer algebra.

In its present form GRUMPS can calculate Christoffel symbols, curvature tensors, and certain scalar invariants. It can also format and display the results using standard notation. The package is alpha software in the early stages of development (the current version number is 0.1.6). Many features have yet to be implemented. However, it's pretty handy and you might find it useful.

Most of this package was written by Alex Gilman during his senior year (2015) at Loyola University Chicago.
