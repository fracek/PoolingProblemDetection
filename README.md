Pooling Problem
===============

This repository contains the source code of the method described in
F. Ceccon, G. Kouyialis, R. Misener,
["Using functional programming to recognize named structure in an
optimization problem: Application to pooling"](http://dx.doi.org/10.1002/aic.15308).

Building
--------

To build run

    ./build.sh

The resulting binary executable is in `build/Pooling.Convert.exe`

Usage
-----

To run the method on a single input:

    mono Pooling.Convert.exe input.osil output.dat

This will write a `.dat` file containing the data of the pooling problem.

License
-------

See `LICENSE.txt`.