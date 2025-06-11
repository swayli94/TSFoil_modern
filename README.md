# TSFoil_modern

Modern Fortran implementation of TSFoil2, an inviscid transonic small-disturbance (TSD) solver for airfoil analysis.

TSFOIL2 is an inviscid transonic small-disturbance (TSD) solver for ﬂow past lifting airfoils. The code was chosen for its rapid solution time, ease of use, and its open source architecture. It solves the transonically-scaled perturbation potential, and similarity variables that lead to the computation of the pressure coefficient distribution, Cp, along the airfoil surface, that can then be integrated to yield lift and drag coefficient.

## The original TSFoil2 code

Murman, E.M., Bailey, F.R., and Johnson, M.L., "TSFOIL - A Computer Code for TwoDimensional Transonic Calculations, Including Wind-Tunnel Wall Effects and Wave Drag Evaluation," NASA SP-347, March 1975.

http://www.dept.aoe.vt.edu/~mason/Mason_f/MRsoft.html#TSFOIL2

The modernized version of TSFoil2 is written in Fortran 2003 and is designed to be more maintainable and easier to read. It includes improvements such as better error handling, modularization, and updated coding standards. It is based on a modified version of the original TSFoil2 code, which is available at:

https://github.com/kjayawar/TSFoil

This version also includes a Python wrapper for easier integration with Python-based workflows, allowing users to call the Fortran code from Python and process the results more easily. In this project, the Python wrapper is also modified to work with the modern Fortran code.

## The Modernization Process

The modernization process is majorly conducted by the large language model, Claude Sonnet 4, with some contributions from the author.
The detailed modernization process is documented in the `src/modernization_process.md` file.
