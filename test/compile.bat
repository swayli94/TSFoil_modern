@echo off


set NAME= test_AMESH
echo Program name = %NAME%

del *.mod *.o *.exe *.out *.dat 2>nul

REM Set compiler flags with different levels of FPE checking
if "%1"=="normal" (
    REM Normal mode - trap only critical exceptions
    set FFLAGS=-O2 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow -g -fbacktrace
    echo Using NORMAL mode: trapping critical floating-point exceptions
) else if "%1"=="debug" (
    REM Debug mode - no optimization, maximum checking
    set FFLAGS=-O0 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow,denormal -g -fbacktrace -fdump-core
    echo Using DEBUG mode: no optimization, maximum exception checking
) else if "%1"=="none" (
    REM No FPE trapping mode - completely match original behavior
    set FFLAGS=-O2 -Wall -g -fbacktrace
    echo Using NONE mode: no floating-point exception trapping (original behavior)
) else (
    REM Default mode - original behavior with basic FPE trapping
    set FFLAGS=-O2 -Wall -ffpe-trap=invalid,zero -g -fbacktrace
    echo Using DEFAULT mode: basic floating-point exception trapping (original-like)
)

gfortran %FFLAGS% -c ../src/common_data.f90

gfortran %FFLAGS% -c ../src/spline_module.f90

gfortran %FFLAGS% -c ../src/math_module.f90

gfortran %FFLAGS% -c ../src/solver_module.f90

gfortran %FFLAGS% -c ../src/mesh_module.f90

gfortran %FFLAGS% -c ../src/airfoil_module.f90

gfortran %FFLAGS% -c ../src/io_module.f90

gfortran %FFLAGS% -c ../src/numerical_solvers.f90

gfortran %FFLAGS% -o %NAME%.exe %NAME%.f90 common_data.o spline_module.o solver_module.o math_module.o mesh_module.o airfoil_module.o io_module.o numerical_solvers.o


echo Compilation modes available:
echo   compile.bat             (default - minimal FPE trapping, original-like)
echo   compile.bat normal      (trap critical exceptions)
echo   compile.bat debug       (debug mode with core dumps)
echo   compile.bat none        (no FPE trapping at all)


