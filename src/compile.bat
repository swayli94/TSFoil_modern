@echo off
REM Compilation script for TSFOIL modern Fortran code with floating-point exception handling
REM This version enables aggressive floating-point exception trapping

echo Compiling TSFOIL modern Fortran code with floating-point exception handling...
echo.

REM Clean up any previous compilation files
del *.mod *.o *.exe 2>nul

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
    echo Using NONE mode: no floating-point exception trapping
) else (
    REM Default mode - original behavior with basic FPE trapping
    set FFLAGS=-O2 -Wall -ffpe-trap=invalid,zero -g -fbacktrace
    echo Available modes: normal, debug, none, default
    echo Using DEFAULT mode: basic floating-point exception trapping
)

echo Compiler flags: %FFLAGS%
echo.

REM Compile modules in dependency order
echo Compiling common_data module...
gfortran %FFLAGS% -c common_data.f90
if errorlevel 1 goto error

echo Compiling solver_data...
gfortran %FFLAGS% -c solver_data.f90
if errorlevel 1 goto error

echo Compiling spline_module...
gfortran %FFLAGS% -c spline_module.f90
if errorlevel 1 goto error

echo Compiling math_module...
gfortran %FFLAGS% -c math_module.f90
if errorlevel 1 goto error

echo Compiling airfoil_module...
gfortran %FFLAGS% -c airfoil_module.f90
if errorlevel 1 goto error

echo Compiling mesh_module...
gfortran %FFLAGS% -c mesh_module.f90
if errorlevel 1 goto error

echo Compiling solver_base...
gfortran %FFLAGS% -c solver_base.f90
if errorlevel 1 goto error

echo Compiling solver_functions...
gfortran %FFLAGS% -c solver_functions.f90
if errorlevel 1 goto error

echo Compiling main_iteration...
gfortran %FFLAGS% -c main_iteration.f90
if errorlevel 1 goto error

echo Compiling io_module...
gfortran %FFLAGS% -c io_module.f90
if errorlevel 1 goto error

REM Compile main program and link
echo Compiling main program and linking...
gfortran %FFLAGS% -o tsfoil_modern.exe main.f90 common_data.o spline_module.o math_module.o airfoil_module.o mesh_module.o solver_data.o solver_base.o solver_functions.o main_iteration.o io_module.o
if errorlevel 1 goto error

move tsfoil_modern.exe ..\ 2>nul
if errorlevel 1 goto error

echo.
echo Compilation successful!
echo Executable: tsfoil_modern.exe
echo.
echo Compilation modes available:
echo   compile.bat             (default - minimal FPE trapping, original-like)
echo   compile.bat normal      (trap critical exceptions)
echo   compile.bat debug       (debug mode with core dumps)
echo   compile.bat none        (no FPE trapping at all)
goto end

:error
echo.
echo ERROR: Compilation failed!
echo Check the error messages above.
exit /b 1

:end
