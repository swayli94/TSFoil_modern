#!/bin/bash

# Compilation script for TSFOIL modern Fortran code with floating-point exception handling
# This version enables aggressive floating-point exception trapping

echo "Compiling TSFOIL modern Fortran code with floating-point exception handling..."

# Clean up any previous compilation files
rm -f *.mod *.o *.exe tsfoil_modern 2>/dev/null

# Set compiler flags with different levels of FPE checking
if [ "$1" = "normal" ]; then
    # Normal mode - trap only critical exceptions
    FFLAGS="-O2 -Wall -Wno-line-truncation -Wno-conversion -Wno-character-truncation -fcheck=all -ffpe-trap=invalid,zero,overflow -g -fbacktrace"
    echo "Using NORMAL mode: trapping critical floating-point exceptions"
elif [ "$1" = "debug" ]; then
    # Debug mode - no optimization, maximum checking
    FFLAGS="-O0 -Wall -Wno-line-truncation -Wno-conversion -Wno-character-truncation -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow,denormal -g -fbacktrace -fdump-core"
    echo "Using DEBUG mode: no optimization, maximum exception checking"
elif [ "$1" = "none" ]; then
    # No FPE trapping mode - completely match original behavior
    FFLAGS="-O2 -Wall -Wno-line-truncation -Wno-conversion -Wno-character-truncation -g -fbacktrace"
    echo "Using NONE mode: no floating-point exception trapping (original behavior)"
elif [ "$1" = "strict" ]; then
    # Strict mode - all warnings enabled (for code cleanup)
    FFLAGS="-O2 -Wall -fcheck=all -ffpe-trap=invalid,zero,overflow -g -fbacktrace"
    echo "Using STRICT mode: all warnings enabled (for code cleanup)"
else
    # Default mode - original behavior with basic FPE trapping
    FFLAGS="-O2 -Wall -Wno-line-truncation -Wno-conversion -Wno-character-truncation -ffpe-trap=invalid,zero -g -fbacktrace"
    echo "Using DEFAULT mode: basic floating-point exception trapping (original-like)"
    echo "Available modes: normal, debug, none, strict"
fi

echo "Compiler flags: $FFLAGS"
echo

# Function to handle compilation errors
handle_error() {
    echo
    echo "ERROR: Compilation failed!"
    echo "Check the error messages above."
    exit 1
}

# Compile modules in dependency order
echo "Compiling common_data module..."
gfortran $FFLAGS -c common_data.f90 || handle_error

echo "Compiling spline_module..."
gfortran $FFLAGS -c spline_module.f90 || handle_error

echo "Compiling math_module..."
gfortran $FFLAGS -c math_module.f90 || handle_error

echo "Compiling solver_module..."
gfortran $FFLAGS -c solver_module.f90 || handle_error

echo "Compiling mesh_module..."
gfortran $FFLAGS -c mesh_module.f90 || handle_error

echo "Compiling airfoil_module..."
gfortran $FFLAGS -c airfoil_module.f90 || handle_error

echo "Compiling io_module..."
gfortran $FFLAGS -c io_module.f90 || handle_error

echo "Compiling numerical_solvers..."
gfortran $FFLAGS -c numerical_solvers.f90 || handle_error

# Compile main program and link
echo "Compiling main program and linking..."
gfortran $FFLAGS -o tsfoil_modern main.f90 common_data.o spline_module.o solver_module.o math_module.o mesh_module.o airfoil_module.o io_module.o numerical_solvers.o || handle_error

# Move executable to parent directory
mv tsfoil_modern ../ || handle_error

echo
echo "Compilation successful!"
echo "Executable: tsfoil_modern"
echo
echo "Compilation modes available:"
echo "  ./compile.sh             (default - minimal FPE trapping, original-like)"
echo "  ./compile.sh normal      (trap critical exceptions)"
echo "  ./compile.sh debug       (debug mode with core dumps)"
echo "  ./compile.sh none        (no FPE trapping at all)"
echo "  ./compile.sh strict      (all warnings enabled for code cleanup)" 
