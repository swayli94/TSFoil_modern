#!/usr/bin/env python3
"""
TSFoil Python Interface
=======================

This script replicates the functionality of src/main.f90 using Python interfaces
to the Fortran modules compiled with f2py.

Author: AI Assistant
Date: 2025
"""

import sys
import os
from pathlib import Path

try:
    import tsfoil_fortran
except ImportError as e:
    print("ERROR: Could not import tsfoil_fortran module!")
    print(f"Import error: {e}")
    print()
    print("Make sure you have compiled the Fortran modules with f2py:")
    print("  cd src")
    print("  python3 compile_f2py.py")
    sys.exit(1)

def print_header():
    """Print the program header (equivalent to the header in main.f90)"""
    print('===============================================')
    print('     TSFOIL - Transonic Small-Perturbation     ')
    print('          Airfoil Analysis Program             ')
    print('         Python Interface Version              ')
    print('    Developed by: Runze, AI Assistant, 2025    ')
    print('===============================================')

def initialize_tsfoil():
    """Initialize TSFoil data structures"""

    try:
        # Initialize common data
        tsfoil_fortran.common_data.initialize_common()
        
        # Initialize spline module with N_MESH_POINTS*2
        n_mesh_points = tsfoil_fortran.common_data.n_mesh_points
        tsfoil_fortran.spline_module.initialize_spline(n_mesh_points * 2)

    except Exception as e:
        print(f'ERROR: Failed to initialize data structures: {e}')
        sys.exit(1)

def read_input():
    """Read input data (equivalent to READIN)"""

    try:
        tsfoil_fortran.io_module.readin()

    except Exception as e:
        print(f'ERROR: Failed to read input data: {e}')
        print('Make sure you have a valid input file (tsfoil.inp)')
        sys.exit(1)

def scale_variables():
    """Scale variables to similarity form (equivalent to SCALE)"""

    try:
        tsfoil_fortran.solver_functions.scale()

    except Exception as e:
        print(f'ERROR: Failed to scale variables: {e}')
        sys.exit(1)

def set_farfield():
    """Set far field boundary conditions (equivalent to FARFLD)"""

    try:
        tsfoil_fortran.solver_functions.farfld()

    except Exception as e:
        print(f'ERROR: Failed to set far-field boundary conditions: {e}')
        sys.exit(1)

def compute_airfoil_geometry():
    """Compute airfoil geometry (equivalent to BODY)"""

    try:
        tsfoil_fortran.airfoil_module.body()

    except Exception as e:
        print(f'ERROR: Failed to compute airfoil geometry: {e}')
        sys.exit(1)

def compute_difference_coefficients():
    """Compute finite difference coefficients (equivalent to DIFCOE)"""

    try:
        tsfoil_fortran.solver_base.difcoe()
        
    except Exception as e:
        print(f'ERROR: Failed to compute finite difference coefficients: {e}')
        sys.exit(1)

def set_boundary_conditions():
    """Set boundary conditions (equivalent to SETBC)"""
    
    try:
        # SETBC takes an integer parameter (0 in main.f90)
        tsfoil_fortran.solver_functions.setbc(0)
        
    except Exception as e:
        print(f'ERROR: Failed to set boundary conditions: {e}')
        sys.exit(1)

def solve_flow():
    """Execute main relaxation solution (equivalent to SOLVE)"""

    try:
        tsfoil_fortran.main_iteration.solve()
        
    except Exception as e:
        print(f'ERROR: Failed to solve transonic flow equations: {e}')
        print('Check convergence parameters and input data')
        sys.exit(1)

def print_results():
    """Print final results (equivalent to PRINT)"""

    try:
        tsfoil_fortran.io_module.print()
        
    except Exception as e:
        print(f'ERROR: Failed to print results: {e}')
        sys.exit(1)

def cleanup():
    """Clean up and close files"""

    try:
        # Clean up spline module
        tsfoil_fortran.spline_module.cleanup_spline()
        
        # Close output files
        tsfoil_fortran.io_module.close_output_files()

    except Exception as e:
        print(f'WARNING: Error during cleanup: {e}')

def main():
    """Main function that replicates the main.f90 program flow"""
    
    # Print program header
    print_header()
    
    try:
        # Main program flow (matching main.f90 exactly)
        
        # 1. Initialize data structures
        initialize_tsfoil()
        
        # 2. Read input data
        read_input()
        
        # 3. Scale variables to similarity form
        scale_variables()
        
        # 4. Set far field boundary conditions
        set_farfield()
        
        # 5. Compute airfoil geometry
        compute_airfoil_geometry()
        
        # 6. Compute finite difference coefficients
        compute_difference_coefficients()
        
        # 7. Set boundary conditions
        set_boundary_conditions()
        
        # 8. Solve transonic flow equations
        solve_flow()
        
        # 9. Print final results
        print_results()

    except KeyboardInterrupt:
        print('\nComputation interrupted by user')
        sys.exit(1)
        
    except Exception as e:
        print(f'\nUnexpected error: {e}')
        import traceback
        traceback.print_exc()
        sys.exit(1)
        
    finally:
        # Always try to clean up
        cleanup()

def show_help():
    """Show help information"""
    print("TSFoil Python Interface")
    print("=======================")
    print()
    print("Usage:")
    print("  python3 tsfoil_via_fortran_inferface.py")
    print()
    print("Description:")
    print("  This script provides a Python interface to the TSFoil transonic")
    print("  airfoil analysis program. It replicates the functionality of the")
    print("  original Fortran main program using f2py-generated interfaces.")
    print()
    print("Requirements:")
    print("  - Compiled Fortran modules (run compile_f2py.py or compile_f2py.sh)")
    print("  - Valid input file (tsfoil.inp)")
    print()
    print("Output files:")
    print("  - tsfoil2.out: Detailed analysis results")
    print("  - smry.out: Summary of key results")
    print("  - cpxs.out: Pressure coefficient data")
    print("  - field.dat: Flow field data")

def check_fortran_module():
    """Check if the Fortran module is properly compiled and accessible"""
    print("Checking TSFoil Fortran module...")
    print()
    
    try:
        import tsfoil_fortran
        print("✓ tsfoil_fortran module imported successfully")
        
        # Check available attributes
        attrs = [attr for attr in dir(tsfoil_fortran) if not attr.startswith('_')]
        print(f"✓ Available modules/functions: {len(attrs)}")
        
        # Show some key modules
        key_modules = ['common_data', 'io_module', 'solver_functions', 'main_iteration']
        available_modules = []
        
        for module in key_modules:
            if hasattr(tsfoil_fortran, module):
                available_modules.append(module)
                
        if available_modules:
            print("✓ Key modules found:", ', '.join(available_modules))
        else:
            print("⚠ Warning: Some key modules may not be available")
            
        print("✓ Fortran module check completed")
        return True
        
    except ImportError as e:
        print(f"✗ Error importing tsfoil_fortran: {e}")
        print()
        print("To fix this, compile the Fortran modules:")
        print("  cd temp")
        print("  python3 compile_f2py.py")
        return False

if __name__ == "__main__":
    
    # Handle command line arguments
    if len(sys.argv) > 1:
        if sys.argv[1] in ['-h', '--help', 'help']:
            show_help()
            sys.exit(0)
        elif sys.argv[1] == '--check':
            success = check_fortran_module()
            sys.exit(0 if success else 1)
    
    # Run the main TSFoil analysis
    main()
    
    
    
    