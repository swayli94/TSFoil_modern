# TSFOIL Modern Fortran Version

This directory contains the modernized version of the TSFOIL2 transonic small-perturbation airfoil analysis program, converted from Fortran 77 (`old/tsfoil.f90`) to modern Fortran.

## Modern Code Structure

The original monolithic `tsfoil.f90` file has been refactored into a modular structure using modern Fortran features:

### File Organization

```text
tsfoil_modern/
├── main.f90                  # Main program entry point
├── common_data.f90           # Global data (replaces COMMON blocks)
├── io_module.f90             # Input/output routines
├── math_module.f90           # Mathematical utilities
├── spline_module.f90         # Cubic spline interpolation  
├── airfoil_module.f90        # Airfoil geometry
├── mesh_module.f90           # Mesh generation/refinement
├── solver_module.f90         # Finite difference setup
├── numerical_solvers.f90     # SOR solver and iteration
├── compile.bat               # Build script
├── tsfoil.inp                # Example input file
└── modernization_process.md  # This documentation
```

### Module Responsibilities

1. **`common_data.f90`** - Foundation module

   - All shared variables with explicit types
   - Public/private visibility controls
   - Named constants and allocatable arrays

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `INPERR(I)` | Error message output | ✅ |

2. **`math_module.f90`** - Mathematical utilities

   | Original Subroutine |  Description | Status |
   |---------------------|--------------|--------|
   | `ARF(X)` | Error function approximation | ✅ |
   | `SIMP(R,X,Y,N,IER)` | Simpson's rule integration | ✅ |
   | `PX(I,J)` | ∂P/∂x finite difference | ✅ |
   | `PY(I,J)` | ∂P/∂y finite difference | ✅ |
   | `EMACH1(U)` | Local Mach number computation | ✅ |
   | `DRAG(CDFACT)` | Pressure drag integration | ✅ |
   | `LIFT(CLFACT)` | Lift coefficient computation | ✅ |
   | `PITCH(CMFACT)` | Pitching moment calculation | ✅ |
   | `TRAP` | Integrate Y DX by trapezoidal rule | ✅ |
   | `report_convergence_error` | Report convergence errors | ✅ |
   | `FINDSK` | Find shock location | ✅ |
   | `NEWISK` | Shock index adjustment | ✅ |

3. **`spline_module.f90`** - Cubic spline interpolation

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `SPLN1(X, Y, N)` | Set up cubic spline coefficients | ✅ |
   | `SPLN1X(X, Y, N, XP, YP, DYP)` | Evaluate spline at point XP | ✅ |
   | `initialize_spline(max_points)` | Initialize spline coefficients | ✅ |
   | `cleanup_spline` | Deallocate spline arrays | ✅ |
   | `set_boundary_conditions` | Set boundary conditions for spline | ✅ |

4. **`airfoil_module.f90`** - Geometry handling

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `BODY` | Airfoil geometry processing | ✅ |
   | `PRBODY` | Geometry summary | ✅ |

5. **`mesh_module.f90`** - Mesh operations

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `AYMESH` | Analytical mesh generation | UNUSED |
   | `CKMESH` | Mesh validation/adjustment | ✅ |
   | `CUTOUT` | Mesh coarsening | UNUSED |
   | `REFINE` | Mesh refinement | UNUSED |
   | `ISLIT(X)` | Leading/trailing edge location | ✅ |
   | `JSLIT(Y)` | Upper/lower surface location | ✅ |

6. **`solver_module.f90`** - Numerical setup

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `DIFCOE` | Finite difference coefficients | ✅ |
   | `SETBC(IJUMP)` | Solution limits and BC setup | ✅ |
   | `BCEND` | Boundary condition application | ✅ |
   | `FARFLD` | Far-field boundary setup | ✅ |
   | `ANGLE` | Angle potential calculation | ✅ |
   | `EXTRAP` | Far-field extrapolation | UNUSED |
   | `VWEDGE` | Viscous wedge corrections | ✅ |
   | `WANGLE` | Wedge angle for viscous correction | ✅ |
   | `DROOTS` | Compute constants for wind tunnel | ✅ |
   | `VROOTS` | Slotted-wall angle roots | ✅ |

7. **`numerical_solvers.f90`** - Core algorithms

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `SYOR` | SOR sweep | ✅ |
   | `SOLVE` | Main iteration loop | ✅ |
   | `RECIRC` | Circulation updates | ✅ |
   | `REDUB` | Doublet strength updates | ✅ |
   | `RESET` | Far-field boundary updates | ✅ |

8. **`io_module.f90`** - Input/output operations

   | Original Subroutine | Description | Status |
   |---------------------|-------------|--------|
   | `READIN` | Input parameter reading | ✅ |
   | `SCALE` | Variable scaling | ✅ |
   | `CDCOLE` | Drag coefficient assembly | ✅ |
   | `PRINT` | Main output driver | ✅ |
   | `PRINT1` | Body Cp and Mach output | Replaced by `CHECK_SHOCK_AND_MACH` |
   | `PRTFLD` | Field output | UNUSED |
   | `PRTMC` | Flow type mapping | Replaced by `OUTPUT_FIELD` |
   | `PRTSK` | Shock wave output | ✅ |
   | `PRTWAL` | Wind tunnel wall condition output | ✅ |
   | `ECHINP` | Input echoing | UNUSED |
   | `DLAOUT` | Output Cp data | UNUSED |
   | `LOADP` | Read restart file | UNUSED |
   | `CPPLOT` | Cp plot preparation | Replaced by `OUTPUT_CP_MACH_XLINE` |
   | `FIXPLT` | Plot array construction | Replaced by `OUTPUT_CP_MACH_XLINE` |
   | `SAVEP` | Solution storage | UNUSED |
   | `M1LINE` | Sonic line detection | UNUSED |
   | `PLTSON` | Sonic line printer | UNUSED |
   | `GUESSP` | Solution initialization | UNUSED |
   | `MACHMP` | Print map of Mach number | Replaced by `OUTPUT_FIELD` |

### Compilation Dependencies

```text
common_data.f90 → (foundation module)
    ↓
math_module.f90 → spline_module.f90
    ↓
airfoil_module.f90 → mesh_module.f90 → solver_module.f90 → numerical_solvers.f90
    ↓
io_module.f90 → main.f90
```

## Key Modernization Improvements

### 1. Memory Management

- **Allocatable arrays** replace fixed-size arrays
- **Dynamic memory allocation** based on mesh size
- **Proper deallocation** and error handling
- **Memory efficiency** improvements

### 2. Interface Design

- **Explicit interfaces** for all procedures
- **Intent declarations** (`in`, `out`, `inout`) for all parameters
- **Optional parameters** where appropriate
- **Type safety** enhancements

### 3. Control Flow

- **`select case`** statements replace computed `GOTO`
- **Structured error handling** with proper return codes
- **Elimination of obsolete** Fortran constructs
- **Improved readability** and maintainability

### 4. Code Organization

- **Logical grouping** of related functionality
- **Clear module dependencies** and interfaces
- **Reduced coupling** between components
- **Separation of concerns** principle

### 5. Standards Compliance

- **Modern Fortran 2003+** features
- **Portable code** with standard-compliant syntax
- **Better compiler optimization** opportunities
- **Future-proof** design patterns

### 6. Documentation and Maintainability

- **Self-documenting** module structure
- **Clear variable naming** conventions
- **Consistent code formatting**
- **Inline documentation** for complex algorithms

## Build and Run Instructions

### Requirements

- Modern Fortran compiler (gfortran, ifort, etc.)
- Windows command prompt or equivalent

### Compilation

```cmd
cd tsfoil_modern
compile.bat
```
