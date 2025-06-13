! main.f90
! Main program for TSFOIL modernized code

program tsfoil_main
  use common_data
  use io_module
  use math_module
  use spline_module
  use airfoil_module
  use mesh_module
  use solver_module
  use numerical_solvers
  implicit none

  ! Program header
  write(*,'(A)') '==============================================='
  write(*,'(A)') '     TSFOIL - Transonic Small-Perturbation     '
  write(*,'(A)') '          Airfoil Analysis Program             '
  write(*,'(A)') '         Modernized Fortran Version            '
  write(*,'(A)') '        Developed by: Runze Li, 2025           '
  write(*,'(A)') '==============================================='

  ! Initialize data structures
  call initialize_common()
  call initialize_spline(N_MESH_POINTS*2)

  ! Call READIN to read one case - it handles termination internally with STOP
  write(*,'(A)') 'Reading input data...'
  call READIN()
  
  ! Continue with complete TSFOIL solution workflow
  write(*,'(A)') 'Starting TSFOIL solution sequence...'
  write(UNIT_OUTPUT,'(A)') 'TSFOIL Solution Sequence'
  write(UNIT_OUTPUT,'(A)') '========================'
  
  ! SCALE: Rescale all physical variables to transonic similarity form
  write(*,'(A)') 'Scaling variables to similarity form...'
  call SCALE()
  
  ! FARFLD: Set far field boundary conditions
  write(*,'(A)') 'Setting far-field boundary conditions...'
  call FARFLD()
  
  ! BODY: Compute airfoil geometry and print geometrical information
  write(*,'(A)') 'Computing airfoil geometry...'
  call BODY()
  
  ! CUTOUT: Remove mesh points for initial coarse mesh solution
  write(*,'(A)') 'Setting up coarse mesh...'
  call CUTOUT()
  
  ! GUESSP: Initialize potential array P
  write(*,'(A)') 'Initializing potential array...'
  call GUESSP()
  
  ! DIFCOE: Compute difference coefficients in field
  write(*,'(A)') 'Computing finite difference coefficients...'
  call DIFCOE()
  
  ! SETBC: Set boundary conditions
  write(*,'(A)') 'Setting boundary conditions...'
  call SETBC(0)
  
  ! SOLVE: Execute main relaxation solution
  write(*,'(A)') 'Solving transonic flow equations...'
  call SOLVE()
  
  ! Check for mesh refinement or final results
  if (IREF <= 0) then
    write(*,'(A)') 'No mesh refinement requested'
  else if (ABORT1) then
    write(*,'(A)') 'Mesh refinement requested, but solution diverged'
  else
    write(*,'(A)') 'Printing intermediate results...'
    call PRINT1()
    write(*,'(A)') 'Refining mesh...'
    call REFINE()
    write(*,'(A)') 'Recomputing finite difference coefficients...'
    call DIFCOE()
    write(*,'(A)') 'Setting boundary conditions after refinement...'
    call SETBC(0) 
    write(*,'(A)') 'Continuing solution after refinement...'
    call SOLVE()
  end if
  
  ! Print final results
  write(*,'(A)') 'Printing final results...'
  call PRINT()
  
  ! Additional mesh refinement if requested
  if (IREF > 0) then
    call REFINE()
    call REFINE()
  end if
  
  ! Store solution for potential next case
  call SAVEP()
  
  write(*,'(A)') 'Case completed successfully'
  write(UNIT_OUTPUT,'(A)') 'Case completed successfully'
  write(UNIT_OUTPUT,*)
  
  ! Close all files and end program
  call cleanup_spline()
  call close_output_files()
  close(UNIT_INPUT)
  close(UNIT_OUTPUT)
  
  write(*,'(A)') 'Program completed normally'

end program tsfoil_main

