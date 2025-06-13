! no_use_subroutines.f90
! This file contains subroutines and functions that are defined but not called in the current TSFoil modernized codebase

module no_use_subroutines
  implicit none

  integer :: IREF   ! mesh refinement flag (0 = no refinement, 1 = refinement)
  integer :: ICUT   ! number of coarse refinements (0)
  
contains

  ! Echo input cards for logging - exactly like original ECHINP
  ! Prints input cards used for run (originally called by TSFOIL main program)
  ! NOTE: This should be called ONCE before any case processing,
  ! not from within READIN, to echo the entire input file
  ! UNUSED: Commented out in main.f90
  subroutine ECHINP()
    use common_data, only: UNIT_INPUT, UNIT_OUTPUT
    implicit none
    character(len=80) :: CRD  ! Input card buffer (20A4 = 80 characters)
    integer :: read_status
    
    ! Write form feed to output file (1H1 format)
    write(UNIT_OUTPUT,'(A1)') char(12)  ! Form feed character equivalent to 1H1
    
    ! Read and echo all input cards until end of file
    do
      read(UNIT_INPUT, '(A80)', iostat=read_status) CRD
      if (read_status /= 0) exit  ! Exit on any read error or EOF
      write(UNIT_OUTPUT, '(1X,A)') trim(CRD)
    end do
    
    ! Rewind input file after reading all cards
    rewind(UNIT_INPUT)

  end subroutine ECHINP

  ! Print INP namelist parameters to UNIT_OUTPUT for debugging
  ! UNUSED: Debug function never called
  subroutine PRINT_INP_NAMELIST()
    use common_data
    implicit none
    integer :: I, J
    
    write(UNIT_OUTPUT, '(A)') '1'  ! Form feed character
    write(UNIT_OUTPUT, '(A)') '0************************************'
    write(UNIT_OUTPUT, '(A)') ' DEBUG: INP NAMELIST PARAMETERS'
    write(UNIT_OUTPUT, '(A)') ' ************************************'
    write(UNIT_OUTPUT, *)
    
    ! Print flow parameters
    write(UNIT_OUTPUT, '(A)') ' FLOW PARAMETERS:'
    write(UNIT_OUTPUT, '(A,F12.6)') '   AK       = ', AK
    write(UNIT_OUTPUT, '(A,F12.6)') '   ALPHA    = ', ALPHA
    write(UNIT_OUTPUT, '(A,F12.6)') '   EMACH    = ', EMACH
    write(UNIT_OUTPUT, '(A,F12.6)') '   DELTA    = ', DELTA
    write(UNIT_OUTPUT, '(A,F12.6)') '   GAM      = ', GAM
    write(UNIT_OUTPUT, '(A,L12)')   '   PHYS     = ', PHYS
    write(UNIT_OUTPUT, *)
    
    ! Print mesh parameters
    write(UNIT_OUTPUT, '(A)') ' MESH PARAMETERS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   IMIN     = ', IMIN
    write(UNIT_OUTPUT, '(A,I12)')   '   IMAXI    = ', IMAXI
    write(UNIT_OUTPUT, '(A,I12)')   '   JMIN     = ', JMIN
    write(UNIT_OUTPUT, '(A,I12)')   '   JMAXI    = ', JMAXI
    write(UNIT_OUTPUT, '(A,L12)')   '   AMESH    = ', AMESH
    write(UNIT_OUTPUT, *)
    
    ! Print boundary condition parameters
    write(UNIT_OUTPUT, '(A)') ' BOUNDARY CONDITIONS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   BCTYPE   = ', BCTYPE
    write(UNIT_OUTPUT, '(A,I12)')   '   BCFOIL   = ', BCFOIL
    write(UNIT_OUTPUT, '(A,F12.6)') '   F        = ', F
    write(UNIT_OUTPUT, '(A,F12.6)') '   H        = ', H
    write(UNIT_OUTPUT, '(A,F12.6)') '   POR      = ', POR
    write(UNIT_OUTPUT, *)
    
    ! Print solver parameters
    write(UNIT_OUTPUT, '(A)') ' SOLVER PARAMETERS:'
    write(UNIT_OUTPUT, '(A,F12.6)') '   CVERGE   = ', CVERGE
    write(UNIT_OUTPUT, '(A,F12.6)') '   DVERGE   = ', DVERGE
    write(UNIT_OUTPUT, '(A,F12.6)') '   EPS      = ', EPS
    write(UNIT_OUTPUT, '(A,I12)')   '   MAXIT    = ', MAXIT
    write(UNIT_OUTPUT, '(A,I12)')   '   IPRTER   = ', IPRTER
    write(UNIT_OUTPUT, '(A,L12)')   '   KUTTA    = ', KUTTA
    write(UNIT_OUTPUT, '(A,L12)')   '   FCR      = ', FCR
    write(UNIT_OUTPUT, *)
    
    ! Print geometry parameters
    write(UNIT_OUTPUT, '(A)') ' GEOMETRY PARAMETERS:'
    write(UNIT_OUTPUT, '(A,F12.6)') '   RIGF     = ', RIGF
    write(UNIT_OUTPUT, '(A,F12.6)') '   WCIRC    = ', WCIRC
    write(UNIT_OUTPUT, '(A,F12.6)') '   CLSET    = ', CLSET
    write(UNIT_OUTPUT, '(A,I12)')   '   NU       = ', NU
    write(UNIT_OUTPUT, '(A,I12)')   '   NL       = ', NL
    write(UNIT_OUTPUT, *)
    
    ! Print output control parameters
    write(UNIT_OUTPUT, '(A)') ' OUTPUT CONTROL:'
    write(UNIT_OUTPUT, '(A,I12)')   '   PRTFLO   = ', PRTFLO
    write(UNIT_OUTPUT, '(A,I12)')   '   PSTART   = ', PSTART
    write(UNIT_OUTPUT, '(A,L12)')   '   PSAVE    = ', PSAVE
    write(UNIT_OUTPUT, '(A,I12)')   '   SIMDEF   = ', SIMDEF
    write(UNIT_OUTPUT, '(A,I12)')   '   ICUT     = ', ICUT
    write(UNIT_OUTPUT, *)
    
    ! Print wind tunnel parameters
    write(UNIT_OUTPUT, '(A)') ' WIND TUNNEL PARAMETERS:'
    write(UNIT_OUTPUT, '(A,3F12.6)') '   WE       = ', WE
    write(UNIT_OUTPUT, *)
    
    ! Print viscous wedge parameters
    write(UNIT_OUTPUT, '(A)') ' VISCOUS WEDGE PARAMETERS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   NWDGE    = ', NWDGE
    write(UNIT_OUTPUT, '(A,E12.3)') '   REYNLD   = ', REYNLD
    write(UNIT_OUTPUT, '(A,F12.6)') '   WCONST   = ', WCONST
    write(UNIT_OUTPUT, *)
    
    ! Print flap parameters
    write(UNIT_OUTPUT, '(A)') ' FLAP PARAMETERS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   IFLAP    = ', IFLAP
    write(UNIT_OUTPUT, '(A,F12.6)') '   DELFLP   = ', DELFLP
    write(UNIT_OUTPUT, '(A,F12.6)') '   FLPLOC   = ', FLPLOC
    write(UNIT_OUTPUT, '(A,I12)')   '   IDLA     = ', IDLA
    write(UNIT_OUTPUT, *)
    
    ! Print XIN array if allocated
    if (IMAXI > 0) then
      write(UNIT_OUTPUT, '(A)') ' XIN ARRAY:'
      write(UNIT_OUTPUT, '(A,I0,A,I0)') '   (I=', IMIN, ' TO ', IMAXI, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (XIN(I), I=IMIN, IMAXI)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print YIN array if allocated
    if (JMAXI > 0) then
      write(UNIT_OUTPUT, '(A)') ' YIN ARRAY:'
      write(UNIT_OUTPUT, '(A,I0,A,I0)') '   (J=', JMIN, ' TO ', JMAXI, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (YIN(J), J=JMIN, JMAXI)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print XU array if NU > 0
    if (NU > 0) then
      write(UNIT_OUTPUT, '(A)') ' XU ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NU, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (XU(I), I=1, NU)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print YU array if NU > 0
    if (NU > 0) then
      write(UNIT_OUTPUT, '(A)') ' YU ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NU, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (YU(I), I=1, NU)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print XL array if NL > 0
    if (NL > 0) then
      write(UNIT_OUTPUT, '(A)') ' XL ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NL, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (XL(I), I=1, NL)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print YL array if NL > 0
    if (NL > 0) then
      write(UNIT_OUTPUT, '(A)') ' YL ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NL, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (YL(I), I=1, NL)
      write(UNIT_OUTPUT, *)
    end if
    
    write(UNIT_OUTPUT, '(A)') ' ************************************'
    write(UNIT_OUTPUT, '(A)') ' END OF INP NAMELIST DEBUG OUTPUT'
    write(UNIT_OUTPUT, '(A)') ' ************************************'
    write(UNIT_OUTPUT, *)
    
  end subroutine PRINT_INP_NAMELIST

  ! Initialize YIN array if not read from namelist
  ! UNUSED: Never called in current implementation
  subroutine check_yin_defaults()
    use common_data, only: YIN, JMIN, JMAX, JUP, JLOW, H, BCTYPE
    implicit none
    integer :: J
    real :: TERM
    
    if (YIN(JMIN) /= 0.0) return
    
    ! Fill YIN with default values for tunnel or free air case
    if (BCTYPE == 1) then
      ! Free air case - exponential distribution
      do J = JMIN, JLOW
        TERM = real(J - JMIN) / real(JLOW - JMIN)
        YIN(J) = -H * (1.0 - EXP(-2.0 * TERM))
      end do
      do J = JUP, JMAX
        TERM = real(J - JUP) / real(JMAX - JUP)
        YIN(J) = H * (1.0 - EXP(-2.0 * TERM))
      end do
    else
      ! Tunnel case - linear distribution
      do J = JMIN, JLOW
        YIN(J) = -H * real(J - JMIN) / real(JLOW - JMIN)
      end do
      do J = JUP, JMAX
        YIN(J) = H * real(J - JUP) / real(JMAX - JUP)
      end do
    end if
  end subroutine check_yin_defaults

  ! Subroutine to check for floating-point exceptions
  ! UNUSED: Commented out in main.f90
  subroutine check_fp_exceptions()
    use, intrinsic :: ieee_arithmetic
    use common_data, only: UNIT_OUTPUT
    implicit none
    logical :: flag_invalid, flag_overflow, flag_divide_by_zero, flag_underflow, flag_inexact
    
    ! Check for floating-point exceptions
    call ieee_get_flag(ieee_invalid, flag_invalid)
    call ieee_get_flag(ieee_overflow, flag_overflow)
    call ieee_get_flag(ieee_divide_by_zero, flag_divide_by_zero)
    call ieee_get_flag(ieee_underflow, flag_underflow)
    call ieee_get_flag(ieee_inexact, flag_inexact)
    
    if (flag_invalid) then
      write(*, '(A)') 'WARNING: Invalid floating-point operation detected'
      write(UNIT_OUTPUT, '(A)') 'WARNING: Invalid floating-point operation detected'
    end if
    
    if (flag_overflow) then
      write(*, '(A)') 'WARNING: Floating-point overflow detected'
      write(UNIT_OUTPUT, '(A)') 'WARNING: Floating-point overflow detected'
    end if
    
    if (flag_divide_by_zero) then
      write(*, '(A)') 'WARNING: Division by zero detected'
      write(UNIT_OUTPUT, '(A)') 'WARNING: Division by zero detected'
    end if
    
    if (flag_underflow) then
      write(*, '(A)') 'WARNING: Floating-point underflow detected'
      write(UNIT_OUTPUT, '(A)') 'WARNING: Floating-point underflow detected'
    end if
    
    if (flag_inexact) then
      ! Inexact results are very common in numerical computations, so we don't report them
      ! write(*, '(A)') 'INFO: Inexact floating-point results detected'
    end if
    
    ! Optionally clear the flags after checking
    ! call ieee_set_flag(ieee_all, .false.)
    
  end subroutine check_fp_exceptions

  ! Subroutine to check for floating-point exceptions during iterations
  ! UNUSED: Commented out in numerical_solvers.f90, SOLVE
  subroutine check_iteration_fp_exceptions(iter_num)
    use, intrinsic :: ieee_exceptions
    implicit none
    
    integer, intent(in) :: iter_num
    logical :: flag_invalid, flag_overflow, flag_divide_by_zero
    
    ! Get the status of critical floating-point exception flags
    call ieee_get_flag(ieee_invalid, flag_invalid)
    call ieee_get_flag(ieee_overflow, flag_overflow)
    call ieee_get_flag(ieee_divide_by_zero, flag_divide_by_zero)
    
    ! Report and halt if any critical exceptions occurred
    if (flag_invalid) then
      write(*,'(A,I0)') 'FATAL: IEEE_INVALID exception (NaN) detected at iteration ', iter_num
      write(*,'(A)') 'This indicates invalid mathematical operations (e.g., 0/0, sqrt(-1))'
      stop 2
    end if
    
    if (flag_overflow) then
      write(*,'(A,I0)') 'FATAL: IEEE_OVERFLOW exception detected at iteration ', iter_num
      write(*,'(A)') 'This indicates numerical values exceeded representable range'
      stop 3
    end if
    
    if (flag_divide_by_zero) then
      write(*,'(A,I0)') 'FATAL: IEEE_DIVIDE_BY_ZERO exception detected at iteration ', iter_num
      write(*,'(A)') 'This indicates division by zero occurred'
      stop 4
    end if
    
  end subroutine check_iteration_fp_exceptions  

  ! Load restart data from previous case
  ! UNUSED: Only called when PSTART=2, which is very rare in typical usage
  subroutine LOADP()
    use common_data
    implicit none
    integer :: I, J, ios_restart
    
    write(UNIT_OUTPUT, '(A)') 'Reading restart data from fort.7'
    
    open(unit=UNIT_RESTART, file='fort.7', status='old', action='read', iostat=ios_restart)
    if (ios_restart /= 0) then
      write(UNIT_OUTPUT, '(A)') 'Error: Cannot open restart file fort.7'
      call INPERR(2)
      return
    end if
    
    rewind(UNIT_RESTART)
    ! Read title using original format 900: FORMAT(20A4)
    read(UNIT_RESTART, '(20A4)', iostat=ios_restart) TITLEO
    if (ios_restart /= 0) then
      write(UNIT_OUTPUT, '(A)') 'Error reading title from restart file'
      call INPERR(2)
      return
    end if
    
    ! Read mesh dimensions using original format 902: FORMAT(4I5)
    read(UNIT_RESTART, '(4I5)', iostat=ios_restart) IMINO, IMAXO, JMINO, JMAXO
    if (ios_restart /= 0) then
      write(UNIT_OUTPUT, '(A)') 'Error reading mesh dimensions from restart file'
      call INPERR(2)
      return
    end if
    ! Read solution parameters using original format 903: FORMAT(8F10.6)
    read(UNIT_RESTART, '(8F10.6)', iostat=ios_restart) CLOLD, EMACHO, ALPHAO, DELTAO, VOLO, DUBO
    if (ios_restart /= 0) then
      write(UNIT_OUTPUT, '(A)') 'Error reading solution parameters from restart file'
      call INPERR(2)
      return
    end if
    ! Read grid coordinates using original format 903: FORMAT(8F10.6)
    read(UNIT_RESTART, '(8F10.6)', iostat=ios_restart) (XOLD(I), I=IMINO, IMAXO)
    if (ios_restart /= 0) then
      write(UNIT_OUTPUT, '(A)') 'Error reading X coordinates from restart file'
      call INPERR(2)
      return
    end if
    
    read(UNIT_RESTART, '(8F10.6)', iostat=ios_restart) (YOLD(J), J=JMINO, JMAXO)
    if (ios_restart /= 0) then
      write(UNIT_OUTPUT, '(A)') 'Error reading Y coordinates from restart file'
      call INPERR(2)
      return
    end if
    
    ! Read solution array P using original format 903: FORMAT(8F10.6)
    do I = IMINO, IMAXO
      read(UNIT_RESTART, '(8F10.6)', iostat=ios_restart) (P(J,I), J=JMINO, JMAXO)
      if (ios_restart /= 0) then
        write(UNIT_OUTPUT, '(A,I0)') 'Error reading P array at I=', I
        call INPERR(2)
        close(UNIT_RESTART)
        return
      end if
    end do

    close(UNIT_RESTART)
    write(UNIT_OUTPUT, '(A)') 'Restart data successfully loaded'
    ! Write restart information to output file (like original)
    write(UNIT_OUTPUT, '(A, /, 1X, 20A4, /, A, /, A, I4, /, A, I4, /, A, I4, /, A, I4, /, &
          &A, F12.8, /, A, F12.8, /, A, F12.8, /, A, F12.8, /, A, F12.8, /, A, F12.8)') &
        '1P INITIALIZED FROM PREVIOUS RUN TITLED', &
        TITLEO, &
        ' WHICH HAD THE FOLLOWING VALUES', &
        ' IMIN  =', IMINO, &
        ' IMAX  =', IMAXO, &
        ' JMIN  =', JMINO, &
        ' JMAX  =', JMAXO, &
        ' CL    =', CLOLD, &
        ' EMACH =', EMACHO, &
        ' ALPHA =', ALPHAO, &
        ' DELTA =', DELTAO, &
        ' VOL   =', VOLO, &
        ' DUB   =', DUBO
    
  end subroutine LOADP

  ! DLAOUT: Output pressure distributions using spline interpolation
  ! UNUSED: Called conditionally but feature is typically commented out
  subroutine DLAOUT(ILE_IN, ITE_IN, ALPHA_IN, DFLP, EM, VF, RE)
    use common_data, only: P, X, Y, IMIN, IMAX, JUP, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: CPFACT, UNIT_OUTPUT, XCP, CPP
    use math_module, only: PX
    use spline_module, only: initialize_spline, set_boundary_conditions, SPLN1, SPLN1X
    implicit none
    
    ! Arguments
    integer, intent(in) :: ILE_IN, ITE_IN
    real, intent(in) :: ALPHA_IN, DFLP, EM, VF, RE
    
    ! Local variables
    real :: CPU(100), CPL(100)
    real :: DY1, DY2, XP, YP, DYP
    integer :: I, NX, IC, IMP
    
    ! Compute pressure coefficients on airfoil surface
    IC = 0
    do I = ILE_IN, ITE_IN
      IC = IC + 1
      ! Upper surface pressure
      CPU(IC) = CPFACT * (CJUP * PX(I, JUP) - CJUP1 * PX(I, JUP+1))
      ! Lower surface pressure
      CPL(IC) = CPFACT * (CJLOW * PX(I, JLOW) - CJLOW1 * PX(I, JLOW-1))
    end do
    
    NX = IC
    
    write(UNIT_OUTPUT, '(A)') '1'
    write(UNIT_OUTPUT, '(A)') 'PRESSURE COEFFICIENT DISTRIBUTION'
    write(UNIT_OUTPUT, '(A,F8.4)') 'ALPHA = ', ALPHA_IN
    write(UNIT_OUTPUT, '(A,F8.4)') 'MACH  = ', EM
    write(UNIT_OUTPUT, *)
    write(UNIT_OUTPUT, '(A)') '    X         CPU        CPL'
    do I = 1, NX
      write(UNIT_OUTPUT, '(3F11.6)') X(ILE_IN+I-1), CPU(I), CPL(I)
    end do
    
    ! Initialize spline interpolation
    call initialize_spline(200)
    
    ! Interpolate upper surface using splines
    DY1 = (CPU(2) - CPU(1)) / (X(ILE_IN+1) - X(ILE_IN))
    DY2 = (CPU(NX) - CPU(NX-1)) / (X(ITE_IN) - X(ITE_IN-1))
    call set_boundary_conditions(1, 1, DY1, DY2)
    call SPLN1(X(ILE_IN:), CPU(ILE_IN:), NX)
    
    ! Generate interpolated points for upper surface
    IMP = 0
    do I = 1, 51
      XP = real(I-1) / 50.0
      call SPLN1X(X(ILE_IN:), CPU(ILE_IN:), NX, XP, YP, DYP)
      IMP = IMP + 1
      XCP(IMP) = XP
      CPP(IMP) = YP
    end do
    
    ! Interpolate lower surface using splines
    DY1 = (CPL(2) - CPL(1)) / (X(ILE_IN+1) - X(ILE_IN))
    DY2 = (CPL(NX) - CPL(NX-1)) / (X(ITE_IN) - X(ITE_IN-1))
    call set_boundary_conditions(1, 1, DY1, DY2)
    call SPLN1(X(ILE_IN:), CPL(ILE_IN:), NX)
    
    ! Generate interpolated points for lower surface
    do I = 1, 51
      XP = real(I-1) / 50.0
      call SPLN1X(X(ILE_IN:), CPL(ILE_IN:), NX, XP, YP, DYP)
      IMP = IMP + 1
      XCP(IMP) = XP
      CPP(IMP) = YP
    end do
    
    write(UNIT_OUTPUT, '(A)') ''
    write(UNIT_OUTPUT, '(A)') 'INTERPOLATED PRESSURE COEFFICIENTS (51 POINTS EACH SURFACE)'
    write(UNIT_OUTPUT, '(A)') ''
    write(UNIT_OUTPUT, '(A)') 'UPPER SURFACE:'
    write(UNIT_OUTPUT, '(A)') '    X         CP'
    do I = 1, 51
      write(UNIT_OUTPUT, '(2F11.6)') XCP(I), CPP(I)
    end do
    
    write(UNIT_OUTPUT, '(A)') ''
    write(UNIT_OUTPUT, '(A)') 'LOWER SURFACE:'
    write(UNIT_OUTPUT, '(A)') '    X         CP'
    do I = 52, 102
      write(UNIT_OUTPUT, '(2F11.6)') XCP(I), CPP(I)
    end do
    
  end subroutine DLAOUT

  ! GUESSP: Initialize potential array P
  ! UNUSED: Never called in current implementation
  ! Initialize potential array P based on PSTART value
  subroutine GUESSP()
    ! SUBROUTINE INITIALIZES P ARRAY AS FOLLOWS
    !      PSTART = 1  P SET TO ZERO
    !      PSTART = 2  P READ FROM UNIT 7
    !      PSTART = 3  P SET TO VALUES IN CORE
    ! IF PSTART = 2 OR 3, SOLUTION IS INTERPOLATED FROM
    ! XOLD, YOLD TO X,Y
    ! BOUNDARY CONDITIONS FOR P ON OUTER BOUNDARIES ARE
    ! AUTOMATICALLY SET DURING INITIALIZATION
    ! Called by - TSFOIL.
    use common_data
    use solver_module, only: EXTRAP
    implicit none
    integer :: I_LOOP, J_LOOP, K_LOOP, INEW, JNEW, ISTEP, JSTEP
    real :: TEST, XP_LOCAL, YP_LOCAL, X1, X2, Y1, Y2, P1, P2
    real :: PT(N_MESH_POINTS)  ! Temporary array for interpolation, matches original COM30
    logical :: x_meshes_same, y_meshes_same
    
    ! Branch to appropriate location using modern select case
    select case (PSTART)
    case (1)
      ! PSTART = 1
      ! P SET TO ZERO
      do I_LOOP = 1, N_MESH_POINTS + 1
        do J_LOOP = 1, N_MESH_POINTS + 2
          P(J_LOOP, I_LOOP) = 0.0
        end do
      end do
      DUB = 0.0
      CIRCFF = 0.0
      CIRCTE = 0.0
      return
    
    case (2)
      ! PSTART = 2
      ! P, X, Y ARRAYS READ FROM UNIT 7 IN SUBROUTINE READIN
      ! TOGETHER WITH INFORMATION ABOUT OLD SOLUTION
      
    case (3)
      ! PSTART = 3
      ! ARRAYS FROM PREVIOUS CASE ARE ALREADY IN P,XOLD,YOLD
      
    end select
    
    ! Common code for PSTART = 2 or 3
    DUB = DUBO
    CIRCFF = CLOLD / CLFACT
    CIRCTE = CIRCFF
    
    ! FOR PSTART = 2 OR 3, OLD P ARRAY ON XOLD, YOLD MESH
    ! MUST BE INTERPOLATED ONTO NEW X Y MESH.
    
    ! INTERPOLATE P FROM XOLD,YOLD TO X,YOLD
    ! CHECK TO SEE IF XOLD AND XIN ARE THE SAME MESH
    x_meshes_same = .true.
    if (IMAXI == IMAXO) then
      do I_LOOP = IMIN, IMAXI
        TEST = abs(XIN(I_LOOP) - XOLD(I_LOOP))
        if (TEST > 0.0001) then
          x_meshes_same = .false.
          exit
        end if
      end do
    else
      x_meshes_same = .false.
    end if
    
    if (x_meshes_same) then
      ! XIN AND XOLD ARE SAME MESH.
      ! P ARRAY MAY BE INTERPOLATED BY SIMPLE DELETION OF
      ! VALUES AT MESH POINTS DELETED IN SUBROUTINE CUTOUT
      ! IF IREF .LE. ZERO, NO INTERPOLATION IS NEEDED
      if (IREF > 0) then
        ISTEP = 2 * IREF
        do J_LOOP = JMINO, JMAXO
          INEW = 0
          do I_LOOP = IMINO, IMAXO, ISTEP
            INEW = INEW + 1
            P(J_LOOP, INEW) = P(J_LOOP, I_LOOP)
          end do
        end do
      end if
      ! INTERPOLATION IN X DIRECTION COMPLETE IF XIN AND XOLD ARE THE SAME.

    else
      ! INTERPOLATE FROM XOLD TO X FOR ARBITRARY CASE
      do J_LOOP = JMINO, JMAXO
        YP_LOCAL = YOLD(J_LOOP)
        do I_LOOP = IMIN, IMAX
          XP_LOCAL = X(I_LOOP)
          if (XP_LOCAL < XOLD(IMINO) .or. XP_LOCAL > XOLD(IMAXO)) then
            ! NEW X MESH POINT IS OUTSIDE RANGE OF OLD X MESH
            ! FOR SUPERSONIC FREESTREAM SET P=0, FOR SUBSONIC
            ! FREESTREAM, EXTRAPOLATE USING FAR FIELD SOLUTION
            PT(I_LOOP) = 0.0
            if (AK > 0.0) call EXTRAP(XP_LOCAL, YP_LOCAL, PT(I_LOOP))
          else
            ! NEW X MESH POINT WITHIN RANGE OF OLD X MESH
            ! FIND VALUE OF XOLD .GT. XP
            X2 = XOLD(1)
            K_LOOP = 0
            do
              K_LOOP = K_LOOP + 1
              X1 = X2
              X2 = XOLD(K_LOOP)
              if (X2 >= XP_LOCAL) exit
            end do
            
            if (X2 == XP_LOCAL) then
              PT(I_LOOP) = P(J_LOOP, K_LOOP)
            else
              P1 = P(J_LOOP, K_LOOP-1)
              P2 = P(J_LOOP, K_LOOP)
              PT(I_LOOP) = P1 + (P2 - P1) / (X2 - X1) * (XP_LOCAL - X1)
            end if
          end if
        end do
        ! WRITE NEW VALUES FOR P INTO P ARRAY
        do I_LOOP = IMIN, IMAX
          P(J_LOOP, I_LOOP) = PT(I_LOOP)
        end do
      end do
    end if
    
    ! INTERPOLATE FROM X,YOLD TO X,Y
    ! CHECK TO SEE IF YIN AND YOLD ARE THE SAME MESH
    y_meshes_same = .true.
    if (JMAXI == JMAXO) then
      do J_LOOP = JMIN, JMAXI
        TEST = abs(YIN(J_LOOP) - YOLD(J_LOOP))
        if (TEST > 0.0001) then
          y_meshes_same = .false.
          exit
        end if
      end do
    else
      y_meshes_same = .false.
    end if
    
    if (y_meshes_same) then
      ! YIN AND YOLD ARE THE SAME MESH
      ! P ARRAY MAY BE INTERPOLATED BY SIMPLE DELETION OF
      ! VALUES AT MESH POINTS DELETED IN SUBROUTINE CUTOUT
      ! IF IREF .LE. ZERO, NO INTERPOLATION IS NEEDED
      if (IREF > 0) then
        JSTEP = 2 * IREF
        do I_LOOP = IMIN, IMAX
          JNEW = 0
          do J_LOOP = JMINO, JMAXO, JSTEP
            JNEW = JNEW + 1
            P(JNEW, I_LOOP) = P(J_LOOP, I_LOOP)
          end do
        end do
      end if
      ! INTERPOLATION IN Y DIRECTION COMPLETE IF YIN AND YOLD ARE THE SAME.

    else      
      ! INTERPOLATE YOLD TO Y FOR ARBITRARY CASE
      do I_LOOP = IMIN, IMAX
        XP_LOCAL = X(I_LOOP)
        K_LOOP = 2
        Y1 = YOLD(1)
        do J_LOOP = JMIN, JMAX
          YP_LOCAL = Y(J_LOOP)
          if (YP_LOCAL < YOLD(JMINO)) then
            ! NEW Y MESH POINT BELOW RANGE OF OLD Y MESH
            PT(J_LOOP) = P(JMINO, I_LOOP)
            if (AK > 0.0 .and. BCTYPE == 1) call EXTRAP(XP_LOCAL, YP_LOCAL, PT(J_LOOP))
          else if (YP_LOCAL > YOLD(JMAXO)) then
            ! NEW Y MESH POINT ABOVE RANGE OF OLD Y MESH
            PT(J_LOOP) = P(JMAXO, I_LOOP)
            if (AK > 0.0 .and. BCTYPE == 1) call EXTRAP(XP_LOCAL, YP_LOCAL, PT(J_LOOP))
          else
            ! NEW Y MESH POINT WITHIN RANGE OF OLD Y MESH
            ! FIND VALUE OF YOLD .GT. YP
            Y2 = Y1
            K_LOOP = K_LOOP - 1
            do
              K_LOOP = K_LOOP + 1
              Y1 = Y2
              Y2 = YOLD(K_LOOP)
              if (Y2 > YP_LOCAL) exit
            end do
            P1 = P(K_LOOP-1, I_LOOP)
            P2 = P(K_LOOP, I_LOOP)
            PT(J_LOOP) = P1 + (P2 - P1) / (Y2 - Y1) * (YP_LOCAL - Y1)
          end if
        end do
        ! PUT NEW P VALUES INTO P ARRAY
        do J_LOOP = JMIN, JMAX
          P(J_LOOP, I_LOOP) = PT(J_LOOP)
        end do
      end do
    end if
  
  end subroutine GUESSP

  ! CUTOUT: Initial mesh setup, original mesh (IREF=-1) or create coarse mesh (IREF>=0)
  ! UNUSED: Never called in current implementation
  ! The function of original mesh setup is now handled in READIN in io_module.f90
  subroutine CUTOUT()
    use common_data, only: X, Y, XMID, YMID, IREF, ICUT, XIN, YIN, IMIN, IMAX, JMIN, JMAX, JLOW, JUP, ITE
    implicit none
    integer :: I, J, K, JE, JST

    ! Check if IREF = -1 (mesh cannot be refined)
    if (IREF == -1) then
      ! Load XIN,YIN into X,Y
      do I = IMIN, IMAX
        X(I) = XIN(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YIN(J)
      end do
      IREF = 0
      return
    end if

    ! First halving: X-direction
    K = IMIN - 1
    do I = IMIN, IMAX, 2
      K = K + 1
      XMID(K) = XIN(I)
    end do
    IMAX = (IMAX - IMIN) / 2 + IMIN
    call ISLIT(XMID)
    
    ! First halving: Y-direction, splitting above and below slit
    K = JMIN - 1
    JE = JLOW - 1
    do J = JMIN, JE, 2
      K = K + 1
      YMID(K) = YIN(J)
    end do
    JST = JUP + 1
    do J = JST, JMAX, 2
      K = K + 1
      YMID(K) = YIN(J)
    end do
    JMAX = (JMAX - JMIN) / 2 + JMIN
    call JSLIT(YMID)

    ! Set IREF to 1 indicating first halving
    IREF = 1
    
    ! First halving complete. Check if no. of points is odd.
    if (ICUT == 1 .or. &
        mod(ITE - IMIN + 1, 2) == 0 .or. &
        mod(IMAX - ITE + 1, 2) == 0 .or. &
        mod(JLOW - JMIN, 2) == 0 .or. &
        mod(JMAX - JUP, 2) == 0) then
      ! Only one mesh refinement possible.
      do I = IMIN, IMAX
        X(I) = XMID(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YMID(J)
      end do
      return
    end if

    ! All points are odd so cut again.
    K = IMIN - 1
    do I = IMIN, IMAX, 2
      K = K + 1
      X(K) = XMID(I)
    end do
    IMAX = (IMAX - IMIN) / 2 + IMIN
    call ISLIT(X)
    
    K = JMIN - 1
    JE = JLOW - 1
    do J = JMIN, JE, 2
      K = K + 1
      Y(K) = YMID(J)
    end do
    JST = JUP + 1
    do J = JST, JMAX, 2
      K = K + 1
      Y(K) = YMID(J)
    end do
    JMAX = (JMAX - JMIN) / 2 + JMIN
    call JSLIT(Y)
    
    ! Set IREF to 2 indicating second halving
    IREF = 2
  end subroutine CUTOUT

  ! REFINE: Refine mesh
  ! UNUSED: Never called in current implementation
  ! Refine mesh and interpolate solution onto finer grid
  subroutine REFINE()
    use common_data, only: XIN, YIN, XMID, YMID, P, X, Y, IMIN, IMAX, JMIN, JMAX, ILE, ITE, JLOW, JUP, IREF
    use common_data, only: NWDGE, WSLP, N_MESH_POINTS
    implicit none
    integer :: I, J, K, JMAXO_LOCAL, JE, JST, IM2, JM2, JL
    integer :: ILEO, INC, M, ISTEP, ISTRT, IEND, IM, IMM
    real :: PT(N_MESH_POINTS)
    real :: D1, D2, CL1, CL2, CU1, CU2, RATIO
    real :: XLEO_LOCAL  ! Must be REAL to store X-coordinate

    JE = 0
    JST = 0
    JL = 0

    ! Store original leading edge position and index for viscous wedge processing
    XLEO_LOCAL = X(ILE)
    ILEO = ILE
    JMAXO_LOCAL = JMAX

    ! Compute new grid size
    IMAX = 2*(IMAX - IMIN) + IMIN
    JMAX = 2*(JMAX - JMIN) + JMIN + 1
    IM2 = IMAX - 2
    JM2 = JMAX - 2

    ! Choose source mesh (coarse or mid) based on IREF
    if (IREF <= 1) then
      do I = IMIN, IMAX
        X(I) = XIN(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YIN(J)
      end do
      IREF = 0
    else
      do I = IMIN, IMAX
        X(I) = XMID(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YMID(J)
      end do
      IREF = 1
    end if

    ! Update mesh indices
    call ISLIT(X)
    call JSLIT(Y)    ! Spread P(J,I) to alternate I(X-MESH) points
    do J = JMIN, JMAXO_LOCAL
      K = IMIN - 1
      do I = IMIN, IMAX, 2
        K = K + 1
        PT(I) = P(J,K)
      end do
      do I = IMIN, IMAX, 2
        P(J,I) = PT(I)
      end do
    end do

    ! Spread P(J,I) to alternate J (Y-MESH) points
    do I = IMIN, IMAX, 2
      K = JMIN - 1
      JE = JLOW - 1
      JL = JLOW - 2
      do J = JMIN, JE, 2
        K = K + 1
        PT(J) = P(K,I)
      end do
      JST = JUP + 1
      do J = JST, JMAX, 2
        K = K + 1
        PT(J) = P(K,I)
      end do
      do J = JMIN, JE, 2
        P(J,I) = PT(J)
      end do
      do J = JST, JMAX, 2
        P(J,I) = PT(J)
      end do
    end do

    ! Interpolate to fill in the missing P values
    do I = IMIN, IM2
      PT(I) = (X(I+1)-X(I)) / (X(I+2)-X(I))
    end do
    do J = JMIN, JE, 2
      do I = IMIN, IM2, 2
        P(J,I+1) = P(J,I) + PT(I) * (P(J,I+2) - P(J,I))
      end do
    end do
    do J = JST, JMAX, 2
      do I = IMIN, IM2, 2
        P(J,I+1) = P(J,I) + PT(I) * (P(J,I+2) - P(J,I))
      end do
    end do
    do J = JMIN, JM2
      PT(J) = (Y(J+1)-Y(J)) / (Y(J+2)-Y(J))
    end do
    do I = IMIN, IMAX
      do J = JMIN, JL, 2
        P(J+1,I) = P(J,I) + PT(J) * (P(J+2,I) - P(J,I))
      end do
      do J = JST, JM2, 2
        P(J+1,I) = P(J,I) + PT(J) * (P(J+2,I) - P(J,I))
      end do
    end do

    ! Use extrapolation for JLOW, JUP
    D1 = Y(JLOW) - Y(JLOW-1)
    D2 = Y(JLOW-1) - Y(JLOW-2)
    CL1 = (D1 + D2) / D2
    CL2 = D1/D2
    D1 = Y(JUP+1) - Y(JUP)
    D2 = Y(JUP+2) - Y(JUP+1)
    CU1 = (D1 + D2) / D2
    CU2 = D1 / D2
    do I = IMIN, IMAX
      P(JUP,I) = CU1*P(JUP+1,I) - CU2*P(JUP+2,I)
      P(JLOW,I) = CL1*P(JLOW-1,I) - CL2*P(JLOW-2,I)
    end do

    ! Expand viscous wedge slopes to new grid
    if (NWDGE == 0) return
    INC = 0
    if (X(ILE) < XLEO_LOCAL) INC = 1
    M = 0
    do while (M < 2)
      M = M + 1
      do I = IMIN, IMAX
        PT(I) = 0.0
      end do
      ISTEP = ILEO - 1
      ISTRT = ILE + INC
      IEND = ITE + INC
      do I = ISTRT, IEND, 2
        ISTEP = ISTEP + 1
        PT(I) = WSLP(ISTEP,M)
      end do
      do I = ISTRT, IEND, 2
        IM = I - 1
        IMM = IM - 1
        WSLP(I,M) = PT(I)
        RATIO = (X(IM)-X(IMM))/(X(I)-X(IMM))
        WSLP(IM,M) = PT(IMM)+(PT(I)-PT(IMM))*RATIO
      end do
      if (M >= 2) exit
    end do
  end subroutine REFINE



end module no_use_subroutines
