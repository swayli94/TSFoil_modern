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

  ! SAVEP: Save solution
  ! UNUSED: Never called in current implementation
  ! SAVEP moves data into old data locations and writes it on tape if requested
  subroutine SAVEP()
    use common_data, only: P, X, IMIN, IMAX, JMIN, JMAX, TITLE, TITLEO
    use common_data, only: YIN, ALPHA, H, POR, YFACT, VFACT
    use common_data, only: ALPHAO, CLOLD, DELTAO, DUBO, EMACHO, VOLO
    use common_data, only: IMINO, IMAXO, JMINO, JMAXO, XOLD, YOLD
    use common_data, only: CL, EMACH, DELTA, VOL, DUB, PSAVE, UNIT_OUTPUT
    implicit none
    integer :: I, J
    
    ! Reset parameters scaled in subroutine SCALE (matches original exactly)
    ALPHA = ALPHA * VFACT
    H = H * YFACT
    POR = POR / YFACT
    do J = JMIN, JMAX
      YIN(J) = YIN(J) * YFACT
    end do
    
    ! Move restart data to old block (matches original exactly)
    do I = 1, 20
      TITLEO(I) = TITLE(I)
    end do
    IMINO = IMIN
    JMINO = JMIN
    IMAXO = IMAX
    JMAXO = JMAX
    CLOLD = CL
    EMACHO = EMACH
    ALPHAO = ALPHA
    DELTAO = DELTA
    VOLO = VOL
    DUBO = DUB
    
    do I = IMINO, IMAXO
      XOLD(I) = X(I)
    end do
    
    do J = JMINO, JMAXO
      YOLD(J) = YIN(J)
    end do
    
    ! Check to see if restart is to be written (matches original logic)
    if (.not. PSAVE) return
    
    ! Write restart data to unit 15 (matching original exactly)
    write(UNIT_OUTPUT, '(20A4)') TITLEO
    write(UNIT_OUTPUT, '(4I5)') IMAXO, JMAXO, IMINO, JMINO
    write(UNIT_OUTPUT, '(8F10.6)') CLOLD, EMACHO, ALPHAO, DELTAO, VOLO, DUBO
    write(UNIT_OUTPUT, '(8F10.6)') (XOLD(I), I=IMINO, IMAXO)
    write(UNIT_OUTPUT, '(8F10.6)') (YOLD(J), J=JMINO, JMAXO)
    do I = IMINO, IMAXO
      write(UNIT_OUTPUT, '(8F10.6)') (P(J,I), J=JMINO, JMAXO)
    end do    
  end subroutine SAVEP

  ! PRINT1_OLD: Print Cp and Mach along body and build plot arrays
  ! SIMPLIFIED: Use simplified version in io_module.f90
  ! Print Cp and Mach along body and build plot arrays
  ! Prints pressure coefficient and Mach number on Y=0 line, and plots CP along side of print
  subroutine PRINT1_OLD()
    use common_data
    use math_module, only: PX, EMACH1, LIFT, PITCH
    implicit none
    
    ! Local variables exactly matching original - renamed to avoid conflicts
    integer :: I_P1, K_P1, NCOL_P1, NCOLS_P1, NCOLU_P1, NCOLL_P1, IEM, KT_P1, IPLOT_P1
    real :: CL_val, CM, CPMIN_P1, CPMAX_P1, CPLARG_P1, UNPCOL_P1, COL_P1
    real :: UL_P1, UU_P1, CJ01, CJ02
    real :: EM1L(N_MESH_POINTS), EM1U(N_MESH_POINTS), YM(N_MESH_POINTS)
    character(len=1) :: LINE1_P1(60)
    character(len=2) :: TMAC_P1(2)
    character(len=1), parameter :: IB_P1 = ' ', IL_P1 = 'L', IU_P1 = 'U', IS_P1 = '*', IBB_P1 = 'B'
    
    ! Initialize data arrays exactly like original
    TMAC_P1(1) = 'M1'
    TMAC_P1(2) = 'K1'
    
    ! Compute coefficients exactly like original
    CL_val = LIFT(CLFACT)
    CM = PITCH(CMFACT)
    CPMIN_P1 = 1.0E37
    CPMAX_P1 = -CPMIN_P1
    IEM = 0
    CJ01 = -Y(JLOW)/(Y(JUP)-Y(JLOW))
    CJ02 = Y(JUP)/(Y(JUP)-Y(JLOW))
    
    ! Main computation loop exactly matching original logic
    do I_P1 = IMIN, IMAX
      UL_P1 = CJLOW*PX(I_P1,JLOW) - CJLOW1*PX(I_P1,JLOW-1)
      if (I_P1 > ITE) UL_P1 = CJ01*PX(I_P1,JUP) + CJ02*PX(I_P1,JLOW)
      if (I_P1 < ILE) UL_P1 = CJ01*PX(I_P1,JUP) + CJ02*PX(I_P1,JLOW)
      CPL(I_P1) = -2.0 * UL_P1 * CPFACT
      EM1L(I_P1) = EMACH1(UL_P1)
      if (EM1L(I_P1) > 1.3) IEM = 1
      
      UU_P1 = CJUP*PX(I_P1,JUP) - CJUP1*PX(I_P1,JUP+1)
      if (I_P1 > ITE) UU_P1 = UL_P1
      if (I_P1 < ILE) UU_P1 = UL_P1
      CPU(I_P1) = -2.0 * UU_P1 * CPFACT
      EM1U(I_P1) = EMACH1(UU_P1)
      if (EM1U(I_P1) > 1.3) IEM = 1
      
      CPMAX_P1 = max(CPMAX_P1, CPU(I_P1), CPL(I_P1))
      CPMIN_P1 = min(CPMIN_P1, CPU(I_P1), CPL(I_P1))
    end do
    
    CPLARG_P1 = max(CPMAX_P1, abs(CPMIN_P1))
    UNPCOL_P1 = CPLARG_P1 / 29.0
    
    ! Locate CP* for printer plot exactly like original
    COL_P1 = -CPSTAR / UNPCOL_P1
    NCOL_P1 = sign(int(abs(COL_P1) + 0.5), nint(COL_P1))
    NCOLS_P1 = NCOL_P1 + 30
      ! Print single variables using exact original format
    write(UNIT_OUTPUT, '(A, /, 2X, A)') &
          '1 FORCE COEFFICIENTS, PRESSURE COEFFICIENT, AND MACH NUMBER', &
          '(OR SIMILARITY PARAMETER) ON BODY AND DIVIDING STREAM LINE.'
    write(UNIT_OUTPUT, '(20X,11H FINAL MESH)')
    
    write(UNIT_OUTPUT, '(1H0,9X,4HCL =,F10.6/10X,4HCM =,F10.6/9X,5HCP* =,F10.6)') CL_val, CM, CPSTAR
    write(UNIT_SUMMARY, '(1H0,9X,4HCL =,F16.12/10X,4HCM =,F16.12/9X,5HCP* =,F16.12)') CL_val, CM, CPSTAR
    
    ! Check for detached shock - exactly like original with GO TO 70 logic
    if (CPL(IMIN) < CPSTAR .and. CPL(IMIN+1) > CPSTAR) then
      write(UNIT_OUTPUT, '(A, //, A)') '0', &
           ' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.'
      return
    end if
    
    ! Print column headers with exact original formatting
    write(UNIT_OUTPUT, '(A, 27X, A, 23X, A, /, 28X, A, 24X, A)') '0', 'LOWER', 'UPPER', 'Y=0-', 'Y=0+'
    
    KT_P1 = 2
    if (PHYS) KT_P1 = 1
    write(UNIT_OUTPUT, '(3X, A, 8X, A, 10X, A, 10X, A2, 14X, A, 10X, A2, /)') 'I', 'X', 'CP', TMAC_P1(KT_P1), 'CP', TMAC_P1(KT_P1)
      IPLOT_P1 = 0
      
    ! Main output loop with exact original logic
    do I_P1 = IMIN, IMAX

      ! Initialize line array 
      do K_P1 = 1, 60
        LINE1_P1(K_P1) = IB_P1
      end do
      
      ! Plot upper surface CP with bounds checking
      COL_P1 = -CPU(I_P1) / UNPCOL_P1
      NCOL_P1 = sign(int(abs(COL_P1) + 0.5), nint(COL_P1))
      NCOLU_P1 = NCOL_P1 + 30
      if (NCOLU_P1 >= 1 .and. NCOLU_P1 <= 60) LINE1_P1(NCOLU_P1) = IU_P1
      
      ! Plot lower surface CP with bounds checking
      COL_P1 = -CPL(I_P1) / UNPCOL_P1
      NCOL_P1 = sign(int(abs(COL_P1) + 0.5), nint(COL_P1))
      NCOLL_P1 = NCOL_P1 + 30
      if (NCOLL_P1 >= 1 .and. NCOLL_P1 <= 60) LINE1_P1(NCOLL_P1) = IL_P1
      if (NCOLL_P1 == NCOLU_P1 .and. NCOLL_P1 >= 1 .and. NCOLL_P1 <= 60) LINE1_P1(NCOLL_P1) = IBB_P1
      if (abs(NCOLS_P1) < 61 .and. NCOLS_P1 >= 1 .and. NCOLS_P1 <= 60) LINE1_P1(NCOLS_P1) = IS_P1
        ! Print leading edge marker exactly like original
      if (I_P1 == ILE) write(UNIT_OUTPUT, '(25X, A, 45X, A)') 'AIRFOIL LEADING EDGE', 'AIRFOIL LEADING EDGE'
      
      ! Print main data line with exact original format
      write(UNIT_OUTPUT, '(1H ,I3,3F12.6,4X,2F12.6,2X,60A1)') &
            I_P1, X(I_P1), CPL(I_P1), EM1L(I_P1), CPU(I_P1), EM1U(I_P1), LINE1_P1
      
      ! Print trailing edge marker exactly like original
      if (I_P1 == ITE) write(UNIT_OUTPUT, '(25X, A, 44X, A)') 'AIRFOIL TRAILING EDGE', 'AIRFOIL TRAILING EDGE'
      
    end do
        ! Mach number warning exactly like original
    if (IEM == 1) then
      if (PHYS) then
        write(UNIT_OUTPUT, '(A, /, A, /, A, A)') &
              '0***** CAUTION *****', &
              ' MAXIMUM MACH NUMBER EXCEEDS 1.3', &
              ' SHOCK JUMPS IN ERROR IF UPSTREAM NORMAL MACH NUMBER GREATER T', &
              'HAN 1.3'
      end if
    end if
      
    ! Print coordinate arrays exactly like original
    do I_P1 = JMIN, JMAX
      YM(I_P1) = Y(I_P1) * YFACT
    end do
    write(UNIT_OUTPUT, '(1H0//9X,7HY(J) J=,I3,3H TO,I3/(6X,6F12.6))') JMIN, JMAX, (YM(I_P1), I_P1=JMIN, JMAX)
    write(UNIT_OUTPUT, '(1H0//9X,7HX(I) I=,I3,3H TO,I3/(6X,6F12.6))') IMIN, IMAX, (X(I_P1), I_P1=IMIN, IMAX)
    write(UNIT_OUTPUT, '(1H0//9X,7HY(J) J=,I3,3H TO,I3/(6X,6F12.6))') JMIN, JMAX, (YM(I_P1), I_P1=JMIN, JMAX)
    
    ! Output airfoil surface data
    call OUTPUT_CP_MACH_XLINE(CL_val, CM, EM1L, EM1U)

    call OUTPUT_MESH()
    
  end subroutine PRINT1_OLD

  ! Print map of flow types at each grid point
  ! PRTMC - Print flow type map at each grid point
  ! UNUSED: Replaced by OUTPUT_FIELD in io_module.f90
  subroutine PRTMC()
    use common_data, only: P, IUP, IDOWN, JMIN, JMAX, IPC, VT, C1, CXL, CXC, CXR, UNIT_OUTPUT
    implicit none    
    integer :: I, J, K
    character(len=1), parameter :: ch_par = 'P'    ! Parabolic (sonic)
    character(len=1), parameter :: ch_hyp = 'H'    ! Hyperbolic (supersonic)  
    character(len=1), parameter :: ch_shock = 'S'  ! Shock point
    character(len=1), parameter :: ch_ell = '-'    ! Elliptic (subsonic)
    character(len=1), parameter :: ch_blank = ' '  ! Blank

    ! Print header
    write(UNIT_OUTPUT, '(A,/,28X,A,/,28X,A,/,28X,A,//)')  &
      '1 FLOW AT EACH GRID POINT.  P PARABOLIC',   &
      'H HYPERBOLIC',                              &
      'S SHOCK',                                   &
      '- ELLIPTIC'

    ! Initialize IPC array
    do I = 1, 50, 2
      IPC(I) = ch_blank
      IPC(I+1) = ch_blank
    end do

    ! Initialize VT array
    do J = JMIN, JMAX
      VT(J,1) = C1(2)
    end do

    ! Main classification loop
    do K = JMIN, JMAX
      J = JMAX - K + 1
      do I = IUP, IDOWN
        VT(J,2) = VT(J,1)
        VT(J,1) = C1(I) - (CXL(I)*P(J,I-1) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
        
        ! Flow type classification using original logic
        if (VT(J,1) > 0.0) then
          if (VT(J,2) < 0.0) then
            ! Shock point
            IPC(I) = ch_shock
          else
            ! Elliptic point (subsonic)
            IPC(I) = ch_ell
          end if
        else
          if (VT(J,2) < 0.0) then
            ! Hyperbolic point (supersonic)
            IPC(I) = ch_hyp
          else
            ! Parabolic point (sonic)
            IPC(I) = ch_par
          end if
        end if
      end do
      
      ! Write line
      write(UNIT_OUTPUT, '(10X,I3,5X,100A1)') J, (IPC(I), I=IUP, IDOWN)
    end do
    
  end subroutine PRTMC

  ! FIXPLT - Sets up arrays for CPPLOT subroutine
  ! UNUSED: Replaced by OUTPUT_CP_MACH_XLINE in io_module.f90
  subroutine FIXPLT()
    use common_data, only: IMIN, IMAX, CPFACT, CPSTAR, CPU, CPL, X
    use common_data, only: NMP_plus1
    implicit none
    
    ! Local variables matching original
    real :: YMX, YMN, QCP, QC1, QC2
    integer :: K, I, IMP
    real :: CPUP(NMP_plus1), CPLO(NMP_plus1), CPS(NMP_plus1), XP(NMP_plus1)
    
    YMX = 5.0 * CPFACT
    YMN = -5.0 * CPFACT
    K = 0
    
    do I = IMIN, IMAX
      K = K + 1
      QCP = -CPU(I)
      QCP = max(QCP, YMN)
      QCP = min(QCP, YMX)
      CPUP(K) = QCP
      
      QC1 = -CPL(I)
      QC1 = max(QC1, YMN)
      QC1 = min(QC1, YMX)
      CPLO(K) = QC1
      
      QC2 = -CPSTAR
      QC2 = max(QC2, YMN)
      QC2 = min(QC2, YMX)
      CPS(K) = QC2
      
      XP(K) = X(I)
    end do
    
    IMP = K + 1
    CPUP(IMP) = YMX
    CPLO(IMP) = YMN
    CPS(IMP) = 0.0
    XP(IMP) = X(IMAX) + 0.001
    
    call CPPLOT(XP, CPUP, CPLO, CPS, IMP)
    
  end subroutine FIXPLT

  ! CPPLOT - Produces a printer plot of critical pressure vs X
  ! UNUSED: Replaced by OUTPUT_CP_MACH_XLINE in io_module.f90
  ! SUBROUTINE CPPLOT PRODUCES A PRINTER PLOT OF CRITICAL PRESSURE VS X
  ! CALLED BY - FIXPLT.
  subroutine CPPLOT(X_ARR, Y_ARR, Z_ARR, W_ARR, NP)
    use common_data, only: AMESH, UNIT_OUTPUT
    use common_data, only: NMP_plus1
    implicit none
    
    ! Arguments
    integer, intent(in) :: NP
    real, intent(in) :: X_ARR(NMP_plus1), Y_ARR(NMP_plus1), Z_ARR(NMP_plus1), W_ARR(NMP_plus1)
    
    ! Local variables exactly matching original
    integer :: M(120), ISYM(8)
    integer :: IC(3)
    real :: A(3)
    integer :: NC, NR, NPL, NPR, NL5
    real :: HL, HR, VB, VT_CPPLOT, VDEL, HDEL, HDELM, VL, VH
    integer :: IROW, I, J, K
    
    ! Data initialization matching original exactly
    IC(1) = 1
    IC(2) = 1024
    IC(3) = 1048576
    
    ISYM(1) = 32  ! ' '
    ISYM(2) = 85  ! 'U'
    ISYM(3) = 76  ! 'L'
    ISYM(4) = 66  ! 'B'
    ISYM(5) = 45  ! '-'
    ISYM(6) = 85  ! 'U'
    ISYM(7) = 76  ! 'L'  
    ISYM(8) = 66  ! 'B'
    
    ! NC is the number of columns, NR is the number of rows
    NC = 120
    NR = 50
    
    ! Initialize ranges exactly like original
    if (AMESH) then
      NPL = 2
      NPR = NP - 2
      NL5 = 3
    else
      NPL = 1
      NPR = NP - 1
      NL5 = 2
    end if
    
    HL = X_ARR(NPL)
    HR = X_ARR(NPL)
    VB = min(Y_ARR(1), Z_ARR(1), W_ARR(1))
    VT_CPPLOT = max(Y_ARR(1), Z_ARR(1), W_ARR(1))
    
    ! Determine ranges exactly like original
    do I = NL5, NPR
      HL = min(HL, X_ARR(I))
      HR = max(HR, X_ARR(I))
      VB = min(VB, Y_ARR(I), Z_ARR(I), W_ARR(I))
      VT_CPPLOT = max(VT_CPPLOT, Y_ARR(I), Z_ARR(I), W_ARR(I))
    end do
    
    ! Skip to new page and write plot heading exactly like original
    write(UNIT_OUTPUT, '(A1, 34X, A50, //, 9X, A17, 5X, A17, 5X, A27, 5X, A18)') &
      '1', &
      'PRINTER PLOT OF CP ON BODY AND DIVIDING STREAMLINE', &
      'U  FOR CP(UPPER)', &
      'L  FOR CP(LOWER)', &
      'B  FOR CP(UPPER)=CP(LOWER)', &
      '---  FOR CP SONIC'
    
    VDEL = (VT_CPPLOT - VB) / real(NR)
    HDEL = (HR - HL) / real(NC)
    HDELM = 1.0 / HDEL
    VL = VT_CPPLOT
    
    ! Main plotting loop exactly like original
    do IROW = 1, NR
      VH = VL
      VL = real(NR - IROW) * VDEL + VB
      
      ! Initialize M array
      do I = 1, NC
        M(I) = 0
      end do
      
      ! Process each data point exactly like original
      do I = NPL, NPR
        J = max(1, min(NC, 1 + int((X_ARR(I) - HL) * HDELM)))
        A(1) = Y_ARR(I)
        A(2) = Z_ARR(I)
        A(3) = W_ARR(I)
        
        do K = 1, 3
          if (A(K) > VH) cycle
          if (A(K) > VL .or. (A(K) <= VB .and. IROW == NR)) then
            M(J) = M(J) + IC(K)
          end if
        end do
      end do
      
      ! Convert to symbols exactly like original
      do I = 1, NC
        J = 1
        if (M(I) >= IC(3)) then
          J = J + 4
          M(I) = mod(M(I), IC(3))
        end if
        if (M(I) >= IC(2)) then
          J = J + 2
          M(I) = mod(M(I), IC(2))
        end if
        if (M(I) > 0) then
          J = J + 1
        end if
        M(I) = ISYM(J)
      end do
      
      ! Write line exactly like original
      write(UNIT_OUTPUT, '(1X, 120A1)') (char(M(I)), I=1, NC)
    end do
    
  end subroutine CPPLOT

  ! Prints coordinates where sonic velocity is computed
  ! Linear interpolation between mesh points is used
  ! Called by - PRINT.
  ! UNUSED: Replaced by OUTPUT_CP_MACH_XLINE in io_module.f90
  subroutine M1LINE()
    use common_data, only: X, Y, IMIN, IMAX, JMIN, JMAX, JLOW
    use common_data, only: AK, SONVEL, YFACT, BCTYPE
    use common_data, only: UNIT_OUTPUT, N_MESH_POINTS
    use math_module, only: PX
    implicit none
    
    real :: XSLPRT(2*N_MESH_POINTS), YSLPRT(2*N_MESH_POINTS)
    real :: XSONIC(10)
    real :: YPR, PX1, PX2, RATIO
    real :: XMIN, XMAX, XINCR, YMIN, YMAX, YINCR
    real :: YM, YX
    integer :: NPTS, J, M, I, L, N
    
    NPTS = 0  ! Number of sonic points
    
    do J = JMAX, JMIN, -1

      ! J is the index of the J-line (constant Y) in the mesh
      ! M is the number of sonic points
      ! I is the index of the current point in the X direction
      ! N is the index of the current point in the X direction

      YPR = YFACT * Y(J)
      PX2 = PX(IMIN, J)
      M = 0
      if (J == JLOW .and. NPTS /= 0) then
        write(UNIT_OUTPUT, '(2X,"BODY LOCATION")')
      end if
      
      do I = IMIN + 1, IMAX

        PX1 = PX2
        PX2 = PX(I, J)
        
        if (PX1 > SONVEL .and. PX2 > SONVEL) cycle
        if (PX1 < SONVEL .and. PX2 < SONVEL) cycle
        if (NPTS == 0) write(UNIT_OUTPUT, &
          '("1SONIC LINE COORDINATES", /, 6X, "Y", 10X, "XSONIC", /, /)')
        
        M = M + 1
        RATIO = (SONVEL - PX1) / (PX2 - PX1)
        XSONIC(M) = X(I-1) + (X(I) - X(I-1)) * RATIO
        NPTS = NPTS + 1
        XSLPRT(NPTS) = XSONIC(M)
        YSLPRT(NPTS) = YPR
        if (NPTS >= 2*N_MESH_POINTS) then
          write(UNIT_OUTPUT, '("0***** CAUTION *****",&
           &" NUMBER OF SONIC POINTS EXCEEDED 2*N_MESH_POINTS",&
           &" ARRAY DIMENSION EXCEEDED",&
           &" EXECUTION OF SUBROUTINE M1LINE TERMINATED")')
          return
        end if
      end do
      
      if (M == 0) cycle
      write(UNIT_OUTPUT, '(11F10.5)') YPR, (XSONIC(L), L=1, M)
    end do
    
    ! Process results if any sonic points were found
    if (NPTS == 0) return
    
    YM = Y(JMIN)
    YX = Y(JMAX)
    do N = 1, NPTS
      if (YSLPRT(N) /= YM .and. YSLPRT(N) /= YX) cycle

        if (AK > 0.0) write(UNIT_OUTPUT, '("0***** CAUTION *****", &
          &" SONIC LINE HAS REACHED A BOUNDARY", &
          &" THIS VIOLATES ASSUMPTIONS USED TO DERIVE BOUNDARY CONDITIONS", &
          &" SOLUTION IS PROBABLY INVALID")')

      if (AK < 0.0 .and. BCTYPE == 1) write(UNIT_OUTPUT, '("0***** CAUTION *****",&
           &" SONIC LINE HAS REACHED A BOUNDARY",&
           &" THIS VIOLATES ASSUMPTIONS USED TO DERIVE BOUNDARY CONDITIONS",&
           &" SOLUTION IS PROBABLY INVALID")')
    end do
    
    XMIN = -0.75
    XMAX = 1.75
    XINCR = 0.25
    YMIN = -1.0
    YMAX = 1.5
    YINCR = 0.5
    
    call PLTSON(XSLPRT, YSLPRT, XMIN, XMAX, XINCR, YMIN, YMAX, YINCR, NPTS)

  end subroutine M1LINE
  
  ! PLTSON - Sonic line printer plot routine 
  ! This is a modified version of PRNPLT and XMAX, XINCR, YMAX, YINCR must be supplied.
  ! XMIN, and YMIN must also be supplied.
  ! Printer plot routine M. S. ITZKOWITZ MAY,1967
  ! Plots the NPTS points given by X(I),Y(I) on a 51 X NMP_plus1 grid using a total of 56 lines on
  ! the printer. If either incremental step size is zero, the program exits.
  ! Neither of the input arrays are destroyed.
  ! Called by - M1LINE.
  ! UNUSED: Replaced by OUTPUT_CP_MACH_XLINE in io_module.f90
  subroutine PLTSON(X_ARG, Y_ARG, XAXMIN, XMAX_ARG, XINCR_ARG, YAXMIN, YMAX_ARG, YINCR_ARG, NPTS_ARG)
    use common_data, only: UNIT_OUTPUT
    implicit none
    
    ! Arguments
    integer, intent(in) :: NPTS_ARG
    real, intent(in) :: X_ARG(NPTS_ARG), Y_ARG(NPTS_ARG)
    real, intent(in) :: XAXMIN, XMAX_ARG, XINCR_ARG, YAXMIN, YMAX_ARG, YINCR_ARG
    
    ! Local variables
    integer, dimension(105) :: IGRID
    real, dimension(11) :: XAXIS
    character(len=1), parameter :: BLANK = ' ', DOT = '.', STAR = '*', DASH = '-'
    integer, parameter :: IBS(9) = [ichar('B'),ichar('O'),ichar('D'),ichar('Y'),ichar(' '),&
                                    ichar('S'),ichar('L'),ichar('I'),ichar('T')]
    real :: YRNGE, XRNGE, YVAL, XVAL, OXV, OYV, FIZERO, FJZERO, FIZER5, FJZER5
    integer :: JSLT, IZERO, JZERO, NOPW, JS, I, J, K, L, M, ITEST
    real :: FM, FI, YAXIS
    
    write(UNIT_OUTPUT, '(''1'')')
    write(UNIT_OUTPUT, '(35X,15HSONIC LINE PLOT,10X,6HY VS X,10X,20H *  FOR SONIC POINTS//)')

    if (XINCR_ARG == 0.0 .or. YINCR_ARG == 0.0) then
      write(UNIT_OUTPUT, '(46H1SCALING ERROR IN PRNPLT, EXECUTION TERMINATED )')
      stop
    end if
    
    YRNGE = YMAX_ARG - YAXMIN
    XRNGE = XMAX_ARG - XAXMIN
    YVAL = YRNGE * 0.02
    XVAL = XRNGE * 0.01
    JSLT = int(51.0 * (YMAX_ARG / YRNGE)) + 1
    OXV = 1.0 / XVAL
    OYV = 1.0 / YVAL
    IZERO = int(YMAX_ARG * OYV + 1.5)
    JZERO = int(103.5 - XMAX_ARG * OXV)
    if (JZERO > 103 .or. JZERO < 4) JZERO = 2
    FIZERO = real(IZERO)
    FJZERO = real(JZERO)
    IGRID(1) = ichar(BLANK)
    IGRID(2) = ichar(DOT)
    IGRID(104) = ichar(DOT)
    IGRID(105) = ichar(BLANK)
    FIZER5 = FIZERO + 0.5
    FJZER5 = FJZERO + 0.5
    
    ! NOPW is the number of print wheels used to span the body length.
    NOPW = int(OXV)
    ! JS - the number of prt wheel positions to be filled at each end of the word "BODY SLIT"
    JS = 0
    if (NOPW > 9) JS = (NOPW - 9) / 2

    write(UNIT_OUTPUT, '(16X,11(1H+,9X))')
    write(UNIT_OUTPUT, '(15X,103(1H.))')
    
    ! Loop to set up one line each time thru.
    do I = 1, 51
      ! Blank the line to be printed.
      do J = 3, 103
        IGRID(J) = ichar(BLANK)
      end do

      ! Search array for points on this line.
      do K = 1, NPTS_ARG
        ITEST = int(FIZER5 - Y_ARG(K) * OYV)
        if (ITEST /= I) then
          if (JSLT == I) then
            ! Write "BODY SLIT" if this is the J0 line.
            J = JZERO + 1
            if (JS /= 0) then
              do L = 1, JS
                if (IGRID(J) /= ichar(STAR)) IGRID(J) = ichar(DASH)
                J = J + 1
              end do
            end if
            
            do L = 1, 9
              if (IGRID(J) /= ichar(STAR)) IGRID(J) = IBS(L)
              J = J + 1
            end do
            
            if (JS /= 0) then
              do L = 1, JS
                if (IGRID(J) /= ichar(STAR)) IGRID(J) = ichar(DASH)
                J = J + 1
              end do
            end if
          end if
          cycle
        end if
        
        J = int(FJZER5 + X_ARG(K) * OXV)
        if (J > 103) J = 103
        if (J < 3) J = 3
        IGRID(J) = ichar(STAR)
        
        if (JSLT == I) then
          ! Write "BODY SLIT" if this is the J0 line.
          J = JZERO + 1
          if (JS /= 0) then
            do L = 1, JS
              if (IGRID(J) /= ichar(STAR)) IGRID(J) = ichar(DASH)
              J = J + 1
            end do
          end if
          
          do L = 1, 9
            if (IGRID(J) /= ichar(STAR)) IGRID(J) = IBS(L)
            J = J + 1
          end do
          
          if (JS /= 0) then
            do L = 1, JS
              if (IGRID(J) /= ichar(STAR)) IGRID(J) = ichar(DASH)
              J = J + 1
            end do
          end if
        end if
      end do
      
      if (mod(I, 10) == 1) then
        FI = real(I - 1)
        YAXIS = YMAX_ARG - FI * YINCR_ARG * 0.1
        if (abs(YAXIS) < YAXMIN) YAXIS = 0.0
        write(UNIT_OUTPUT, '(1X,F10.1,2X,1H+,105A1,1H+)') YAXIS, (char(IGRID(J)), J=1, 105)
      else
        write(UNIT_OUTPUT, '(14X,105A1)') (char(IGRID(J)), J=1, 105)
      end if
    end do
    
    write(UNIT_OUTPUT, '(15X,103(1H.))')
    write(UNIT_OUTPUT, '(16X,11(1H+,9X))')
    
    do M = 1, 11
      FM = real(11 - M)
      XAXIS(M) = XMAX_ARG - XINCR_ARG * FM
      if (XAXIS(M) < XAXMIN) XAXIS(M) = XAXMIN
    end do
    
    write(UNIT_OUTPUT, '(7X,11(F10.2),2H (,I4,5H PTS) )') XAXIS, NPTS_ARG
    write(UNIT_OUTPUT, '(''1'')')
    
  end subroutine PLTSON

  ! Print Cp, flow angle (theta), and Mach number on selected j-lines
  ! Prints pressure coefficient, flow angle and Mach number in flow field.
  ! Number of J lines printed is determined from the input value of PRTFLO.
  ! PRTFLO = 1, NONE.
  ! PRTFLO = 2, ALL J LINES EXCEPT J0.
  ! PRTFLO = 3, THREE J LINES AROUND JERROR.
  ! UNUSED: Never called in current implementation
  subroutine PRTFLD()
    use common_data, only: X, Y, JMIN, JMAX, JUP, JLOW, JERROR, CPFACT, VFACT, CPSTAR
    use common_data, only: JLIN, IMIN, IMAX, PHYS, PRTFLO, UNIT_OUTPUT
    use math_module, only: PX, PY, EMACH1
    implicit none
    integer :: JL, MPR, MPREND, M, MQ, I, J, K, IS, IE, KT
    real :: U
    real, dimension(3) :: YPRINT, CPPR, PYPR, EM1
    character(len=4), dimension(10), parameter :: PRT = (/ &
      'MACH', ' NUM', 'BERS', '    ', '    ', &
      'SIMI', 'LARI', 'TY P', 'ARAM', 'ETER' /)
    character(len=2), dimension(2), parameter :: TMAC = (/ 'M1', 'K1' /)

    ! Skip if PRTFLO = 1
    if (PRTFLO == 1) return

    ! Determine which lines to print
    if (PRTFLO == 2) then
      ! Print all J lines except J0
      JL = JMAX - JMIN + 1
      K = 1
      do J = JMIN, JMAX
        JLIN(K) = J
        K = K + 1
      end do
      
    else
      ! PRTFLO = 3: Locate three lines around JERROR
      JL = 3
      if (JERROR == JMIN .or. JERROR == JUP) then
        JLIN(1) = JERROR
        JLIN(2) = JERROR + 1
        JLIN(3) = JERROR + 2
      else if (JERROR == JLOW .or. JERROR == JMAX) then
        JLIN(1) = JERROR - 2
        JLIN(2) = JERROR - 1
        JLIN(3) = JERROR
      else
        JLIN(1) = JERROR - 1
        JLIN(2) = JERROR
        JLIN(3) = JERROR + 1
      end if
    end if

    ! Print flow field in 3 J lines per page
    do MPR = 1, JL, 3
      MPREND = min(MPR+2, JL)
      do M = MPR, MPREND
        MQ = M - MPR + 1
        J = JLIN(M)
        YPRINT(MQ) = Y(J) * VFACT
      end do

      ! Write page header
      IS = 1
      if (PHYS) IS = 6
      IE = IS + 4
      
      write(UNIT_OUTPUT,'(A,5A4,A)') &
        'PRESSURE COEFFICIENTS, FLOW ANGLES, AND LOCAL ', &
        (PRT(I), I=IS,IE), ' ON Y=CONSTANT LINES'
      write(UNIT_OUTPUT,'(A,F12.7)') ' CPSTAR =', CPSTAR
      write(UNIT_OUTPUT,*)
      write(UNIT_OUTPUT,'(13X,3(15X,A,I4,15X))') &
        ('J=', JLIN(M), M=MPR,MPREND)
      write(UNIT_OUTPUT,'(13X,3(12X,A,F10.6,12X))') &
        ('Y=', YPRINT(M), M=1,MPREND-MPR+1)
      
      KT = 2
      if (PHYS) KT = 1
      write(UNIT_OUTPUT,'(A,8X,A,5X,3(6X,A,8X,A,7X,A,6X))') &
        '  I', 'X', ('CP', 'THETA', TMAC(KT), M=1,MPREND-MPR+1)
      write(UNIT_OUTPUT,*)
      
      do I = IMIN, IMAX
        do M = MPR, MPREND
          MQ = M - MPR + 1
          J = JLIN(M)
          U = PX(I, J)
          CPPR(MQ) = -2.0 * CPFACT * U
          PYPR(MQ) = VFACT * PY(I, J)
          EM1(MQ) = EMACH1(U)
        end do
        write(UNIT_OUTPUT,'(1X,I3,2X,F10.6,1X,3(2X,3F11.6,1X))') &
              I, X(I), (CPPR(M), PYPR(M), EM1(M), M=1,MPREND-MPR+1)
      end do
    end do
    
    write(UNIT_OUTPUT,'(A,I0,A)') 'Field data written for ', JL, ' J-lines'
  end subroutine PRTFLD


end module no_use_subroutines
