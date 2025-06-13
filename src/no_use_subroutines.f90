! no_use_subroutines.f90
! This file contains subroutines and functions that are defined but not called in the current TSFoil modernized codebase

module no_use_subroutines
  implicit none
  
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

end module no_use_subroutines
