! common_data.f90
! Module replacing legacy COMMON blocks for shared data

module common_data
  implicit none
  private

  public :: N_MESH_POINTS
  public :: IMIN, IMAX, IUP, IDOWN, ILE, ITE, JMIN, JMAX, JUP, JLOW, JTOP, JBOT
  public :: AK, ALPHA, PHYS
  public :: ABORT1
  public :: P, X, Y, XIN, YIN
  public :: FXL, FXU
  public :: RIGF
  public :: EMACH
  public :: CDFACT, CLFACT, CMFACT, CPFACT, CPSTAR
  public :: HALFPI, PI, TWOPI
  public :: IMAXI, JMAXI
  public :: BCTYPE
  public :: XDIFF, YDIFF
  public :: CJUP, CJUP1, CJLOW, CJLOW1
  public :: PJUMP, FCR, KUTTA
  public :: CLSET
  public :: EPS
  public :: GAM, GAM1
  public :: initialize_common, INPERR
  public :: UNIT_INPUT, UNIT_OUTPUT
  public :: UNIT_SUMMARY, UNIT_CPXS, UNIT_MESH, UNIT_FIELD
  
  ! Constants
  
  integer, parameter :: N_MESH_POINTS = 1000            ! Mesh size parameter - change this to adjust mesh dimensions
  integer, parameter :: NMP_plus2 = N_MESH_POINTS + 2   ! Number of mesh points + 2
  integer, parameter :: NMP_plus1 = N_MESH_POINTS + 1   ! Number of mesh points + 1

  integer, parameter :: IMIN = 1
  integer, parameter :: JMIN = 1

  real, parameter :: GAM = 1.4            ! Specific heat ratio
  real, parameter :: GAM1 = GAM + 1.0     ! gamma + 1
  real, parameter :: PI = 3.14159265      ! pi
  real, parameter :: HALFPI = 1.570796325 ! 1/2 pi
  real, parameter :: TWOPI = 6.28318531   ! 2 pi

  ! User-input parameters (global to many modules)
  ! Note: some user-input parameters are local to other modules

  real :: AK = 0.0        ! Free stream similarity parameter
  real :: ALPHA = 0.0     ! Angle of attack
  real :: EMACH = 0.75    ! Mach number
  integer :: BCTYPE = 1   ! Boundary condition identifiers (1 = free air, 2 = tunnel)
  real :: CLSET = 0.0     ! Lift coefficient setpoint

  real :: EPS = 0.2       ! Convergence tolerance 

  logical :: FCR = .true.   ! Whether difference equations are fully conservative
  logical :: PHYS = .true.  ! Physical (True) vs similarity (False)
  logical :: KUTTA = .true. ! Whether Kutta condition is enforced

  real :: RIGF = 0.0        ! Rigidity factor for transonic effects

  integer :: IMAXI, JMAXI   ! User-input maximum number of X/Y-direction grid points

  ! Mesh indices (from COMMON /COM1/)
  integer :: IMAX, JMAX  ! maximum number of grid points in X/Y-direction used in code
  integer :: IUP, IDOWN       ! upstream/downstream indices
  integer :: ILE, ITE         ! leading/trailing edge i-indices
  integer :: JUP              ! upper surface j-indices, index of first point where Y > 0.0 (calculated by JSLIT)
  integer :: JLOW             ! lower surface j-indices, JLOW = JUP - 1 (calculated by JSLIT)
  integer :: JTOP, JBOT       ! far-field top/bottom j-indices

  ! Mesh coordinate arrays
  real :: X(NMP_plus2), Y(NMP_plus2)  ! Mesh coordinate arrays
  real :: XIN(NMP_plus2), YIN(NMP_plus2)  ! User-input mesh coordinate arrays
  real :: XDIFF(N_MESH_POINTS), YDIFF(N_MESH_POINTS) ! mesh derivative arrays

  ! Main solution arrays
  real :: P(NMP_plus2, NMP_plus1)    ! Potential solution array
  real :: FXU(N_MESH_POINTS), FXL(N_MESH_POINTS)

  ! Control flags and refinement (from /COM3/)
  logical :: ABORT1 ! input abort flag

    
  ! COM7: boundary extrapolation/coefficient flags
  real :: CJUP, CJUP1, CJLOW, CJLOW1
  
  ! COM13: coefficient scaling factors
  real :: CDFACT, CLFACT, CMFACT, CPFACT, CPSTAR

  ! COM19: jump arrays and pressure jump
  real :: PJUMP(N_MESH_POINTS)
      
  ! File unit numbers for different output files  
  integer, parameter :: UNIT_INPUT = 2          ! Input file
  integer, parameter :: UNIT_OUTPUT = 15        ! tsfoil2.out (Main output file with comprehensive results)
  integer, parameter :: UNIT_SUMMARY = 16       ! smry.out (Summary file with key results)
  integer, parameter :: UNIT_CPXS = 17          ! cpxs.out (Pressure coefficient vs. X-coordinate data)
  integer, parameter :: UNIT_MESH = 20          ! mesh.dat (Mesh coordinate data)
  integer, parameter :: UNIT_FIELD = 11         ! field.dat (Pressure coefficient and Mach number field data)

contains

  ! Initialize common data arrays and parameters
  subroutine initialize_common()
    implicit none

    ! Initialize mesh coordinate arrays
    X = 0.0
    Y = 0.0    
    XIN = 0.0
    YIN = 0.0

    ! Initialize potential array P
    P = 0.0

    ! Default initial values (will be overridden by READIN with IMAXI/JMAXI from input)
    IMAX = N_MESH_POINTS
    JMAX = N_MESH_POINTS
    
    ! Initialize mesh indices to safe defaults (will be recalculated later)
    IUP = 2
    IDOWN = IMAX - 1
    ILE = IMIN + 5  ! Safe default
    ITE = IMAX - 5  ! Safe default
    JUP = (JMAX + JMIN) / 2 + 1   ! Safe default above center
    JLOW = (JMAX + JMIN) / 2 - 1  ! Safe default below center
    JTOP = JMAX - 1
    JBOT = JMIN + 1

    ! Grid parameters (from BLOCK DATA)
    IMAXI = 77  ! User-input maximum number of streamwise (X-direction) grid points (match default XIN)
    JMAXI = N_MESH_POINTS  ! User-input maximum number of spanwise (Y-direction) grid points

    ! Logical flags (from BLOCK DATA)
    ABORT1 = .false.

  end subroutine initialize_common


  ! Fatal error - write message and stop
  subroutine INPERR(I_ERROR_CODE)
    implicit none
    integer, intent(in) :: I_ERROR_CODE
    
    select case (I_ERROR_CODE)
    case (1)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'IMAX OR JMAX IS GREATER THAN N_MESH_POINTS, NOT ALLOWED.'
    case (2)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'X MESH POINTS NOT MONOTONIC INCREASING.'
    case (3)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'Y MESH POINTS NOT MONOTONIC INCREASING.'
    case (4)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'MACH NUMBER NOT IN PERMITTED RANGE. (.5,2.0)'
    case (5)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'ALPHA NOT IN PERMITTED RANGE. (-9.0, 9.0)'
    case (6)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'DELTA NOT IN PERMITTED RANGE. ( 0.0, 1.0)'
    case (7)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'AK=0. VALUE OF AK MUST BE INPUT SINCE PHYS=F.'
    case (8)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'MACH NUMBER IS NOT LESS THAN 1.0 FOR VISCOUS WEDGE INCLUSION'
    case default
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'UNKNOWN ERROR CODE.'
    end select
    
    stop
  end subroutine INPERR


end module common_data
