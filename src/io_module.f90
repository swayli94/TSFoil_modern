! io_module.f90
! Module for input/output routines

module io_module
  use common_data
  implicit none

  ! Complete namelist matching the original /INP/ namelist exactly
  namelist /INP/ AK, ALPHA, AMESH, BCFOIL, BCTYPE, CLSET, &
                  CVERGE, DELTA, DVERGE, EMACH, EPS, F, &
                  FCR, GAM, H, ICUT, IMAXI, IMIN, &
                  IPRTER, JMAXI, JMIN, KUTTA, MAXIT, NL, &
                  NU, PHYS, POR, PRTFLO, PSAVE, &
                  PSTART, RIGF, SIMDEF, WCIRC, WE, &
                  XIN, YIN, XL, YL, XU, YU, &
                  NWDGE, REYNLD, WCONST, IFLAP, DELFLP, &
                  FLPLOC, IDLA
  
  ! Declare public procedures
  public :: READIN, SCALE, PRINT, PRINT1, PRTFLD, PRTMC, PRTSK, PRTWAL, SAVEP, GUESSP
  public :: open_output_files, close_output_files, CDCOLE, FIXPLT, CPPLOT, PLTSON, INPERR

contains
  
  ! Open all output files with unique file units
  subroutine open_output_files()
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY, UNIT_CNVG
    implicit none
    
    open(unit=UNIT_OUTPUT, file='tsfoil2.out', status='replace', action='write')   ! Unit 15
    open(unit=UNIT_SUMMARY, file='smry.out', status='replace', action='write')     ! Unit 16
    open(unit=UNIT_CNVG, file='cnvg.out', status='replace', action='write')        ! Unit 19
        
  end subroutine open_output_files

  ! Close all output files
  subroutine close_output_files()
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY, UNIT_CNVG
    implicit none

    close(UNIT_OUTPUT)   ! tsfoil2.out
    close(UNIT_SUMMARY)  ! smry.out
    close(UNIT_CNVG)     ! cnvg.out

  end subroutine close_output_files
  
  ! Main input reading routine - reads one case at a time and returns for processing
  ! Reads title card, namelist input, and manages restart data for current case  
  ! The original READIN is designed to be called once per case from main program
  subroutine READIN()
    use common_data
    use mesh_module, only: ISLIT, JSLIT, CKMESH, AYMESH
    implicit none    
    character(len=4), parameter :: DONE = 'FINI'  ! Declare DONE to match original exactly
    integer :: J_VAR, IM1, JM1, IDX, JDX
    real :: TERM, HTM, HTP, YS, YE, TIME1, TIME2, ELPTM
    character(len=100) :: IN_FILENAME
    integer :: ios
    
    ! Open output files and handle input file
    ! Handle command line argument for input file (match original exactly)
    call get_command_argument(1, IN_FILENAME)

    if (IN_FILENAME == '') then
      IN_FILENAME = 'tsfoil.inp'  ! Default input file name
    else
      write(*, '(A)') 'Using input file: ' // trim(IN_FILENAME)
    end if
    
    open(unit=UNIT_INPUT, file=trim(IN_FILENAME), status='old')
    
    call open_output_files()
    
    TIME1 = 0.0
    TIME2 = 0.0
    
    ! TIME2 = get_time()  ! Would need system-specific timing
    ELPTM = TIME2 - TIME1
    if (ELPTM >= 0.01) then
      write(UNIT_OUTPUT, '(25H0  TIME TO RUN CASE WAS  ,F6.2,9H SECONDS.)') ELPTM
    end if
    
    ! Read title card for this case
    read(UNIT_INPUT, '(20A4)', iostat=ios) TITLE
    
    ! Handle end-of-file or read error
    if (ios /= 0) then
        if (ios < 0) then
            ! End of file reached
            write(UNIT_OUTPUT, '(A)') 'End of input file reached. Program terminated.'
        else
            ! Read error
            write(UNIT_OUTPUT, '(A,I0)') 'Error reading title card. IOSTAT = ', ios
        end if
        stop
    end if
    
    ! Write title to output files exactly as original    
    write(UNIT_OUTPUT, '(1H1,4X,20A4)') TITLE
    
    ! Check for termination string exactly as original
    if (TITLE(1) == DONE) then
        stop  ! Terminate program exactly as original
    end if

    read(UNIT_INPUT, INP, iostat=ios)
    
    ! Check if namelist read was successful
    if (ios /= 0) then
        write(UNIT_OUTPUT, '(A)') 'Error reading namelist input. Please check the input file.'
        stop  ! Terminate on input error like original
    end if
    
    ! Print input namelist for debugging
    ! call PRINT_INP_NAMELIST()

    ! Handle PSTART=3 case - test if P array in core is usable (original check)
    if (PSTART == 3) then
      if (ABORT1) then
        write(UNIT_OUTPUT, '(21H0 CALCULATION ABORTED//43H OUTPUT OF PREVIOUS SOLUTION NOT AVAILABLE.)')
        ! Return to start of loop for next case
        return ! Will re-enter READIN for next case
      end if
    end if
    
    ! Set AK=0 for physical coordinates
    if (PHYS) AK = 0.0
    
    ! Handle automatic mesh generation or YIN initialization
    if (AMESH) then
      call AYMESH()
      call OUTPUT_MESH()
    else if (YIN(JMIN) == 0.0) then
      ! YIN needs default initialization for tunnel or free air case
      if (BCTYPE == 1) then
        ! Free air case
        JMAXI = JMXF
        do J_VAR = JMIN, JMAXI
          YIN(J_VAR) = YFREE(J_VAR)
        end do
      else
        ! Tunnel case
        JMAXI = JMXT
        do J_VAR = JMIN, JMAXI
          YIN(J_VAR) = YTUN(J_VAR)
        end do
      end if
    end if

    ! Echo input parameters to output file with exact original format strings
    write(UNIT_OUTPUT, '(1H0,4X,7HEMACH =,F9.5,5X,5HPOR =,F9.5,3X,6HIMIN =,I4,3X,8HBCTYPE =,I3,5X,8HAMESH = ,L1)') &
      EMACH, POR, IMIN, BCTYPE, AMESH
    write(UNIT_OUTPUT, '(1H0,4X,7HDELTA =,F9.5,3X,7HCLSET =,F9.5,2X,7HIMAXI =,I4,3X,8HBCFOIL =,I3,6X,7HPHYS = ,L1)') &
      DELTA, CLSET, IMAXI, BCFOIL, PHYS    
    write(UNIT_OUTPUT, '(1H0,4X,7HALPHA =,F9.5,5X,5HEPS =,F9.5,3X,6HJMIN =,I4,3X,8HPSTART =,I3,5X,8HPSAVE = ,L1)') &
      ALPHA, EPS, JMIN, PSTART, PSAVE
    write(UNIT_OUTPUT, '(1H0,7X,4HAK =,F9.5,4X,6HRIGF =,F9.5,2X,7HJMAXI =,I4,3X,8HPRTFLO =,I3,5X,8HKUTTA = ,L1)') &
      AK, RIGF, JMAXI, PRTFLO, KUTTA
    write(UNIT_OUTPUT, '(1H0,6X,5HGAM =,F9.5,3X,7HWCIRC =,F9.5,2X,7HMAXIT =,I4,3X,8HIPRTER =,I3,7X,6HFCR = ,L1)') &
      GAM, WCIRC, MAXIT, IPRTER, FCR
    write(UNIT_OUTPUT, '(1H0,8X,3HF =,F9.5,2X,8HCVERGE =,F9.5,5X,4HNU =,I4,3X,8HSIMDEF =,I3)') &
      F, CVERGE, NU, SIMDEF
    write(UNIT_OUTPUT, '(1H0,8X,3HH =,F9.5,2X,8HDVERGE =,F9.1,5X,4HNL =,I4,5X,6HICUT =,I3)') &
      H, DVERGE, NL, ICUT
    write(UNIT_OUTPUT, '(1H0,7X,5HWE = ,F4.2,2(1H,,F4.2))') WE
    
    if (NWDGE == 1) write(UNIT_OUTPUT, '(1H0,15X,12HMURMAN WEDGE,5X,8HREYNLD =,E10.3,5X,8HWCONST =,F9.5)') REYNLD, WCONST
    if (NWDGE == 2) write(UNIT_OUTPUT, '(1H0,15X,15HYOSHIHARA WEDGE)')
    if (IFLAP /= 0) write(UNIT_OUTPUT, '(1H0,15X,17HFLAP IS DEFLECTED,F5.2,20H DEGREES FROM H.L. =,F6.3,8H TO T.E.)') DELFLP, FLPLOC
    
    write(UNIT_OUTPUT, '(1H0,4X,3HXIN)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (XIN(IDX), IDX=IMIN, IMAXI)
    write(UNIT_OUTPUT, '(1H0,4X,3HYIN)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (YIN(JDX), JDX=JMIN, JMAXI)
    
    ! Print airfoil coordinates if needed (BCFOIL > 2 and BCFOIL /= 5)
    if (BCFOIL > 2 .and. BCFOIL /= 5) then
      write(UNIT_OUTPUT, '(1H0,15X,2HXU)')
      write(UNIT_OUTPUT, '(4X,6F11.6)') (XU(IDX), IDX=1, NU)
      write(UNIT_OUTPUT, '(1H0,15X,2HYU)')
      write(UNIT_OUTPUT, '(4X,6F11.6)') (YU(IDX), IDX=1, NU)
      write(UNIT_OUTPUT, '(1H0,15X,2HXL)')
      write(UNIT_OUTPUT, '(4X,6F11.6)') (XL(IDX), IDX=1, NL)
      write(UNIT_OUTPUT, '(1H0,15X,2HYL)')
      write(UNIT_OUTPUT, '(4X,6F11.6)') (YL(IDX), IDX=1, NL)
    end if
    
    ! Set derived constants
    GAM1 = GAM + 1.0
    IMAX = IMAXI
    JMAX = JMAXI
    IM1 = IMAX - 1
    JM1 = JMAX - 1
    
    ! Check array bounds (any call to INPERR causes message to be printed and execution stopped)
    if (IMAXI > N_MESH_POINTS .or. JMAXI > N_MESH_POINTS) call INPERR(1)
    
    ! Check input mesh for monotonically increasing values
    do IDX = IMIN, IM1
      if (XIN(IDX) >= XIN(IDX+1)) call INPERR(2)
    end do
    
    do JDX = JMIN, JM1
      if (YIN(JDX) >= YIN(JDX+1)) call INPERR(3)
    end do
    
    ! Check parameter ranges
    if (EMACH < 0.5 .or. EMACH > 2.0) call INPERR(4)
    if (ALPHA < -9.0 .or. ALPHA > 9.0) call INPERR(5)
    if (DELTA < 0.0 .or. DELTA > 1.0) call INPERR(6)
    if (NWDGE > 0 .and. EMACH > 1.0) call INPERR(8)
    
    ! Compute ILE and ITE (leading and trailing edge)
    call ISLIT(XIN)
    call JSLIT(YIN)
    
    ! Check number of mesh points, if not odd add points to appropriate areas to make odd no.
    call CKMESH()
    
    ! Check bounds of YMESH for tunnel calculations
    if (BCTYPE /= 1) then
      HTM = H - 0.00001
      HTP = H + 0.00001
      YS = abs(YIN(JMIN))
      YE = abs(YIN(JMAX))
      if (.not. ((YS >= HTM .and. YS <= HTP) .and. (YE >= HTM .and. YE <= HTP))) then
        ! Rescale Y mesh to -H,+H bounds
        TERM = -H / YIN(JMIN)
        do JDX = JMIN, JLOW
          YIN(JDX) = TERM * YIN(JDX)
        end do
        TERM = H / YIN(JMAX)
        do JDX = JUP, JMAX
          YIN(JDX) = TERM * YIN(JDX)
        end do
      end if
    end if
    
    ! If PSTART = 2 read old values from restart file using LOADP subroutine
    if (PSTART == 2) then
      write(*, '(A)') 'PSTART = 2 not used in modern code'
      stop
      ! call LOADP()
    end if
    
  end subroutine READIN
  
  ! Scale physical variables to transonic similarity variables
  subroutine SCALE()
    ! IF PHYS = .TRUE., ALL INPUT/OUTPUT QUANTITIES ARE IN PHYSICAL UNITS NORMALIZED 
    ! BY FREESTREAM VALUES AND AIRFOIL CHORD. 
    ! THIS SUBROUTINE THEN SCALES THE QUANTITIES TO TRANSONIC VARIABLES BY THE FOLLOWING CONVENTION
    !   SIMDEF = 1  COLE SCALING
    !   SIMDEF = 2  SPREITER SCALING
    !   SIMDEF = 3  KRUPP SCALING
    !   SIMDEF = 4  USER CHOICE
    ! IF PHYS = .FALSE., INPUT IS ALREADY IN SCALED VARIABLES AND NO FURTHER SCALING IS DONE.
    ! CALLED BY - TSFOIL.
    use common_data, only: PHYS, DELTA, EMACH, SIMDEF
    use common_data, only: AK, ALPHA, GAM1, RTK, YFACT, CPFACT, CLFACT, CDFACT, CMFACT, VFACT
    use common_data, only: YIN, YOLD, JMIN, JMAX, JMINO, JMAXO, PSTART
    use common_data, only: H, POR, SONVEL, CPSTAR, DELRT2, EMROOT
    implicit none
    real :: EMACH2, BETA, DELRT1
    real :: YFACIV
    integer :: J
    
    if (.not. PHYS) then
      ! PHYS = .FALSE.  NO SCALING
      CPFACT = 1.0
      CDFACT = 1.0
      CLFACT = 1.0
      CMFACT = 1.0
      YFACT = 1.0
      VFACT = 1.0

    else
      ! PHYS = .TRUE.  COMPUTE CONSTANTS
      EMACH2 = EMACH*EMACH
      BETA = 1.0 - EMACH2
      DELRT1 = DELTA**(1.0/3.0)
      DELRT2 = DELTA**(2.0/3.0)

      ! Branch to appropriate scaling
      select case (SIMDEF)
      case (1)
        ! SIMDEF = 1
        ! COLE SCALING
        AK = BETA / DELRT2
        YFACT = 1.0 / DELRT1
        CPFACT = DELRT2
        CLFACT = DELRT2
        CDFACT = DELRT2 * DELTA
        CMFACT = DELRT2
        VFACT = DELTA * 57.295779
        
      case (2)
        ! SIMDEF = 2
        ! SPREITER SCALING
        EMROOT = EMACH**(2.0/3.0)
        AK = BETA / (DELRT2 * EMROOT * EMROOT)
        YFACT = 1.0 / (DELRT1 * EMROOT)
        CPFACT = DELRT2 / EMROOT
        CLFACT = CPFACT
        CMFACT = CPFACT
        CDFACT = CPFACT * DELTA
        VFACT = DELTA * 57.295779
        
      case (3)
        ! SIMDEF = 3
        ! KRUPP SCALING
        AK = BETA / (DELRT2 * EMACH)
        YFACT = 1.0 / (DELRT1 * EMACH**0.5)
        CPFACT = DELRT2 / (EMACH**0.75)
        CLFACT = CPFACT
        CMFACT = CPFACT
        CDFACT = CPFACT * DELTA
        VFACT = DELTA * 57.295779
        
      case (4)
        ! SIMDEF = 4
        ! THIS ADDRESS IS INACTIVE
        ! USER MAY INSERT SCALING OF OWN CHOICE
        ! DEFINITION FOR LOCAL MACH NUMBER MUST BE ADJUSTED
        ! IN EMACH1.
        write(UNIT_OUTPUT, '(A, /, A)') '1ABNORMAL STOP IN SUBROUTINE SCALE', ' SIMDEF=4 IS NOT USEABLE'
        stop
        
      case default
        write(UNIT_OUTPUT, '(A, /, A)') '1ABNORMAL STOP IN SUBROUTINE SCALE', ' INVALID SIMDEF VALUE'
        stop

      end select

      ! SCALE Y MESH
      YFACIV = 1.0 / YFACT
      do J = JMIN, JMAX
        YIN(J) = YIN(J) * YFACIV
      end do
      
      if (PSTART /= 1) then
        do J = JMINO, JMAXO
          YOLD(J) = YOLD(J) * YFACIV
        end do
      end if

      ! SCALE TUNNEL PARAMETERS
      H = H / YFACT
      POR = POR * YFACT
      write(UNIT_OUTPUT,'(//10X,11HSCALED POR=,F10.5)') POR

      ! SCALE ANGLE OF ATTACK
      ALPHA = ALPHA / VFACT
    end if

    ! CHECK VALUE OF AK FOR DEFAULT.
    if (AK == 0.0) call INPERR(7)

    ! COMPUTE SQUARE ROOT OF AK
    RTK = sqrt(abs(AK))

    ! COMPUTE SONIC VELOCITY
    if (abs(GAM1) <= 0.0001) then
      SONVEL = 1.0
      CPSTAR = 0.0
      return
    end if
    
    SONVEL = AK / GAM1
    CPSTAR = -2.0 * SONVEL * CPFACT
    return

  end subroutine SCALE  
    
  ! Main print driver: prints configuration parameters and calls specialized subroutines
  ! Subroutine for main output print control. Prints relative parameters and calls
  ! specialized print/plot subroutines as required.
  ! Matches original PRINT subroutine functionality exactly
  subroutine PRINT()
    use common_data
    use math_module, only: PITCH, LIFT
    implicit none
    
    ! Local variables matching original exactly
    character(len=4) :: TPH(6), SIM(8), BCT(15), FCP(14)
    real :: ALPHVF
    integer :: IS, IE, I
    
    ! Data initialization matching original exactly
    TPH = ['SIMI', 'LARI', 'TY  ', ' PHY', 'SICA', 'L   ']
    SIM = ['COLE', '    ', 'SPRE', 'ITER', 'KRUP', 'P   ', 'USER', '    ']
    BCT = ['FREE', ' AIR', '    ', 'SOLI', 'D WA', 'LL  ', 'FREE', &
           ' JET', '    ', 'SLOT', 'TED ', 'WALL', 'PORO', 'US W', 'ALL ']
    FCP = ['FULL', 'Y CO', 'NSER', 'VATI', 'VE. ', '    ', '    ', &
           'NOT ', 'CONS', 'ERVA', 'TIVE', ' AT ', 'SHOC', 'K.  ']
    
    ! Write page break
    write(UNIT_OUTPUT, '(1H1)')
    
    ! Print similarity/physical variables information
    IS = 1
    if (PHYS) IS = 4
    IE = IS + 2
    write(UNIT_OUTPUT, '(14H0 PRINTOUT IN ,2A4,A2,11H VARIABLES.)') (TPH(I), I=IS, IE)
    write(UNIT_SUMMARY, '(14H0 PRINTOUT IN ,2A4,A2,11H VARIABLES.)') (TPH(I), I=IS, IE)
    
    ! Print similarity parameter definition
    IE = 2 * SIMDEF
    IS = IE - 1
    write(UNIT_OUTPUT, '(41H0 DEFINITION OF SIMILARITY PARAMETERS BY ,2A4)') (SIM(I), I=IS, IE)
    write(UNIT_SUMMARY, '(41H0 DEFINITION OF SIMILARITY PARAMETERS BY ,2A4)') (SIM(I), I=IS, IE)
    
    ! Print boundary condition information
    IE = 3 * BCTYPE
    IS = IE - 2
    write(UNIT_OUTPUT, '(25H0 BOUNDARY CONDITION FOR ,3A4)') (BCT(I), I=IS, IE)
    write(UNIT_SUMMARY, '(25H0 BOUNDARY CONDITION FOR ,3A4)') (BCT(I), I=IS, IE)
    
    ! Print difference equation information
    IS = 8
    if (FCR) IS = 1
    IE = IS + 6
    write(UNIT_OUTPUT, '(27H0 DIFFERENCE EQUATIONS ARE ,7A4)') (FCP(I), I=IS, IE)
    write(UNIT_SUMMARY, '(27H0 DIFFERENCE EQUATIONS ARE ,7A4)') (FCP(I), I=IS, IE)
    
    ! Print Kutta condition information
    if (KUTTA) then
      write(UNIT_OUTPUT, '(30H0 KUTTA CONDITION IS ENFORCED.)')
      write(UNIT_SUMMARY, '(30H0 KUTTA CONDITION IS ENFORCED.)')
    else
      write(UNIT_OUTPUT, '(37H0 LIFT COEFFICIENT SPECIFIED BY USER.)')
      write(UNIT_SUMMARY, '(37H0 LIFT COEFFICIENT SPECIFIED BY USER.)')
    end if
    
    ! Print flow parameters
    ALPHVF = ALPHA * VFACT
    write(UNIT_OUTPUT, '(1H0)')
    write(UNIT_SUMMARY, '(1H0)')
    
    if (PHYS) then
      write(UNIT_OUTPUT, '(14X,6HMACH =,F12.7/13X,7HDELTA =,F12.7)') EMACH, DELTA
      write(UNIT_SUMMARY, '(14X,6HMACH =,F12.7/13X,7HDELTA =,F12.7)') EMACH, DELTA
    end if
    
    write(UNIT_OUTPUT, '(13X,7HALPHA =,F12.7/17X,3HK =,F12.7)') ALPHVF, AK
    write(UNIT_SUMMARY, '(13X,7HALPHA =,F12.7/17X,3HK =,F12.7)') ALPHVF, AK
    
    if (AK > 0.0) then
      write(UNIT_OUTPUT, '(2X,18HDOUBLET STRENGTH =,F12.7)') DUB
      write(UNIT_SUMMARY, '(2X,18HDOUBLET STRENGTH =,F12.7)') DUB
    end if
      if (PHYS) then
      write(UNIT_OUTPUT, '(A, F12.7, /, 12X, A, F12.7, /, 12X, A, F12.7, /, 12X, A, F12.7, /, 13X, A, F12.7, /, 13X, A, F12.7)') &
        'CPFACT =', CPFACT, 'CDFACT =', CDFACT, 'CMFACT =', CMFACT, 'CLFACT =', CLFACT, 'YFACT =', YFACT, 'VFACT =', VFACT
      write(UNIT_SUMMARY, '(A, F12.7, /, 12X, A, F12.7, /, 12X, A, F12.7, /, 12X, A, F12.7, /, 13X, A, F12.7, /, 13X, A, F12.7)') &
        'CPFACT =', CPFACT, 'CDFACT =', CDFACT, 'CMFACT =', CMFACT, 'CLFACT =', CLFACT, 'YFACT =', YFACT, 'VFACT =', VFACT
    end if
    
    ! Call specialized print routines
    call PRINT1()
    call PRTMC()
    
    ! Output field data
    call OUTPUT_CP_MACH_FIELD()
    
    if (ABORT1) return
    
    ! Call FIXPLT to generate the dedicated printer plot
    call FIXPLT()
    
    ! Call PRTWAL for boundary conditions other than 1 and 3 (matches original logic)
    if (BCTYPE /= 1 .and. BCTYPE /= 3) then
      call PRTWAL()
    end if
    
    call M1LINE()
    
    ! Call PRTFLD only if PRTFLO is not 1 (matches original logic)
    if (PRTFLO /= 1) then
      call PRTFLD()
    end if
    
    call CDCOLE()  ! Momentum integral drag calculation
    
  end subroutine PRINT
  
  ! Print Cp and Mach along body and build plot arrays
  ! Prints pressure coefficient and Mach number on Y=0 line, and plots CP along side of print
  subroutine PRINT1()
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
    select case (IREF)
      case (2)
        write(UNIT_OUTPUT, '(20X,11HCOARSE MESH)')
      case (1) 
        write(UNIT_OUTPUT, '(20X,11HMEDIUM MESH)')
      case (0)
        write(UNIT_OUTPUT, '(20X,11H FINAL MESH)')
    end select
    
    write(UNIT_OUTPUT, '(1H0,9X,4HCL =,F10.6/10X,4HCM =,F10.6/9X,5HCP* =,F10.6)') CL_val, CM, CPSTAR
    write(UNIT_SUMMARY, '(1H0,9X,4HCL =,F16.12/10X,4HCM =,F16.12/9X,5HCP* =,F16.12)') CL_val, CM, CPSTAR
    
    ! Check for detached shock - exactly like original with GO TO 70 logic
    if (CPL(IMIN) < CPSTAR .and. CPL(IMIN+1) > CPSTAR) then
      write(UNIT_OUTPUT, '(A, //, A)') '0', &
           ' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.'
      if (IREF /= 2) then
        ABORT1 = .true.
      end if
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
    
  end subroutine PRINT1

  ! Output mesh data to file in Tecplot format
  subroutine OUTPUT_MESH(OPTIONAL_FILE_NAME)
    use common_data, only: X, Y, JMIN, JMAX, UNIT_MESH, IMIN, IMAX
    implicit none
    integer :: I_P1, J_P1
    character(len=*), optional :: OPTIONAL_FILE_NAME

    if (present(OPTIONAL_FILE_NAME)) then
      open(unit=UNIT_MESH, file=trim(OPTIONAL_FILE_NAME), status='replace', action='write')
    else
      open(unit=UNIT_MESH, file='mesh.dat', status='replace', action='write')
    end if

    write(UNIT_MESH, '(A)') 'VARIABLES = "X", "Y"'
    write(UNIT_MESH, '(A,I5,A,I5,A)') 'ZONE I= ', IMAX-IMIN+1, ' J= ', JMAX-JMIN+1, ' F= POINT'
    do J_P1 = JMIN, JMAX
      do I_P1 = IMIN, IMAX
        write(UNIT_MESH, '(2F12.6)') X(I_P1), Y(J_P1)
      end do
    end do

    close(UNIT_MESH)

    write(*, '(A)') 'Output to mesh.dat: Mesh data'
    
  end subroutine OUTPUT_MESH

  ! Output Cp, Mach distribution on a x-line (Y=0) in Tecplot format
  subroutine OUTPUT_CP_MACH_XLINE(CL_val, CM, EM1U, EM1L)
    use common_data, only: IMIN, IMAX, UNIT_CPXS
    use common_data, only: X, EMACH, CPSTAR
    use common_data, only: CPU, CPL
    implicit none
    real, intent(in) :: CL_val, CM
    real, intent(in) :: EM1U(:), EM1L(:)

    integer :: I_P1

    open(unit=UNIT_CPXS, file='cpxs.dat', status='replace', action='write')

    ! Write coefficients
    write(UNIT_CPXS, '(A, F10.6)') '# Mach = ', EMACH
    write(UNIT_CPXS, '(A, F10.6)') '# CL = ', CL_val
    write(UNIT_CPXS, '(A, F10.6)') '# CM = ', CM
    write(UNIT_CPXS, '(A, F10.6)') '# Cp* = ', CPSTAR

    write(UNIT_CPXS, '(A)') 'VARIABLES = "X", "Cp-up", "M-up", "Cp-low", "M-low"'

    do I_P1 = IMIN, IMAX
      write(UNIT_CPXS, '(2x,f10.5,2x,f10.5,2x,f10.5,2x,f10.5,2x,f10.5)') &
            X(I_P1), CPU(I_P1), EM1U(I_P1), CPL(I_P1), EM1L(I_P1)
    end do

    close(UNIT_CPXS)

    write(*, '(A)') 'Output to cpx.dat: Cp, Mach distribution on a x-line (Y=0)'
    
  end subroutine OUTPUT_CP_MACH_XLINE

  ! Output Cp, Mach field in Tecplot format
  subroutine OUTPUT_CP_MACH_FIELD()
    use common_data, only: UNIT_FIELD, X, Y, JMIN, JMAX, IMIN, IMAX, CPFACT
    use math_module, only: PX, EMACH1
    implicit none
    
    integer :: I, J
    real :: U, EM, CP_VAL

    open(unit=UNIT_FIELD, file='field.dat', status='replace', action='write')

    ! Write Tecplot header
    write(UNIT_FIELD, '(A,F10.6)') '# CPFACT = ', CPFACT
    write(UNIT_FIELD, '(A)') 'VARIABLES = "X", "Y", "Mach", "Cp"'
    write(UNIT_FIELD, '(A,I5,A,I5,A)') 'ZONE I= ', IMAX-IMIN+1, ' J= ', JMAX-JMIN+1, ' F= POINT'

    ! Write field data in point format
    do J = JMIN, JMAX
      do I = IMIN, IMAX
        U = PX(I, J)    ! Function PX computes U = DP/DX at point I,J
        EM = EMACH1(U)  ! Function EMACH1 computes Mach number from U
        CP_VAL = -2.0 * U * CPFACT  ! CPFACT is a scaling factor for pressure coefficient
        write(UNIT_FIELD, '(4F16.12)') X(I), Y(J), EM, CP_VAL
      end do
    end do

    close(UNIT_FIELD)

    write(*, '(A)') 'Output to field.dat: Cp, Mach field data'

  end subroutine OUTPUT_CP_MACH_FIELD

  ! Print Cp, flow angle (theta), and Mach number on selected j-lines
  ! Prints pressure coefficient, flow angle and Mach number in flow field.
  ! Number of J lines printed is determined from the input value of PRTFLO.
  ! PRTFLO = 1, NONE.
  ! PRTFLO = 2, ALL J LINES EXCEPT J0.
  ! PRTFLO = 3, THREE J LINES AROUND JERROR.
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

  ! Print map of flow types at each grid point
  ! PRTMC - Print flow type map at each grid point
  ! Matches original PRTMC functionality exactly
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

  ! Print shock wave drag contributions and total pressure loss along shock wave
  ! PRINTOUT WAVE DRAG CONTRIBUTION AND TOTAL PRESSURE
  ! LOSS ALONG SHOCK WAVE
  ! CALLED BY - CDCOLE.
  subroutine PRTSK(Z,ARG_PARAM,L,NSHOCK,CDSK,LPRT1)
    use common_data, only: CDFACT, GAM1, DELTA, YFACT, UNIT_OUTPUT
    implicit none
    real, intent(in) :: Z(:), ARG_PARAM(:)
    integer, intent(in) :: L, NSHOCK, LPRT1
    real, intent(in) :: CDSK
    real :: CDYCOF, POYCOF, YY, CDY, POY
    integer :: K

    CDYCOF = -CDFACT * GAM1 / (6.0 * YFACT)
    POYCOF = DELTA**2 * GAM1 * (GAM1 - 1.0) / 12.0
    
    ! Write header for first shock wave only (format 1001 equivalent)
    if (NSHOCK == 1) then
      write(UNIT_OUTPUT,'(A)') char(12) // 'INVISCID WAKE PROFILES FOR INDIVIDUAL SHOCK WAVES WITHIN MOMENTUM CONTOUR'
    end if
    
    ! Write shock information (format 1002 equivalent)
    write(UNIT_OUTPUT,'(A)') ''  ! blank line for 0 carriage control
    write(UNIT_OUTPUT,'(A,I3)') 'SHOCK', NSHOCK
    write(UNIT_OUTPUT,'(A,F12.6)') ' WAVE DRAG FOR THIS SHOCK=', CDSK
    write(UNIT_OUTPUT,'(A,A,A,A,A)') '      Y', '         ', 'CD(Y)', '        ', 'PO/POINF'
    
    ! Write shock profile data (format 1003 equivalent)
    do K = 1, L
      YY = Z(K) * YFACT
      CDY = CDYCOF * ARG_PARAM(K)
      POY = 1.0 + POYCOF * ARG_PARAM(K)
      write(UNIT_OUTPUT,'(1X,3F12.8)') YY, CDY, POY
    end do
    
    ! Write footer if shock extends outside contour (format 1004 equivalent)
    if (LPRT1 == 1) then
      write(UNIT_OUTPUT,'(A)') ''  ! blank line for 0 carriage control
      write(UNIT_OUTPUT,'(A)') 'SHOCK WAVE EXTENDS OUTSIDE CONTOUR'
      write(UNIT_OUTPUT,'(A)') ' PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK'
    end if
  end subroutine PRTSK

  ! Print Cp and flow angles on tunnel walls
  ! Prints pressure coefficient and flow angle on Y=-H and Y=+H, 
  ! and plots CP along side of tabulation. 
  subroutine PRTWAL()
    use common_data, only: P, X, Y, CPFACT, VFACT, YFACT, JMIN, JMAX, &
                          IUP, IDOWN, JBOT, JTOP, JTOP, JBOT, &
                          BCTYPE, CIRCFF, FHINV, POR, F, H, CPSTAR, &
                          XDIFF, UNIT_OUTPUT
    use math_module, only: PX, PY
    implicit none
    
    ! Local variables
    integer :: I, K, NCOL, NCOLS, NCOLU, NCOLL, I2_LOCAL, I1_LOCAL
    ! Character variables for plotting symbols (matching original DATA statements)
    character(len=1), parameter :: IB = ' '    ! Blank space
    character(len=1), parameter :: IL = 'L'    ! Lower surface
    character(len=1), parameter :: IU = 'U'    ! Upper surface  
    character(len=1), parameter :: IS = '*'    ! CP* reference line
    character(len=1), parameter :: IBB = 'B'   ! Both surfaces same CP
    real :: THH, PORF, CPMIN, CPMAX, CPT
    real :: CPLW(N_MESH_POINTS), CPUW(N_MESH_POINTS), VLW(N_MESH_POINTS), VUW(N_MESH_POINTS)
    real :: COL, CPLARG, UNPCOL
    character(len=1) :: LINE1(60)
    character(len=4) :: BCT(15)
    
    ! Data statements
    character(len=1), parameter :: BLANK = ' ', DOT = '.', STAR = '*', DASH = '-'
    
    ! Print single variables
    I2_LOCAL = 3 * BCTYPE
    I1_LOCAL = I2_LOCAL - 2
    write(UNIT_OUTPUT, '(2H1 ,3A4,20H BOUNDARY CONDITION.)') (BCT(I),I=I1_LOCAL,I2_LOCAL)
    
    THH = H * YFACT
    write(UNIT_OUTPUT, '(1H0,10X,24HH (TUNNEL HALF HEIGHT) =,F9.6)') THH
    
    if (BCTYPE >= 5) then
      PORF = POR / YFACT
      write(UNIT_OUTPUT, '(1H0,11X,23HPOR (POROSITY FACTOR) =,F9.6)') PORF
    end if
    
    if (BCTYPE == 4 .or. BCTYPE == 6) then
      write(UNIT_OUTPUT, '(1H0,14X,20HF (SLOT PARAMETER) =,F9.6)') F
    end if
    
    write(UNIT_OUTPUT, '(1H0,29X,5HCP* =,F9.6)') CPSTAR
    
    CPMIN = 1.0E37
    CPMAX = -CPMIN
    
    ! Compute CP values using proper PX function calls
    CPT = -2.0 * CPFACT
    do I = IUP, IDOWN
      CPLW(I) = CPT * PX(I,JMIN)
      CPUW(I) = CPT * PX(I,JMAX)
      CPMAX = max(CPMAX, CPUW(I), CPLW(I))
      CPMIN = min(CPMIN, CPUW(I), CPLW(I))
    end do
    
    ! Compute flow angles based on boundary condition type
    do I = IUP, IDOWN
      select case (BCTYPE)
      case (2)
        ! Solid wall
        VLW(I) = 0.0
        VUW(I) = 0.0
        
      case (3)
        ! Free jet
        VLW(I) = VFACT * PY(I,JMIN)
        VUW(I) = VFACT * PY(I,JMAX)
        
      case (4)
        ! Slotted wall
        VLW(I) =  VFACT * FHINV * (P(JBOT,I) + 0.75 * CIRCFF)
        VUW(I) = -VFACT * FHINV * (P(JTOP,I) - 0.25 * CIRCFF)
        
      case (5, 6)
        ! Porous wall
        if (POR <= 1.5) then
          VLW(I) =  VFACT * POR * XDIFF(I)*(P(JMIN,I)-P(JMIN,I-1))
          VUW(I) = -VFACT * POR * XDIFF(I)*(P(JMAX,I)-P(JMAX,I-1))
        else
          VLW(I) = VFACT * 0.25*(P(JMIN+1,I+1)+2.*P(JMIN+1,I)+P(JMIN+1,I-1) &
                  - P(JMIN  ,I+1)-2.*P(JMIN  ,I)-P(JMIN  ,I-1)) &
                  / (Y(JMIN+1)-Y(JMIN))
          VUW(I) = VFACT * 0.25*(P(JMAX,I+1)  +2.*P(JMAX,I)  +P(JMAX,I-1) &
                  - P(JMAX-1,I+1)-2.*P(JMAX-1,I)-P(JMAX-1,I-1)) &
                  / (Y(JMAX)-Y(JMAX-1))
        end if
        
      case default
        ! Default case
        VLW(I) = 0.0
        VUW(I) = 0.0
      end select
    end do
    
    ! Set up plotting scale
    CPLARG = max(CPMAX, abs(CPMIN))
    UNPCOL = CPLARG / 29.0
    
    ! Locate CP* for printer plot
    COL = -CPSTAR / UNPCOL
    NCOL = sign(int(abs(COL) + 0.5), nint(COL))
    NCOLS = NCOL + 30
    
    ! Print column headers
    write(UNIT_OUTPUT,'(1H0,27X,5HLOWER,23X,5HUPPER/28X,4HY=-H,24X,4HY=+H)')
    write(UNIT_OUTPUT,'(3X,1HI,8X,1HX,10X,2HCP,9X,5HTHETA,12X,2HCP,9X,5HTHETA/)')
    
    ! Print data with character-based plot
    do I = IUP, IDOWN
      ! Initialize line
      do K = 1, 60
        LINE1(K) = IB
      end do
      
      ! Plot upper wall CP
      COL = -CPUW(I) / UNPCOL
      NCOL = sign(int(abs(COL) + 0.5), nint(COL))
      NCOLU = NCOL + 30
      if (NCOLU >= 1 .and. NCOLU <= 60) LINE1(NCOLU) = IU
      
      ! Plot lower wall CP
      COL = -CPLW(I) / UNPCOL
      NCOL = sign(int(abs(COL) + 0.5), nint(COL))
      NCOLL = NCOL + 30
      if (NCOLL >= 1 .and. NCOLL <= 60) LINE1(NCOLL) = IL
      if (NCOLL == NCOLU .and. NCOLL >= 1 .and. NCOLL <= 60) LINE1(NCOLL) = IBB
      
      ! Plot CP* reference line
      if (abs(NCOLS) <= 60 .and. NCOLS >= 1) LINE1(NCOLS) = IS
      
      ! Write formatted output
      write(UNIT_OUTPUT,'(1H ,I3,3F12.6,4X,2F12.6,2X,60A1)') I, X(I), CPLW(I), VLW(I), CPUW(I), VUW(I), LINE1
    end do
    
  end subroutine PRTWAL
  
  ! SAVEP moves data into old data locations and writes it on tape if requested
  ! Matches original SAVEP functionality exactly
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

  ! Compute drag coefficient by momentum integral method
  ! Integrates around a contour enclosing the body and along all shocks inside the contour
  ! CALLED BY - PRINT.
  subroutine CDCOLE()
    use common_data, only: X, Y, IMIN, IMAX, IUP, ILE, ITE
    use common_data, only: JMIN, JMAX, JUP, JLOW
    use common_data, only: AK, GAM1, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: CDFACT, YFACT
    use common_data, only: SONVEL, FXL, FXU
    use common_data, only: XI, ARG  ! Working arrays
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    use math_module, only: PX, PY, TRAP, FINDSK, NEWISK, DRAG
    implicit none
    
    ! Local variables
    integer :: IU, ID, JT, JB, ISTOP, IBOW, ISK, JSTART, J, JJ, JSK, ISKOLD
    integer :: ILIM, IB, I, L, NSHOCK, LPRT1, LPRT2, ISTART
    real :: GAM123, U, V, UU, UL, SUM, CDSK, CDWAVE, CDC, CD
    real :: CDUP, CDTOP, CDBOT, CDDOWN, CDBODY
    real :: XU_LOC, XD_LOC, YT_LOC, YB_LOC, ULE
    
    GAM123 = GAM1 * 2.0 / 3.0
    
    ! Set locations of contour boundaries
    
    ! Upstream boundary
    ! If AK = 0.0 CDCOLE will not be called. AMACH may not be = 1.0
    if (AK > 0.0) then
      IU = (ILE + IMIN) / 2
    else
      IU = IUP
    end if
    
    ! Top and bottom boundaries
    ! Subsonic freestream
    ! Set JB,JT to include as much of shocks as possible
    JT = JMAX - 1
    JB = JMIN + 1
    
    if (AK <= 0.0) then
      ! Supersonic freestream
      ! Set JB,JT to include only subsonic part of detached bow wave
      
      ! Find bow shock wave
      ISTOP = ILE - 3
      call FINDSK(IUP, ISTOP, JUP, IBOW)
      if (IBOW < 0) then
        ! Shock is too close to body to do contour integral.
        ! Write message and return
        ULE = PX(ILE, JUP)
        
        if (ULE > SONVEL) then
          write(UNIT_OUTPUT, '("31H1SHOCK WAVE IS ATTACHED TO BODY/", &
               & "33H MOMENTUM INTEGRAL CANNOT BE DONE/", &
               & "45H DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL/")')
        else
          write(UNIT_OUTPUT, '("41H1DETACHED SHOCK WAVE IS TOO CLOSE TO BODY/", &
               & "33H MOMENTUM INTEGRAL CANNOT BE DONE/", &
               & "45H DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL/")')
        end if
        
        CD = DRAG(CDFACT)
        write(UNIT_OUTPUT, '("4H0CD=", F12.6)') CD
        return
      end if
      
      ! Search up shock to find tip of subsonic region
      ISK = IBOW
      JSTART = JUP + 1
      JT = JUP - 1
      do J = JSTART, JMAX
        JT = JT + 1
        ISKOLD = ISK
        call NEWISK(ISKOLD, J, ISK)
        if (ISK < 0) exit
      end do
      
      ! Search down shock to find tip of subsonic region
      ISK = IBOW
      JB = JLOW + 2
      do J = JMIN, JLOW
        JJ = JLOW - J + JMIN
        JB = JB - 1
        ISKOLD = ISK
        call NEWISK(ISKOLD, JJ, ISK)
        if (ISK < 0) exit
      end do
      
      ! Save I location of bow shock wave on lower boundary
      IBOW = ISKOLD
    end if
    
    ! Downstream boundary
    ID = (ITE + IMAX) / 2
    if (PX(ITE+1, JUP) >= SONVEL) then
      ! Trailing edge is supersonic. Place downstream
      ! boundary ahead of trailing edge to avoid tail shock
      I = ITE
      do while (X(I) > 0.75)
        I = I - 1
      end do
      ID = I
    end if
    
    ! All boundaries are fixed
    ! Compute integrals along boundaries
    ! Integral on upstream boundary
    CDUP = 0.0
    if (AK >= 0.0) then
      L = 0
      do J = JB, JT
        L = L + 1
        XI(L) = Y(J)
        U = PX(IU, J)
        V = PY(IU, J)
        ARG(L) = ((AK - GAM123*U)*U*U - V*V) * 0.5
      end do
      call TRAP(XI, ARG, L, SUM)
      CDUP = 2.0 * CDFACT * SUM
    end if
    
    ! Integral on top boundary
    L = 0
    do I = IU, ID
      L = L + 1
      XI(L) = X(I)
      ARG(L) = -PX(I, JT) * PY(I, JT)
    end do
    call TRAP(XI, ARG, L, SUM)
    CDTOP = 2.0 * CDFACT * SUM
    
    ! Integral on bottom boundary
    L = 0
    do I = IU, ID
      L = L + 1
      ARG(L) = PX(I, JB) * PY(I, JB)
    end do
    call TRAP(XI, ARG, L, SUM)
    CDBOT = 2.0 * CDFACT * SUM
    
    ! Integral on downstream boundary
    L = 0
    do J = JB, JT
      L = L + 1
      XI(L) = Y(J)
      U = PX(ID, J)
      ! If flow supersonic, use backward difference formula
      if (U > SONVEL) U = PX(ID-1, J)
      V = PY(ID, J)
      ARG(L) = ((GAM123*U - AK)*U*U + V*V) * 0.5
    end do

    call TRAP(XI, ARG, L, SUM)
    CDDOWN = 2.0 * CDFACT * SUM
      
    ! Integral on body boundary
    CDBODY = 0.0
    if (ID <= ITE) then
      ILIM = ITE + 1
      L = 0
      do I = ID, ILIM
        IB = I - ILE + 1
        L = L + 1
        XI(L) = X(I)
        UU = CJUP*PX(I, JUP) - CJUP1*PX(I, JUP+1)
        UL = CJLOW*PX(I, JLOW) - CJLOW1*PX(I, JLOW-1)
        ARG(L) = -UU*FXU(IB) + UL*FXL(IB)
      end do
      call TRAP(XI, ARG, L, SUM)
      CDBODY = 2.0 * CDFACT * SUM
    end if
      
    ! Integration along shock waves
    CDWAVE = 0.0
    LPRT1 = 0
    LPRT2 = 0
    NSHOCK = 0
    
    if (AK <= 0.0) then
      ! Integrate along detached bow wave
      NSHOCK = NSHOCK + 1
      LPRT1 = 1
      LPRT2 = 1
      L = 0
      ISK = IBOW
      do J = JB, JT
        L = L + 1
        ISKOLD = ISK
        call NEWISK(ISKOLD, J, ISK)
        XI(L) = Y(J)
        ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
      end do
      call TRAP(XI, ARG, L, SUM)
      CDSK = -GAM1/6.0 * CDFACT * SUM
      CDWAVE = CDWAVE + CDSK
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1)
    end if
      
    ! Integrate along shocks above airfoil
    ISTART = ILE
    
    ! Loop to find and process all shocks above airfoil
    do
      call FINDSK(ISTART, ITE, JUP, ISK)
      if (ISK < 0) exit  ! No more shocks found
      
      ! Shock wave found
      ISTART = ISK + 1
      NSHOCK = NSHOCK + 1
      LPRT1 = 0
      L = 1
      XI(L) = 0.0
      ARG(L) = (CJUP*(PX(ISK+1, JUP) - PX(ISK-2, JUP)) - &
                CJUP1*(PX(ISK+1, JUP+1) - PX(ISK-2, JUP+1)))**3
      
      do J = JUP, JT
        L = L + 1
        XI(L) = Y(J)
        ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
        ISKOLD = ISK
        JSK = J + 1
        call NEWISK(ISKOLD, JSK, ISK)
        if (ISK < 0) exit
        if (ISK > ID) then
          LPRT1 = 1
          exit
        end if
      end do
      
      if (ISK < 0) LPRT1 = 1
      
      call TRAP(XI, ARG, L, SUM)
      CDSK = -GAM1/6.0 * CDFACT * SUM
      CDWAVE = CDWAVE + CDSK
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1)
      if (LPRT1 == 1) LPRT2 = 1
    end do
      
    ! Integrate along shocks below airfoil
    ISTART = ILE
    
    ! Loop to find and process all shocks below airfoil  
    do
      call FINDSK(ISTART, ITE, JLOW, ISK)
      if (ISK < 0) exit  ! No more shocks found
      
      ! Shock wave found
      ISTART = ISK + 1
      NSHOCK = NSHOCK + 1
      LPRT1 = 0
      L = 1
      XI(L) = 0.0
      ARG(L) = (CJLOW*(PX(ISK+1, JLOW) - PX(ISK-2, JLOW)) - &
                CJLOW1*(PX(ISK+1, JLOW-1) - PX(ISK-2, JLOW-1)))**3
      
      do JJ = JB, JLOW
        J = JLOW + JB - JJ
        L = L + 1
        XI(L) = Y(J)
        ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
        ISKOLD = ISK
        JSK = J - 1
        call NEWISK(ISKOLD, JSK, ISK)
        if (ISK < 0) exit
        if (ISK > ID) then
          LPRT1 = 1
          exit
        end if
      end do
      
      if (ISK < 0) LPRT1 = 1
      
      call TRAP(XI, ARG, L, SUM)
      CDSK = -GAM1/6.0 * (-SUM)
      CDWAVE = CDWAVE + CDSK
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1)
      if (LPRT1 == 1) LPRT2 = 1
    end do
    
    ! Integration along shocks is complete
    ! Printout CD information
    XU_LOC = X(IU)
    XD_LOC = X(ID)
    YT_LOC = Y(JT) * YFACT
    YB_LOC = Y(JB) * YFACT
    CDC = CDUP + CDTOP + CDBOT + CDDOWN + CDBODY
    CD = CDC + CDWAVE
    
    ! Write drag coefficient breakdown
    write(UNIT_OUTPUT, '("1CALCULATION OF DRAG COEFFICIENT BY MOMENTUM INTEGRAL METHOD")')
    
    write(UNIT_OUTPUT, '("0BOUNDARIES OF CONTOUR USED", 15X, "18HCONTRIBUTION TO CD/", &
           & "16H UPSTREAM    X =", F12.6, 15X, "8HCDUP   =", F12.6, &
           & "16H DOWNSTREAM  X =", F12.6, 15X, "8HCDDOWN =", F12.6, &
           & "16H TOP         Y =", F12.6, 15X, "8HCDTOP  =", F12.6, &
           & "16H BOTTOM      Y =", F12.6, 15X, "8HCDBOT  =", F12.6)') &
           & XU_LOC, CDUP, XD_LOC, CDDOWN, YT_LOC, CDTOP, YB_LOC, CDBOT
    
    if (XD_LOC < 1.0) then
      write(UNIT_OUTPUT, '("16H BODY AFT OF X =", F12.6, 15X, "8HCDBODY =", F12.6)') XD_LOC, CDBODY
    end if
    
    write(UNIT_OUTPUT, '(15X, "36HTOTAL CONTRIBUTIONS AROUND CONTOUR =", F12.6)') CDC
    write(UNIT_OUTPUT, '("10H0THERE ARE", I3, "38H SHOCKS INSIDE CONTOUR. TOTAL CDWAVE =", F12.6)') NSHOCK, CDWAVE
    write(UNIT_SUMMARY, '("TOTAL CDWAVE =", F16.12)') CDWAVE
    
    if (NSHOCK > 0 .and. LPRT2 == 0) then
      write(UNIT_OUTPUT, '("43H0NOTE - ALL SHOCKS CONTAINED WITHIN CONTOUR/", &
           &"30H CDWAVE EQUALS TOTAL WAVE DRAG")')
    end if
    
    if (NSHOCK > 0 .and. LPRT2 == 1) then
      write(UNIT_OUTPUT, '("52H0NOTE - ONE OR MORE SHOCKS EXTEND OUTSIDE OF CONTOUR/", &
           &"38H CDWAVE DOES NOT EQUAL TOTAL WAVE DRAG")')
    end if
    
    write(UNIT_OUTPUT, '("51H0DRAG CALCULATED FROM MOMENTUM INTEGRAL    CD     =", F12.6)') CD
    write(UNIT_SUMMARY, '("TOTAL CD     =", F16.12)') CD
    
  end subroutine CDCOLE

  ! Fatal error in input - write message and stop
  ! CALLED BY - READIN, SCALE
  subroutine INPERR(I_ERROR_CODE)
    use common_data, only: UNIT_OUTPUT
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

  ! FIXPLT - Sets up arrays for CPPLOT subroutine
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

end module io_module
