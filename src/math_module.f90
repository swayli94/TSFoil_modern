! math_module.f90
! Module for general mathematical utilities

module math_module
  implicit none
  private

  public :: ARF, SIMP, PX, PY, EMACH1, DRAG, LIFT, PITCH, TRAP
  public :: FINDSK, NEWISK, report_convergence_error, CDCOLE

contains

  ! Rational approximation for the error function erf(X)
  ! Error < 1.5E-7 by rational approximation 7.1.26 from 
  ! Handbook of Math. Functions, U.S. Dept. of Commerce, NBS Appl Math Ser 55
  function ARF(X_in) result(Y_out)
    implicit none
    real, intent(in) :: X_in
    real :: Y_out
    real :: T, POLY, Y_ABS
    real, parameter :: C(5) = [1.061405429, -1.453152027, 1.421413741, &
                              -0.284496736, 0.254829592]
    integer :: I_loop

    Y_ABS = abs(X_in)
    if (Y_ABS >= 10.0) then
      Y_out = 1.0
    else
      T = 1.0 / (1.0 + 0.3275911 * Y_ABS)
      POLY = 0.0
      do I_loop = 1, 5
        POLY = (POLY + C(I_loop)) * T
      end do
      Y_out = 1.0 - POLY * exp(-Y_ABS * Y_ABS)
    end if
    
    if (X_in < 0.0) Y_out = -Y_out
  end function ARF  
  
  ! Simpson's rule integration for non-uniform spacing
  ! Integrates Y(X) from X(1) to X(N) using variable-spacing Simpson's rule
  ! Original implementation from TSFOIL for non-uniform grids
  subroutine SIMP(R, X_arr, Y_arr, N, IER)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: X_arr(N), Y_arr(N)
    real, intent(out) :: R
    integer, intent(out) :: IER
    integer :: I, NM1, NM2, N1
    real :: S1, S2, S3, S4, P

    R = 0.0
    
    ! Check for valid input
    if (N <= 1) then
      IER = 2
      return
    end if
    
    ! Check for identical first two points
    if (X_arr(1) == X_arr(2)) then
      IER = 4
      return
    end if
    
    NM1 = N - 1
    
    ! Handle N=2 case with trapezoidal rule
    if (N == 2) then
      R = (X_arr(2) - X_arr(1)) * (Y_arr(1) + Y_arr(2)) / 2.0
      IER = 1
      return
    end if
    
    ! Test for monotonically increasing or decreasing X
    if (X_arr(1) < X_arr(2)) then
      ! Test for monotonically increasing
      do I = 2, NM1
        if (X_arr(I+1) <= X_arr(I)) then
          IER = 4
          return
        end if
      end do

    else
      ! Test for monotonically decreasing
      do I = 2, NM1
        if (X_arr(I+1) >= X_arr(I)) then
          IER = 4
          return
        end if
      end do

    end if
    
    NM2 = N - 2
    P = 0.0
    
    ! Handle even N case - fit polynomial through first 3 points
    if (mod(N, 2) == 0) then
      S1 = X_arr(2) - X_arr(1)
      S2 = X_arr(3) - X_arr(1)
      S3 = Y_arr(2) - Y_arr(1)
      S4 = Y_arr(3) - Y_arr(1)
      P = S1/6.0 * (2.0*S3 + 6.0*Y_arr(1) + (S2*S2*S3 - S1*S1*S4)/(S2*(S2-S1)))
      N1 = 2
    else
      N1 = 1
    end if
    
    ! Apply Simpson's rule with non-uniform spacing
    S1 = X_arr(N1+1) - X_arr(N1)
    S2 = X_arr(N1+2) - X_arr(N1+1)
    S3 = X_arr(NM1) - X_arr(NM2)
    S4 = X_arr(N) - X_arr(NM1)
    
    R = (2.0*S1*S1 + S1*S2 - S2*S2)/S1 * Y_arr(N1) + &
        (2.0*S4*S4 + S3*S4 - S3*S3)/S4 * Y_arr(N)
    
    N1 = N1 + 1
    
    ! Middle points with odd indices
    do I = N1, NM1, 2
      S1 = X_arr(I) - X_arr(I-1)
      S2 = X_arr(I+1) - X_arr(I)
      R = R + (S1 + S2)**3 / (S1 * S2) * Y_arr(I)
    end do
    
    ! Handle points with even indices if N >= 5
    if (N >= 5) then
      N1 = N1 + 1
      do I = N1, NM2, 2
        S1 = X_arr(I-1) - X_arr(I-2)
        S2 = X_arr(I) - X_arr(I-1)
        S3 = X_arr(I+1) - X_arr(I)
        S4 = X_arr(I+2) - X_arr(I+1)
        R = R + ((2.0*S2*S2 + S1*S2 - S1*S1)/S2 + &
                 (2.0*S3*S3 + S3*S4 - S4*S4)/S3) * Y_arr(I)
      end do
    end if
    
    R = R/6.0 + P
    IER = 1
    
  end subroutine SIMP

  ! Computes U = DP/DX at point I,J
  function PX(I, J) result(result_px)
    use common_data, only: IMIN, IMAX, P, XDIFF
    implicit none
    integer, intent(in) :: I, J
    real :: result_px
    real :: PJI
    
    ! Test to locate end points
    if (I == IMIN) then
      ! Upstream boundary
      result_px = 1.5*XDIFF(I+1)*(P(J,I+1)-P(J,I)) - &
                  0.5*XDIFF(I+2)*(P(J,I+2)-P(J,I+1))
    else if (I == IMAX) then
      ! Downstream boundary  
      result_px = 1.5*XDIFF(I)*(P(J,I)-P(J,I-1)) - &
                  0.5*XDIFF(I-1)*(P(J,I-1)-P(J,I-2))
    else
      ! Interior mesh point
      PJI = P(J,I)
      result_px = 0.5*(XDIFF(I+1)*(P(J,I+1)-PJI) + XDIFF(I)*(PJI-P(J,I-1)))
    end if
  end function PX
  
  ! Computes V = DP/DY at point I,J
  function PY(I, J) result(result_py)
    use common_data, only: JMIN, JMAX, JUP, JLOW, ILE, ITE, P
    use common_data, only: YDIFF, ALPHA, FXL, FXU, PJUMP
    implicit none
    integer, intent(in) :: I, J
    real :: result_py
    real :: PJI, VMINUS, VPLUS
    integer :: IC
    
    ! Test for end points or points near airfoil slit
    if (J == JMIN) then
      ! I,J is on lower boundary. Use one sided derivative
      result_py = 1.5*YDIFF(J+1)*(P(J+1,I) - P(J,I)) - &
                  0.5*YDIFF(J+2)*(P(J+2,I) - P(J+1,I))
      return
    else if (J == JLOW) then
      ! I,J is on row of mesh points below airfoil
      VMINUS = YDIFF(J)*(P(J,I) - P(J-1,I))
      
      ! Test to see if I,J is ahead, under, or behind slit
      if (I < ILE) then
        ! I,J is ahead of airfoil
        result_py = 0.5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
      else if (I > ITE) then
        ! I,J is behind airfoil
        result_py = 0.5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
      else
        ! I,J is under airfoil. Use derivative boundary condition
        IC = I - ILE + 1
        result_py = 0.5 * (FXL(IC) - ALPHA + VMINUS)
      end if
      return
    else if (J == JUP) then
      ! I,J is on row of mesh points above airfoil
      VPLUS = YDIFF(J+1)*(P(J+1,I) - P(J,I))
      
      ! Test to see if I is ahead of, over, or behind airfoil slit
      if (I < ILE) then
        ! I,J is ahead of airfoil
        result_py = 0.5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
      else if (I > ITE) then
        ! I,J is behind airfoil
        result_py = 0.5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
      else
        IC = I - ILE + 1
        result_py = 0.5 * (VPLUS + FXU(IC) - ALPHA)
      end if
      return
    else if (J == JMAX) then
      ! I,J is on top row of mesh points. Use one sided formula
      result_py = 1.5*YDIFF(J)*(P(J,I) - P(J-1,I)) - &
                  0.5*YDIFF(J-1)*(P(J-1,I) - P(J-2,I))
      return
    else
      ! I,J is an interior point
      PJI = P(J,I)
      result_py = 0.5*(YDIFF(J+1)*(P(J+1,I)-PJI) + YDIFF(J)*(PJI-P(J-1,I)))
    end if
  end function PY
  
  ! Computes local similarity parameter or local Mach number
  ! Called by - VWEDGE, PRINT_SHOCK, OUTPUT_FIELD
  function EMACH1(U, DELTA, SIMDEF) result(result_emach)
    use common_data, only: AK, GAM1, PHYS, EMACH, UNIT_OUTPUT
    implicit none
    real, intent(in) :: U         ! Local velocity
    real, intent(in) :: DELTA     ! Maximum thickness of airfoil
    integer, intent(in) :: SIMDEF ! Scaling definition
    real :: result_emach
    real :: AK1, ARG, DELRT2
    
    ! Compute similarity parameter based on local velocity
    AK1 = AK - GAM1*U
    
    if (.not. PHYS) then
      ! Return value of local similarity parameter
      result_emach = AK1
      
    else
      ! Compute value of local Mach number and return
      DELRT2 = DELTA**(2.0/3.0)

      if (SIMDEF == 1) then ! Cole scaling
        ARG = 1.0 - DELRT2*AK1
      else if (SIMDEF == 2) then ! Spreiter scaling
        ARG = 1.0 - DELRT2*AK1*EMACH**(4.0/3.0)
      else if (SIMDEF == 3) then ! Krupp scaling
        ARG = 1.0 - DELRT2*AK1*EMACH
      else
        write(UNIT_OUTPUT, '(A, /, A, I3)') '1ABNORMAL STOP IN SUBROUTINE EMACH1', ' SIMDEF not supported', SIMDEF
        stop
      end if

      result_emach = 0.0
      if (ARG > 0.0) result_emach = sqrt(ARG)
      
    end if
  end function EMACH1
  
  ! Computes pressure drag coefficient by integrating
  ! U*V around airfoil using trapezoidal rule.
  function DRAG(CDFACT_in) result(result_drag)
    use common_data, only: X, ILE, ITE, JUP, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: FXU, FXL, N_MESH_POINTS
    implicit none
    real, intent(in) :: CDFACT_in
    real :: result_drag
    real :: PXUP, PXLOW, SUM
    real :: XI(N_MESH_POINTS), ARG(N_MESH_POINTS)
    integer :: K, I
    
    K = 1
    ARG(1) = 0.0
    XI(1) = X(ILE-1)
    do I = ILE, ITE
        K = K + 1
        PXUP = CJUP*PX(I,JUP) - CJUP1*PX(I,JUP+1)
        PXLOW = CJLOW*PX(I,JLOW) - CJLOW1*PX(I,JLOW-1)
        ARG(K) = FXU(K-1)*PXUP - FXL(K-1)*PXLOW
        XI(K) = X(I)
    end do
    K = K + 1
    ARG(K) = 0.0
    XI(K) = X(ITE+1)
    call TRAP(XI, ARG, K, SUM)
    result_drag = -SUM*CDFACT_in*2.0
  end function DRAG

  ! Computes lift coefficient from jump in P at trailing edge
  function LIFT(CLFACT_in) result(result_lift)
    use common_data, only: P, JUP, ITE, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    implicit none
    real, intent(in) :: CLFACT_in
    real :: result_lift
    real :: PTOP, PBOT
    
    PTOP = CJUP*P(JUP,ITE) - CJUP1*P(JUP+1,ITE)
    PBOT = CJLOW*P(JLOW,ITE) - CJLOW1*P(JLOW-1,ITE)
    result_lift = 2.0*CLFACT_in*(PTOP-PBOT)
  end function LIFT
  
  ! Computes airfoil pitching moment about X = XM, Y = 0
  function PITCH(CMFACT_in) result(result_pitch)
    use common_data, only: P, X, ILE, ITE, JUP, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: N_MESH_POINTS
    implicit none
    real, intent(in) :: CMFACT_in
    real :: result_pitch
    real :: XM, PTOP, PBOT, SUM
    real :: XI(N_MESH_POINTS), ARG(N_MESH_POINTS)
    integer :: K, I_loop
    
    ! Set XM to quarter chord
    XM = 0.25
    K = 0
    do I_loop = ILE, ITE
      K = K + 1
      PTOP = CJUP*P(JUP,I_loop) - CJUP1*P(JUP+1,I_loop)
      PBOT = CJLOW*P(JLOW,I_loop) - CJLOW1*P(JLOW-1,I_loop)
      ARG(K) = PTOP - PBOT
      XI(K) = X(I_loop)
    end do
    call TRAP(XI, ARG, K, SUM)
    result_pitch = CMFACT_in*((1.0-XM)*ARG(K) - SUM) * (-2.0)
  end function PITCH
  
  ! Integrates Y DX by trapezoidal rule
  subroutine TRAP(X_arr, Y_arr, N, SUM)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: X_arr(N), Y_arr(N)
    real, intent(out) :: SUM
    integer :: I_loop, NM1
    real :: Z, W
    
    SUM = 0.0
    NM1 = N - 1
    do I_loop = 1, NM1
      Z = X_arr(I_loop+1) - X_arr(I_loop)
      W = Y_arr(I_loop+1) + Y_arr(I_loop)
      SUM = SUM + Z*W
    end do
    SUM = 0.5*SUM
  end subroutine TRAP
  
  ! Helper subroutine for convergence error reporting
  subroutine report_convergence_error(subroutine_name, variable_name, iteration_number)
    use common_data, only: UNIT_OUTPUT
    implicit none
    character(len=*), intent(in) :: subroutine_name, variable_name
    integer, intent(in) :: iteration_number
    
    write(*,'(A,A)') 'ABNORMAL STOP IN SUBROUTINE ', subroutine_name
    write(*,'(A,A,I0)') 'NONCONVERGENCE OF ITERATION FOR ', variable_name, iteration_number
    write(UNIT_OUTPUT,'(A,A)') 'ABNORMAL STOP IN SUBROUTINE ', subroutine_name  
    write(UNIT_OUTPUT,'(A,A,I0)') 'NONCONVERGENCE OF ITERATION FOR ', variable_name, iteration_number
    stop
  end subroutine report_convergence_error

  ! Finds shock location on line J between ISTART and IEND
  subroutine FINDSK(ISTART, IEND, J, SONVEL, ISK)
    implicit none
    integer, intent(in) :: ISTART, IEND, J
    real, intent(in) :: SONVEL
    integer, intent(out) :: ISK
    real :: U1, U2
    
    ISK = ISTART - 1
    U2 = PX(ISK, J)
    
    do
      ISK = ISK + 1
      U1 = U2
      U2 = PX(ISK, J)
      if (U1 > SONVEL .and. U2 <= SONVEL) exit
      if (ISK >= IEND) then
        ISK = -IEND
        exit
      end if
    end do
  end subroutine FINDSK

  ! Find new location of shockwave (ISKNEW) on line J
  ! given an initial guess for location (ISKOLD).
  ! Shock location is defined as location of shock point.
  ! If no shock is found, ISKNEW is set negative.
  ! Called by - CDCOLE.
  subroutine NEWISK(ISKOLD, J, SONVEL, ISKNEW)
    implicit none
    integer, intent(in) :: ISKOLD, J
    real, intent(in) :: SONVEL
    integer, intent(out) :: ISKNEW
    integer :: I2
    real :: U1, U2
    
    I2 = ISKOLD + 2
    ISKNEW = ISKOLD - 3
    U2 = PX(ISKNEW, J)
    
    do
      ISKNEW = ISKNEW + 1
      U1 = U2
      U2 = PX(ISKNEW, J)
      if (U1 > SONVEL .and. U2 <= SONVEL) exit
      if (ISKNEW >= I2) then
        ! No shock point found, tip of shock reached
        ISKNEW = -ISKNEW
        exit
      end if
    end do
  end subroutine NEWISK

  ! Compute drag coefficient by momentum integral method
  ! Integrates around a contour enclosing the body and along all shocks inside the contour
  ! CALLED BY - PRINT.
  subroutine CDCOLE(SONVEL, YFACT, DELTA)
    use common_data, only: X, Y, IMIN, IMAX, IUP, ILE, ITE, N_MESH_POINTS
    use common_data, only: JMIN, JMAX, JUP, JLOW
    use common_data, only: AK, GAM1, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: CDFACT
    use common_data, only: FXL, FXU
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    implicit none

    real, intent(in) :: SONVEL  ! Speed of sound
    real, intent(in) :: YFACT   ! Scaling factor for Y-coordinate
    real, intent(in) :: DELTA   ! Maximum thickness of airfoil
    
    ! Local variables
    integer :: IU, ID, JT, JB, ISTOP, IBOW, ISK, JSTART, J, JJ, JSK, ISKOLD
    integer :: ILIM, IB, I, L, NSHOCK, LPRT1, LPRT2, ISTART
    real :: GAM123, U, V, UU, UL, SUM, CDSK, CDWAVE, CDC, CD
    real :: CDUP, CDTOP, CDBOT, CDDOWN, CDBODY
    real :: XU_LOC, XD_LOC, YT_LOC, YB_LOC, ULE
    real :: XI(N_MESH_POINTS), ARG(N_MESH_POINTS)
    
    GAM123 = GAM1 * 2.0 / 3.0
    ISKOLD = 0
    
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
      call FINDSK(IUP, ISTOP, JUP, SONVEL, IBOW)
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
        call NEWISK(ISKOLD, J, SONVEL, ISK)
        if (ISK < 0) exit
      end do
      
      ! Search down shock to find tip of subsonic region
      ISK = IBOW
      JB = JLOW + 2
      do J = JMIN, JLOW
        JJ = JLOW - J + JMIN
        JB = JB - 1
        ISKOLD = ISK
        call NEWISK(ISKOLD, JJ, SONVEL, ISK)
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
        call NEWISK(ISKOLD, J, SONVEL, ISK)
        XI(L) = Y(J)
        ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
      end do
      call TRAP(XI, ARG, L, SUM)
      CDSK = -GAM1/6.0 * CDFACT * SUM
      CDWAVE = CDWAVE + CDSK
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1, YFACT, DELTA)
    end if
      
    ! Integrate along shocks above airfoil
    ISTART = ILE
    
    ! Loop to find and process all shocks above airfoil
    do
      call FINDSK(ISTART, ITE, JUP, SONVEL, ISK)
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
        call NEWISK(ISKOLD, JSK, SONVEL, ISK)
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
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1, YFACT, DELTA)
      if (LPRT1 == 1) LPRT2 = 1
    end do
      
    ! Integrate along shocks below airfoil
    ISTART = ILE
    
    ! Loop to find and process all shocks below airfoil  
    do
      call FINDSK(ISTART, ITE, JLOW, SONVEL, ISK)
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
        call NEWISK(ISKOLD, JSK, SONVEL, ISK)
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
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1, YFACT, DELTA)
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
    write(UNIT_OUTPUT, '(A)') '1'
    write(UNIT_OUTPUT, '(A)') ' CALCULATION OF DRAG COEFFICIENT BY MOMENTUM INTEGRAL METHOD'
    write(UNIT_OUTPUT, '(A)') ''
    write(UNIT_OUTPUT, '(A)') ' BOUNDARIES OF CONTOUR USED CONTRIBUTION TO CD'
    write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' UPSTREAM    X =', XU_LOC, '  CDUP   =', CDUP
    write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' DOWNSTREAM  X =', XD_LOC, '  CDDOWN =', CDDOWN  
    write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' TOP         Y =', YT_LOC, '  CDTOP  =', CDTOP
    write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' BOTTOM      Y =', YB_LOC, '  CDBOT  =', CDBOT
    write(UNIT_OUTPUT, '(A)') ''
    write(UNIT_OUTPUT, '(A,I3)')    'Number of shock inside contour, N =      ', NSHOCK
    write(UNIT_OUTPUT, '(A,F15.9)') 'Body aft location,              X =      ', XD_LOC
    write(UNIT_OUTPUT, '(A,F15.9)') 'Drag due to body,               CD_body =', CDBODY
    write(UNIT_OUTPUT, '(A,F15.9)') 'Drag due to shock,              CD_wave =', CDWAVE
    write(UNIT_OUTPUT, '(A,F15.9)') 'Drag by momentum integral,      CD_int = ', CDC
    write(UNIT_OUTPUT, '(A,F15.9)') 'Total drag (CD_int + CD_wave),  CD =     ', CD
    write(UNIT_OUTPUT, '(A)') ''

    if (NSHOCK > 0 .and. LPRT2 == 0) then
      write(UNIT_OUTPUT, '("NOTE - All shocks contained within contour, CD_wave equals total wave drag")')
    end if
    
    if (NSHOCK > 0 .and. LPRT2 == 1) then
      write(UNIT_OUTPUT, '("NOTE - One or more shocks extend outside of contour, CD_wave does not equal total wave drag")')
    end if

    write(UNIT_SUMMARY, '(A,I3)')    'Number of shock inside contour, N =      ', NSHOCK
    write(UNIT_SUMMARY, '(A,F15.9)') 'Body aft location,              X =      ', XD_LOC
    write(UNIT_SUMMARY, '(A,F15.9)') 'Drag due to body,               CD_body =', CDBODY
    write(UNIT_SUMMARY, '(A,F15.9)') 'Drag due to shock,              CD_wave =', CDWAVE
    write(UNIT_SUMMARY, '(A,F15.9)') 'Drag by momentum integral,      CD_int = ', CDC
    write(UNIT_SUMMARY, '(A,F15.9)') 'Total drag (CD_int + CD_wave),  CD =     ', CD

  end subroutine CDCOLE

  ! Print shock wave drag contributions and total pressure loss along shock wave
  ! PRINTOUT WAVE DRAG CONTRIBUTION AND TOTAL PRESSURE
  ! LOSS ALONG SHOCK WAVE
  ! CALLED BY - CDCOLE.
  subroutine PRTSK(XI,ARG,L,NSHOCK,CDSK,LPRT1,YFACT,DELTA)
    use common_data, only: CDFACT, GAM1, UNIT_OUTPUT, N_MESH_POINTS
    implicit none
    real, intent(in) :: XI(N_MESH_POINTS), ARG(N_MESH_POINTS)
    integer, intent(in) :: L, NSHOCK, LPRT1
    real, intent(in) :: CDSK
    real, intent(in) :: YFACT
    real, intent(in) :: DELTA   ! Maximum thickness of airfoil
    real :: CDYCOF, POYCOF, YY, CDY, POY
    integer :: K

    CDYCOF = -CDFACT * GAM1 / (6.0 * YFACT)
    POYCOF = DELTA**2 * GAM1 * (GAM1 - 1.0) / 12.0
    
    ! Write header for first shock wave only (format 1001 equivalent)
    if (NSHOCK == 1) then
      write(UNIT_OUTPUT, '(A)') '0'
      write(UNIT_OUTPUT,'(A)') ' INVISCID WAKE PROFILES FOR INDIVIDUAL SHOCK WAVES WITHIN MOMENTUM CONTOUR'
    end if
    
    ! Write shock information (format 1002 equivalent)
    write(UNIT_OUTPUT,'(A)') ''  ! blank line for 0 carriage control
    write(UNIT_OUTPUT,'(A,I3)') 'SHOCK', NSHOCK
    write(UNIT_OUTPUT,'(A,F12.6)') ' WAVE DRAG FOR THIS SHOCK=', CDSK
    write(UNIT_OUTPUT,'(A,A,A,A,A)') '      Y', '         ', 'CD(Y)', '        ', 'PO/POINF'
    
    ! Write shock profile data (format 1003 equivalent)
    do K = 1, L
      YY = XI(K) * YFACT
      CDY = CDYCOF * ARG(K)
      POY = 1.0 + POYCOF * ARG(K)
      write(UNIT_OUTPUT,'(1X,3F12.8)') YY, CDY, POY
    end do
    
    ! Write footer if shock extends outside contour (format 1004 equivalent)
    if (LPRT1 == 1) then
      write(UNIT_OUTPUT,'(A)') ''  ! blank line for 0 carriage control
      write(UNIT_OUTPUT,'(A)') ' SHOCK WAVE EXTENDS OUTSIDE CONTOUR'
      write(UNIT_OUTPUT,'(A)') ' PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK'
    end if
  end subroutine PRTSK

end module math_module
