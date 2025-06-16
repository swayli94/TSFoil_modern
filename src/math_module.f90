! math_module.f90
! Module for general mathematical utilities

module math_module
  implicit none
  private

  public :: ARF, SIMP, PX, PY, EMACH1, DRAG, LIFT, PITCH, TRAP
  public :: FINDSK, NEWISK, report_convergence_error

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

  ! Function PX computes U = DP/DX at point I,J
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
  
  ! Function PY computes V = DP/DY at point I,J
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
  
  ! Function EMACH1 computes local similarity parameter or local Mach number
  function EMACH1(U) result(result_emach)
    use common_data, only: AK, GAM1, PHYS, DELRT2, SIMDEF, EMROOT, EMACH, UNIT_OUTPUT
    implicit none
    real, intent(in) :: U
    real :: result_emach
    real :: AK1, ARG
    
    ! Compute similarity parameter based on local velocity
    AK1 = AK - GAM1*U
    
    if (.not. PHYS) then
      ! Return value of local similarity parameter
      result_emach = AK1
    else
      ! Compute value of local Mach number and return
      if (SIMDEF == 1) then ! Cole scaling
        ARG = 1.0 - DELRT2*AK1
      else if (SIMDEF == 2) then ! Spreiter scaling
        ARG = 1.0 - EMROOT*EMROOT*DELRT2*AK1
      else if (SIMDEF == 3) then ! Krupp scaling
        ARG = 1.0 - EMACH*DELRT2*AK1
      else
        write(UNIT_OUTPUT, '(A, /, A, I3)') '1ABNORMAL STOP IN SUBROUTINE EMACH1', ' SIMDEF not supported', SIMDEF
        stop
      end if

      result_emach = 0.0
      if (ARG > 0.0) result_emach = sqrt(ARG)
      
    end if
  end function EMACH1
  
  ! Function LIFT computes pressure drag coefficient by integrating
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

  ! Function LIFT computes lift coefficient from jump in P at trailing edge
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
  
  ! Function PITCH computes airfoil pitching moment about X = XM, Y = 0
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
  
  ! Subroutine TRAP integrates Y DX by trapezoidal rule
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

  ! Subroutine to find shock location on line J between ISTART and IEND
  subroutine FINDSK(ISTART, IEND, J, ISK)
    use common_data, only: SONVEL
    implicit none
    integer, intent(in) :: ISTART, IEND, J
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
  subroutine NEWISK(ISKOLD, J, ISKNEW)
    use common_data, only: SONVEL
    implicit none
    integer, intent(in) :: ISKOLD, J
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

end module math_module
