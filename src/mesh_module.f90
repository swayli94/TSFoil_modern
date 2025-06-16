! mesh_module.f90
! Module for mesh generation and refinement routines

module mesh_module
  use common_data, only: N_MESH_POINTS
  implicit none
  private
  public :: CKMESH, ISLIT, JSLIT, setup_mesh


  ! Free-stream/tunnel default mesh arrays
  real :: YFREE(N_MESH_POINTS), YTUN(N_MESH_POINTS)
  integer :: JMXF, JMXT
  

contains

  ! Setup mesh
  ! This subroutine contains functions that are originally in READIN (io_module.f90)
  subroutine setup_mesh()
    use common_data, only: X, Y, XIN, YIN, IMIN, IMAX
    use common_data, only: JMIN, JMAX, IMAXI, JMAXI
    use common_data, only: N_MESH_POINTS
    use common_data, only: BCTYPE, INPERR
    implicit none
    integer :: J_VAR, IDX, JDX, I_ITER, J_ITER

    call setup_default_mesh()

    ! Handle YIN initialization
    if (YIN(JMIN) == 0.0) then
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

    ! Set derived constants
    IMAX = IMAXI
    JMAX = JMAXI

    ! Check array bounds (any call to INPERR causes message to be printed and execution stopped)
    if (IMAXI > N_MESH_POINTS .or. JMAXI > N_MESH_POINTS) call INPERR(1)
    
    ! Check input mesh for monotonically increasing values
    do IDX = IMIN, IMAX - 1
      if (XIN(IDX) >= XIN(IDX+1)) call INPERR(2)
    end do
    
    do JDX = JMIN, JMAX - 1
      if (YIN(JDX) >= YIN(JDX+1)) call INPERR(3)
    end do
    
    ! Compute ILE and ITE (leading and trailing edge)
    call ISLIT(XIN)
    call JSLIT(YIN)

    ! Check number of mesh points, if not odd add points to appropriate areas to make odd no.
    call CKMESH()
    
    ! Load XIN,YIN into X,Y
    do I_ITER = IMIN, IMAX
      X(I_ITER) = XIN(I_ITER)
    end do
    do J_ITER = JMIN, JMAX
      Y(J_ITER) = YIN(J_ITER)
    end do

  end subroutine SETUP_MESH

  ! Ensure odd/even mesh counts before/after tail and slit
  subroutine CKMESH()
    use common_data, only: XIN, YIN, IMIN, IMAX, ITE
    use common_data, only: JMIN, JMAX, JLOW, JUP
    use common_data, only: UNIT_OUTPUT, N_MESH_POINTS
    use common_data, only: BCTYPE, H
    implicit none
    integer :: I, LP, L, J, JDX
    real :: TERM, HTM, HTP, YS, YE
    
    ! Add extra X-point ahead of airfoil if needed (check for even number of points)
    if (mod(ITE - IMIN + 1, 2) == 0) then
      LP = IMAX + IMIN + 1
      do I = IMIN, IMAX
        L = LP - I
        XIN(L) = XIN(L - 1)
      end do
      IMAX = IMAX + 1
      XIN(IMIN) = 2.0 * XIN(IMIN + 1) - XIN(IMIN + 2)
      call ISLIT(XIN)
    end if

    ! Add extra X-point after airfoil if needed (check for even number of points)
    if (mod(IMAX - ITE + 1, 2) == 0) then
      IMAX = IMAX + 1
      XIN(IMAX) = 2.0 * XIN(IMAX - 1) - XIN(IMAX - 2)
    end if    
    
    ! Check Y mesh and adjust to contain even number of points above and below slit
    ! Add extra Y-point below slit if needed (check for even number of points)
    if (mod(JLOW - JMIN, 2) == 0) then
      LP = JMAX + JMIN + 1
      do J = JMIN, JMAX
        L = LP - J
        YIN(L) = YIN(L - 1)
      end do
      JMAX = JMAX + 1
      YIN(JMIN) = 2.0 * YIN(JMIN + 1) - YIN(JMIN + 2)
      call JSLIT(YIN)
    end if

    ! Add extra Y-point above slit if needed (check for even number of points)
    if (mod(JMAX - JUP, 2) == 0) then
      JMAX = JMAX + 1
      YIN(JMAX) = 2.0 * YIN(JMAX - 1) - YIN(JMAX - 2)
    end if

    ! Check bounds of YIN (mesh y coordinates) for tunnel calculations
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

  end subroutine CKMESH
  
  ! Compute ILE and ITE for mesh X array
  subroutine ISLIT(X_MESH)
    use common_data, only: IMIN, IMAX, ILE, ITE
    implicit none
    real, intent(in) :: X_MESH(:)
    integer :: i

    ! Find first point where X >= 0.0 (leading edge)
    ! Exactly matching original FORTRAN logic with bounds checking
    i = IMIN - 1
    do
      i = i + 1
      if (i > IMAX) then
        ! If no point found with X >= 0.0, set ILE to IMIN
        ILE = IMIN
        exit
      end if
      if (X_MESH(i) >= 0.0) then
        ILE = i
        exit
      end if
    end do
    
    ! Find first point where X > 1.0 (trailing edge) 
    do
      i = i + 1
      if (i > IMAX) then
        ! If no point found with X > 1.0, set ITE to IMAX
        ITE = IMAX
        exit
      end if
      if (X_MESH(i) > 1.0) then
        ITE = i - 1
        exit
      end if
    end do
  end subroutine ISLIT
  
  ! Compute JLOW and JUP for mesh Y array
  ! JSLIT computes:
  ! 1.) JLOW = index of first point where Y >= 0.0
  ! 2.) JUP = index of first point where Y > 0.0
  ! Called by - CKMESH, CUTOUT, REFINE
  subroutine JSLIT(Y_MESH)
    use common_data, only: JMIN, JLOW, JUP
    implicit none
    real, intent(in) :: Y_MESH(:)
    integer :: j

    ! Y_MESH is the Y-mesh array, size(Y_MESH) = JMAXI
    ! JMIN is the minimum index of the Y-mesh array
    ! JMAXI is the maximum index of the Y-mesh array

    j = JMIN - 1
    do
      j = j + 1
      if (Y_MESH(j) >= 0.0) exit
    end do
    JLOW = j - 1
    JUP  = j
  end subroutine JSLIT

  ! default mesh distribution
  subroutine setup_default_mesh()
    use common_data, only: XIN, IMIN
    implicit none

    ! Initialize XIN array with default mesh distribution
    if (XIN(IMIN) == 0.0) then
      XIN = 0.0
      XIN(1:77) = (/ -1.075, -0.950, -0.825, -0.7, -0.575, -0.45, -0.35, &
                   -0.25, -0.175, -0.125, -0.075, -0.0525, -0.035, -0.0225, -0.015, &
                   -0.0075, -0.0025, 0.0025, 0.0075, 0.0125, 0.0175, 0.0225, &
                   0.0275, 0.0325, 0.0375, 0.045, 0.055, 0.065, 0.075, 0.085, &
                   0.0975, 0.115, 0.140625, 0.171875, 0.203125, 0.234375, 0.265625, &
                   0.296875, 0.328125, 0.359375, 0.390625, 0.421875, 0.453125, &
                   0.484375, 0.515625, 0.546875, 0.578125, 0.609375, 0.640625, &
                   0.671875, 0.703125, 0.734375, 0.765625, 0.796875, 0.828125, &
                   0.859375, 0.885, 0.9, 0.915, 0.93, 0.945, 0.96, 0.975, 0.99, &
                   1.0, 1.01, 1.025, 1.05, 1.09, 1.15, 1.225, 1.3, 1.4, 1.5, &
                   1.625, 1.75, 1.875 /)
    end if
    
    JMXF = 56  ! Maximum number of grid points for free-air distribution (YFREE)
    JMXT = 48  ! Maximum number of grid points for tunnel distribution (YTUN)

    ! Initialize YFREE array with default free-air distribution (from BLOCK DATA)
    YFREE = 0.0
    YFREE(1:56) = (/ -5.2, -4.4, -3.6, -3.0, -2.4, -1.95, -1.6, -1.35, -1.15, -0.95, &
                     -0.80, -0.65, -0.55, -0.45, -0.39, -0.34, -0.30, -0.27, -0.24, -0.21, &
                     -0.18, -0.15, -0.125, -0.1, -0.075, -0.05, -0.03, -0.01, 0.01, 0.03, &
                     0.05, 0.075, 0.1, 0.125, 0.15, 0.18, 0.21, 0.24, 0.27, 0.30, &
                     0.34, 0.39, 0.45, 0.55, 0.65, 0.8, 0.95, 1.15, 1.35, 1.60, &
                     1.95, 2.4, 3.0, 3.6, 4.4, 5.2 /)
    
    ! Initialize YTUN array with default tunnel distribution (from BLOCK DATA)
    YTUN = 0.0
    YTUN(1:48) = (/ -2.0, -1.8, -1.6, -1.4, -1.2, -1.0, -0.8, -0.65, -0.55, -0.45, &
                    -0.39, -0.34, -0.30, -0.27, -0.24, -0.21, -0.18, -0.15, -0.125, -0.1, &
                    -0.075, -0.05, -0.03, -0.01, 0.01, 0.03, 0.05, 0.075, 0.1, 0.125, &
                    0.15, 0.18, 0.21, 0.24, 0.27, 0.3, 0.34, 0.39, 0.45, 0.55, &
                    0.65, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0 /)

  end subroutine setup_default_mesh

end module mesh_module
