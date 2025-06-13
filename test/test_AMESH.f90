

program test_AMESH
    use common_data
    use io_module
    use math_module
    use spline_module
    use airfoil_module
    use mesh_module
    use solver_module
    use numerical_solvers
    implicit none

    call initialize_common()

    call READIN()
    write(*,*) 'A'

    call CUTOUT()
    write(*,*) 'B'
    call OUTPUT_MESH()


end program test_AMESH


