!Only for the full rank linear equation, which have a unique solution.
PROGRAM solve_liner_equation
	USE typedef
	USE algorithm_control, ONLY: method
	IMPLICIT NONE

	!IO route
	CALL input_route
	!algorithm control input
	CALL read_control
    !read in matrix and slove liner equation
	SELECT CASE (method)
	CASE(1)
		CALL gauss_elimination
	CASE(2)
		CALL LU_decomposition
	CASE(3)
		CALL jacobi_iteration
	CASE(4)
		CALL GS_iteration
	CASE(5)
		CALL SOR_iteration
	END SELECT
	!Output result
	WRITE(*,*) solution
	!deallocate arrays
!	CALL dealloc
	
	STOP
	
END PROGRAM solve_liner_equation
	