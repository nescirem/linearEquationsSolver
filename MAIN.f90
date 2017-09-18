!Only for the full rank linear equation, which have a unique solution.
PROGRAM solve_liner_equation
	USE typedef
	IMPLICIT NONE

	!IO route
	CALL input_route
    !read in and slove liner equation
	CALL doolittle_elimination

	!Output result
	WRITE(*,*) solution
	!deallocate arrays
!	CALL dealloc
	
	STOP
	
END PROGRAM solve_liner_equation
	