REAL(8) FUNCTION norm_vec_inf(diff_solution)
	IMPLICIT NONE
	REAL(8),ALLOCATABLE :: diff_solution(:)

	diff_solution=ABS(diff_solution)
	norm_vec_inf=MAXVAL(diff_solution)
	
	RETURN
END FUNCTION 