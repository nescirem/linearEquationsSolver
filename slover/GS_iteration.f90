
SUBROUTINE GS_iteration
	USE typedef, only: n_var,A,b,solution,i_row,i_col
	USE algorithm_control, ONLY: iter,Err_limit
	IMPLICIT NONE
	INTERFACE
		REAL(8) FUNCTION norm_vec_inf(diff_solution)
			IMPLICIT NONE
			REAL(8),ALLOCATABLE :: diff_solution(:)
		END FUNCTION
	END INTERFACE
	INTEGER :: i_i,i_s,i_t
	REAL(8),ALLOCATABLE :: solution_old(:),diff_solution(:)
	REAL(8) :: add

	!read in martrix
	CALL read_matrix_GS
	
	ALLOCATE(solution_old(n_var),diff_solution(n_var))
	!initiate x_0
	solution=0.0d0
	
	DO i_i=1,iter
		solution_old=solution
		DO i_s=1,n_var
			add=0.0d0
			DO i_t=1,i_s-1
				add=add+A(i_t,i_s)*solution(i_t)
			ENDDO
			DO i_t=i_s+1,n_var
				add=add+A(i_t,i_s)*solution_old(i_t)
			ENDDO
			solution(i_s)=(b(i_s)-add)/A(i_s,i_s)
		ENDDO
		
		WRITE(*,"('iter=',I6)") i_i
		WRITE(*,"('   x1,                     x2,                     ...')")
		WRITE(*,*) solution(1),solution(2)

		diff_solution=solution-solution_old
		IF (norm_vec_inf(diff_solution)<Err_limit) THEN
			WRITE(*,"('Converged')")
			EXIT
		ELSEIF (norm_vec_inf(diff_solution)>1d31) THEN
			WRITE(*,"('Diverged')")
			EXIT
		ENDIF
	ENDDO
	
END SUBROUTINE GS_iteration