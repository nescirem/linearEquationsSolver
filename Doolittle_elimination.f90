SUBROUTINE doolittle_elimination
	USE typedef, ONLY: n_var,b,A,L,IL,U,solution,y,i_row,i_col
	IMPLICIT NONE
	LOGICAL l_invertible
	INTEGER :: i_i,i_s
	REAL(8) :: multiplier,add,det_A
	
	!read in martrix
	CALL read_matrix_doolittle	
	!initiate L,IL as unit matrix
	L=0.0d0
	IL=0.0d0
	FORALL(i_i=1:n_var) L(i_i,i_i)=1.0d0
	FORALL(i_i=1:n_var) IL(i_i,i_i)=1.0d0
	!initiate done
	
	!slove IL
	U=A
	DO i_row=1,n_var-1 
		DO i_col=i_row+1,n_var
			multiplier=-(U(i_col,i_row)/U(i_row,i_row))
			L(i_col,i_row)=multiplier
			IL=MATMUL(L,IL)
			U=MATMUL(L,U)
			L(i_col,i_row)=0.0d0
		ENDDO
	ENDDO
	L=0.0d0
	!invert matrix
	CALL invert_matrix(IL,L)
	
	!A invertible judgment
	det_A=1.0d0
	DO i_s=1,n_var
		det_A=det_A*U(i_s,i_s)
	ENDDO
	
	IF(det_A == 0) CALL error_output(9)
	
	!Reverse solution Ly=b
	y(1)=b(1)/L(1,1)
	DO i_row=2,n_var
		add=0.0d0
		DO i_col=1,i_row-1
			add=add+L(i_row,i_col)*y(i_col)
		ENDDO
		y(i_row)=(b(i_row)-add)/L(i_row,i_row)
	ENDDO
	!Reverse solution Ux=y,x represented solution
	solution(n_var)=y(n_var)/U(n_var,n_var)
	DO i_s=2,n_var
		i_row=n_var-i_s+1
		add=0.0d0
		DO i_col=i_row+1,n_var
			add=add+U(i_row,i_col)*solution(i_col)
		ENDDO
		solution(i_row)=(y(i_row)-add)/U(i_row,i_row)
	ENDDO

END SUBROUTINE doolittle_elimination