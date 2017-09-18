SUBROUTINE gauss_elimination
	USE typedef, ONLY: A,y,n_var,column,row,solution,i_row,i_col
	IMPLICIT NONE
	INTEGER :: i_rr,i_s,ti_c
	REAL(8) :: multiplier,add
	
	!Read in matrix which represent a linear equation.
	CALL read_matrix_gauss
	
	!Have a solution or not judgement
	
	!Gaussian Elimination with Pivoting Method
	DO i_row=1,n_var 
        !find the largest (in absolute value) element among lower triangular part of that matrix
		DO i_col=i_row+1,n_var !ehchange rows
			IF (ABS(A(i_row,i_col)) .GT. ABS(A(i_row,i_row))) THEN
				y(:)=A(:,i_col)
				A(:,i_col)=A(:,i_row)
				A(:,i_row)=y
			ELSE
				CYCLE
			ENDIF
		ENDDO
        !Elimination
        DO i_col=i_row+1,n_var
		    multiplier=-(A(i_row,i_col)/A(i_row,i_row))
			FORALL (i_rr=i_row:row)
				A(i_rr,i_col)=A(i_rr,i_col)+A(i_rr,i_row)*multiplier
			ENDFORALL
		ENDDO
	ENDDO
	!Reverse solution
	solution(n_var)=A(row,column)/A(n_var,n_var)
	DO i_s=2,column
		ti_c=n_var-i_s+1
		add=0.0d0
		DO i_row=ti_c+1,n_var
			add=add+A(i_row,ti_c)*solution(i_row)
		ENDDO
		solution(ti_c)=(A(row,ti_c)-add)/A(ti_c,ti_c)
	ENDDO
	
END SUBROUTINE gauss_elimination