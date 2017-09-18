!Matrix inverse
SUBROUTINE invert_matrix(A,InvA)
	USE typedef, ONLY: n_var
	IMPLICIT NONE
	INTEGER :: i,j,k,t_i,t_j
	REAL(8) :: multiplier
	REAL(8) :: A(n_var,n_var),InvA(n_var,n_var)
	!initialize InvA as an unit matrix
	InvA=0.0d0
	FORALL(i=1:n_var) InvA(i,i)=1.0d0
	
	DO j=1,n_var-1
		DO i=j+1,n_var
			multiplier=-(A(i,j)/A(j,j))
			FORALL (k=1:n_var) InvA(i,k)=InvA(i,k)+InvA(j,k)*multiplier
		ENDDO
	ENDDO
	
	DO j=1,n_var-1
		DO i=j+1,n_var
			t_i=n_var-i+1
			t_j=n_var-j+1
			multiplier=-(A(t_i,t_j)/A(t_j,t_j))
			FORALL (k=1:n_var) InvA(t_i,k)=InvA(t_i,k)+InvA(t_j,k)*multiplier
		ENDDO
	ENDDO


END SUBROUTINE invert_matrix