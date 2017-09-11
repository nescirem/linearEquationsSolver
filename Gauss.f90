!Only for the full rank linear equation, which have a unique solution.
MODULE typedef
	INTEGER :: num_of_var
	INTEGER :: row,column
	INTEGER :: i_row,i_col,i_rr,i_s,ti_c
	REAL(8),ALLOCATABLE :: MAT_lin_equ(:,:)
	!REAL(8),ALLOCATABLE :: MAT_lower_triangular(:,:)
	REAL(8),ALLOCATABLE :: solution(:),inter_column(:)
END MODULE typedef

PROGRAM solve_liner_equation
	USE typedef
	IMPLICIT NONE
	LOGICAL alive
	CHARACTER(LEN=128) workdir,case,file_route
	REAL(8) :: multiplier,add
	
	OPEN(777,FILE='input.name')
	READ(777,*) workdir
	READ(777,*) case
	CLOSE(777)
	
	file_route=TRIM(workdir)//'\'//TRIM(case)//'.txt'
	INQUIRE(FILE=file_route,EXIST=alive)
	IF(.NOT.alive) CALL error_output(1)
	
	!Read in matrix which represent a linear equation.
	CALL read_matrix(file_route)
	
	!Have a solution or not judgement
	
	!Gaussian Elimination with Pivoting Method
	DO i_row=1,num_of_var 
        	!find the largest (in absolute value) element among lower triangular part of that matrix
		DO i_col=i_row+1,num_of_var !ehchange row
			IF (ABS(MAT_lin_equ(i_row,i_col)) .GT. ABS(MAT_lin_equ(i_row,i_row))) THEN
				inter_column(:)=MAT_lin_equ(:,i_col)
				MAT_lin_equ(:,i_col)=MAT_lin_equ(:,i_row)
				MAT_lin_equ(:,i_row)=inter_column
			ELSE
				CYCLE
			ENDIF
		ENDDO
        	!Elimination
        	DO i_col=i_row+1,num_of_var
			multiplier=-(MAT_lin_equ(i_row,i_col)/MAT_lin_equ(i_row,i_row))
			FORALL (i_rr=i_row:row)
				MAT_lin_equ(i_rr,i_col)=MAT_lin_equ(i_rr,i_col)+MAT_lin_equ(i_rr,i_row)*multiplier
			ENDFORALL
		ENDDO
	ENDDO
	!Reverse solution
	solution(num_of_var)=MAT_lin_equ(row,column)/MAT_lin_equ(num_of_var,num_of_var)
	DO i_s=2,column
		ti_c=num_of_var-i_s+1
		add=0.0d0
		DO i_row=ti_c+1,num_of_var
			add=add+MAT_lin_equ(i_row,ti_c)*solution(i_row)
		ENDDO
		solution(ti_c)=(MAT_lin_equ(row,ti_c)-add)/MAT_lin_equ(ti_c,ti_c)
	ENDDO
	
	WRITE(*,*) solution
	
	CALL dealloc

	STOP
END PROGRAM solve_liner_equation
	
SUBROUTINE read_matrix(file_route)
	USE typedef
	IMPLICIT NONE
	INTEGER :: status
	CHARACTER(LEN=128) file_route
	CHARACTER(LEN=256) dum

	OPEN(666,FILE=file_route,STATUS='OLD',POSITION='REWIND')
	!skip first line
	READ(666,'(A)',IOSTAT=status) dum
	
	DO WHILE (.TRUE.)
		READ(666,'(A)',IOSTAT=status) dum
		IF(status<0 .OR. dum(1:3)=='END') EXIT
		
		IF(dum(1:1)=='*' .OR. dum(1:1)=='#') THEN
			IF(dum(3:21)=='number of variables') THEN
				READ(666,*,IOSTAT=status) num_of_var
				column=num_of_var
				row=num_of_var+1
				ALLOCATE(MAT_lin_equ(row,column))
				ALLOCATE(solution(num_of_var),inter_column(row))
			ELSEIF(dum(3:8)=='matrix') THEN
				DO i_col=1,column
					READ(666,*,IOSTAT=status)  (MAT_lin_equ(i_row,i_col),i_row=1,row)
				ENDDO
			ENDIF
			IF(status<0) CALL error_output(2)
        	IF(status>0) CALL error_output(3)
		ENDIF
	ENDDO
	CLOSE(666)
	
	IF(isNaN(MAT_lin_equ(row,column))) CALL error_output(4)
	
ENDSUBROUTINE read_matrix

SUBROUTINE error_output(num_of_err)
	INTEGER :: num_of_err
	SELECT CASE (num_of_err)
	CASE(1)
		WRITE(*,"('ERROR code:',I2.2)") num_of_err
		WRITE(*,*) file_route," doesn't exist."
		STOP
	CASE(2)
		WRITE(*,"('ERROR code:',I2.2)") num_of_err
		WRITE(*,*) "End of the file, but haven't got the value"
		STOP
	CASE(3)
		WRITE(*,"('ERROR code:',I2.2)") num_of_err
		WRITE(*,*) "Unexpect error when reading the file"
		STOP
	CASE(4)
		WRITE(*,"('ERROR code:',I2.2)") num_of_err
		WRITE(*,*) "NaN,Out of precision!"
		STOP
	CASE DEFAULT
		STOP "Undefined error."
	END SELECT
ENDSUBROUTINE error_output

SUBROUTINE dealloc
	USE typedef
	IMPLICIT NONE
	
	DEALLOCATE(MAT_lin_equ)
	DEALLOCATE(solution)
	DEALLOCATE(inter_column)
	
END SUBROUTINE dealloc
	
	
	
	
	
	
	
	

	
