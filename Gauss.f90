!仅可求解满秩的线性方程组，只存在唯一解。
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
	
	!读入矩阵
	CALL read_matrix(file_route)
	
	!判断方程组是否有解
	
	!列主元高斯消元法
	DO i_row=1,num_of_var !按列循环
        !行变换列主元
		DO i_col=i_row+1,num_of_var !在该列下三角区域内寻找绝对值最大的数，调换绝对值最大的数所在行
									!至该列
			IF (ABS(MAT_lin_equ(i_row,i_col)) .GT. ABS(MAT_lin_equ(i_row,i_row))) THEN
				inter_column(:)=MAT_lin_equ(:,i_col)
				MAT_lin_equ(:,i_col)=MAT_lin_equ(:,i_row)
				MAT_lin_equ(:,i_row)=inter_column
			ELSE
				CYCLE
			ENDIF
		ENDDO
        !消元
        DO i_col=i_row+1,num_of_var
		    multiplier=-(MAT_lin_equ(i_row,i_col)/MAT_lin_equ(i_row,i_row))
			FORALL (i_rr=i_row:row)
				MAT_lin_equ(i_rr,i_col)=MAT_lin_equ(i_rr,i_col)+MAT_lin_equ(i_rr,i_row)*multiplier
			ENDFORALL
		ENDDO
	ENDDO
	!反代求解
	solution(num_of_var)=MAT_lin_equ(row,column)/MAT_lin_equ(num_of_var,num_of_var)
	DO i_s=2,column
		ti_c=num_of_var-i_s+1
		add=0
		DO i_row=ti_c+1,num_of_var
			add=add+MAT_lin_equ(i_row,ti_c)*solution(i_row)
		ENDDO
		solution(ti_c)=(MAT_lin_equ(row,ti_c)-add)/MAT_lin_equ(ti_c,ti_c)
	ENDDO
	
	WRITE(*,*) solution
	
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
			ELSEIF(dum(3:9)=='martrix') THEN
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
    CASE(3)
		WRITE(*,"('ERROR code:',I2.2)") num_of_err
		WRITE(*,*) "Unexpect error when reading the file"
	CASE(4)
		WRITE(*,"('ERROR code:',I2.2)") num_of_err
		WRITE(*,*) "Out of precision!"
	CASE DEFAULT
		STOP "Undefined error."
	END SELECT
ENDSUBROUTINE error_output
	
	
	
	
	
	
	
	
	

	
