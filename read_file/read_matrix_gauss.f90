SUBROUTINE read_matrix_gauss
	USE typedef, ONLY: A,y,n_var,column,row,solution,i_row,i_col
	USE IO_control, ONLY: file_route,status
	IMPLICIT NONE
	
	CHARACTER(LEN=256) dum
	

	OPEN(666,FILE=file_route,STATUS='OLD',POSITION='REWIND')
	!skip first line
	READ(666,'(A)',IOSTAT=status) dum
	
	DO WHILE (.TRUE.)
		READ(666,'(A)',IOSTAT=status) dum
		IF(status<0 .OR. dum(1:3)=='END') EXIT
		
		IF(dum(1:1)=='*' .OR. dum(1:1)=='#') THEN
			IF(dum(3:21)=='number of variables') THEN
				READ(666,*,IOSTAT=status) n_var
				column=n_var
				row=n_var+1
				ALLOCATE(A(row,column))
				ALLOCATE(solution(n_var),y(row))
			ELSEIF(dum(3:9)=='martrix') THEN
				DO i_col=1,column
					READ(666,*,IOSTAT=status)  (A(i_row,i_col),i_row=1,row)
				ENDDO
			ENDIF
			IF(status<0) CALL error_output(2)
            IF(status>0) CALL error_output(3)
		ENDIF
	ENDDO
	CLOSE(666)
	
	IF(isNaN(A(row,column))) CALL error_output(4)
	
ENDSUBROUTINE read_matrix_gauss
