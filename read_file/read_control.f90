SUBROUTINE read_control
	USE IO_control, ONLY: file_route,status
	USE algorithm_control
	IMPLICIT NONE
	CHARACTER(LEN=256) dum
	
	OPEN(666,FILE=file_route,STATUS='OLD',POSITION='REWIND')
	!skip first line
	READ(666,'(A)',IOSTAT=status) dum
	
	DO WHILE (.TRUE.)
		READ(666,'(A)',IOSTAT=status) dum
		IF(status<0 .OR. dum(1:3)=='END') EXIT
		
		IF (dum(1:1)=='*' .OR. dum(1:1)=='#') THEN
			IF (dum(3:8)=='method') THEN
				READ(666,*,IOSTAT=status) method
			ELSEIF(dum(3:6)=='iter') THEN
				READ(666,*,IOSTAT=status) iter
			ELSEIF(dum(3:13)=='error limit') THEN
				READ(666,*,IOSTAT=status) Err_limit
			ELSEIF(dum(3:20)=='relaxiation factor') THEN
				READ(666,*,IOSTAT=status) Lax_factor
			ENDIF
			IF(status<0) CALL error_output(2)
			IF(status>0) CALL error_output(3)
		ENDIF
	ENDDO
	CLOSE(666)
	
ENDSUBROUTINE read_control
