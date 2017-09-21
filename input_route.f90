SUBROUTINE input_route
	USE IO_control
	IMPLICIT NONE

	OPEN(777,FILE='input.name')
	READ(777,*) workdir
	READ(777,*) case
	CLOSE(777)
	file_route=TRIM(workdir)//'\'//TRIM(case)//'.txt'
	INQUIRE(FILE=file_route,EXIST=alive)
	IF(.NOT.alive) CALL error_output(1)
	
END SUBROUTINE input_route
