SUBROUTINE error_output(num_of_err)
	USE IO_control, ONLY: file_route
	IMPLICIT NONE
	INTEGER :: num_of_err

	WRITE(*,"('ERROR code:',I2.2)") num_of_err
	SELECT CASE (num_of_err)
	CASE(1)
		WRITE(*,*) file_route," doesn't exist."
		STOP
	CASE(2)
		WRITE(*,*) "End of the file, but haven't got the value"
		STOP
	CASE(3)
		WRITE(*,*) "Unexpect error when reading the file"
		STOP
	CASE(4)
		WRITE(*,*) "NaN,Out of precision!"
		STOP
	CASE(9)
		WRITE(*,*) "Uninvertible A"
	CASE DEFAULT
		STOP "Undefined error."
	END SELECT
ENDSUBROUTINE error_output