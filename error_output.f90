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
	CASE(9)
		WRITE(*,"('ERROR code:',I2.2)") num_of_err
		WRITE(*,*) "Uninvertible A"
	CASE DEFAULT
		STOP "Undefined error."
	END SELECT
ENDSUBROUTINE error_output