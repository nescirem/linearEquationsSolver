MODULE IO_control
	LOGICAL alive
	INTEGER :: status
	CHARACTER(LEN=128) workdir,case,file_route
END MODULE IO_control

MODULE typedef
	INTEGER :: n_var,row,column
	REAL(8),ALLOCATABLE :: b(:)
	REAL(8),ALLOCATABLE :: A(:,:),P(:,:),L(:,:),IL(:,:),U(:,:)
	REAL(8),ALLOCATABLE :: solution(:),y(:)
	INTEGER :: i_row,i_col
END MODULE typedef