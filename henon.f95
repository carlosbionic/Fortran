program henon

	implicit none
	real :: A,B
	integer :: N,i
	real, dimension(:), allocatable :: x,y
	logical file_exists

	A = 1.4
	B = 0.3
	N = 10**6 !Numero de iteraciones para el mapa
	allocate(x(N))
	allocate(y(N)) !Vectores x e y de N componentes
	x(1) = 1
	y(1) = 1

	!Mapa de Henon
	do i = 1, N-1 
		x(i+1) = A - x(i)**2 + B*y(i)
		y(i+1) = x(i)
	end do

	inquire(file="henon.dat", exist=file_exists)
	if (file_exists) then
		open(1, file="henon.dat", status='old') 
	else
		open(1, file="henon.dat", status='new')
	end if

    do i = 1, N
        write(1,*) x(i), y(i)
    end do
    close(1)

    call system('gnuplot -p plot.plt')

end program henon