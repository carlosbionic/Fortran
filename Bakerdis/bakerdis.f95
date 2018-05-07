	module subroutines

	contains

	!Mapa de Baker con disipacion
	subroutine map (q,p)

	real, parameter :: eps = 0.8
	real :: q, p

		if (q < 0.5) then
			q = 2.0*q
			p = p*eps/2.0
		else
			q = 2.0*q - 1.0
			p = (p*eps + 1.0)/2.0
		end if

	end subroutine map

	subroutine orbits
	!Hago el diagrama de bifurcaciones para ver que onda con las orbitas
    !Analizo solo la componente q, ya que en la direccion de p hay simetria
    !El q lo voy a tomar q=0.5 arbitrariamente

    logical :: file_exists
    real :: q,p,q1,p1
    integer :: i

	inquire(file="bakerorbits.dat", exist=file_exists)
	if (file_exists) then
		open(1, file="bakerorbits.dat", status='old') 
	else
		open(1, file="bakerorbits.dat", status='new')
	end if

    q1=1.0 !Punto inicial
    !p=0.745
    p1=10.0/17.0 !El q lo elegi arbitrariamente
	

	do while (p1<1)
		p=p1
		q=q1
		!do i=1,100
		!	call map (q,p)
		!end do

		do i=1,100000
			call map (q,p)
			write(1,*) q,p
		end do
		p=p1
		q=q1

		p1=p1+0.4

	end do

	close(1)

	end subroutine orbits

	subroutine boxcount (q,p,N,file)

	implicit none

	real, dimension(N) :: q, p
	real, dimension(10) :: count, eps
	integer :: i, j, k, k1, k2, N, m
	character :: file
	logical file_exists
	!N es el tamaño de la grilla. Hay N*N puntos (2 dimensiones)
	!eps es el tamaño del elemento que voy a tomar

	m = 10
	count = 0
	eps = 0 

	do k = 1, 10
	  eps(k) = 1.0/real(m) !Quedan m**2 cuadrados
	  !Recorro los m**2 cuadrados
	  do k1 = 1,m !Vertical	
			point1: do i = 1, N
				point2: do j = 1, N
					!Veo si el punto (q(i),p(j)) pertenece al cuadrado
					if (p(j)<=real(k1)*eps(k) .and. p(j)>=(real(k1)-1.0)*eps(k)) then
						count(k) = count(k) + 1.0	
						exit point1
					end if
				end do point2				
			end do point1
	  end do
	  m = 2*m
	end do

	!Escribo los valores calculados en boxcount.dat
	inquire(file="boxcount.dat", exist=file_exists)
	if (file_exists) then
		open(2, file="boxcount.dat", status='old') 
	else
		open(2, file="boxcount.dat", status='new')
	end if

	do k = 1, 10
        	write(2,*) log(count(k)),log(1.0/eps(k))
    end do
    close(2)

	end subroutine boxcount

	end module subroutines

program bakerdis

	use subroutines

	implicit none
	
	real, dimension(:), allocatable :: q, p
	integer :: i, k
	!N define la cantidad de puntos en la grilla. m es la cantidad de iteraciones del mapa
	integer, parameter :: N = 1000000, m = 1
	logical file_exists

	allocate(q(N))
	allocate(p(N))

	q=0.0
	p=0.0
	k=1

	!Grilla en el [0,1]x[0,1] con numeros random
	do i = 1,N
		q(k) = rand(0)
		p(k) = rand(0)
		k = k+1
	end do

	!Aplico el mapa m veces en cada punto de la grilla
	do i = 1, m 
		do k = 1, N			
			call map (q(k),p(k))		
		end do
	end do

	inquire(file="bakerdis.dat", exist=file_exists)
	if (file_exists) then
		open(1, file="bakerdis.dat", status='old') 
	else
		open(1, file="bakerdis.dat", status='new')
	end if

    do i = 1, N
        	write(1,*) q(i), p(i)
    end do
    close(1)

    !call boxcount (q,p,N,"bakerdis.dat")
    !call orbits

end program bakerdis

	
