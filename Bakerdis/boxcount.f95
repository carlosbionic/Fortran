subroutine boxcount (N,file)

	implicit none

	real :: q, p
	integer :: i, j, k1, k2, N, eps
	integer, parameter :: count = 0, m = 3
	!N es el tamaño de la grilla. Hay N*N puntos (2 dimensiones)
	!eps es el tamaño del elemento que voy a tomar
	
	open(1, file="file", status='old')
	do i = 1, N
		read(1,*) q(i), p(i)
	end do

	count = 0
	eps = N/m !Quedan m**2 cuadrados

	!Recorro los m**2 cuadrados
	do k1 = 1,m !Horizontal
		do k2 = 1,m !Vertical
			do i = 1, N
				do j = 1, N
					!Veo si el punto (q(i),p(j)) pertenece al cuadrado
					if (q(i)<=k1*eps .and. q(i)>=(k1-1)*eps .and. p(j)<=k2*eps .and. p(j)>=(k2-1)*eps) then
						count = count + 1
						exit
					end if
				end do
				exit
			end do
		end do
	end do

	count = -log(count)/log(eps)
	write(*,*) "Box counting dimension = ",count

end subroutine boxcount