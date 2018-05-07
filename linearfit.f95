subroutine linearfit(rows)

 implicit none

 logical :: file_exists
 integer :: rows
 real, dimension(rows) :: x,y
 real :: sumx,sumy,sumsqx,sumxy,deno,slope,b

 inquire(file="fitdata.dat", exist=file_exists)
	if (file_exists) then
		open(1, file="fitdata.dat", status='old', action='read') 
		do i = 1,rows
			read(1,*) x(i),y(i)
		end do
	else
		write(*,*) 'No existe el archivo para hacer el ajuste'
	end if
 close(1)

 sumx=0.0
 sumy=0.0
 sumsqx=0.0
 sumxy=0.0

 do i=1,rows
 	sumx=sumx+x(i)
 	sumy=sumy+y(i)
 	sumsqx=sumsqx+x(i)*x(i)
 	sumxy=sumxy+x(i)*y(i)
 end do

 deno=rows*sumsqx-sumx*sumx
 slope=(rows*sumxy-sumx*sumy)/deno
 b=(sumsqx*sumy-sumx*sumxy)/deno
 write(*,*)'Slope, Intercept= ',slope,b
 stop

end subroutine linearfit
