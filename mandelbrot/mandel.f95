program mandelbrot

complex*8 :: c,z
complex*8,dimension(:),allocatable :: w
integer :: N,j,k
integer,dimension(:),allocatable :: cant
logical file_exists

N=50000000

allocate(cant(N))
allocate(w(N))
w = 0
cant = 0

do j=1,N

 !c = cmplx(3.0*rand(0)-2.0,3.0*rand(0)-2.0)
 c = cmplx(0.05*rand(0)-1.79,0.05*rand(0)-1.79)
 !write(6,*) c
 z=0

 k=1

  do while (abs(abs(z)-2.0) .ge. 0.0001 .and. k .le. 119)
  z=z**2+c
  k=k+1
  end do

 cant(j) = k 


  w(j) = c


end do

inquire(file="mandel.dat", exist=file_exists)
 if (file_exists) then
  open(1, file="mandel.dat", status='old') 
 else
  open(1, file="mandel.dat", status='new')
 end if

 do j = 1,N
  write(1,*) real(w(j)),aimag(w(j)),cant(j)
 end do
close(1)

end program mandelbrot
