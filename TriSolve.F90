module tri_eq


contains


subroutine uptri(A,b,x,N)

implicit real*8(a-z)

integer::i,j,k,N

real*8::A(N,N),b(N),x(N)

x(N)=b(N)/A(N,N)


do i=n-1,1,-1
   
    x(i)=b(i)
   do j=i+1,N
    x(i)=x(i)-a(i,j)*x(j)
   end do
    x(i)=x(i)/A(i,i)

end do

end subroutine uptri


subroutine downtri(A,b,x,N)


implicit real*8(a-z)
integer::i,j,N
real*8::A(N,N),b(N),x(N)

x(1)=b(1)/a(1,1)

do k=2,N
   x(k)=b(k)
   do i=1,k-1
      x(k)=x(k)-a(k,i)*x(i)
   end do
   x(k)=x(k)/a(k,k)

end do

end subroutine downtri


end module tri_eq

!##############################################################
module driver

contains 

subroutine dri_main()


implicit real*8(a-z)
integer::ioerr
character(20)::upordown

open(unit=11,file='fin.txt',iostat=ioerr)
open(unit=12,file='fout.txt')

do 
  read(11,*)upordown

  
  if ( upordown(1:5) == 'uptri' ) then
    call dri_up()
  else if (upordown(1:)=='downtri')then
    call dri_down()
  end if
  
  if (ioerr/=0) exit


end do

end subroutine dri_main


subroutine dri_up()


use tri_eq
implicit real*8(a-z)

integer,parameter::N=4

integer::i,j
real*8::A(N,N),b(N),x(N)


read(11,*)((A(i,j),j=1,N),i=1,N)

read(11,*) b

call uptri(A,b,x,N)

write(12,101)x
101 format(T5,'上三角形方程组的解',/,T4,'x=',4(/F12.8))

end subroutine dri_up


subroutine dri_down()

use tri_eq

implicit real*8(a-z)

integer,parameter::N=4

integer::i,j
real*8::A(N,N),b(N),x(N)


read(11,*)((A(i,j),j=1,N),i=1,N)


read(11,*) b

call downtri(A,b,x,N)

write(12,101)x
101 format(/,T5,'下三角形方程组的解',/,T4,'x=',4(/F12.8))

end subroutine dri_down

end module driver



!-----------------------------------------------------
program main

use driver


call dri_main()

end program main