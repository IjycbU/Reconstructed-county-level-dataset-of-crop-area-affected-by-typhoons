program main
implicit none
integer i,yr,num,a(8),b(7),TCnum
real c(3),d(8),area,sump(656) !!!!!!ĞŞ¸Ä

open(11,file='ty_imp-1100.dat',status='old')
open(12,file='ty_inf_656.dat',status='new')
open(13,file='pre-v.dat',status='old')

do i=1,656
  read(13,*) TCnum,area,sump(i)
enddo

do i=1,656
  read(11,*) yr,num,a(:),b(:),c(:),d(:)
  write(12,100) yr,num,b(:),c(2:3),sump(i),d(:)
  100 format(3i4,1x,i4,1x,i2,1x,i2,4x,i4,1x,i2,1x,i2,11f7.1)
  !print*,yr,num,a(:),b(:),c(:),d(:)
  !pause
enddo


end