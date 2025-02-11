program impactTCsinformation
implicit none
integer line
parameter(line=19138) !!!!!!!!!!!!数据文件行数
integer i,j,m,constant1(line),yr(line),k(line),n(line),yr1(line),mon1(line),dy1(line),t(line),pp(line),wd(line),mm(line),number(line),constant2(line)
real lat(line),lon(line)

open(11,file='huanan1980-2022.txt',status='old') !!!!!!!!!!!!修改
open(12,file='track_INF_huanan1980-2022-656.TXT',status='new',position='append') !!!!!!!!!!!!修改


do m=1,line
 read(11,*) constant1(m),yr(m),k(m),n(m),yr1(m),mon1(m),dy1(m),t(m),lat(m),lon(m),pp(m),wd(m),mm(m),number(m),constant2(m)
enddo
close(11)


do i=2,line
   if(number(i)/=number(i-1)) then
    mm(i-1)=3
   endif
enddo
mm(line)=3

do j=1,line
write(12,100) constant1(j),yr(j),k(j),n(j),yr1(j),mon1(j),dy1(j),t(j),lat(j),lon(j),pp(j),wd(j),mm(j),number(j),constant2(j)
enddo
close(12)

100 format(i5,i6,2i5,i6,3i4,f8.1,f9.1,i6,i7,i3,i8,i6) 

end