      program rainv
	integer ord,a(6)
	integer,allocatable::sta(:)
	real,allocatable::lat(:),lon(:),pre(:)
	character*50 name,ptyd
      name='ptyd196001.dat'
	open(10,file='OSATrain2\pre-v.dat')
	open(11,file='OSATrain2\output\filename.txt')   !filename.txt
	do i=1,1271      !TC个数，要随数据改变，即filename里数据个数
	 read(11,101) ptyd,ord
  101 format(a4,i6)
	 write(name(5:10),'(i6)')ord
	 open(12,file='OSATrain2\output\1100\forTrack\1100-newj\'//
     &trim(name)//'')
	 n=0
	 do while(.not.eof(12)) 
        read(12,*)
        n=n+1
       enddo
	 close(12)
	 if(n>0) then
	  allocate(sta(n),lat(n),lon(n),pre(n))
        open(12,file='OSATrain2\output\1100\forTrack\1100-newj\'
     &//trim(name)//'')
	  do j=1,n
	   read(12,*) sta(j),lat(j),lon(j),pre(j)
	  enddo
	  close(12)
	  call get_a_v(pre,lat,lon,n,area,volume)
	  write(10,100) ord,area,volume
	  deallocate(sta,lat,lon,pre)
	 endif	 
  100  format(i8,f12.2,f8.2)
      enddo
      end



      subroutine get_a_v( prcp,lat,lon,num_stn,area,volume)
c***************************************************************************     
c***  中国降水区面积及总降水容积计算子程序
c***  输入参数:
c***     num_stn       ---- 影响台站总数
c***     lat(num_stn)  ---- 台站纬度
c***     lon(num_stn)  ---- 台站经度
c***	   prcp(num_stn) ---- 台站降水(单位:毫米)
c***  输出参数:
c***     area          ---- 降水区面积(单位:平方公里)
C***     volume        ---- 总降水容积(单位:立方公里)
c***
c***     2003.6.23
c***     Fumin Ren
c***************************************************************************     
      parameter(n=10000)
c!!!!  'n' must equal 'length' insubroutine grid_inv
      dimension igrid05(73,123)
	real prcp(num_stn),lat(num_stn),lon(num_stn)
     &      ,prcp_g(n),alat_g(n),alon_g(n)
	 call grid_inv( prcp,lat,lon,num_stn,
     &                   prcp_g,alat_g,alon_g,num_grd)
       open(90,file='chinaland.g05',status='old')
c*2001.10.18  read(90,109) ((i0,ii=1,42),(igrid05(i,j),j=1,81),i=73,1,-1)
       read(90,109) ((igrid05(i,j),j=1,123),i=73,1,-1)
	close(90)
109   format(123i1)
      area=0
	volume=0
      do 1001 k=1,num_grd
	ik=((alat_g(k)-18.0)*10+0.1)/5+1
	jk=((alon_g(k)-74.0)*10+0.1)/5+1
!!!!!!!!!!!!this if block added by wxl 2004.2.9 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! when the typhoon precipitation is not in China mainland,alat_g(k)<18.0 !!!!!
!!!!! then ik<1, igrid05(ik,jk) overflowed !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(ik.lt.1.or.ik.gt.73.or.jk.lt.1.or.jk.gt.123)then
	  write(*,*)'ik=',ik,'jk=',jk
	  write(*,*)'the typhoon precipitation is not in China mainland'
	  area=area
	  volume=volume
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	else
	  if(igrid05(ik,jk).eq.1) then
	    call r_area(alat_g(k),area0)
	  area=area+area0
	  volume=volume+area0*prcp_g(k)
	  endif
	endif
1001  continue
      volume=volume/1000000
      return	      
	end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine grid_inv( prcp,  lat,  lon,num_stn,
     &                   prcp_g,lat_g,lon_g,num_grd) 
c***************************************************************************     
c***  Inverse Distance Interpolators
c***  parameters:
c***  part I:(can be changed according to what to do)
c***    bgn_lat(E)     ---- the starting latituede for grid nodes
c***    bgn_lon(E)     ---- the starting longituede for grid nodes
c***    delta          ---- the distance in degree between nodes
c***    dis_neib       ---- the distance in km for defining an area
c***                        to calculate the node's value
c***    length         ---- should be big enough for all cases to keep
c***                        the information of rainning nodes
c***    power          ---- the power of inverse distance(such as 1,2,3 or 4)
c***
c***  part II:
c***   input:
c***     num_stn       ---- total number of stations
c***     prcp(num_stn) ---- station precipitation 
c***     lat(num_stn)  ---- station latitude
c***     lon(num_stn)  ---- station longitude
c***   output:
c***     num_grd       ---- total number of nodes which has precipitation
c***     prcp_g(num_grd)--- node precipitation 
c***     lat_g(num_grd)---- node latitude
c***     lon_g(num_grd)---- node longitude
c***
c***     Jan. 7, 2000
c***     Fumin Ren
c***************************************************************************     
c*2001.10.18 parameter(bgn_lat=18.0,bgn_lon=95.0,delta=0.5,dis_neib=50
c*2003.08.05 change dis_neib=110, the old value is 50  *********************
        parameter(bgn_lat=18.0,bgn_lon=74.0,delta=0.5,dis_neib=100
     &            ,length=10000,power=2)
	real prcp(num_stn),lat(num_stn),lon(num_stn)
     &      ,prcp_g(length),lat_g(length),lon_g(length)
     &      ,lat0(length),lon0(length),glt(2,length),grd(2),wk(2,length)
        dimension irg(2,2)        
	if(num_stn.gt.length) then
	  write(*,*) '  please change the parameter "length" to '
	  write(*,*) '  ',length+1000,' in subroutine "grid_inv"'
	  stop
	endif
        call putr(num_stn,1,lat0,lat)
        call putr(num_stn,1,lon0,lon)
        do 1001 k=1,num_stn
        if(lat0(k).lt.0) lat0(k)=lat0(k)+360
        if(lon0(k).lt.0) lon0(k)=lon0(k)+360
	glt(1,k)=lat0(k)
	glt(2,k)=lon0(k)
1001    continue
        call get_irg(num_stn,lat0,lon0,irg,bgn_lat,bgn_lon,delta)
        num_grd=0	
        DO 1002 I=irg(1,1),irg(1,2)
        DO 1002 j=irg(2,1),irg(2,2)
        grd(1)=bgn_lat+i*delta
        grd(2)=bgn_lon+j*delta
	neib=0
	sum=0
        do 1003 k=1,num_stn
	call cptd(grd,glt(1,k),dis)
	if(dis.eq.0.0) then
	  num_grd=num_grd+1
	  if(num_grd.gt.length) then
	    write(*,*) '  please change the parameter "length" to '
            write(*,*) '  ',length+1000,' in subroutine "grid_inv"'
	    stop
 	  endif
	  prcp_g(num_grd)=prcp(k)
	  lat_g(num_grd)=grd(1)
	  lon_g(num_grd)=grd(2)
	  go to 1002
	else if(dis.le.dis_neib) then
	  neib=neib+1
	  wk(1,neib)=prcp(k)
	  wk(2,neib)=1.0/(dis**power)
	  sum=sum+wk(2,neib)
	endif
1003    continue
        if(neib.gt.0) then
	  num_grd=num_grd+1
	  if(num_grd.gt.length) then
	    write(*,*) '  please change the parameter "length" to '
            write(*,*) '  ',length+1000,' in subroutine "grid_inv"'
	    stop
 	  endif
	  v0=0
          do 1004 k=1,neib
	  v0=v0+wk(1,k)*wk(2,k)
1004      continue
	  if(sum.gt.0) then
	    prcp_g(num_grd)=v0/sum
	  else
	    prcp_g(num_grd)=wk(1,1)
	  endif
	  lat_g(num_grd)=grd(1)
	  lon_g(num_grd)=grd(2)
	endif
1002    continue
        return
        end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine get_irg(n,gt,gn,irg,bgn_lat,bgn_lon,delta)
       dimension gt(n),gn(n),irg(2,2),rg(2,2)
       do 1001 k=1,n
         if(k.eq.1) then
	    rg(1,1)=gt(k)
	    rg(1,2)=gt(k)
	    rg(2,1)=gn(k)
	    rg(2,2)=gn(k)
	  endif
         if(gt(k).lt.rg(1,1)) rg(1,1)=gt(k)
         if(gt(k).gt.rg(1,2)) rg(1,2)=gt(k)
         if(gn(k).lt.rg(2,1)) rg(2,1)=gn(k)
         if(gn(k).gt.rg(2,2)) rg(2,2)=gn(k)
1001   continue
       call get_line(rg(1,1),bgn_lat,delta,irg(1,1),-1)
       call get_line(rg(1,2),bgn_lat,delta,irg(1,2),1)
       call get_line(rg(2,1),bgn_lon,delta,irg(2,1),-1)
       call get_line(rg(2,2),bgn_lon,delta,irg(2,2),1)
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine get_line(v,bgn,delta,line,ir)
       line=(int(v-bgn)*1000)/(delta*1000)
       aline=bgn+line*delta
       if(ir.eq.1) then
         if(v.gt.aline) line=line+1
       else if(ir.eq.-1) then
         if(v.lt.aline) line=line-1
       endif
       return
       end
c*********************************************following is used
	subroutine r_area(alat,area)
c****  to compute a 0.5X0.5 area at latitude alat
	 pai=3.1415926
        r=6371.229
        call du_fudu(abs(alat),fai)
        area=pai*pai*r*r*cos(fai)/(360*360)
	return
	end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
	subroutine cptd(a,b,dis)      
      dimension a(2),b(2)
      r=6371.229
      a22=a(2)
      b22=b(2)
      if(a(2).lt.0) a22=a(2)+360
      if(b(2).lt.0) b22=b(2)+360
      call du_fudu(a(1),a1)
      call du_fudu(a22,a2)
      call du_fudu(b(1),b1)
      call du_fudu(b22,b2)
      fai=(a1+b1)*0.5
      dis=(a1-b1)**2+((a2-b2)*cos(fai))**2
      dis=sqrt(dis)*r
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 	 subroutine du_fudu(a,f)
	 pai=3.1415926/180.0
	 f=a*pai
         return 
	 end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine putr(m,n,ia,ib)
      real ia(m,n),ib(m,n)
      do 1001 i=1,m
      do 1001 j=1,n
      ia(i,j)=ib(i,j)
1001  continue
      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	

