      parameter(ma=160,m=150,dist0=200,amiss=-99.0,ns=530,wind_p0=-99.0!ma\m
     & ,pres_p0=1500.0)
	integer,parameter:: iyrstart=1979
      character*50   fntyd,name2
	dimension neib_0(ns,ma),prcp_0(ns),ibelt_0(ns),nbelt_0(10)
	dimension ist(ns),aa0(2,ns),tfn_cnt(2,4),pr724(ns,16071),iymd_e(3)
     & , neib(ns,ma),istbelt(ns),wk(ns,2),rr(ns),winddate(4),ms4(4),
     & nbelt(10),r(ns),rlat_sh(ns),rlon_sh(ns),info(11,80),iymd_b(3),
     & tracknew(2,150),idat(5,150),iymd(3),a(ns),press(150),windr(150)
     &,trckdate(2,4),wind(150),ist_case(ns),pm_case(ns),rlat_sh1(ns)
     &,dm_case(ns),ist_ord(ns),ptyd(ns),ptyd1(ns),dir724(ns,16071),
     &k_ptyd(ns),k0_ptyd(ns),prcp_max(ns),d(ns),d1(ns),dir_case(ns),
     &rlon_sh1(ns),kdm_case(ns),num_belt_day(18),ra(18),ptyd_dir(ns)
!	real::pmax
c	open(79,file='fjlxh\ri_rain.txt',status='old')     
!	open(11,file='1100\2479\ty_all_rain-whole.txt',status='unknown')
	open(22,file='D:\Wwj\data\Console1\Console1\OSATw2\wind-osat\
     &ty_all_rain-150.txt',status='unknown')
	open(33,file='D:\Wwj\data\Console1\Console1\OSATw2\wind-osat\
     &ty_all_rain-150-2.txt',status='unknown')
	open(44,file='D:\Wwj\data\Console1\Console1\OSATw2\wind-osat\
     &ty_all_rain-100-2.txt',status='unknown')
ccc      open(13,file='tc-r-before\ty_ri_av-old.txt',status='unknown')
c1      open(67,file='fjlxh\xxx\ty_all_lin.dat',status='unknown')
	open(68,file='D:\Wwj\data\Console1\Console1\OSATw2\wind-osat\
     &ty_imp-1100.dat',status='unknown')
	open(97,file='D:\Wwj\data\Console1\Console1\OSATw2\wind-osat\
     &stat_frg-1100.dat',status='unknown')  
      open(197,file='D:\Wwj\data\Console1\Console1\OSATw2\wind-osat\
     &statfrg-1100-2.dat',status='unknown')  
      open(77,file='D:\Wwj\data\Console1\Console1\530sta.dat',
     &status='old')
c      open(1,file='fjlxh\tyn724_dlyrain.dat',status='unknown'
c     &,access='direct',form='formatted',recl=3*4+724*6)
      
	 do i=1,ns
      read(77,*) ist(i),(aa0(j,i),j=1,2)
	 enddo																							   
 !     close(77)
!!!!!!!!!!!读取每一日724站降水资料!!!!!!!!!!!
 !     pmax=0.0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
      call difdays(iyrstart,12,31,2022,12,31,nleng0) !计算整个时段的天数
      write(*,*) ' nleng0=',nleng0
      fntyd='ptyd200701.dat'
      call newprcp1(ist,pr724,dir724,nleng0,ns) !读取每个台站的日降水量
 !     print*,"降水=",pr724; stop
      do iyear=1980,2022
c      iyear=1997
       call sh_inf(iyear,ntyphn,info,70)  !读取台风的序号以及始末时间
c       write(*,*) ' iyear,ntyphn=',iyear,ntyphn 
      do  ktyphn=1,ntyphn !每年当中的若干个台风
c	ktyphn=9

      wind_pm=wind_p0    !影响期最大风速
	wind_pmean=0.0    !影响平均风速
      num_wmean=0     !影响期有风力天数
	wind_pmin=9999   !影响期最小风速
	pres_pn=pres_p0   !影响期最低气压
	pres_pmean=0.0    !影响平均气压 和
      num_pmean=0      !影响期有气压天数
      pres_pmax=-999.0  !影响期最高气压
      call rinitn(ns,1,pm_case,0.0)
	call rinitn(ns,1,dm_case,0.0)
      call initn(1,ns,k_ptyd,0)

	do iptyd=1,ns
	ptyd(iptyd)=-99.9
      ptyd_dir(iptyd)=-99.9
	enddo

      write(fntyd(5:10),'(i4,i2.2)')iyear,ktyphn !用年和台风序号来表示资料
      open(62,file='D:\Wwj\data\Console1\Console1\
     &OSATw2\1100-new\'//fntyd)  
	!每个台风过程中在各个台站产生的累积降水量
   !   pause
          call sh_trck(iyear,ktyphn,tracknew,wind,idat,ntim,71) 
	     !读取台风资料
c	write(*,*) iyear,ktyphn,idat(1:5,20)
c1      call putr(info(1,ktyphn),1,press,wind)
	do ij=1,info(1,ktyphn) !某个台风的第ij个时次
       windr(ij)=idat(5,ij)  !第iyear年第k个TC的第ij个时次的风速
      enddo

	call rmaxi(windr,info(1,ktyphn),wind_p0,200.0,wind_m)!求最大风速

	call rmini(wind,info(1,ktyphn),700.0,pres_p0,pres_n)!求最低气压
!	write(67,106) iyear,ktyphn,wind_m,pres_n !路径？？
c       write(*,*) iyear,ktyphn,wind_m,pres_n
c   2005.10.12.rfm
106   format(2i5,2f7.1)

        call difdays(iyear,info(2,ktyphn),info(3,ktyphn),
     *iyear,info(5,ktyphn),info(6,ktyphn),k1)   !计算1975年6号台风的生命期mm,天
	  mm=k1+1  !计算某个TC的降水持续天数
!       write(*,*) iyear,info(2,ktyphn),info(3,ktyphn),
!     *iyear,info(5,ktyphn),info(6,ktyphn),mm
!!!!!!!!!!!!!!屏幕输出到这!!!!!!!!!!!!!!!!
      n_day=0
	print*,'1' !可删除
	 do ii=0,k1 !某个TC降水的每一天  对应的年、月、日
         call cptdays(iyear,info(2,ktyphn),info(3,ktyphn),ii,
     *	   iymd(1),iymd(2),iymd(3))   !求iymd(1),iymd(2),iymd(3)

c       write(*,*)  iymd(1),iymd(2),iymd(3)
!!!!!!!!!!!!!读取一日台风四次位置!
      call trdate1(iymd,tracknew,wind,idat,trckdate,ms4,winddate,ntim,
     *amiss)!有降水的每一天的TC一日四次的位置、气压、风速



      call rmini(winddate,4,700.0,pres_p0,pres_pp) !在一天中找最小气压
	call m_maxi(ms4,4,-999,200,mm0)       !在一天中找风速最大
	wind_pp=mm0
      call putr(2,4,tfn_cnt,trckdate)
       print*,'2' !可删除    
        

!!!!!! 获取一日逐日降水资料!
       call difdays(iyrstart,12,31,iymd(1),iymd(2),iymd(3),k)
        write(*,*) 'k =',k !; pause
	  !有降水的每一天距离时段第一天的时间长度，天数

	 r(:)=pr724(:,k)
	 d(:)=dir724(:,k)
!       print*,"pr724=",pr724(:,k),dir724(:,k)
 !     do 5101 i=1,ns 
  !     r(i)=pr724(i,k) !TC有降水的这一天对应的所有站的降水
   !    print*,"pr724=",pr724(i,k)
!	 print*,"ri=",r(i	)
!5101  continue
      wind_tfn=(ms4(1)+ms4(2)+ms4(3)+ms4(4))/4.0 !这一天的平均风速
 
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call rainchn6(ist,ns,aa0,r,ma,m,neib,tfn_cnt,4,istbelt,
     &wind_tfn,num_belt,wk,n_b,nbelt,amiss,
     &dist0,neib_0,prcp_0,ibelt_0,nbelt_0)
      print*,'num_belt=',num_belt

c      call chn_724(num_belt,istbelt,iymd,r) 
      if(num_belt>0)then
       write(*,*) 'iymd=', iymd(1:3),'num_belt=', num_belt
       n_day=n_day+1

	 if(n_day.eq.1) call put(3,1,iymd_b,iymd)
       call put(3,1,iymd_e,iymd)
	

      
      print*,'3' !可删除
      if(pres_pp.gt.0.0.and.pres_pp.le.1200.0) then
        if(pres_pn.gt.pres_pp) pres_pn=pres_pp  !找影响期内最低气压
	  if(pres_pmax.lt.pres_pp) pres_pmax=pres_pp  !找影响期内最大气压
	  pres_pmean=pres_pmean+pres_pp
        num_pmean=num_pmean+1
	endif
      if(wind_pp.gt.0.0.and.wind_pp.le.200.0) then
	  if(wind_pm.lt.wind_pp) wind_pm=wind_pp   !找影响期内最大风速
	  if(wind_pmin.gt.wind_pp) wind_pmin=wind_pp  !找影响期内最小风速
	  wind_pmean=wind_pmean+wind_pp
        num_wmean=num_wmean+1
	endif

c	call chn_677(num_belt,istbelt,iymd,rain)
	
      call difdays(iymd_b(1),iymd_b(2),iymd_b(3),iymd(1),iymd(2),iymd(3)
     &	,kk)   
	 kk=kk+1  !第ii天距降水第一天的天数
       name2='ptyd19600101.dat'  !1960年第1个台风第1天的降水
	if(ktyphn<10.and.kk<10) then
	 write(name2(5:12),'(i4,4i1)')iyear,0,ktyphn,0,kk    !用年和台风序号及降水第几天来表示资料
	elseif(ktyphn<10.and.kk>=10) then
	 write(name2(5:12),'(i4,2i1,i2)')iyear,0,ktyphn,kk
	elseif(ktyphn>=10.and.kk<10) then
	 write(name2(5:12),'(i4,i2,2i1)')iyear,ktyphn,0,kk
	else 
	 write(name2(5:12),'(i4,2i2)')iyear,ktyphn,kk   
	endif
	write(888,*) name2
      open(11,file='D:\Wwj\data\Console1\Console1\
     &OSATw2\\1100-new-day\'//trim(name2)//'')      !每个台风过程中在每日各个台站产生的日降水量  
       num_belt_day(n_day)=num_belt
!      write(22,'(i4,i2,3i5,i6)')  iyear,ktyphn,iymd(1:3), num_belt
c1      num_d=0  
      do 1001 j=1,num_belt
	i=istbelt(j) !1-652
	a(j)=r(i) !识别出的TC日降水
      
	write(11,'(i4,i4,i8,i6,2i4,i7,f7.2,f8.2,2f7.2)') iyear,ktyphn,
     &info(11,ktyphn),iymd(1:3),ist(i),(aa0(jj,i),jj=1,2),a(j),d(i)
      if(ra(n_day)<a(j)) then
	 ra(n_day)=a(j)  !每日最大日降水量
	endif

      if(a(j)>=10.8) then
      write(22,'(i6,2f8.2,f6.1)') ist(i),      !所有台风雨带台站的位置和日降水量
     *	(aa0(jj,i),jj=1,2),a(j)
	write(33,'(i4,i2.2,3i5,i6,2f8.2,f6.1)') iyear,ktyphn,iymd(1:3),
	1ist(i),(aa0(jj,i),jj=1,2),a(j)
	endif

	if(a(j)>=100) then
	write(44,'(i4,i2.2,3i5,i6,2f8.2,f6.1)') iyear,ktyphn,iymd(1:3),
	1ist(i),(aa0(jj,i),jj=1,2),a(j)
	endif
	   
      rlat_sh(j)=aa0(1,i)  
      rlon_sh(j)=aa0(2,i)

	if(neib(istbelt(j),m+3).eq.99)then  !????????????????????
c1           num_d=num_d+1
ccccccccc20051114 rfmcccccc
	     pm_case(istbelt(j))=pm_case(istbelt(j))+r(istbelt(j))
	     if(r(istbelt(j)).gt.dm_case(istbelt(j))) then !单站累积
          dm_case(istbelt(j))=r(istbelt(j))
	    dir_case(istbelt(j))=d(istbelt(j))
!          ptyd_dir(istbelt(j))=d(istbelt(j))
           kdm_case(istbelt(j))=n_day !24h单站最大

	      endif
!??????????????????????????????????????????????????????????????????????
	     if(ptyd(istbelt(j)).eq.-99.9) ptyd(istbelt(j))=0
           if(ptyd(istbelt(j)).eq.-99.9) ptyd_dir(istbelt(j))=0
              ptyd(istbelt(j))=dm_case(istbelt(j))
              ptyd_dir(istbelt(j))=dir_case(istbelt(j))
	       k_ptyd(istbelt(j))=k_ptyd(istbelt(j))+1 !过程累积
          
!	print*,'4' !可删除
	end if
	  

1001  continue 

         write(*,*) 'n_day=',n_day  
      endif
   
	enddo

      if(num_pmean.ne.0) then
	  ppmm00=pres_pmean/num_pmean
	else
        ppmm00=9999
	endif
      if(num_wmean.ne.0) then
	  wwmm00=wind_pmean/num_wmean
	else
        wwmm00=9999
	endif
	if(wind_pm.lt.0) wind_pm=9999

      ipd=0
	do iptyd=1,ns
	  if(ptyd(iptyd).ne.-99.9)then
	    ipd=ipd+1
	    ist_ord(ipd)=iptyd
	    ist_case(ipd)=ist(iptyd)
	    ptyd1(iptyd)=ptyd(iptyd)
	    d1(iptyd)=ptyd_dir(iptyd)
c1	    tyd(ipd)=ptyd(iptyd)
c1	    tydlat(ipd)=aa0(1,iptyd)
c1	    tydlon(ipd)=aa0(2,iptyd)
c1	    k0_ptyd(ipd)=k_ptyd(iptyd)
c1	    k0_ist(ipd)=ist(iptyd)  
        else
	    ptyd1(iptyd)=0.0
          d1(iptyd)=0.0
	  end if

	  write(62,'(i5,1x,2f8.2,f10.2,f10.1)')ist(iptyd),aa0(1,iptyd),
	1aa0(2,iptyd),ptyd1(iptyd),d1(iptyd)   !!计算台风过程累积
        rlat_sh1(iptyd)=aa0(1,iptyd)
        rlon_sh1(iptyd)=aa0(2,iptyd)
	enddo

ccc        call get_a_v(ptyd,rlat_sh1,rlon_sh1,iptyd,tyda,tydv)
ccc	tyda=tyda/10000

      if(n_day.gt.0) then
	write(68,330) iyear,ktyphn,n_day,iymd_b,iymd_e, !tyda,tydv结果有误，可另外计算
     &wind_m,pres_n,n_day,num_mean
330	format(3i4,(i5,1h/,i2,1h/,i2,3h --,i5,1h/,i2,1h/,i2),2f7.1,2i4) 
      endif

      if(ipd.gt.0) then 
        write(97,779) iyear,ktyphn,ipd
	  write(97,777) (ist_case(jj0),jj0=1,ipd)
        write(97,777) (ist_ord(jj0),jj0=1,ipd)
        write(97,778) (pm_case(ist_ord(jj0)),jj0=1,ipd)
        write(97,778) (dm_case(ist_ord(jj0)),jj0=1,ipd)
	  write(97,777) (kdm_case(ist_ord(jj0)),jj0=1,ipd)


	 write(197,779) iyear,ktyphn,ipd
	  write(197,222) (ist_case(jj0),jj0=1,ipd)
        write(197,222) (ist_ord(jj0),jj0=1,ipd)
        write(197,333) (pm_case(ist_ord(jj0)),jj0=1,ipd)
        write(197,333) (dm_case(ist_ord(jj0)),jj0=1,ipd)
	  write(197,222) (kdm_case(ist_ord(jj0)),jj0=1,ipd)
	endif
779	format(3i4)  
778	format(50f6.1)  
777	format(50i6) 
333   format(f7.1)
222   format(i6)

	enddo
	enddo
       close(11)
	close(22)
	close(33)
	close(44)
c1	 close(67)
	close(68)
ccc	close(13)
      close(62)
	close(97)
	close(197)
      end
	  
      
	 
      
      

      
!!!!!!!!!!!!!!!!!end main program!!!!!!!!


      subroutine rainchn6(ist,n,glt,prcp,ma,m,neib,tfn_cnt,ntfn,istbelt,
     &wind_tfn,num_belt,wk,n_b,nbelt,amiss,
     &dist0,neib_0,prcp_0,ibelt_0,nbelt_0)
c******************************************************************
c******************************************************************
c***接口：
c***        INPUT：
c***                  n       ----中国最多台站数
c***                  ist     ---- WMO台站号
c***                  glt     ---- 台站纬、经度（单位：度）
c***                  prcp  ---- 台站日降水量
c***                  ntfn  ---- 当日台风位置次数
c***                  tfn_cnt  ---- 当日ntfn 次台风位置（纬、经度） （单位：度）
c***                  wind_tfn ---- 当日平均台风最大风速（单位：米/秒）
c***                  amiss  ----  资料缺省值
c***
c***         OUTPUT：
c***                 num_belt  ---- 台风雨带拥有的台站数
c***                 Istbelt    ----  台风雨带站号
c***
c******************************************************************
c******************************************************************
c*** function:  to search a typhoon rainbelt for all China
c***    
c*** parameters:
c***
c***   part 1:  parameters to be changed
c***   thd_prcp   ---- the threshold precipitation(in mm) for difining the main
c***                   feature of a rainbelt
c***   thd_rate   ---- the threshold rate( rainning neighbour station rate) for
c***                   difining the main feature of a rainbelt
c***   dist_stn   ---- the threshold radius(in km) for difining a typhoon
c***                   station
c***   dist_belt  ---- the threshold radius(in km) for difining a possibly
c***                   affected rainbelt
c***   dismax0    ---- the threshold radius(in km) for selecting a typhoon
c***                   station in a possibly affected rainbelt
c***	 wind_tfn   ---- the maximum wind speed in m/s
c***
c***   dist0      ---- the distance to define neighbour stationa          c
c***
c***   part 2: I/O parameters
c***   input:
c***   n          ---- the total number of stationa 
c***   ist(n)     ---- the n station numbers
c***   glt(2,n)   ---- the latitudes(glt(1,i)) and langitudes(glt(2,i))
c***                   for every station, where unit is in degree
c***   prcp(n)    ---- the precipitation for every station, where unit 
c***                   is in mm
c***   ma,m       ---- the dimension parameters for neib, where ma>=m+10
C***   neib(n,ma) ---- working space and output  /2005/7/19 by rfm 
c***                   the neighbour station information for the n stationa,
c***                   where neib(i,m) is the number of neighbour stationa 
c***                   station i( whose station number is ist(i)), and the
c***                   neib(i,m) neighbour station numbers are neib(i,1),
c***                   neib(i,2),neib(i,3)...neib(i,neib(i,m)), so m>neib(i,m).
c***                   the space from m+1 to ma is workspace.
c***   amiss       ---- missing data's value
c***   ntfn        ---- number of typhoon obervationa 
c***   tfn_cnt(2,ntfn)---- the latitudes(tfn_cnt(1,*)) and langitudes(tfn_cnt(2,*))
c***                       of the ntfn positiona of typhoon during the period
c***                       in which the precipitation was observed
c***c99.12.22   tfn_cnt(2) ---- the latitude(tfn_cnt(1)) and langitude(tfn_cnt(2))
c***c99.12.22                   for the average position of typhoon during the period
c***c99.12.22                   which the precipitation was observed
c***   output:
c***   num_belt   ---- the total number of stationa in the typhoon rainbelt
c***   istbelt(n) ---- the first num_belt positiona are the orders for the 
c***                   num_belt stationa, so their station numbers are 
c***                   ist(istbelt(1)),ist(istbelt(1)),...,ist(istbelt( num_belt))
c***   n_b        ---- the total number of rainbelt
c***   neib(i,m+8)---- the rainbelt number(varies from 0 to n_b, where 0 meana
c***                   no rain or can't be included into any rainbelt),
c***                   which station i belongs to
c***   neib(i,m+3)---- the rainbelt number(varies from 0 to n_b, and 99, where 
c***                   99 meana the typhoon rainbelt, and others are the same
c***                   as neib(i,m+8)
c***   nbelt(10)  ---- nbelt(1) is the total number of rainbelts which wholly
c***                   or partly included in the typhoon rainbelt, where
c***                   nbelt(2),nbelt(3),...,nbelt(nbelt(1)+1) is the rainbelt 
c***                   number
c***   workspace:
c***   neib(i,m+1) to neib(i,ma), and wk(i,2),nbelt(10)
c***   neib_0(n,ma),prcp_0(n),ibelt_0(n),nbelt_0(10)
c***
c***         by   Fumin Ren
c***            2005.7.19
c***
c******************************************************************     
c**************************************************************
c      parameter(na0=679,ma0=90,dist0=180) 
c*** 	na0 and ma0 must equal the values of na and ma in the main program
c*** 2005.7.19 by rfm
c**************************************************************
       dimension ist(n),glt(2,n),neib(n,ma),
     & istbelt(n),tfn_cnt(2,ntfn),prcp(n),wk(n,2),nbelt(10)
       dimension neib_0(n,ma),prcp_0(n),ibelt_0(n),nbelt_0(10)
      call  get_neib(neib,n,glt,m,dist0,ist)
c      subroutine rainchn3(ist,n,glt,prcp,ma,m,neib,tfn_cnt,ntfn,
c	&wind_tfn,istbelt,num_belt,wk,n_b,nbelt,irec9910,amiss)
	call rainchn3(ist,n,glt,prcp,ma,m,neib,tfn_cnt,ntfn,
     &wind_tfn,istbelt,num_belt,wk,n_b,nbelt,1,amiss)
	if(num_belt.ge.3) then
	  call put(n,ma,neib_0,neib)
	  CALL rinitn(n,1,prcp_0,0.0)
	  do 1001 k_0=1,num_belt
	  prcp_0(istbelt(k_0))=prcp(istbelt(k_0))
	  neib(istbelt(k_0),m+3)=neib(istbelt(k_0),m+8)
1001    continue
	  call rainchn3(ist,n,glt,prcp_0,ma,m,neib_0,tfn_cnt,ntfn,
     &wind_tfn,ibelt_0,num_0,wk,n_b_0,nbelt_0,2,amiss)
	  do 1002 k_0=1,num_0
	  neib(ibelt_0(k_0),m+3)=99  !判定属于台风雨带
1002    continue
        num_belt=num_0
	  call put(num_0,1,istbelt,ibelt_0)
	endif
	return
c*** 2005.7.19 by rfm
       end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         subroutine get_neib(neib,na,alt,m,dist0,ist)
       dimension alt(2,na),neib(na,m),ist(na)
c       write(*,*) ist(na),alt(1,na),alt(2,na),na
c**** dist0 is in km    
       call initn(na,m,neib,0)   
       do 3001 i=1,na
         do 2001 j=i+1,na
	 call cptd(alt(1,i),alt(1,j),dis)
	 if(dis.le.dist0) then
c	    write(*,*) '  i,m=',i,m
	   if(neib(i,m).lt.m) then
 	     neib(i,m)=neib(i,m)+1
	     neib(i,neib(i,m))=ist(j)
	   else
	     write(*,*) '  please make the parameters "ma" and "m"'
	     write(*,*) '  bigger, such as', m+20,m+10
	     stop
	   endif
	   if(neib(j,m).lt.m) then
 	     neib(j,m)=neib(j,m)+1
	     neib(j,neib(j,m))=ist(i)
	   else
	     write(*,*) '  please make the parameters "ma" and "m"'
	     write(*,*) '  bigger, such as', m+20,m+10
	     stop
	   endif
	 endif
2001     continue
3001   continue
c       write(*,*) na, neib(na,m),(neib(na,i),i=1,neib(na,m))
       return
       end
      subroutine rainchn3(ist,n,glt,prcp,ma,m,neib,tfn_cnt,ntfn,
     &wind_tfn,istbelt,num_belt,wk,n_b,nbelt,irec9910,amiss)
c******************************************************************
c*** function:  to search a typhoon rainbelt for all China
c***    
c*** parameters:
c***
c***   part 1:  parameters to be changed
c***   thd_prcp   ---- the threshold precipitation(in mm) for difining the main
c***                   feature of a rainbelt
c***   thd_rate   ---- the threshold rate( rainning neighbour station rate) for
c***                   difining the main feature of a rainbelt
c***   dist_stn   ---- the threshold radius(in km) for difining a typhoon
c***                   station
c***   dist_belt  ---- the threshold radius(in km) for difining a possibly
c***                   affected rainbelt
c***   dismax0    ---- the threshold radius(in km) for selecting a typhoon
c***                   station in a possibly affected rainbelt
c***	 wind_tfn   ---- the maximum wind speed in m/s
c***   part 2: I/O parameters
c***   input:
c***   n          ---- the total number of stationa 
c***   ist(n)     ---- the n station numbers
c***   glt(2,n)   ---- the latitudes(glt(1,i)) and langitudes(glt(2,i))
c***                   for every station, where unit is in degree
c***   prcp(n)    ---- the precipitation for every station, where unit 
c***                   is in mm
c***   ma,m       ---- the dimension parameters for neib, where ma>=m+10
C***   neib(n,ma) ---- the neighbour station information for the n stationa,
c***                   where neib(i,m) is the number of neighbour stationa 
c***                   station i( whose station number is ist(i)), and the
c***                   neib(i,m) neighbour station numbers are neib(i,1),
c***                   neib(i,2),neib(i,3)...neib(i,neib(i,m)), so m>neib(i,m).
c***                   the space from m+1 to ma is workspace.
c***   amiss       ---- missing data's value
c***   ntfn        ---- number of typhoon obervationa 
c***   tfn_cnt(2,ntfn)---- the latitudes(tfn_cnt(1,*)) and langitudes(tfn_cnt(2,*))
c***                       of the ntfn positiona of typhoon during the period
c***                       in which the precipitation was observed
c***c99.12.22   tfn_cnt(2) ---- the latitude(tfn_cnt(1)) and langitude(tfn_cnt(2))
c***c99.12.22                   for the average position of typhoon during the period
c***c99.12.22                   which the precipitation was observed
c***   output:
c***   num_belt   ---- the total number of stationa in the typhoon rainbelt
c***   istbelt(n) ---- the first num_belt positiona are the orders for the 
c***                   num_belt stationa, so their station numbers are 
c***                   ist(istbelt(1)),ist(istbelt(1)),...,ist(istbelt( num_belt))
c***   n_b        ---- the total number of rainbelt
c***   neib(i,m+8)---- the rainbelt number(varies from 0 to n_b, where 0 meana
c***                   no rain or can't be included into any rainbelt),
c***                   which station i belongs to
c***   neib(i,m+3)---- the rainbelt number(varies from 0 to n_b, and 99, where 
c***                   99 meana the typhoon rainbelt, and others are the same
c***                   as neib(i,m+8)
c***   nbelt(10)  ---- nbelt(1) is the total number of rainbelts which wholly
c***                   or partly included in the typhoon rainbelt, where
c***                   nbelt(2),nbelt(3),...,nbelt(nbelt(1)+1) is the rainbelt 
c***                   number
c***   workspace:
c***   neib(i,m+1) to neib(i,ma), and wk(i,2), i=1,n
c***
c***            Fumin Ren
c***            2005.7.19 
c******************************************************************     
       parameter(num_tw=10,dis_tw=250)
       dimension ist(n),glt(2,n),ist_tw(num_tw),neib(n,ma),
     & istbelt(n),tfn_cnt(2,ntfn),prcp(n),wk(n,2),nbelt(10)
       data ist_tw/46691,46692,46699,46708,46741,
     &             46749,46753,46757,46759,
     &             46766/
       common /dfn/thd_prcp,thd_rate,dismax0,dist_stn,dist_belt
       common /natorg/nato(3000)
       if(n.le.3000) then
         call put(n,1,nato,ist)
       else
         call put(3000,1,nato,ist)
       endif
c*****************************************
c***  parameters to be changed
       thd_prcp=17.2
       thd_rate=0.01
       dist_belt=400 !已修改
       call dmin_tfn(n,glt,tfn_cnt,d_mt0)
	   if(d_mt0.ge.300) then
	if(wind_tfn.lt.17.2) then
       dismax0=400  !D1
       dist_stn=200 !D0
	else if(wind_tfn.lt.24.4) then
       dismax0=600
       dist_stn=300
	else if(wind_tfn.lt.32.6) then
       dismax0=800
       dist_stn=400
	else 
       dismax0=1000
       dist_stn=500
	endif
	else
	  if(wind_tfn.lt.17.2) then
       dismax0=600
       dist_stn=300
c	  else if(wind_tfn.lt.24.4) then
c       dismax0=1000
c       dist_stn=400
	  else 
       dismax0=1000
       dist_stn=500
	  endif
	endif

c*****************************************

c*** to change the station number into station order
       if(irec9910.eq.1) call to_order(ist,n,n,m,neib)
101    format(50i7)       

c*** to find the typhoon rainbelt       
        write(*,*) '  rainbelt bg'
       call rainbelt(ist,n,glt,prcp,ma,m,neib,istbelt,
     & num_belt,tfn_cnt,ntfn,wk,n_b,nbelt,amiss)
     
c*** to conaider if Taiwan need to be included in the typhoon rainbelt
       
         call put(num_tw,1,neib(1,ma),ist_tw)
         do 4001 j2=1,ntfn
         if(tfn_cnt(1,j2).eq.amiss.or.tfn_cnt(2,j2).eq.amiss) go to 4001
         call to_ordn(ist,n,num_tw,neib(1,ma),irtn)
	 
	 if(irtn.eq.num_tw) then
           call conad_tw(n,glt,prcp,istbelt,num_belt,
     &     neib(1,ma),num_tw,dis_tw,num_dmin,neib(1,m+3),tfn_cnt(1,j2)
     &     ,nbelt,n_b)
         endif
	 go to 4002
4001     continue
4002     continue

c***   original rainbelt code save
       call put(n,1,neib(1,m+8),neib(1,m+3))

c***   use dist_stn to control the typhoon rainbelt
        num_belt=0
        do 4005 j2=1,ntfn
        if(tfn_cnt(1,j2).eq.amiss.or.tfn_cnt(2,j2).eq.amiss) go to 4005
        call dfn_by_r(n,neib(1,m+3),glt,tfn_cnt(1,j2),99
     &  ,num_belt,istbelt,prcp)
4005    continue

c***   use dismax0 to control the typhoon rainbelt
        do 4006 j2=1,ntfn
        if(tfn_cnt(1,j2).eq.amiss.or.tfn_cnt(2,j2).eq.amiss) go to 4006
	do 3006 j0=1,nbelt(1)
        call chg_cd0(n,neib(1,m+3),glt,tfn_cnt(1,j2),nbelt(j0+1),99
     &  ,num_belt,istbelt)
3006    continue
4006     continue
c********************if num_belt =< 3 then num_belt=0	 /2005/7/19 by rfm
       if(num_belt.le.3) then
         call put(n,1,neib(1,m+3),neib(1,m+8))
	   num_belt=0
	 endif
	return
       end
       subroutine to_order(ist,n,ip,m,neib)
       dimension ist(n),neib(ip,m)
       do 1001 k=1,ip
         if(neib(k,m).eq.0) go to 1001
         do 1002 j=1,neib(k,m)
           do 1003 i=1,n
	   if(neib(k,j).eq.ist(i)) then
	     neib(k,j)=i
	     go to 1002
	   endif
1003       continue       
1002     continue       
1001   continue       
       return
       end
      subroutine initn(n,m,ia,iv)
       dimension ia(n,m)
       do 1001 i=1,n
       do 1001 j=1,m
       ia(i,j)=iv
1001   continue
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine rinitn(n,m,ia,iv)
      real ia(n,m),iv
       do 1001 i=1,n
       do 1001 j=1,m
       ia(i,j)=iv
1001   continue
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine put(m,n,ia,ib)
      dimension ia(m,n),ib(m,n)
      do 1001 i=1,m
      do 1001 j=1,n
      ia(i,j)=ib(i,j)
1001  continue
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
       subroutine rainbelt(ist,n,glt,prcp,ma,m,neib,istbelt,
     & num_belt,tfn_cnt0,ntfn,rate,n_b,nbelt,amiss)
c***************************************************************
c*** function: to seperate the typhoon rainbelt from a certain 
c*** precipitation distribution
c*** parameters:
c*** input:
c***      n          ----  the total of stationa
c***      ist        ----  the n station numbers
c***      prcp       ----  the n station precipitation data
c***      glt        ----  the latitude and longitude of the stationa 
c***      ma         ----  the total of lines for NEIB
C***      m          ----  the line number for the number of neighbour  stationa
c***      neib       ----  the information of neighbour stationa for every
c***         station. For station i, neib(i,m) is the number of neighbour
c***         stationa, neib(i,1)...neib(i,neib(i,m)) is the neighbour station
c***         orders, while M+1...Ma lines is workspace.
c***      tfn_cnt0    ----  see the tfn_cnt in subroutine rainchn
c***         
c***      rate       ----  workspace
c*** output:
c***      nbelt(1)   ----  the number of the typhoon rainbelt
c***      nbelt(2)...nbelt(nbelt(1)+1)  ----  the  nbelt rainbelt code
c***      num_belt   ----  the total of stationa in the typhoon rainbelts
c***      istbelt    ----  the  num_belt stationa in the typhoon rainbelt
c***************************************************************
       parameter(n_max=100,idefault=90)
       dimension ist(n),glt(2,n),neib(n,ma),nbelt(10),
     & istbelt(n),tfn_cnt(2),prcp(n),rate(n,2),tfn_cnt0(2,ntfn)
       common /dfn/thd_prcp,thd_rate,dismax0,dist_stn,dist_belt
       num_belt=0

c*** to get the basic rainning information
c       write(*,*) '  getrate='
c       write(*,*) ' neib(1,1),neib(1,m),prcp(1),glt(1,1)=',
c     & neib(1,1),neib(1,m),prcp(1),glt(1,1)
       call getrate(ist,n,prcp,ma,m,neib,rate)
       call putr(n,1,rate(1,2),rate)
101    format(f8.3,i7)      

c*** to select the n_max rainning points to start to define a rainbelt
c       write(*,*) '  sort_m='
       call sort_max(rate(1,2),n,1,neib(1,m+8),neib(1,m+6),neib(1,m+1))
c       write(*,*) '  get_m='
c       write
c       write(*,*) '  (rate(i,2),i=1,10)=', (rate(i,2),i=1,10)
       call get_max(glt,n,neib(1,m),neib(1,m+1),neib(1,m+2),n_max
     & ,prcp,nhave)
c       write(*,*) '  nhave=',nhave

c*** to get the main pattern of rainning
       call initn(n,1,neib(1,m+3),0)
       n_b=0
       do 1001 k=1,nhave
       if(neib(neib(k,m+2),m+3).ne.0.or.
     & rate(neib(k,m+2),1).lt.0.01) go to 1001
c       if(neib(neib(k,m+2),m+3).ne.0.or.
c     & rate(neib(k,m+2),1).lt.thd_rate) go to 1001
        n_b=n_b+1
       call sgl_belt(n_b,neib(k,m+2),n,prcp,m,neib,rate
     & ,neib(1,m+3),neib(n_b,m+4),neib(1,m+5),neib(1,m+6))
1001   continue       

c*** to define the edges for all rainbelts
       ksl=0
       call put(n,1,neib(1,m+6),neib(1,m+3))
4      ksl=ksl+1
       do 1004 k=1,n
c       if(istrfmc(k).eq.58102) then
c         k00=k
c         write(*,*) '  neib(k,m+6),neib(k,m),prcp(k)=',
c     &   neib(k,m+6),neib(k,m),prcp(k) 
c       endif
       if(neib(k,m+6).eq.0.and.neib(k,m).gt.0.and.
     & prcp(k).gt.0.001.and.n_b.ge.1) then
         call select(k,neib,n,ma,m,n_b,rate(1,2))
       endif       
1004   continue     
       if(ksl.eq.1) go to 4  

c***  to select significant rainbelt
       n_b0=n_b
       n_b=0
       do 4004 k0=1,n_b0
       num1=0
       do 4005 k1=1,n
       if(neib(k1,m+3).eq.k0) num1=num1+1
4005   continue       
c       if(k0.eq.6) write(*,*) '  num1=',num1
       if(num1.le.2) then
         icd0=0
       else
         n_b=n_b+1
	 icd0=n_b
       endif
       if(icd0.ne.k0) call chg_code(n,neib(1,m+3),k0,icd0)
4004   continue       

c*** to redefine the edges for all rainbelts and 
c*** keep the no_belong stationa
       ksl=0
       call put(n,1,neib(1,m+6),neib(1,m+3))
24      ksl=ksl+1
       do 2004 k=1,n
       if(neib(k,m+6).eq.0.and.neib(k,m).gt.0.and.
     & prcp(k).gt.0.001.and.n_b.ge.1) then
         call select(k,neib,n,ma,m,n_b,rate(1,2))
       endif       
2004   continue     
       if(ksl.eq.1) go to 24  
       do 4007 k0=1,n
       if(neib(k0,m+3).eq.0.and.prcp(k0).gt.0.0) 
     &  neib(k0,m+3)=idefault
4007   continue       

c*** to select the typhoon rainbelts
       nbelt(1)=0
       do 7001 j8=1,ntfn
       if(tfn_cnt0(1,j8).eq.amiss.or.tfn_cnt0(2,j8).eq.amiss) go to 7001
       call putr(2,1,tfn_cnt,tfn_cnt0(1,j8))
       call dmin_tfn(n,glt,tfn_cnt,d_mt)
c       write(*,*) ' d_mt=',d_mt
c       if(d_mt.gt.dismax0-300) d_mt=dismax0-300
cc  changed by wxl 2004.4.22 300---1000
c20040510       if(d_mt.gt.dismax0-1000) d_mt=dismax0-1000
       if(d_mt.gt.200) d_mt=200+(d_mt-200)/4.0
c       if(d_mt.gt.200) d_mt=d_mt/4.0
c20050711 by rfm
       if(n_b.gt.0) then
         do 1005 k=1,n_b
	 do 1007 k5=1,nbelt(1)
	 if(nbelt(1+k5).eq.k) go to 1005
1007     continue
         call belt_cnt(neib(1,m+3),n,prcp,k,glt,rate(n_max+1,2),rainall)
    

c*** 99.10.14
         call cptd(rate(n_max+1,2),tfn_cnt,d0)
	 p1=0
         if(d0.le.dist_belt+d_mt) then
	 else
	   do 4001 k1=1,n
	   if(neib(k1,m+3).eq.k) then
	     call cptd(glt(1,k1),tfn_cnt,d1)
	     if(d1.le.300+d_mt) then
	       p1=p1+4
	     else  if(d1.le.400+d_mt) then
	       p1=p1+2
	     else  if(d1.le.500+d_mt) then
	       p1=p1+1
	     else  if(d1.le.600+d_mt) then
	       p1=p1+0.5
	     endif
	   endif
4001       continue	   
c           write(*,*) '  p1=',p1
           if(p1.lt.7.99) go to 1005
         endif
c	write(*,*) '  k,p1,d0,rate(n_max+1,2),rate(n_max+2,2),tfn_cnt=',
c     & k,p1,d0,rate(n_max+1,2),rate(n_max+2,2),tfn_cnt
         nbelt(1)=nbelt(1)+1
         nbelt(nbelt(1)+1)=k
	 do 1002 k0=1,n
	 if(neib(k0,m+3).eq.k) then
	   num_belt=num_belt+1
           istbelt(num_belt)=k0
         endif
1002     continue       
c*** 99.10.14
         if(nbelt(1).eq.9) return
1005     continue       
       endif
7001   continue
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
       subroutine dmin_tfn(n,glt,tfn_cnt,d_mt)
       dimension glt(2,n),tfn_cnt(2)
!       common /natorg/natn(3000)
       d_mt=99999
       do 4001 k1=1,n
       if(glt(1,k1).lt.18.0) go to 4001
       call cptd(glt(1,k1),tfn_cnt,d1)
       if(d_mt.gt.d1) then
         d_mt=d1
	 k0=k1
       endif
4001   continue	   
c       write(*,*) '  glt(1,k0),glt(2,k0)=', glt(1,k0),glt(2,k0)
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
       subroutine belt_cnt(kregst,n,prcp,n_belt,glt,wght_cnt,rainall)
       dimension kregst(n),glt(2,n),wght_cnt(2),prcp(n),wk(2)
       ipnt=0
       rainall=0
       do 1001 k=1,n
       if(kregst(k).eq.n_belt) then       
         ipnt=ipnt+1
	 rainall=rainall+prcp(k)
	 if(ipnt.eq.1) then
	   call putr(2,1,wght_cnt,glt(1,k))
	 else
	   call cnt_w(wght_cnt,rainall,glt(1,k),prcp(k),wk)
	   call putr(2,1,wght_cnt,wk)
	 endif
       endif
1001   continue       
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine cnt_w(a,wa,b,wb,c)
       dimension a(2),b(2),c(2)
       do 1001 i=1,2
       c(i)=a(i)+(b(i)-a(i))*wb/(wa+wb)
1001   continue       
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine chg_code(n,nd,iold,new)
       dimension nd(n)
         do 5006 k=1,n
	 if(nd(k).eq.iold) nd(k)=new
5006     continue
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine select(k0,neib,n,ma,m,n_b,wk)
       dimension neib(n,ma),wk(n_b)
       na=0
       call rinitn(n_b,1,wk,0.0)
       do 1002 k=1,neib(k0,m)
       if(neib(neib(k0,k),m+3).gt.n_b) then
         write(*,*) '   n_b,k0,n,neib(k0,k),neib(neib(k0,k),m+3)=',
     &   n_b,k0,n
     &   ,neib(k0,k),neib(neib(k0,k),m+3)
	 stop 
       else if(neib(neib(k0,k),m+3).gt.0) then
         na=na+1
	 wk(neib(neib(k0,k),m+3))=wk(neib(neib(k0,k),m+3))+1
       endif
1002   continue
c       if(istrfmc(k).eq.58102) then
c         write(*,*) '  wk=',wk
c       endif
       call sort_max(wk,n_b,1,neib(1,m+6),ib0,neib(1,m+7))
       if(wk(1).gt.0.0001) then
         neib(k0,m+3)=neib(1,m+7)
       endif
c       if(istrfmc(k).eq.58102) then
c         write(*,*) '  wk=',wk
c       endif
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine sgl_belt(n_belt,ipnt_bg,n,prcp,m,neib,
     &rate,kregst,num,nwk1,nwk2)
       dimension neib(n,m),prcp(n),rate(n),nwk1(n),nwk2(n),kregst(n)
       num=0
       kpnt=1
       nwk1(1)=ipnt_bg
1      if(kpnt.eq.0) return
       do 1001 k=1,kpnt
       kregst(nwk1(k))=n_belt
1001   continue       
       num=num+kpnt
       call round(kpnt,nwk1,n_belt,n,prcp,m,neib,rate,kregst,
     & next_pnt,nwk2)
       if(next_pnt.gt.0) then
         kpnt=next_pnt
	 call put(next_pnt,1,nwk1,nwk2)
	 go to 1
       endif
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine round(kpnt,nwk1,n_belt,n,prcp,m,neib,rate,kregst,
     & next_pnt,nwk2)
       common /dfn/thd_prcp,thd_rate,dismax0,dist_stn,dist_belt
       dimension neib(n,m),prcp(n),rate(n),nwk1(n),nwk2(n),kregst(n)
       next_pnt=0
       do 1001 k=1,kpnt
       if(neib(nwk1(k),m).gt.0) then
         do 1002 j=1,neib(nwk1(k),m)
	 if(kregst(neib(nwk1(k),j)).eq.0.and.
     &   ((prcp(neib(nwk1(k),j)).ge.thd_prcp.and.
     &    rate(neib(nwk1(k),j)).ge.thd_rate).or.
     &    (prcp(neib(nwk1(k),j)).ge.0.1.and.
     &    rate(neib(nwk1(k),j)).ge.0.5))) then
           if(next_pnt.gt.0) then
	     do 1003 i=1,next_pnt
	     if(nwk2(i).eq.neib(nwk1(k),j)) go to 1002
1003         continue       
	   endif
           next_pnt=next_pnt+1
	   nwk2(next_pnt)=neib(nwk1(k),j)
         endif
1002     continue       
       endif
1001   continue       
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine get_max(glt,n,neib,iod,maxi,n_max,prcp,na)
       dimension glt(2,n),neib(n),iod(n),maxi(n_max),prcp(n)
       common /dfn/thd_prcp,thd_rate,dismax0,dist_stn,dist_belt
       call initn(n_max,1,maxi,0)
       na=0
       do 1001 k=1,n
       if(neib(iod(k)).lt.1.or.prcp(iod(k)).lt.0.09) go to 1001
c       if(neib(iod(k)).lt.2) go to 1001
       if(na.ge.1) then
         do 1002 j=1,na
         call cptd(glt(1,maxi(j)),glt(1,iod(k)),dis)
         if(dis.le.300) go to 1001
1002     continue
       endif
5      na=na+1
       maxi(na)=iod(k)
c       write(*,*) glt(1,iod(k)),glt(2,iod(k))
       if(na.eq.n_max) return
1001   continue       
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       subroutine getrate(ist,n,prcp,ma,m,neib,rate)
       parameter(p_rate=0.09)
c***                    10.0mm       
       dimension ist(n),neib(n,ma),prcp(n),rate(n)
       do 1001 k=1,n
         p_num=0
c________________________________________________________	 
	 if(neib(k,m).lt.1.or.prcp(k).lt.p_rate) then
c________________________**______________*******_________
	   rate(k)=0.0
	 else
  	   do 1002 j=1,neib(k,m)
	   if(prcp(neib(k,j)).lt.p_rate) go to 1002
	   p_num=p_num+1
1002       continue
           rate(k)=p_num/neib(k,m)
         endif
1001   continue       
       return
       end
c*************************************************
       subroutine belt_tw(nat,na,glt,ist_tw,num_tw,info,prcp,
     & dismax,num_min,irfm)
c***************************************************************
c*** function: to determine if Taiwan is included in the rainbelt.
c***************************************************************
c***
c*** dismax maybe 250km,
c*** 
c***  IRFM is a return flag, "1" for accepting Taiwan is included in the
c***  rainbelt, where "0" for not.
c***
      dimension info(num_tw),nat(na),glt(2,na),ist_tw(num_tw),prcp(na)
      irfm=0
      num_ok=0
       do 4001 i=1,na
       do 4001 j0=1,num_tw
	 if(nat(i).eq.ist_tw(j0))  info(j0)=i
4001   continue	 
       do 3001 i=1,na
         if(nat(i).lt.58000) go to 3001
	 if(prcp(i).lt.0.0001) go to 3001
         do 3002 j0=1,num_tw	 
	 if(nat(i).eq.ist_tw(j0)) go to 3001
3002     continue	 
         do 2001 j=1,num_tw
	   if(prcp(info(j)).lt.0.0001) go to 2001
	   call cptd(glt(1,i),glt(1,info(j)),dis0)
	   if(dis0.lt.dismax) num_ok=num_ok+1
2001     continue
3001   continue
       if(num_ok.ge.num_min) irfm=1
       return
       end
       subroutine chg_cd0(n,nd,gtl,tfn_cnt,iold,new,num_belt,istbelt)
       dimension nd(n),gtl(2,n),tfn_cnt(2),istbelt(n)
       common /dfn/thd_prcp,thd_rate,dismax0,dist_stn,dist_belt
         do 5006 k=1,n
	 if(nd(k).eq.iold) then 
	   call cptd(gtl(1,k),tfn_cnt,dis)
	   if(dis.le.dismax0) then
	     nd(k)=new
	     num_belt=num_belt+1
             istbelt(num_belt)=k
	   endif
	 endif
5006     continue
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        subroutine dfn_by_r(n,nd,gtl,tfn_cnt,new,num_belt,istbelt,
     & prcp)
       dimension nd(n),gtl(2,n),tfn_cnt(2),istbelt(n),prcp(n)
       common /dfn/thd_prcp,thd_rate,dismax0,dist_stn,dist_belt
         do 5006 k=1,n
	 if(nd(k).eq.new) go to 5006
	 call cptd(gtl(1,k),tfn_cnt,dis)
	 if(dis.le.dist_stn.and.prcp(k).gt.0.09) then
	   nd(k)=new
	   num_belt=num_belt+1
           istbelt(num_belt)=k
	 endif
5006     continue
       return
       end
       subroutine conad_tw(n,glt,prcp,istbelt,num_belt,
     & ist_tw,num_tw,dis_tw,num_dmin,kregst,tfn_cnt,nbelt,n_b)
       dimension glt(2,n),prcp(n),ist_tw(num_tw),istbelt(n),kregst(n)
     & ,cnt(2),tfn_cnt(2),nbelt(10)
       common /dfn/thd_prcp,thd_rate,dismax0,dist_stn,dist_belt
	 rmin=0.001
       num_dmin=0
       nr_tw=0
       if(num_belt.eq.0) call initn(n,1,istbelt,0)
       do 1001 i=1,num_tw
       if(prcp(ist_tw(i)).gt.rmin) nr_tw=nr_tw+1
       if(prcp(ist_tw(i)).gt.rmin.and.prcp(ist_tw(i)).lt.thd_prcp) then
         npl=1
       else if(prcp(ist_tw(i)).ge.thd_prcp) then
         npl=2
       else
         go to 1001
       endif
       if(num_belt.eq.0) then
         num_dmin=num_dmin+npl*5
	 istbelt(ist_tw(i))=88
       else
         do 1002 j=1,num_belt
         call cptd(glt(1,ist_tw(i)),glt(1,istbelt(j)),dis)
         if(dis.le.dis_tw) then
	   num_dmin=num_dmin+npl
	   icd_tfn=kregst(istbelt(j))
         endif 
1002     continue       
       endif 
1001   continue       
       if(num_dmin.lt.30) then
         if(nr_tw.ge.4) then
	   n_b=n_b+1
           do 5001 i=1,num_tw
           if(prcp(ist_tw(i)).gt.rmin) kregst(ist_tw(i))=n_b
5001       continue	   
	 endif
         return
       endif
       if(num_belt.eq.0) then
	   n_b=n_b+1
           do 5003 i=1,num_tw
           if(prcp(ist_tw(i)).gt.rmin) kregst(ist_tw(i))=n_b
5003       continue	   
         call belt_cnt(istbelt,n,prcp,88,glt,cnt,rainall)
	 call cptd(cnt,tfn_cnt,dis)
!	 write(*,*) ' dis=',dis
	 if(dis.lt.500) then
	   nbelt(1)=1
	   nbelt(nbelt(1)+1)=88
	   num_belt=0
	   do 1007 k0=1,n
	   if(istbelt(k0).eq.88) then
	     num_belt=num_belt+1
	     istbelt(num_belt)=k0
	   endif
1007       continue	   
	 endif
       else
         do 2001 i=1,num_tw
         if(prcp(ist_tw(i)).lt.rmin) go to 2001
	 do 2003 i0=1,num_belt
	 if(istbelt(i0).eq.ist_tw(i)) go to 2001
2003     continue       	 
         num_belt=num_belt+1
	 kregst(ist_tw(i))=icd_tfn
         istbelt(num_belt)=ist_tw(i)
2001     continue       
       endif
       return
       end

       subroutine to_ordn(ist,n,m,nat,irtn)
       dimension ist(n),nat(m)
       irtn=0
         do 1002 j=1,m
           do 1003 i=1,n
	   if(nat(j).eq.ist(i)) then
	     nat(j)=i
	     irtn=irtn+1
	     go to 1002
	   endif
1003       continue       
1002     continue       
1001   continue       
       return
       end
 	 subroutine sort_max(ir,n,m,ib,ib0,iod)
	 real ir(n),ir0
c	 dimension ir(n),ir0
c         real ib(m,n),ib0(m)
	 dimension ib(m,n),ib0(m)
         dimension iod(n)
	 do 1001 k=1,n
	 iod(k)=k
1001     continue
	 do 1002 k=2,n
	 do 1003 j=1,k-1
	   if(ir(k).gt.ir(j)) then
	     id=iod(k)
	     ir0=ir(k)
	     do 1004 m1=1,m
	     ib0(m1)=ib(m1,k)
1004         continue
             do 1005 i=k,j+1,-1
	     ir(i)=ir(i-1)
	     iod(i)=iod(i-1)
	     do 1006 m1=1,m
	     ib(m1,i)=ib(m1,i-1)
1006         continue
1005         continue
	     iod(j)=id
	     ir(j)=ir0
	     do 2004 m1=1,m
	     ib(m1,j)=ib0(m1)
2004         continue
	   endif
1003     continue
1002     continue
         return 
	 end
!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!	 
!!!!!!!!!!!!!!!!!!!!!!
      subroutine get_a_v( prcp,lat,lon,num_stn,area,volume)
c***************************************************************************     
c***   降水区面积及总降水容积计算子程序
c***  输入参数:
c***     num_stn       ---- 台站总数
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
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
     &            ,length=100000,power=2)
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
      subroutine r_area(alat,area)
c****  to compute a 0.5X0.5 area at latitude alat
	 pai=3.1415926
        r=6371.229
        call du_fudu(abs(alat),fai)
        area=pai*pai*r*r*cos(fai)/(360*360)
	return
	end

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

      
       subroutine newprcp1(istn,prcp,dir,n,ns)
	     
c*** to read the new precipitation data for date-iymd-
c*** prcp data is in mm
c*** istn is the station number
c*** prcp is the daily precipitation 
      character*50 newfn
      dimension istn(2004),prcp(ns,n),dir0(n),dir(ns,n)     
	real ip(n)
c110   format(100i6)
       newfn='station\50353.dat'
	do 1001 i=1,ns
	print*,'已读取',i
      write(newfn(9:13),102) istn(i)
      open(21,file=newfn,status='old')!读入文件，原始大风数据
	do j=1,n
      read(21,*,iostat=ierror) ist,ilat,ilon,iy,im,id,ip(j),dir0(j)
!	print*,ip(j)
	 if(ierror==-1)then
	   close(21)
	 endif
!      if(ip(j).le.10.8) ip(j)=0.0 
	if(ip(j).gt.30000) ip(j)=0.0
!	if(ip(j).le.10.8) dir0(j)=0.0
      prcp(i,j)=ip(j)/10.0
	!prcp(i,j)=ip(j)
	dir(i,j)=dir0(j)
	end do
c      do 1002 j=1,n
c1002  continue
      close(21)
12    format(f6.2)
1001  continue
102   format(i5)
818   format(i7,f6.2,f8.2,i13,f6.2,i3)
!      open(1,file='c:\a.txt')
!	 do i=1,n
!	 write(1,'(i5)') ip(i)
!	enddo
!       write(1,'(i5)') n
!	close(1)
      return
      end
!!!!!!!!!!!!!!!!求某一年第几号台风所有信息！！！！！！！
	subroutine sh_trck(iyear,ktyphn,tracknew,wind,idat,ntim,ICHNL)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc  idat(j,i): j=1,5---- month,day,time(in Beijing),year and wind(in interger,unit:m/s)    
ccc             for step i
ccc  wind(i)  :      ---- pressure in hPa for step i
ccc  track(j,i): j=1,2---- latitude and longitude for step i
       character*50 fn
       dimension idat(5,150),tracknew(2,150),ids(4),wind(150)
	 
      fn='tracksh_1980-2022-6h.txt'	 
       open(ICHNL,file=fn,status='old')
       ntim=0   !指台风ktyphn的记录次数

103   format(i4,i3,i7,i3,i5,1x,i2,1x,i2,1x,i2,1x,2f6.1,2i5)
1	read(ICHNL,103,end=2) iy,itfnod,iden,iorder,ids,    !!!!加入了编号iden
     & rlat,rlon,ip,iams
c4002  continue
c1      read(71,102,end=2) iy,itfnod,iorder,im1,id1,it1,
c     & rlat,rlon,p,knot,ams0
       if(iy.eq.iyear.and.itfnod.eq.ktyphn) then
         ntim=ntim+1
	 if(iorder.ne.ntim) then
	   write(*,*) iy,itfnod,iorder,ids,
     & rlat,rlon,ip,iams
           stop
         endif
	 idat(1,ntim)=ids(2)
	 idat(2,ntim)=ids(3)
	 idat(3,ntim)=ids(4)
	 idat(4,ntim)=ids(1)
       idat(5,ntim)=iams
	 wind(ntim)=ip
	 tracknew(1,ntim)=rlat
	 tracknew(2,ntim)=rlon
       else if(iy.gt.iyear.or.(iy.eq.iyear.and.itfnod.gt.ktyphn)) then
         go to 2
       endif
       go to 1
2      close(ICHNL)
c102   format(i5,2i3,1x,2(i2,1x),i2,2x,2f6.1,i7,2i4)
       return
       end

!!!!!!!!!!!!!!!!求一日台风4次路径！ 
       subroutine trdate1(iymd,tracknew,wind,idat,trckdate,ms4,
     & winddate,ntim,amiss)
       dimension idat(5,150),tracknew(2,150),wind(150),trckdate(2,4),
     & winddate(4),itimod(4),ms4(4),iymd(3)

       data itimod/2,8,14,20/
       call rinitn(2,4,trckdate,amiss)
       call rinitn(4,1,winddate,amiss)
       kb=0
       do 1001 k=1,ntim
       if(idat(2,k).eq.iymd(3)) then
         do 1002 j=1,4
	 if(idat(3,k).eq.itimod(j)) then 
	     do 1005 i0=j,4
             call putr(2,1,trckdate(1,i0),tracknew(1,k)) !???????????????
             winddate(i0)=wind(k)
	       ms4(i0)=idat(5,k)
1005         continue  
	   if(kb.eq.0) then
	     kb=9
	    if(j.gt.1) then
	     do 1003 i0=1,j-1
             call putr(1,2,trckdate(1,i0),tracknew(1,k))
             winddate(i0)=wind(k)
	       ms4(i0)=idat(5,k)
1003         continue              	     
	    endif
	   endif
	 endif
1002     continue              
       endif
1001   continue              
       return
       end




!!!!!!!!!!!!!!!!!!!!!!!!!!!1call cptdays(1950,12,31,k,iyear,imonth,idate)！
       subroutine cptdays(iy1,im1,id1,ndays,iy2,im2,id2)

c*** to gain the day id2/im2/iy2 from id1/im1/iy1 and the later days NDAYS

       dimension kdays(12)
       data kdays/31,28,31,30,31,30,31,31,30,31,30,31/
       if(iy1.le.1000) then
         write(*,*) '  the years IY1 ',iy1
         write(*,*) '  must be input in full( such as "1999")!'
         write(*,*) '  Are you sure it is in full?(1 -- Yes, 0 -- No)'
1        read(*,*,err=2) ir
         if(ir.eq.0) stop
         if(ir.eq.1) go to 3
2         write(*,*) '  input again!'
       endif
3      continue
       if(im1.lt.1.or.im1.gt.12) then
         write(*,*) '  IM1=',im1
         write(*,*) '  It must be between 1 and 12 !'
         stop
       endif
       mdays1=kdays(im1)
       if(im1.eq.2) then
         call byear(iy1,ibig1)
         if(ibig1.eq.1) mdays1=29
       endif
       if(id1.lt.1.or.id1.gt.mdays1) then
         write(*,*) '  IY1,IM1,ID1=',IY1,IM1,id1
         write(*,*) '  ID1 must be between 1 and ',mdays1
         stop
       endif

c*** to compute the later day

       iy2=iy1
       im2=im1
       id2=id1
       if(ndays.eq.0) return
       jds=ndays
       if(ndays.lt.0) jds=-ndays
       do 1001 kd=1,jds
         if(ndays.gt.0) then
           id2=id2+1
           mdays=kdays(im2)
           if(im2.eq.2) then
             call byear(iy2,ibig)
             mdays=mdays+ibig
           endif
           if(id2.gt.mdays) then
             id2=1
             im2=im2+1
             if(im2.gt.12) then
               im2=1
               iy2=iy2+1
             endif
           endif
         else
           id2=id2-1
           if(id2.lt.1) then
             im2=im2-1
             if(im2.lt.1) then
               im2=12
               iy2=iy2-1
             endif
             mdays=kdays(im2)
             if(im2.eq.2) then
               call byear(iy2,ibig)
               mdays=mdays+ibig
             endif
             id2=mdays
           endif
         endif
1001   continue
       return
       end   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
       subroutine sh_inf(iyear,ntyphn,info,ICHNL)   !求某一年的台风个数,起终时间

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc  info(j,i): j=1,10---- nstep,month_bg,day_bg,time_bg(in Beijing time)    
ccc             month_nd,day_nd,time_nd,year_bg,iorder,year_nd
ccc             for step i
       character*50 fn
       dimension info(11,80),IDS1(4),IDS2(4)

       
	 fn='tracksh_1980-2022-6h.inf'
       open(ICHNL,file=fn,status='old')
       ntyphn=0  !表每年的台风数
1     READ(ICHNL,102,end=2) iy,iorder,iden,NSTEP,IDS1,IDS2
c      write(*,*) '  ',iy,iorder,NSTEP,IDS1,IDS2
102   format(i4,i3,i7,i3,i5,1x,i2,1x,i2,1x,i2,4x,i4,
     &1x,i2,1x,i2,1x,i2,1x)
c102   format(i4,2i3,i5,1h/,i2,1h/,i2,1h/,i2,4h -- ,I4,
c     &1h/,i2,1h/,i2,1h/,i2,1hB)
C1      read(70,101,end=2) iy,iorder,itimes,im1,id1,it1,im2,id2,it2,
C     & ichnod,ichnber,iusaod
       if(iy.eq.iyear) then
         ntyphn=ntyphn+1
	 if(iorder.ne.ntyphn) then
	   write(*,*) iy,iorder,iden,NSTEP,IDS1,IDS2
           stop
         endif
	 info(1,ntyphn)=NSTEP	 
 	 info(2,ntyphn)=IDS1(2)
 	 info(3,ntyphn)=IDS1(3)
 	 info(4,ntyphn)=IDS1(4)
  	 info(5,ntyphn)=IDS2(2)
 	 info(6,ntyphn)=IDS2(3)
 	 info(7,ntyphn)=IDS2(4)
 	 info(8,ntyphn)=IDS1(1)
 	 info(9,ntyphn)=iorder
 	 info(10,ntyphn)=IDS2(1)
	 info(11,ntyphn)=iden  
       else if(iy.gt.iyear) then
	   
         go to 2
       endif
       go to 1
2      close(ICHNL)
       return
       end

       subroutine byear(iy,ibig)
c      to decide whether it is a bigger year (ibig=1 is the bigger year)
       ie400=mod(iy,400) 
       ie100=mod(iy,100) 
       ie4=mod(iy,4)
       if(ie400.eq.0.or.(ie4.eq.0.and.ie100.ne.0)) then
         ibig=1
       else
         ibig=0
       endif
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
       subroutine difdays(iy1,im1,id1,iy2,im2,id2,ndays)

c*** to gain the days from id1/im1/iy1 to id2/im2/iy2

       dimension kdays(12)
       data kdays/31,28,31,30,31,30,31,31,30,31,30,31/
       if(iy1.le.1000.or.iy2.le.1000) then
         write(*,*) '  the years IY1 and IY2 =',iy1,iy2
         write(*,*) '  they must be input in full( such as "1999")!'
        write(*,*)'  Are you sure they are in full?(1 -- Yes, 0 -- No)'
1        read(*,*,err=2) ir
         if(ir.eq.0) stop
         if(ir.eq.1) go to 3
2         write(*,*) '  input again!'
       endif
3      continue
       if(im1.lt.1.or.im1.gt.12.or.im2.lt.1.or.im2.gt.12) then
         write(*,*) '  IM1 and IM2 =',im1,im2
         write(*,*) '  they must be between 1 and 12 !'
         stop
       endif
       mdays1=kdays(im1)
       mdays2=kdays(im2)
       if(im1.eq.2) then
         call byear(iy1,ibig1)
         if(ibig1.eq.1) mdays1=29
       endif
       if(im2.eq.2) then
         call byear(iy2,ibig2)
         if(ibig2.eq.1) mdays2=29
       endif
       if(id1.lt.1.or.id1.gt.mdays1.or.id2.lt.1.or.id2.gt.mdays2) then
         write(*,*) '  ID1 and ID2 =',id1,id2
         write(*,*) '  they must be between 1 and ',mdays1
         write(*,*) '  , and between 1 and ',mdays2,' separatively !'
         stop
       endif
       iymd1=iy1*10000+im1*100+id1
       iymd2=iy2*10000+im2*100+id2
       if(iymd1.gt.iymd2) then
         iy4=iy1
         im4=im1
         id4=id1
         iy3=iy2
         im3=im2
         id3=id2
       else
         iy3=iy1
         im3=im1
         id3=id1
         iy4=iy2
         im4=im2
         id4=id2
       endif

c***  the former days before id3/im3/iy3 in iy3(the smaller one)

       nd1=0
       if(im3.gt.1) then
         call byear(iy3,ibig)
         do 1001 km=1,im3-1
         nd1=nd1+kdays(km)
         if(km.eq.2.and.ibig.eq.1) nd1=nd1+1
1001     continue
       endif
       nd1=nd1+id3-1

c***  the later days after id4/im4/iy4 in iy4(the larger one)

       nd2=0
       if(im4.lt.12) then
         call byear(iy4,ibig)
         do 1002 km=im4+1,12
         nd2=nd2+kdays(km)
         if(km.eq.2.and.ibig.eq.1) nd2=nd2+1
1002     continue
       endif
       nd2=nd2+kdays(im4)-id4
       if(im4.eq.2.and.ibig.eq.1) nd2=nd2+1

c***  all days btween iy3 and iy4

       ndall=0
       do 1003 ky=iy3,iy4
       call byear(ky,ibig)
       ndall=ndall+365+ibig
1003   continue

c***  to compute ndays

       ndays=ndall-nd1-nd2-1
       if(iymd1.gt.iymd2) ndays=-ndays
       return
       end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine chn_724(num_belt,istbelt,iymd,rain)
	character*60  cc
      dimension istbelt(num_belt),a(662),iymd(3),rain(662)
      call difdays(1959,12,31,iymd(1),iymd(2),iymd(3),k)
c	write(*,*) '  k,iymd(1),iymd(2),iymd(3)=',
c     &k,iymd(1),iymd(2),iymd(3)
c	read(1,'(a)',rec=k) cc
c	write(*,*) '  cc=',cc
	read(1,101,rec=k) iy2,im2,id2,a
c	write(*,*) '  ',iy2,im2,id2,a
c	pause
101   format(3i4,662f6.1)
      do 1001 j=1,num_belt
	i=istbelt(j)
	a(i)=rain(i)
1001  continue
	write(1,101,rec=k) iy2,im2,id2,a
 	return
	end

      subroutine m_maxi(na,n,ml,mh,max0)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc  整型数组求最大值
ccc   na(n) ---- the data
ccc   ml,mh ---- 数组的有效取值范围 (ml<na(i)<mh)
ccc   max0  ---- 最大值,当max0=ml时,数组无有效取值
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension na(n)
      max0=ml
	do 1001 i=1,n
	if(na(i).gt.ml.and.na(i).lt.mh) then
	  if(na(i).gt.max0) max0=na(i)
	endif
1001  continue
 	return
	end
      subroutine rmaxi(na,n,ml,mh,max0)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc  实型数组求最大值
ccc   na(n) ---- the data
ccc   ml,mh ---- 数组的有效取值范围 (ml<na(i)<mh)
ccc   max0  ---- 最大值,当max0=ml时,数组无有效取值
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real na(n),ml,mh,max0
c      write(*,*) '  rmaxi sub'
c	write(*,*) '  na=',na
      max0=ml
	do 1001 i=1,n
	if(na(i).gt.ml.and.na(i).lt.mh) then
	  if(na(i).gt.max0) max0=na(i)
	endif
1001  continue
c      write(*,*) '  max0=',max0
 	return
	end
      subroutine rmini(na,n,ml,mh,min0)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccc  实型数组求最小值
ccc   na(n) ---- the data
ccc   ml,mh ---- 数组的有效取值范围 (ml<na(i)<mh)
ccc   min0  ---- 最小值,当min0=mh时,数组无有效取值
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real na(n),ml,mh,min0
c      write(*,*) '  rmaxi sub'
c	write(*,*) '  na=',na
      min0=mh
	do 1001 i=1,n
	if(na(i).gt.ml.and.na(i).lt.mh) then
	  if(na(i).lt.min0) min0=na(i)
	endif
1001  continue
c      write(*,*) '  min0=',min0
 	return
	end
























































	    