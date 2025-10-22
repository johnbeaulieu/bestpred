subroutine bestpred &
      (mtrait,use3X,maxprnt &
      ,tstdays,dim,super,Xmilk,weigh,sample,MRD, yield &
      ,herdavg,agefac,cowid,fresh,parity,length &
      ,DCRm,DCRc,DCRs,YLDvec,PERSvec,RELyld,RELpers &
      ,Yvec,bump,BLUPout,BLUPn                            &
      ,GRAFplot,DEBUGmsgs,ONscreen                        &
      ,laclen,dailyfreq, INTmethod, maxlen                &
      ,DAILYbp,DAILYherd,WRITEcurve,CURVEfile             &
      ,WRITEdata,DATAfile,plotfreq,INTmethodSCS           &
      ,READparms,UNITSin, UNITSout, breedUNK, dim0        &
      ,dim0flag, LOGon, LOGfile, LOGfreq, maxshow         &
      ,herdstate, CURVEsmall, CURVEsingle                 &
      ,region, season                                     &
      )
  integer,parameter :: last=2, part=1
  integer,parameter :: nbump=0
  integer,save :: nbmp
  integer,save :: lacn
  integer,save :: month
  integer,save :: ncall=0
  integer,save :: precise=1
  integer :: READparms, maxshow
  real,save :: maxbump
  real, dimension(:,:,:), allocatable, save :: dyield
  real*8,save :: lplus
  real*8, dimension(:,:,:), allocatable, save :: meanyld
  real*8, dimension(:,:,:,:,:), allocatable, save :: covari
  real*8,save :: corr305(4,4)
  real*8, dimension(:,:,:,:,:), allocatable, save :: covd
  real*8,save :: meanp(2,4)
  real*8,save :: varp(2,4)
  real*8,save :: lacdif
  real*8,save :: sddif
  real*8,save :: stdvar(4,last)
  real*8, dimension(:,:,:), allocatable, save :: sd
  real*8,save :: summ
  real*8,save :: dV1(4,4)
  real*8,save :: dVd(4,4)
  real*8 :: d0(2,4)
  real*8,save :: vijkl
  real*8,save :: qpq=0.d0
  real*8,save :: tdhi
  real*8,save :: tdlo
  real*8,save :: zero
  integer :: dim0(8), dim0flag
  real :: dim0wgts(2) = (/ 0.285, 0.715 /)
  real*8 :: DAILYbp(2,4,maxlen)
  real*8 :: DAILYherd(4,maxlen)
  character :: INTmethod, INTmethodSCS, UNITSin, UNITSout
  character*2 :: herdstate
  integer :: region, season
  integer, save :: oldregion, oldseason
  integer, parameter :: maxy=200, maxtd=50
  integer mtrait,use3X,maxprnt, &
     dim(maxtd),Xmilk(maxtd),weigh(maxtd),sample(maxtd), &
     MRD(maxtd),super(maxtd),tstdays,length, &
     parity,BLUPn(10), DEBUGmsgs             &
     ,ONscreen, laclen, maxlen
  character(100) BLUPout(10)
  real*8 DCRm,DCRc,DCRs,YLDvec(2,16),PERSvec(4),RELyld(4),RELpers(4), &
    var(maxy,maxy),ymean,yield(2,4,maxtd),                            &
    vary,covary,vari(4,4),dev(maxy,1),                              &
    multi(4,1), &
    herdavg(2,4), &
    herd305(2,4),hratio(4),herd365(2,4),herd999(2,4),herdpart(2,4), &
    sum305(4),sum365(4),sum999(4),sumpart(4), &
    dvari(4,4),dcov(4,maxy),dcovp(maxy,4), &
    persist(4,1),pers(4,1), &
    partial(4,maxy),partrec(4,1), &
    xsingl(4),xmulti(4),covsum(4,maxy), &
    agefac(4),varfac(4),test3X(4,maxtd),fact3X(4),part3X(4), &
    Yvec(2,4),DCRvec(4),lacwt(4),milk3X, &
    qCVC1(4,4),Rvar(2,2),Rvec(12), &
    Rvecs,Rvecm,Rvecf,Rvecp,Rcorr, &
    Zdev(maxtd),Zdif,regrel(4), &
    tdregr,bump(4),bumpsd(4),bigbump(4),varb(4)
  real*8, dimension(:,:), allocatable :: curve, curvem, graph1
  real*8, dimension(:,:,:), allocatable :: grafall
  real*8, dimension(:,:,:), allocatable :: curves
  real*8, dimension(:), allocatable :: freq3X
  real*8, dimension(:,:,:), allocatable :: std, tempSTD
  real*8, dimension(:,:), allocatable :: stdslice, tempSTDslice, tempTD
  real*8  cov305(4,maxy),covp305(maxy,4),                  &
        covar305(4,maxy),vari305(4,4),multi305(4,1),     &
        cov365(4,maxy),covp365(maxy,4),                  &
        covar365(4,maxy),vari365(4,4),multi365(4,1),     &
        cov999(4,maxy),covp999(maxy,4),                  &
        covar999(4,maxy),vari999(4,4),                   &
        multi999(4,1),single305(4,1),single365(4,1),     &
        single999(4,1)
  integer dailyfreq, st_start, plotfreq, breedUNK
  character*8 :: BESTPREDversion = '2.0rc7'
  character*10 :: BESTPREDdate = '11/26/2014'
  character*64 :: BESTPREDname = 'John B. Cole'
  character*64 :: BESTPREDemail = 'john.cole@ars.usda.gov'
  character*64 :: BESTPREDwebsite = 'http://www.aipl.arusda.gov/software/bestpred/'
  real, save :: zero0=0.d0, lb=2.205
  real, save :: dyld(12,4,last)=reshape((/ &
      53.3,63.,66.3,65.5,63.9,62.,60.1,58.1,55.6,52.3,49.,47. &
     ,2.3,2.32,2.31,2.28,2.25,2.22,2.19,2.16,2.11,2.03,1.95,1.9 &
     ,1.81,1.9,1.99,2.01,2.01,1.99,1.96,1.92,1.87,1.8,1.74,1.67 &
     ,3.38,2.63,2.41,2.44,2.48,2.52,2.57,2.6,2.66,2.74,2.82,2.9 &
     ,75.1,86.,86.9,82.2,77.2,72.2,67.2,62.1,56.2,49.8,46.,42. &
     ,3.33,3.2,3.0,2.85,2.71,2.58,2.45,2.29,2.12,1.92,1.81,1.7 &
     ,2.6,2.55,2.55,2.49,2.4,2.3,2.17,2.04,1.89,1.72,1.62,1.55 &
     ,3.35,2.84,2.8,2.9,3.02,3.15,3.28,3.43,3.58,3.76,3.88,4.0/) &
     ,(/12,4,last/)) &
     ,dsd(12,4,last)=reshape((/ &
      10.7,10.6,9.9,9.6,9.5,9.5,9.5,9.6,9.8,10.3,10.7,11.2 &
     ,.51,.45,.41,.39,.38,.38,.37,.37,.37,.39,.40,.41 &
     ,.34,.31,.28,.28,.28,.29,.29,.29,.30,.32,.33,.34 &
     ,1.51,1.45,1.42,1.41,1.4,1.41,1.41,1.41,1.41,1.42,1.42,1.42 &
     ,15.,15.3,14.4,13.8,13.3,13.,12.9,13.,13.6,14.7,15.,15.5 &
     ,.75,.69,.62,.58,.55,.52,.51,.51,.53,.57,.58,.59 &
     ,.50,.43,.40,.38,.38,.38,.38,.39,.42,.46,.47,.49 &
     ,1.63,1.69,1.71,1.69,1.66,1.61,1.56,1.51,1.47,1.45,1.44,1.4/) &
     ,(/12,4,last/))
  character breed6*6
  character, save :: breed(6)=(/'A', 'B',  'G',  'H',  'J',  'M'/)
  real, save :: brdyld(6,4,2)=reshape((/ &
      15080., 16616., 13864., 20845., 14120., 14472. &
     ,  589.,   668.,   625.,   763.,   662.,   518. &
     ,  505.,   586.,   481.,   656.,   531.,   476. &
     ,  3.16,   3.22,   3.35,   3.20,   3.31,   2.87 &
     ,18532., 21577., 16850., 25658., 18161., 17475. &
     ,  713.,   867.,   745.,   935.,   833.,   624. &
     ,  582.,   714.,   551.,   771.,   644.,   544. &
     ,  2.95,   2.93,   3.29,   3.08,   3.32,   3.06/),(/6,4,2/))
  real, save :: brdsd(6,4)=reshape((/ &
       2300.,  2530.,  2324.,  2946.,  2128.,  2332. &
     ,  115.,   115.,   112.,   119.,   109.,   112. &
     ,   92.,    92.,    92.,    97.,    89.,    92. &
     ,  1.28,   1.21,   1.35,   1.34,   1.18,   1.30/),(/6,4/))
  real, save :: rmonth(4,last)=reshape((/ &
      .962,.960,.962,.943, &
      .958,.956,.958,.960/),(/4,last/))
   real Fvalue
   real, save :: Fval(5,10)=reshape((/ &
     2.70555,   6.63495,   10.8277,   15.1369,   19.5118, &
     2.30260,   4.60521,    6.9079,    9.2105,   11.5132, &
     2.08381,   3.78166,    5.4222,    7.0360,    8.6341, &
     1.94487,   3.31921,    4.6168,    5.8783,    7.1185, &
     1.84728,   3.01729,    4.1031,    5.1491,    6.1714, &
     1.77412,   2.80202,    3.7430,    4.6428,    5.5180, &
     1.71673,   2.63937,    3.4746,    4.2683,    5.0371, &
     1.67021,   2.51131,    3.2656,    3.9786,    4.6666, &
     1.63153,   2.40737,    3.0975,    3.7468,    4.3713, &
     1.59873,   2.32096,    2.9589,    3.5565,    4.1298/),(/5,10/))
  real, save :: expamt=0.6 &
     ,repty(4)=(/.55, .55, .55, .30/) &
     ,reptp(4)=(/.44, .44, .44, .20/) &
     ,reptyp(4)=(/.00, .00, .00, .00/)
  real, save :: X305(4)=(/1., 1., 1., 305./)
  real, save :: X100(4)=(/1., 1., 1., 100./)
  integer q(6),qq(6,maxy),size,i,j,k,l,m,n,i6,brd &
     ,ntests(4),maxyld,minyld,middle(maxtd) &
     ,dimmilk(maxtd),dimsort(maxtd) &
     ,mstart,mstop,kk,MRDi &
     ,testday,yearfr
  integer, save :: oldbrd
  integer usetd
  integer :: maxyield(4),minyield(4)
  integer, dimension(:), allocatable :: dimvec
  integer :: LOGon, LOGfreq
  character cowid*17,fresh*8,brd1*1
  character, save :: plus*1='+'
  character(4) percent
  character(4), dimension(:), allocatable :: fatpct, propct, scs
  character plot(40),plt
  character, dimension(:), allocatable :: tests
  character, save :: capital*13='LUSBACOQPRVXD',small*13='lusbacoqprvxd'
  character(2) MTorST(4)
  character(7), save :: trt(-3:8)=(/'devMilk','dev.Fat','devProt','dev.SCS' &
      ,            '   Milk','    Fat','   Prot','    SCS' &
      ,            'ME.Milk','ME..Fat','ME.Prot','ME..SCS'/)
  integer :: GRAFplot(4), GRAFplotMIXED, GRAFplotSUM
  character(4), save :: GRAFname(4)=(/'Milk','Fat ','Prot','SCS '/)
  character(1), save :: SHORTtrait(4) = (/'M','F','P','S'/)
  character(2), save :: STorMTlabel(2) = (/'ST','MT'/)
  integer :: WRITEcurve, WRITEdata, CURVEsmall
  character(len=*) :: CURVEfile, DATAfile, LOGfile
  integer :: YIELDunits(4) = (/5,1,1,1/)
  integer :: YIELDsteps(4) = (/1,1,1,1/)
  character*80 :: LogMessage
  character*4 :: int2str
  integer :: overlaps, ntd, ndup, CURVEsingle
  external vary, covary
  equivalence (breed,breed6)
  if ( ONscreen == 1 .and. ncall == 0 ) then
    print *,'--------------------------------------------------------------------------------'
    print *,'BESTPRED version ', trim(BESTPREDversion)
    print *,'    Released: ', trim(BESTPREDdate)
    print *,'    Support : ', trim(BESTPREDname), ' (',trim(BESTPREDemail),')'
    print *,'    Website : ', trim(BESTPREDwebsite)
    print *,'--------------------------------------------------------------------------------'
  end if
  if ( DEBUGmsgs > 1 ) print *,'Allocating memory'
  if ( ncall == 0 ) then
    if ( LOGon == 1 .and. LOGfreq > 0 ) then
      LogMessage = 'Allocating matrices for means, SD, and covariances'
      call log_message(LOGfile,LogMessage)
    end if
    allocate(dyield(maxlen,4,last))
    allocate(meanyld(4,maxlen,last))
    allocate(sd(4,maxlen,last))
    allocate(covari(4,4,maxlen,maxlen,last))
    allocate(covd(4,4,maxlen,maxlen,last))
  end if
  if ( LOGon == 1 .and. LOGfreq > 0 .and. mod(ncall,LOGfreq) == 0 ) then
    LogMessage = 'Allocating arrays in call '//trim(char(ncall+ICHAR('0')))
    call log_message(LOGfile,LogMessage)
  end if
  allocate(curve(maxlen,maxy))
  allocate(curves(maxlen,maxy,4))
  allocate(curvem(maxlen,maxy))
  allocate(graph1(maxlen,1))
  allocate(grafall(2,maxlen,4))
  allocate(freq3X(maxlen))
  allocate(std(2,4,maxlen))
  allocate(stdslice(1,maxlen))
  allocate(tempTD(4,maxlen))
  allocate(tempSTD(2,4,maxlen))
  allocate(tempSTDslice(1,maxlen))
  allocate(fatpct(maxlen))
  allocate(propct(maxlen))
  allocate(scs(maxlen))
  allocate(tests(maxlen/plotfreq))
  allocate(dimvec(maxlen))
  std = -999.0
  tempTD = -999.0
  tempSTD = -999.0
  DAILYbp = -999.0
  DAILYherd = 0.0
  sum305 = 0.
  sum365 = 0.
  sum999 = 0.
  sumpart = 0.
  if ( DEBUGmsgs > 1 ) then
    print *, 'UNITSin: ', UNITSin
    print *, 'UNITSout: ', UNITSout
  end if
  if ( UNITSin == 'P' ) then
    if ( LOGon == 1 .and. LOGfreq > 0 .and. mod(ncall,LOGfreq) == 0 ) then
      LogMessage = 'Converting from lbs to kg for interal calculations'
      call log_message(LOGfile,LogMessage)
    end if
    yield(:,1,:) = yield(:,1,:) / lb
    herdavg(:,1:3) = herdavg(:,1:3) / lb
    if ( ncall == 0 ) then
      dyld(:,1:3,:) = dyld(:,1:3,:) / lb
      dsd(:,1:3,:) = dsd(:,1:3,:) / lb
      brdyld(:,1:3,:) = brdyld(:,1:3,:) / lb
      brdsd(:,1:3) = brdsd(:,1:3) / lb
    end if
  end if
  if ( dim0flag == 1 ) then
    dim0 = 0
    d0 = 0
  else
    d0 = 0
    do j=1,4
      d0(1,j) = dim0(j)
      d0(2,j) = dim0(4+j)
    end do
  end if
  GRAFplotMIXED = 0
  GRAFplotSUM = sum(GRAFplot)
  if ( GRAFplotSUM > 0 ) then
    do j = 1, 4
      if ( j == 1 .and. GRAFplot(j) == 1 ) GRAFplotMIXED = 1
      if ( j == 1 .and. GRAFplot(j) == 2 ) GRAFplotMIXED = 2
      if ( j > 1 .and. GRAFplot(j) == 1 .and. GRAFplotMIXED == 2 ) GRAFplotMIXED = 3
      if ( j > 1 .and. GRAFplot(j) == 2 .and. GRAFplotMIXED == 1 ) GRAFplotMIXED = 3
    end do
  end if
 if ( ncall == 0 .and. DEBUGmsgs > 0 ) then
     print *,'[bestpred]: Tipping points (1): ', dim0(1:4)
     print *,'[bestpred]: Tipping points (2): ', dim0(5:8)
 end if
 if ( DEBUGmsgs > 0 ) then
    print *,'======================================================'
    print *,'Inputs to bestpred for cow=',cowid,' fresh=',fresh
    print *,'       tstdays=',tstdays,' parity=',parity, &
                     ' length=',length, ' maxprnt=',maxprnt
    print *,' dim sup frq way sam mrd milk   fat  prot   scs'
    do j = 1,min(tstdays,maxtd)
      if ( UNITSin == 'P' ) then
        print 1,dim(j),super(j),Xmilk(j),weigh(j),sample(j),mrd(j),yield(1,1,j)*lb,yield(1,2:4,j)
      else
        print 1,dim(j),super(j),Xmilk(j),weigh(j),sample(j),mrd(j),yield(1,:,j)
      end if
 1        format(6i4,4f6.1)
      if(tstdays > maxtd) print *,tstdays,' tests but',maxtd,' used'
    enddo
    if ( UNITSin == 'P' ) then
      print 2,'Herd average mlk fat pro scs =', herdavg(:,1:3)*lb, herdavg(:,4)*100
    else
      print 2,'Herd average mlk fat pro scs =', herdavg
    end if
 2      format(a,8f10.2)
    print *,'======================================================'
end if
if (  DEBUGmsgs > 1 ) then
    print *,'======================================================'
    print *,"John's arguments"
    print *,"    GRAFplot     :", GRAFplot
    print *,"    DEBUGmsgs    :", DEBUGmsgs
    print *,"    ONscreen     :", ONscreen
    print *,"    laclen       :", laclen
    print *,"    dailyfreq    :", dailyfreq
    print *,"    INTmethod    :", INTmethod
    print *,"    maxlen       :", maxlen
    print *,"    WRITEcurve   :", WRITEcurve
    print *,"    CURVEfile    :", CURVEfile
    print *,"    WRITEdata    :", WRITEdata
    print *,"    DATAfile     :", DATAfile
    print *,"    plotfreq     :", plotfreq
    print *,"    INTmethodSCS :", INTmethodSCS
    print *,"    READparms    :", READparms
    print *,"    UNITSin      :", UNITSin
    print *,"    UNITSout     :", UNITSout
    print *,"    breedUNK     :", breedUNK
    print *,"    dim0         :", dim0
    print *,"    dim0         :", dim0
    print *,"    dim0flag     :", dim0flag
    print *,"    LOGon        :", LOGon
    print *,"    LOGfile      :", LOGfile
    print *,"    LOGfreq      :", LOGfreq
    print *,"    agefac       :", agefac
    print *,'======================================================'
  end if
  read(cowid,'(a1)') brd1
  brd = index(breed6,brd1)
  if ( brd == 0 ) then
    if ( DEBUGmsgs == 1 ) print *, "[WARNING]: Unknown breed ", brd1, &
      "provided; defaulting to ", breedUNK, " (", breed(breedUNK), ")."
    if ( LOGon == 1 .and. LOGfreq > 0 .and. mod(ncall,LOGfreq) == 0 ) then
      LogMessage = 'Unknown breed '//trim(char(brd+ICHAR('0')))//' provided; defaulting to ' &
        //trim(char(breedUNK+ICHAR('0')))//' ('//trim(breed(breedUNK))//')'
      call log_message(LOGfile,LogMessage)
    end if
    brd = breedUNK
  end if
  if ( ( INTmethod == 'L' .or. INTmethod == 'W' ) .and. ( INTmethodSCS == 'L' .or. INTmethodSCS == 'G' ) ) then
    herdstate = '00'
  end if
  if ( herdstate .eq. '00' ) then
    region = 2
    season = 1
  else
    call state_to_region(herdstate, region, DEBUGmsgs)
    call date_to_season(fresh, season, DEBUGmsgs)
  end if
  if ( ncall == 0 ) then
    oldbrd = brd
    oldregion = region
    oldseason = season
  end if
  if ( ncall > 0 .and. brd .eq. oldbrd .and. dim0flag == 0 .and. season .eq. oldseason .and. region .eq. oldregion ) go to 20
  if ( maxprnt > 0 .and. ncall == 0 ) then
    print *,'Lactation yields are MATURE (adjusted for age, etc.) and ACTUAL (unadjusted) values'
    if ( GRAFplotMIXED == 0 ) print *, 'No lactation curves are being plotted.'
    if ( GRAFplotMIXED == 1 ) print *, 'Plotted lactation curves are MATURE (adjusted for age, etc.)'
    if ( GRAFplotMIXED == 2 ) print *, 'Plotted lactation curves are ACTUAL (unadjusted for age, etc.)'
    if ( GRAFplotMIXED == 3 ) &
      print *, 'Plotted lactation curves are a mix of MATURE (adjusted for age, etc.) and ACTUAL (unadjusted) values'
    if ( mtrait > 1 ) then
      print *,'Multi-trait methods include data for',trt(1:mtrait)
    else
      print *,'!! Only single-trait methods were used !!'
    end if
    if ( use3X == 0 ) print *,'3X adjustments were not used'
    if ( use3X == 1 ) print *,'Old 3X factors were used'
    if ( use3X == 2 ) print *,'New 3X factors were used'
    if ( use3X == 3 ) print *,'New 3X factors phased in over time'
  end if
  if ( nbump > 0 ) then
    if ( maxprnt > 0 ) print *, &
      'DCR was reduced for bumpiest 1 /',10**nbump,'lacts'
    nbmp = nbump
    maxbump = 1.0
  else
    maxbump = 99.
    if ( maxprnt > 0 .and. ncall == 0 ) print *,'DCRs were not adjusted for bumpiness'
  end if
  do 19 lacn = last,1,-1
    do j = 1,4
      if ( j <= 3 ) then
        if ( DEBUGmsgs > 0 ) then
          print *,'[bestpred]: Calling interpolate for trait ', j, ' with method ', INTmethod, ' and breed ', brd &
            , ' and region ', region, ' and season ', season
        end if
        if ( LOGon == 1 .and. LOGfreq > 0 .and. mod(ncall,LOGfreq) == 0 ) then
          LogMessage = '[bestpred]: Calling interpolate(): trait '//trim(char(j+ICHAR('0')))//', method ' &
            //trim(INTmethod)//', breed '//trim(char(brd+ICHAR('0')))//', region '            &
            //trim(char(region+ICHAR('0')))//', and season '//trim(char(season+ICHAR('0')))
          call log_message(LOGfile,LogMessage)
        end if
        call interpolate(j, month, dyield, lacn, dyld, summ, meanyld, meanp &
              , sd, dsd, INTmethod, DEBUGmsgs, maxlen, brd, region, season)
      else
        if ( LOGon == 1 .and. LOGfreq > 0 .and. mod(ncall,LOGfreq) == 0 ) then
          LogMessage = '[bestpred]: Calling interpolate(): trait '//trim(char(j+ICHAR('0')))//', method ' &
            //trim(INTmethodSCS)//', breed '//trim(char(brd+ICHAR('0')))//', region '         &
            //trim(char(region+ICHAR('0')))//', and season '//trim(char(season+ICHAR('0')))
          call log_message(LOGfile,LogMessage)
        end if
        if ( DEBUGmsgs > 0 ) then
          print *,'[bestpred]: Calling interpolate for trait ', j, ' with method ', INTmethodSCS, ' and breed ', brd &
            , ' and region ', region, ' and season ', season
        end if
        call interpolate(j, month, dyield, lacn, dyld, summ, meanyld, meanp &
          , sd, dsd, INTmethodSCS, DEBUGmsgs, maxlen, brd, region, season)
      end if
    end do
    do j = 1,4
      stdvar(j,lacn) = 0.
      stdvar(j,lacn) = covary(j,0,0,305,maxlen,1,covari,covd,sd,lacn,last,precise,1)
    end do
  lplus = (precise - 1)/2.
  do 8 j=1,4
    do 7 k=1,4
      corr305(j,k) = 0.d0
      dV1(j,k) = 0.d0
      dVd(j,k) = 0.d0
      do 7 i=1,305
        do 7 l=1,305,precise
          vijkl = vary(i,j,1,2,2,1,l,k,1,2,2,1,sd,lacn,last,maxlen)
          vijkl = vijkl*min(precise,306-l)
          corr305(j,k) = corr305(j,k) + vijkl
          dV1(j,k) = dV1(j,k) + i*vijkl
 7            dVd(j,k) = dVd(j,k) + i*(l+lplus)*vijkl
          if ( dim0flag == 1 ) then
            dim0(4*(lacn-1)+j) = dV1(j,j) / stdvar(j,lacn)
            d0(lacn,j) = dim0(4*(lacn-1)+j)
          end if
 8 continue
  do 10 j=1,4
    meanp(lacn,j) = meanp(lacn,j) - d0(lacn,j)*meanyld(j,305,lacn)
    varp(lacn,j) = dVd(j,j) - 2*dV1(j,j)*d0(lacn,j) + stdvar(j,lacn)*d0(lacn,j)**2
    do 10 k=1,4
      do 10 l=1,maxlen
 10         covd(j,k,l,305,lacn) = (covd(j,k,l,305,lacn) - &
                 covari(j,k,l,305,lacn)*d0(lacn,j)) / dsqrt(varp(lacn,j))
  do 12 i=1,305
    tdhi = dyield(i,1,lacn) + tdregr
    tdlo = dyield(i,1,lacn) - tdregr
 12     qpq = qpq + (i - d0(lacn,1))**2
  if ( maxprnt > 0 .and. ncall == 0 ) then
    if ( lacn == last ) print *,'=======================' &
        ,'=============================================='
    if ( lacn == last ) print *,'Breed ',brd1,' Lact  Mean, ' &
        ,'St.Dev.   Max.DCR   Lactation Corr.        mid-DIM'
    if ( lacn < last ) plus = ' '
  end if
  do 18 j=1,4
    lacdif = meanyld(j,305,lacn) / meanyld(j,305,last)
    zero = 0.
    sddif = dsqrt(stdvar(j,lacn) / stdvar(j,last))
    if ( maxprnt > 0 .and. ncall == 0 ) then
      if ( UNITSout .eq. 'P' ) then
        print 17,  &
        trt(j+4),lacn,plus,brdyld(brd,j,2)*X100(j) &
          *lacdif*lb,brdsd(brd,j)*X100(j)*sddif*lb,100./rmonth(j,lacn) &
          ,(corr305(j,k)/dsqrt(stdvar(j,lacn)*stdvar(k,lacn)) &
          ,k=1,4),d0(lacn,j)
      else
        print 17,  &
          trt(j+4),lacn,plus,brdyld(brd,j,2)*X100(j) &
          *lacdif,brdsd(brd,j)*X100(j)*sddif,100./rmonth(j,lacn) &
          ,(corr305(j,k)/dsqrt(stdvar(j,lacn)*stdvar(k,lacn)) &
          ,k=1,4),d0(lacn,j)
      end if
    end if
 17   format(a7,i4,a1,2f8.0,f9.1,1x,4f6.2,f8.0)
 18   continue
 19   continue
  if ( dim0flag == 1 ) then
    print *,'New tipping points (1): ', dim0(1:4)
    print *,'New tipping points (2): ', dim0(5:8)
  end if
  if ( oldbrd .ne. brd ) then
    oldbrd = brd
  end if
  if ( oldregion .ne. region ) oldregion = region
  if ( oldseason .ne. season ) oldseason = season
  if ( maxprnt > 0 .and. ncall == 0 ) then
    print *,'===========================================' &
              ,'=========================='
  end if
  if ( GRAFplot(1) > 0 .or. GRAFplot(2) > 0 .or. GRAFplot(3) > 0 .or. GRAFplot(4) > 0 ) then
    print*,'Compute DCR, predict yield of cow ---, graph contemps ...'
    print*,'Supervised =S    ampm1of2=A  2of3=B  1of3=C' &
           ,' ver=V  LER=L dup=D'
    print*,'OwnerSample=O  OSampm1of2=P  2of3=Q  1of3=R' &
           ,'  ownerLER=U, bad=X'
    print*,'Capital letters if milk was sampled, small if milk-only'
  end if
20   ncall = ncall + 1
  lacn = min(parity,last)
  if(lacn <= 0) lacn = last
  read(cowid,'(a1)') brd1
  if ( length > maxlen ) length = maxlen
  if ( tstdays <= 0 ) then
    ntd = 1
    dim(1) = maxlen + 1
  else
    ntd = tstdays
  end if
  if ( ntd > maxtd ) ntd = maxtd
  dimvec = 0
  MRDi = 0
  do 23 i = 1,ntd
    if ( dim(i) <= maxlen ) then
      if ( dim(i) < 1 ) then
        print *,'Bad DIM',dim(i),' cow ',cowid,' test',i
      else
        if ( dim(i) - MRD(i) < 0 ) MRD(i) = dim(i)
        MRDi = MRD(i)
        do 22 k=1,MRDi
          if ( dimvec(dim(i)-k+1) > 0 ) then
            if ( MRDi == 1 ) then
              ndup = ndup + 1
              if ( ndup <= 10 ) print * &
                ,'Warning: cow ',cowid,' has duplicate tests ' &
                ,'lact',parity,' day',dim(i)
            else
              overlaps = overlaps + 1
              if ( overlaps <= 10 ) print * &
                ,'Warning: cow ',cowid,' LER segments overlap' &
                ,' lact',parity,' day',dim(i)-k+1
              MRD(i) = k - 1
              go to 23
            end if
          else
            dimvec(dim(i)-k+1) = i
          end if
 22       continue
        end if
      end if
 23   continue
  read(fresh,'(i4)') yearfr
  call adjust3X &
       (ntd,dim,length,yearfr,parity,Xmilk,meanyld, &
        test3X,fact3X,part3X,use3X,maxtd,last,maxlen)
  do j=1,4
    if ( herdavg(1,j) <= 0.d0 ) herdavg(1,j) = brdyld(brd,j,lacn)
    herd305(1,j) = herdavg(1,j)*X305(j)
    herd305(1,j) = herd305(1,j)*fact3X(j)
    if ( herd305(1,j) <= 0.d0 ) then
      if(yearfr <  2000) herd305(1,j) = brdyld(brd,j,1)*X305(j)
      if(yearfr >= 2000) herd305(1,j) = brdyld(brd,j,2)*X305(j)
    endif
    hratio(j) = herd305(1,j) / meanyld(j,305,lacn)
    do i = 1,maxlen
      std(1,j,i) = ymean(j,i,maxlen,1,dyield,lacn,last,hratio,herd305(1,j))
      if ( i <= 305 ) sum305(j) = sum305(j) + std(1,j,i)
      if ( i <= 365 ) sum365(j) = sum365(j) + std(1,j,i)
      if ( i <= laclen ) sum999(j) = sum999(j) + std(1,j,i)
      if ( i <= length ) sumpart(j) = sumpart(j) + std(1,j,i)
    end do
    if ( herdavg(1,j) <= 0.d0 ) then
      herd305(1,j) = herd305(1,j) * ( herd305(1,j) / sum305(j) )
    end if
    herd365(1,j) = herd305(1,j) * ( sum365(j) / sum305(j) )
    herd999(1,j) = herd305(1,j) * ( sum999(j) / sum305(j) )
    herdpart(1,j) = herd305(1,j) * ( sumpart(j) / sum305(j) )
    varfac(j) = 0.
    varfac(j) = brdsd(brd,j)*305./dsqrt(stdvar(j,lacn))
    maxyield(j) = ymean(j,60,maxlen,1,dyield,lacn,last &
      ,hratio,herd305) / (agefac(j)*part3X(j))
    minyield(j) = ymean(j,maxlen,maxlen,1,dyield,lacn,last &
       ,hratio,herd305) / (agefac(j)*part3X(j))
    if ( j == 2 .or. j == 3 ) then
        minyield(j) = minyield(j)*minyield(1)*10
        maxyield(j) = maxyield(j)*minyield(1)*10
    end if
    if ( j == 4 ) then
        minyield(j) = minyield(j)*10
        maxyield(j) = maxyield(j)*10
    end if
    if ( j == 4 ) then
        k = minyield(j)
        minyield(j) = maxyield(j)
        maxyield(j) = k
    end if
  end do
  maxyld = ymean(1,60,maxlen,1,dyield,lacn,last &
         ,hratio,herd305(1,:)) / (agefac(1)*part3X(1))
  minyld = ymean(1,maxlen,maxlen,1,dyield,lacn,last &
         ,hratio,herd305(1,:)) / (agefac(1)*part3X(1))
  size = 0
if ( mtrait > 1 ) then
  if ( DEBUGmsgs > 0 ) print *,'Doing MT prediction, mtrait= ', mtrait
  size = 0
  ntests = 0
  usetd = 0
  do 40 i=1,ntd
    if(dim(i) <   1) go to 40
    if(dim(i) > maxlen) go to 40
    if(dimvec(dim(i)) .ne. i) go to 40
    usetd = usetd + 1
    dimsort(usetd) = dim(i)
    if(sample(i) > Xmilk(i)) sample(i) = Xmilk(i)
    if(MRD(i) > 1) weigh(i) = Xmilk(i)
    if ( j == 1 ) dimsort(i) = dim(i)
    do 35 j=1,4
      if(weigh(i) < 1) yield(1,j,i) = zero
      if(sample(i) < 1) then
        yield(1,2,i) = zero
        yield(1,3,i) = zero
        if(yield(1,4,i) > zero) sample(i) = 1
      end if
      if(yield(1,j,i) <= zero) go to 35
      if ( j == 2 ) yield(1,j,i) = yield(1,1,i)*yield(1,j,i)/100.d0
      if ( j == 3 ) yield(1,j,i) = yield(1,1,i)*yield(1,j,i)/100.d0
      if(yield(1,1,i) > maxyld) maxyld = yield(1,1,i)
      if(yield(1,1,i) < minyld) minyld = yield(1,1,i)
      if ( j > mtrait ) go to 35
      if ( size == maxy ) then
        print *,' Too much data (size > maxy) for cow ', cowid
        go to 35
      end if
      size = size + 1
      if(dim(i) <= maxlen) then
        ntests(j) = ntests(j) + 1
        if ( j == 1 ) ntests(1) = ntests(1) + MRD(i) - 1
      end if
      q(1) = dim(i)
      q(2) = j
      q(3) = super(i)
      q(4) = Xmilk(i)
      if(j == 1) q(5) = weigh(i)
      if(j > 1) q(5) = sample(i)
      q(6) = MRD(i)
      qq(:,size) = q(:)
      dev(size,1) = yield(1,j,i)*test3X(j,i) - &
        ymean(j,dim(i),maxlen,MRD(i),dyield,lacn,last &
        ,hratio,herd305(1,:))/agefac(j)
      if ( yield(1,j,i) /= 0.0 ) tempTD(j,dim(i)) = yield(1,j,i)
      do  k=1,size
        kk = qq(2,k)
        var(size,k) = vary(q(1),q(2),q(3),q(4),q(5),q(6), &
          qq(1,k),qq(2,k),qq(3,k),qq(4,k),qq(5,k),qq(6,k), &
          sd,lacn,last,maxlen) &
            * varfac(j)*varfac(kk) / (agefac(j)*agefac(kk))
        var(k,size) = var(size,k)
      end do
      do m = 1,maxlen/dailyfreq
        do l = 1,4
          curves(m,size,l) = vary(q(1),q(2),q(3),q(4),q(5),q(6), &
             m*dailyfreq,l,1,2,2,1,sd,lacn,last,maxlen) &
             * varfac(j)*varfac(l) / (agefac(j)*agefac(l))
        end do
      end do
      do 34 k=1,4
        cov305(k,size) = covary(k,j,dim(i),305,maxlen,MRD(i),covari,covd, &
          sd,lacn,last,precise,1) * varfac(j)*varfac(k) / agefac(j)
        covp305(size,k) = cov305(k,size)
        cov365(k,size) = covary(k,j,dim(i),365,maxlen,MRD(i),covari,covd, &
          sd,lacn,last,precise,1) * varfac(j)*varfac(k) / agefac(j)
        covp365(size,k) = cov365(k,size)
        if ( laclen .ne. 305 .and. laclen .ne. 365 ) then
          cov999(k,size) = covary(k,j,dim(i),laclen,maxlen,MRD(i),covari, &
            covd,sd,lacn,last,precise,1) * varfac(j)*varfac(k) / agefac(j)
          covp999(size,k) = cov999(k,size)
        end if
        dcov(k,size) = covary(k,j,dim(i),305,maxlen,MRD(i),covari,covd, &
           sd,lacn,last,precise,2) * varfac(j)*varfac(k) / agefac(j)
        dcovp(size,k) = dcov(k,size)
        covsum(k,size) = 0.d0
        if ( part == 0 ) go to 34
        do 33 m = 1,length
 33       covsum(k,size) = covsum(k,size) + vary(q(1),q(2),q(3), &
            q(4),q(5),q(6),m,k,1,2,2,1,sd,lacn,last,maxlen) &
            * varfac(j)*varfac(k) / agefac(j)
 34     continue
        call binsort(dimsort,usetd,maxtd)
        do k=1,usetd
          dimmilk(k) = dimsort(k)
        end do
 35     continue
 40   continue
 if ( DEBUGmsgs > 1 ) then
     print *, '[bestpred]: Test day data for milk used to calculate deviations.'
     print *, ''
     print *, 'dev               yield    test3X   ymean    hratio   herd305    agefac'
     print *, '-----------------------------------------------------------------------'
     do i = 1, ntd
             j = 1
             print 1701, dev(i,1), yield(1,j,i), test3X(j,i), ymean(j,dim(i),maxlen,MRD(i),dyield,lacn,last,hratio,herd305(1,:)), &
                 hratio(j), herd305(1,j), agefac(j)
     end do
     1701    format(f48.2, 1x, f7.2, 1x, f7.2, 1x, f7.2, 1x, f7.2, 1x, f7.2, 1x, f7.2 )
 end if
  if ( size == 0 ) then
    do l=1,4
      vari(l,l) = 0.0
      dvari(l,l) = 0.0
      vari305(l,l) = 0.0
      vari365(l,l) = 0.0
      vari999(l,l) = 0.0
      partrec(l,1) = 0.0
      multi(l,1) = 0.0
      multi305(l,1) = 0.0
      multi365(l,1) = 0.0
      multi999(l,1) = 0.0
      persist(l,1) = 0.0
    end do
    graph1 = 0.0
  else
    call invrt2(var,maxy,size,cowid)
    call mult(covar305,cov305,var,4,size,size,4,maxy,maxy)
    call mult(vari305,covar305,covp305,4,size,4,4,maxy,4)
    call mult(multi305,covar305,dev,4,size,1,4,maxy,1)
    call mult(covar365,cov365,var,4,size,size,4,maxy,maxy)
    call mult(vari365,covar365,covp365,4,size,4,4,maxy,4)
    call mult(multi365,covar365,dev,4,size,1,4,maxy,1)
    if ( laclen == 305 ) then
        multi999 = multi305
    else if ( laclen == 365 ) then
        multi999 = multi365
    else
        call mult(covar999,cov999,var,4,size,size,4,maxy,maxy)
        call mult(vari999,covar999,covp999,4,size,4,4,maxy,4)
        call mult(multi999,covar999,dev,4,size,1,4,maxy,1)
    end if
    call mult(curvem,curves,var,maxlen/dailyfreq,size,size,maxlen,maxy,maxy)
    call mult(graph1,curvem,dev,maxlen/dailyfreq,size,1,maxlen,maxy,1)
    do j = 1,mtrait
      do l = 1,maxlen/dailyfreq
        do i = 1,size
          curve(l,i) = curves(l,i,j)
        end do
      end do
      call mult(curvem,curve,var,maxlen/dailyfreq,size,size,maxlen,maxy,maxy)
      call mult(graph1,curvem,dev,maxlen/dailyfreq,size,1,maxlen,maxy,1)
      grafall(1,:,j) = 0.
      do l = 1,maxlen,dailyfreq
        grafall(1,l,j) = graph1(l,1)
      end do
      DAILYbp(1,j,:) = -999.0
      do l = 1,maxlen,dailyfreq
        DAILYbp(1,j,l) = graph1(l,1)
      end do
    end do
    if ( part == 1 ) then
      call mult(partial,covsum,var,4,size,size,4,maxy,maxy)
      call mult(partrec,partial,dev,4,size,1,4,maxy,1)
    end if
    call mult(covar305,dcov,var,4,size,size,4,maxy,maxy)
    call mult(dvari,covar305,dcovp,4,size,4,4,maxy,4)
    call mult(persist,covar305,dev,4,size,1,4,maxy,1)
  end if
  do l=1,4
    YLDvec(1,l) = multi305(l,1) + herd305(1,l)
    YLDvec(1,4+l) = multi365(l,1) + herd365(1,l)
    YLDvec(1,8+l) = multi999(l,1) + herd999(1,l)
     if ( part == 0 .or. mtrait < l .or. length == 0 ) then
        YLDvec(1,12+l) = 0.d0
        go to 45
    end if
    YLDvec(1,12+l) = partrec(l,1) + meanyld(l,length,lacn) * hratio(l)
 45 RELyld(l) = vari305(l,l) / (stdvar(l,lacn)*varfac(l)**2)
    regrel(l) = RELyld(l)
    DCRvec(l) = 100.*RELyld(l) / rmonth(l,lacn)
    PERSvec(l) = persist(l,1)
    RELpers(l) = dvari(l,l) / varfac(l)**2
    xmulti(l) = 1.d0 / RELyld(l)
  end do
  n = 0
end if
if ( mtrait < 4 ) then
  if ( mtrait == 1 ) then
    st_start = 1
  else if ( mtrait == 3 ) then
    st_start = 4
  else
    print *, '[ERROR]: Unrecognized value of mtrait (1|3|4): ', mtrait
  end if
  do 55 j=st_start,4
    ntests(j) = 0
    size = 0
    do 50 i=1,ntd
      if(dim(i) <   1) go to 50
      if(dim(i) > maxlen) go to 50
      if(dimvec(dim(i)) .ne. i) go to 50
      if(sample(i) > Xmilk(i)) sample(i) = Xmilk(i)
      if(MRD(i) > 1) weigh(i) = Xmilk(i)
      if(weigh(i) < 1) yield(1,j,i) = zero
      if(sample(i) < 1) then
        yield(1,2,i) = zero
        yield(1,3,i) = zero
        if(yield(1,4,i) > zero) sample(i) = 1
      end if
      if(yield(1,j,i) <= zero) go to 50
      if(j == 2) yield(1,j,i) = yield(1,1,i)*yield(1,j,i)/100.d0
      if(j == 3) yield(1,j,i) = yield(1,1,i)*yield(1,j,i)/100.d0
      if(yield(1,1,i) > maxyld) maxyld = yield(1,1,i)
      if(yield(1,1,i) < minyld) minyld = yield(1,1,i)
      if ( size == maxy ) then
        print *,' Too much data (size > maxy) for cow ',cowid
        go to 50
      end if
      size = size + 1
      dimsort(size) = dim(i)
      if(dim(i) <= maxlen) then
        ntests(j) = ntests(j) + 1
        if(j == 1) ntests(1) = ntests(1) + MRD(i) - 1
      end if
      q(1) = dim(i)
      q(2) = j
      q(3) = super(i)
      q(4) = Xmilk(i)
      if(j == 1) q(5) = weigh(i)
      if(j > 1) q(5) = sample(i)
      q(6) = MRD(i)
      qq(:,size) = q(:)
      dev(size,1) = yield(1,j,i)*test3X(j,i) -  &
        ymean(j,dim(i),maxlen,MRD(i),dyield,lacn,last &
        ,hratio,herd305(1,:)) / agefac(j)
      do k=1,size
        kk = qq(2,k)
        var(size,k) = vary(q(1),q(2),q(3),q(4),q(5),q(6), &
          qq(1,k),qq(2,k),qq(3,k),qq(4,k),qq(5,k),qq(6,k), &
          sd,lacn,last,maxlen) * &
          varfac(j)*varfac(kk)/(agefac(j)*agefac(kk))
        var(k,size) = var(size,k)
      end do
      if ( yield(1,j,i) /= 0.0 ) tempTD(j,dim(i)) = yield(1,j,i)
      do m=1,maxlen/dailyfreq
        curve(m,size) = vary(q(1),q(2),q(3),q(4),q(5),q(6), &
          m*dailyfreq,j,1,2,2,1,sd,lacn,last,maxlen) &
          * varfac(j)**2 / agefac(j)**2
      end do
      cov305(1,size) = covary(j,j,dim(i),305,maxlen,MRD(i),covari,covd, &
        sd,lacn,last,precise,1) * varfac(j)**2 / agefac(j)
      covp305(size,1) = cov305(1,size)
      cov365(1,size) = covary(j,j,dim(i),365,maxlen,MRD(i),covari,covd, &
        sd,lacn,last,precise,1) * varfac(j)**2 / agefac(j)
      covp365(size,1) = cov365(1,size)
      if ( laclen .ne. 305 .and. laclen .ne. 365 ) then
        cov999(1,size) = covary(j,j,dim(i),laclen,maxlen,MRD(i),covari, &
          covd,sd,lacn,last,precise,1) * varfac(j)**2 / agefac(j)
        covp999(size,1) = cov999(1,size)
      end if
      dcov(1,size) = covary(j,j,dim(i),305,maxlen,MRD(i),covari,covd, &
        sd,lacn,last,precise,2) * varfac(j)**2 / agefac(j)
      dcovp(size,1) = dcov(1,size)
      Zdev(i) = dev(size,1)*305/(X305(j) * dsqrt(var(size,size)))
      covsum(1,size) = 0.d0
      if ( part == 0 ) go to 50
      do m = 1,length
        covsum(1,size) = covsum(1,size) + vary(q(1),q(2),q(3), &
          q(4),q(5),q(6),m,j,1,2,2,1,sd,lacn,last,maxlen) &
          * varfac(j)**2 / agefac(j)
      end do
    50 continue
    call binsort(dimsort,size,maxtd)
    bump(j) = 0.d0
    bumpsd(j) = 0.d0
    varb(j) = 0.d0
    if ( size > 1 ) then
      do k=2,size
        Zdif = Zdev(dimvec(dimsort(k))) - Zdev(dimvec(dimsort(k-1)))
        bigbump(j) = max(bump(j),dabs(Zdif))
        bumpsd(j) = bumpsd(j) + Zdif**2
        varb(j) = varb(j) + 2.*(1. - var(k,k-1) / &
          dsqrt(var(k,k)*var(k-1,k-1)))
      end do
      bump(j) = bumpsd(j)/varb(j)
      if ( bump(j) < 0 ) bump(j) = 0.
      if ( bump(j) > 99. ) bump(j) = 99.
    end if
    if ( j == 1 ) then
      usetd = size
      do k=1,size
        dimmilk(k) = dimsort(k)
      end do
      regrel(1) = RELyld(1)
    end if
    xsingl(j) = 0.d0
    lacwt(j) = 0.d0
    partrec(j,1) = 0.0
    if ( size == 0 ) then
      vari(1,1) = 0.0
      dvari(1,1) = 0.0
      vari305(1,1) = 0.0
      vari365(1,1) = 0.0
      vari999(1,1) = 0.0
      partrec(1,1) = 0.0
      single305(1,1) = 0.0
      single365(1,1) = 0.0
      single999(1,1) = 0.0
      pers(1,1) = 0.0
      graph1 = 0.0
    else
      call invrt2(var,maxy,size,cowid)
      call mult(covar305,cov305,var,1,size,size,4,maxy,maxy)
      call mult(vari305,covar305,covp305,1,size,1,4,maxy,4)
      call mult(single305,covar305,dev,1,size,1,4,maxy,1)
      call mult(covar365,cov365,var,1,size,size,4,maxy,maxy)
      call mult(vari365,covar365,covp365,1,size,1,4,maxy,4)
      call mult(single365,covar365,dev,1,size,1,4,maxy,1)
      if ( laclen == 305 ) then
        single999 = single305
      else if ( laclen == 365 ) then
        single999 = single365
      else
        call mult(covar999,cov999,var,1,size,size,4,maxy,maxy)
        call mult(vari999,covar999,covp999,1,size,1,4,maxy,4)
        call mult(single999,covar999,dev,1,size,1,4,maxy,1)
      end if
      call mult(curvem,curve,var,maxlen/dailyfreq,size,size,maxlen,maxy,maxy)
      call mult(graph1,curvem,dev,maxlen/dailyfreq,size,1,maxlen,maxy,1)
      grafall(1,:,j) = graph1(:,1)
       DAILYbp(1,j,:) = -999.0
       where(graph1(:,1) /= 0.0) DAILYbp(1,j,:) = graph1(:,1)
      if ( part == 1 ) then
        call mult(partial,covsum,var,1,size,size,4,maxy,maxy)
        call mult(partrec,partial,dev,1,size,1,4,maxy,1)
      end if
      call mult(qCVC1,covar305,dcovp,1,size,1,4,maxy,4)
      call mult(covar305,dcov,var,1,size,size,4,maxy,maxy)
      call mult(dvari,covar305,dcovp,1,size,1,4,maxy,4)
      call mult(pers,covar305,dev,1,size,1,4,maxy,1)
    end if
    YLDvec(1,j) = single305(1,1) + herd305(1,j)
    YLDvec(1,4+j) = single365(1,1) + herd365(1,j)
    YLDvec(1,8+j) = single999(1,1) + herd999(1,j)
    if ( part == 0 .or. length == 0 ) then
      YLDvec(1,12+j) = 0.d0
    else
      YLDvec(1,12+j) = partrec(1,1) + meanyld(j,length,lacn) * hratio(j)
    end if
    RELyld(j) = vari305(1,1)/(stdvar(j,lacn)*varfac(j)**2)
    regrel(j) = RELyld(j)
    if(nbump > 0) then
      Fvalue = Fval(nbmp,min(size,10))
      if(bump(j) > Fvalue) then
        RELyld(j) = RELyld(j) * Fvalue / bump(j)
        DCRvec(j) = DCRvec(j) * Fvalue / bump(j)
        if(bump(j)/Fvalue > maxbump) then
          print *,'------------------------------------------------'
          print *,trt(j),' Regular REL =',regrel(j)
          print *,trt(j),' Reduced REL =',RELyld(j)
          print *,'  Unusual data for cow ',cowid,'   F=',bump(j)
          if ( maxprnt > 0 ) maxprnt = max(maxprnt,ncall)
          maxbump = bump(j)/Fvalue
        end if
      end if
    end if
    DCRvec(j) = 100. * RELyld(j) / rmonth(j,lacn)
    xsingl(j) = (1.D0 / regrel(j) - 1.)*expamt + 1.
    lacwt(j) = (1. - repty(j)) / (xsingl(j) - repty(j))
    PERSvec(j) = pers(1,1)
    RELpers(j) = dvari(1,1) / varfac(j)**2
    Rvar(1,1) = (1./regrel(j) - repty(j)) &
      *stdvar(j,lacn)*varfac(j)**2
    Rvar(2,2) = (1./RELpers(j) - reptp(j))*varfac(j)**2
    Rcorr = qCVC1(1,1) / dsqrt(vari(1,1)*dvari(1,1))
    Rvar(2,1) = Rcorr * dsqrt(Rvar(1,1)*Rvar(2,2))
    if(Rcorr> .99) Rvar(2,1)= .99*dsqrt(Rvar(1,1)*Rvar(2,2))
    if(Rcorr<-.99) Rvar(2,1)=-.99*dsqrt(Rvar(1,1)*Rvar(2,2))
    if(j == 1) Rvecm = 1.d0 / Rvar(1,1)
    if(j == 2) Rvecf = 1.d0 / Rvar(1,1)
    if(j == 3) Rvecp = 1.d0 / Rvar(1,1)
    if(j == 4) Rvecs = 1.d0 / Rvar(1,1)
    Rvar(1,2) = Rvar(2,1)
    call invrt2(Rvar,2,2,cowid)
    n = 0
    do l = 1,2
      do m = l,2
        n = n + 1
        Rvec(n) = Rvar(l,m)
      end do
    end do
55  continue
else
  print *, '[ERROR]: Unrecognized value of mtrait (1|3|4): ', mtrait
end if
  if ( DEBUGmsgs > 1 ) then
    print *,'Correlation among 305-d yields for ', lacn, ' parity'
    print *, corr305
  end if
    do j=1,4
      YLDvec(2,j) = YLDvec(1,j) / (agefac(j)*fact3X(j))
      YLDvec(2,j+4) = YLDvec(1,j+4) / (agefac(j)*fact3X(j))
      YLDvec(2,j+8) = YLDvec(1,j+8) / (agefac(j)*fact3X(j))
      herd305(2,j) = herd305(1,j) / (agefac(j)*fact3X(j))
      herd365(2,j) = herd365(1,j) / (agefac(j)*fact3X(j))
      herd999(2,j) = herd999(1,j) / (agefac(j)*fact3X(j))
      YLDvec(2,j+12) = YLDvec(1,j+12) / (agefac(j)*part3X(j))
    end do
  if ( UNITSout .eq. 'P' ) then
    do j=1,3
      YLDvec(:,j) = YLDvec(:,j) * lb
      YLDvec(:,j+4) = YLDvec(:,j+4) * lb
      YLDvec(:,j+8) = YLDvec(:,j+8) * lb
      YLDvec(:,j+12) = YLDvec(:,j+12) * lb
      herd305(:,j) = herd305(:,j) * lb
      herd365(:,j) = herd365(:,j) * lb
      herd999(:,j) = herd999(:,j) * lb
    end do
  end if
  YLDvec(:,4) = YLDvec(:,4)/305.
  YLDvec(:,8) = YLDvec(:,8)/365.
  YLDvec(:,12) = YLDvec(:,12)/laclen
  if ( length > 0 ) then
    YLDvec(:,16) = YLDvec(:,16)/length
  else
    YLDvec(:,16) = 0.
  end if
  herd305(:,4) = herd305(:,4) / 305.
  herd365(:,4) = herd365(:,4) / 365.
  herd999(:,4) = herd999(:,4) / laclen
  if ( mtrait == 4 ) then
    MTorST = 'MT'
  else if ( mtrait == 1 ) then
    MTorST = 'ST'
  else if ( mtrait == 3 ) then
    MTorST(1:3) = 'MT'
    MTorST(4) = 'ST'
  else
    print *, '[ERROR]: Unrecognized value of mtrait (1|3|4): ', mtrait
  end if
  DCRm = DCRvec(1)
  DCRc = (DCRvec(2) + DCRvec(3))/2.
  DCRs = DCRvec(4)
  if ( ntests(3) == 0 ) DCRc = DCRvec(2)
  do j = 1,4
    if ( mtrait > 1 ) then
      do l = 1,mtrait
        Yvec(1,j) = herd305(1,j) + (YLDvec(1,j) - herd305(1,j)) / regrel(j)
        Yvec(2,j) = herd305(2,j) + (YLDvec(2,j) - herd305(2,j)) / regrel(j)
      end do
      if ( mtrait == 3 .and. j == 4 ) then
        Yvec(1,j) = PERSvec(j) / RELpers(j)
        Yvec(2,j) = Yvec(1,j)
      end if
    else
      if ( RELpers(j) == 0 ) then
        Yvec(1,j) = PERSvec(j)
        Yvec(2,j) = Yvec(1,j)
      else
        Yvec(1,j) = PERSvec(j) / RELpers(j)
        Yvec(2,j) = Yvec(1,j)
      end if
    end if
  end do
  Yvec(1,4) = Yvec(1,4) / 305.
  Yvec(2,4) = Yvec(2,4) / 305.
  write(BLUPout(5),60) (Rvec(n),n=1,3)
  write(BLUPout(6),60) (Rvec(n),n=4,6)
  write(BLUPout(7),60) (Rvec(n),n=7,9)
  write(BLUPout(8),60) Rvecm,zero0,zero0
  write(BLUPout(9),60) Rvecf,zero0,zero0
  write(BLUPout(10),60) Rvecp,zero0,zero0
 60   format(3a8)
  do k = 5,10
    BLUPn(k) = 24
  end do
  do j = 1, 4
    tempSTD(1,j,:) = 0.
    tempSTD(1,j,:) = grafall(1,:,j) + std(1,j,:)
  end do
  do j = 1,4
    where(std(1,j,:) > -999.0) std(2,j,:) = std(1,j,:) / (agefac(j)*fact3X(j))
    where(grafall(1,:,j) > -999.0) grafall(2,:,j) = grafall(1,:,j) / (agefac(j)*fact3X(j))
    where(tempSTD(1,j,:) > -999.0) tempSTD(2,j,:) = tempSTD(1,j,:) / (agefac(j)*fact3X(j))
    where(DAILYbp(1,j,:) > -999.0) DAILYbp(2,j,:) = DAILYbp(1,j,:) / (agefac(j)*fact3X(j))
    where(yield(1,j,:) > -999.0) yield(2,j,:) = yield(1,j,:) / (agefac(j)*fact3X(j))
  end do
  if ( UNITSout .eq. 'P' ) then
    where(std(:,1:3,:) > -999.0) std(:,1:3,:) = std(:,1:3,:) * lb
    where(grafall(:,:,1:3) > -999.0) grafall(:,:,1:3) = grafall(:,:,1:3) * lb
    where(tempSTD(:,1:3,:) > -999.0) tempSTD(:,1:3,:) = tempSTD(:,1:3,:) * lb
    where(DAILYbp(:,1:3,:) > -999.0) DAILYbp(:,1:3,:) = DAILYbp(:,1:3,:) * lb
    where(yield(:,1:3,:) > -999.0) yield(:,1:3,:) = yield(:,1:3,:) * lb
    where(tempTD(1:3,:) > -999.0) tempTD(1:3,:) = tempTD(1:3,:) * lb
    herdavg(:,1:3) = herdavg(:,1:3) * lb
  end if
  if ( WRITEcurve == 1 ) then
    call write_curve_data(mtrait, CURVEfile, SHORTtrait, cowid, STorMTlabel, &
    maxlen, DAILYbp, tempTD, std, parity, GRAFplot, CURVEsmall, CURVEsingle)
  end if
  if ( WRITEdata == 1 ) then
    call write_yield_data(DATAfile,cowid,STorMTlabel,trt, ntests,  &
    X100,YLDvec,herd305,DCRvec,MTorST,PERSvec,RELpers,parity, CURVEsingle)
  end if
    mstart = 1
    if(usetd > 0) then
      do i=1,usetd
        mstop = dimmilk(i)
        milk3X = test3X(1,dimvec(dimmilk(i)))
        if ( plotfreq > 0 ) then
          do m=mstart,mstop,plotfreq
            freq3X((m+5)/plotfreq) = milk3X
          end do
        end if
        mstart = mstop + 1
      end do
    end if
    do 68 i=1,ntd
      if(dim(i) <   1) go to 68
      if(dim(i) > maxlen) go to 68
      if(MRD(i) > 1) then
        plot(i) = 'L'
        if(super(i) == 2) plot(i) = 'U'
        if(super(i) == 6) plot(i) = 'U'
      else
        plot(i) = 'S'
        if(weigh(i) < Xmilk(i)) plot(i) = 'B'
        if(weigh(i)*2 == Xmilk(i)) plot(i) = 'A'
        if(weigh(i)*2 < Xmilk(i)) plot(i) = 'C'
        if(super(i) == 2 .or. super(i) == 6) then
          plot(i) = 'O'
          if(weigh(i) < Xmilk(i)) plot(i) = 'Q'
          if(weigh(i)*2 == Xmilk(i)) plot(i) = 'P'
          if(weigh(i)*2 < Xmilk(i)) plot(i) = 'R'
        end if
      end if
      if(super(i) > 7) plot(i) = 'V'
      if(super(i) == 0) plot(i) = 'X'
      if(super(i) == 4) plot(i) = 'X'
      if(dimvec(dim(i)) .ne. i) plot(i) = 'D'
      if(yield(1,2,i) <= zero) then
        k = index(capital,plot(i))
        plot(i) = small(k:k)
      end if
 68     continue
  do i=1,maxlen/24
    fatpct(i) = '    '
    propct(i) = '    '
    scs(i)    = '    '
  end do
  if ( ncall <= maxprnt ) then
    do l = 1,4
      if ( GRAFplot(l) > 0 .and. maxshow > 0 .and. ncall <= maxshow ) then
        if ( GRAFplot(l) > 2 ) then
          if ( LOGon == 1 .and. LOGfreq > 0 ) then
            write(int2str, FMT='(I5)') l
            LogMessage = 'GRAFplot ('//int2str//') has an invalid value of '
            write(int2str, FMT='(I5)') GRAFplot(l)
            LogMessage = LogMessage//int2str//'.'//'Using the default of 2.'
            call log_message(LOGfile,LogMessage)
          end if
          GRAFplot(l) = 2
        end if
        stdslice = 0.
        stdslice(1,:) = std(GRAFplot(l),l,:)
        tempSTDslice = 0.
        tempSTDslice(1,:) = tempSTD(GRAFplot(l),l,:)
        if ( l > 1 ) then
          stdslice = stdslice * 10
          tempSTDslice = tempSTDslice * 10
        end if
        if ( l == 4 ) then
          stdslice(1,1:5) = 0.
          tempSTDslice(1,1:5) = 0.
        end if
        minyield(l) = 0
        maxyield(l) = max(maxval(stdslice,mask=stdslice>0),maxval(tempSTDslice,mask=tempSTDslice>0))
        do i=1,ntd
          if ( l == 1 ) then
            if (yield(GRAFplot(l),l,i) > maxyield(l)) maxyield(l) = yield(GRAFplot(l),l,i)
            if ( maxyield(l) > 150 ) maxyield(l) = 150
          else
            if ( yield(GRAFplot(l),l,i)*10 > maxyield(l) ) maxyield(l) = yield(GRAFplot(l),l,i)*10
          end if
        end do
        print *,'-------------------------------------------------------' &
              ,'---------'
        if ( GRAFplot(l) == 1 ) then
          print 70,GRAFname(l),'(ME)',cowid,fresh(1:4),fresh(5:6),fresh(7:8)
        else
          print 70,GRAFname(l),'(Actual)',cowid,fresh(1:4),fresh(5:6),fresh(7:8)
        end if
70       format(a4,' lactation curve ',a,' for cow ',a17,',  fresh ',a4,2(1x,a2))
        do 73 j = maxyield(l)/YIELDunits(l), minyield(l)/YIELDunits(l), -YIELDsteps(l)
          do 71 i = 1,maxlen/plotfreq
            plt = ' '
            if ( i == maxlen/plotfreq ) plt = ':'
            if ( i == 305/plotfreq ) plt = '.'
            if ( i == length/plotfreq ) plt = '|'
            if ( l > 1 ) then
              testday = std(GRAFplot(l),l,i*plotfreq)*10.0
            else
              testday = std(GRAFplot(l),l,i*plotfreq)
            end if
            if ( testday/YIELDunits(l) == j ) plt = '.'
            if ( l > 1 ) then
              testday = tempSTD(GRAFplot(l),l,i*plotfreq)*10.0
            else
              testday = tempSTD(GRAFplot(l),l,i*plotfreq)
            end if
            if ( testday / YIELDunits(l) == j ) plt = '-'
 71         tests(i) = plt
          do 72 i = 1,ntd
            if (dim(i) <   1) go to 72
            if (yield(GRAFplot(l),2,i) > zero) then
              write(percent,'(f4.1)') yield(GRAFplot(l),2,i)*100 / yield(GRAFplot(l),1,i)
              fatpct(dim(i)/24 + 1) = percent
              write(percent,'(f4.1)') yield(GRAFplot(l),3,i)*100 / yield(GRAFplot(l),1,i)
              propct(dim(i)/24 + 1) = percent
            end if
            if (yield(GRAFplot(l),4,i) > zero) write(percent,'(f4.1)') yield(GRAFplot(l),4,i)
            if (yield(GRAFplot(l),4,i) > zero) scs(dim(i)/24 + 1) = percent
            if ( l > 1 ) then
              testday = yield(GRAFplot(l),l,i)*10
            else
              testday = yield(GRAFplot(l),l,i)
            end if
            middle(i) = dim(i) - (MRD(i)-1)/2
            if ( testday/YIELDunits(l) == j ) then
              if ( middle(i) / plotfreq <= 0 ) then
                tests(middle(i)) = plot(i)
              else if ( middle(i) / plotfreq <= 60 ) then
                tests(middle(i)/plotfreq) = plot(i)
              else
                continue
              end if
            end if
 72       continue
        if ( l == 1 ) then
          print 741,j*YIELDunits(l),tests
 741      format(i4,1x,999a1)
        else
          print 742,float(j*YIELDunits(l))/10.,tests
 742      format(f3.1,1x,999a1)
        end if
 73     continue
        print '(99i5)',(i*30,i=0,maxlen/30)
 75     continue
        print 78,'Fat%',(fatpct(i),i=1,maxlen/24)
        print 78,'Pro%',(propct(i),i=1,maxlen/24)
        print 78,'SCS ',(scs(i)   ,i=1,maxlen/24)
 78     format(1x,a4,99a4)
      end if
    end do
    print *, ""
    print 790, cowid, fresh(1:4), fresh(5:6), fresh(7:8), parity
 790  format(' Lactation summary for cow ',a17,',  fresh ', a4, 2(1x,a2), ', parity ', i2 )
    print 791,length, laclen
 791  format(/,52x,                 'Cntmp     Adjust       Persistency     Data' &
        ,/,16x,i3,        '-d    305-d    365-d    ',i3,'-d    305-d     Factors    Esti-   Relia-    Coll' &
        ,'ectn',/                                                                           &
        ,' TRAIT Tests    Yield    Yield    Yield    Yield     Mean   Age,     3X  mate    bility    Rating')
    do l=1,4
      print 851,trt(l+4),ntests(l), &
        YLDvec(1,l+12)*X100(l),YLDvec(1,l)*X100(l), &
        YLDvec(1,l+4)*X100(l),YLDvec(1,l+8)*X100(l), &
        herd305(1,l)*X100(l),agefac(l),fact3X(l), &
        PERSvec(l),RELpers(l)*100.,DCRvec(l),MTorST(l)
    end do
    do l=1,4
      print 851,trt(l),ntests(l), &
        YLDvec(2,l+12)*X100(l),YLDvec(2,l)*X100(l), &
        YLDvec(2,l+4)*X100(l),YLDvec(2,l+8)*X100(l), &
        herd305(2,l)*X100(l),agefac(l),fact3X(l), &
        PERSvec(l),RELpers(l)*100.,DCRvec(l),MTorST(l)
    end do
 851 format(1x,a7,i4,5f9.0,2f7.2,f7.2,f7.0,'% ',f7.0,1x,a2)
    print *,' '
    print *,' '
  end if
  if ( ncall <= maxshow .and. ncall > maxprnt .and. maxshow > 0 ) then
    print *, ""
    print 792, cowid, fresh(1:4), fresh(5:6), fresh(7:8), parity
 792  format(' Lactation summary for cow ',a17,',  fresh ', a4, 2(1x,a2), ', parity ', i2 )
    print 793,length, laclen
 793  format(/,52x,                 'Cntmp     Adjust       Persistency     Data' &
        ,/,16x,i3,        '-d    305-d    365-d    ',i3,'-d    305-d     Factors    Esti-   Relia-    Coll' &
        ,'ectn',/                                                                           &
        ,' Trait Tests    Yield    Yield    Yield    Yield     Mean   Age,     3X  mate    bility    Rating')
    do j=1,4
      print 852,trt(j+4),ntests(j), &
        YLDvec(1,j+12)*X100(j),YLDvec(1,j)*X100(j), &
        YLDvec(1,j+4)*X100(j),YLDvec(1,j+8)*X100(j), &
        herd305(1,j)*X100(j),agefac(j),fact3X(j), &
        PERSvec(j),RELpers(j)*100.,DCRvec(j),MTorST(j)
    end do
    do j=1,4
      print 852,trt(j),ntests(j), &
        YLDvec(2,j+12)*X100(j),YLDvec(2,j)*X100(j), &
        YLDvec(2,j+4)*X100(j),YLDvec(2,j+8)*X100(j), &
        herd305(2,j)*X100(j),agefac(j),fact3X(j), &
        PERSvec(j),RELpers(j)*100.,DCRvec(j),MTorST(j)
    end do
 852 format(1x,a7,i4,5f9.0,2f7.2,f7.2,f7.0,'% ',f7.0,1x,a2)
    print *,' '
    print *,' '
  end if
 90    do 91 i=1,maxlen
 91      dimvec(i) = 0
    do 95 i=1,maxlen/plotfreq
      i6 = i*dailyfreq
      do 92 j=1,ntd
        if(dim(j) <   1) go to 92
        if(dim(j) > maxlen) go to 92
 92        continue
 95      continue
  if ( maxprnt < 0 ) then
    print *,'======================================================'
    print *,'Outputs from bestpred for cow=',cowid,' fresh=',fresh
    print *,'       tstdays=',tstdays,' parity=',parity, &
                     ' length=',length, ' maxprnt=',maxprnt
    print *, '305d ME yields    : ', YLDvec(1,1:4)
    print *, '305d Actual yields: ', YLDvec(2,1:4)
    print *, 'DCR               : ', DCRvec
    print *, 'Persistencies     : ', PERSvec
    print *, 'Persist REL       : ', RELpers
    print *,'======================================================'
  end if
  if ( DEBUGmsgs > 1 ) print *,'Deallocating memory'
  deallocate(curve)
  deallocate(curvem)
  deallocate(curves)
  deallocate(graph1)
  deallocate(grafall)
  deallocate(freq3X)
  deallocate(std)
  deallocate(stdslice)
  deallocate(tempTD)
  deallocate(tempSTD)
  deallocate(tempSTDslice)
  deallocate(fatpct)
  deallocate(propct)
  deallocate(scs)
  deallocate(tests)
  deallocate(dimvec)
  return
end subroutine bestpred
