C     Last change:  CG    18 December 2005    08:01 am
C******** ********* ********* ********* ********* ********* ********* **
C
C                        SMARTS MODEL, Version 2.9.5
C
C***********************************************************************
C
C CALCULATES CLEAR SKY SPECTRAL SOLAR IRRADIANCES FROM 280 TO 4000 nm.
C
C  Research code written by C. GUEYMARD, Solar Consulting Services
c
C      Version 2.0: November 1995
c      (...)
c      Version 2.8: November 1996
c      Version 2.9: February 2002
c      Version 2.9.1: May 2002
c      Version 2.9.2: March 2003
c      Version 2.9.3: July 2004 (remained beta)
c      Version 2.9.4: November 2004 (remained beta)
c      Version 2.9.5: December 2005
c			Errors in ALBDAT subroutine corrected 15 May 2006
C
C***
C***    Consult the 'HISTORY.TXT' file and the 'New Features'
C***    Section of the User's Manual for details about the
C***      changes that occurred in the successive versions.
C***    Some input cards may have changed content!
C***
C***    Consult the User's Manual for details and explanations
C***      about the INPUT data cards and OUTPUT files!
C
C
c      Program SMARTS_295
c
      Double Precision TO3,TAUZ3,DIR,DIF0,DIF,GLOB,GLOBS,DIRH,FHTO,rocb
      Double Precision DIRS,DIFS,DIREXP,DIFEXP,DGRND,HT,DRAY,TH2O,TH2OP
      Double Precision TABS,TDIR,DAER,PFGS,PFD,PFB,GAMOZ,WPHT,GRNS
      Double Precision PPFG,PPFB,PPFD,PPFGS,PhoteV,PFGeV,PFBeV,PFDeV
      Double Precision PFGSeV,Avogad,evolt,sumd0,fht1,upward,TTP5
      Double Precision DIRWL,SUMBN,SUMBX,SUMD,SUMDX,SUMBS,Difccs,Glob0
      Double Precision BNORM(2002),GLOBH(2002),GLOBT(2002),DIRX(2002)
      Double Precision Tcoro3,tz3,AO4,TO4,TO4P,TxO3,TABS0,TABS0p,TAAP
      Double Precision Julian,dec,Longit,Latit,phi2,xlim,HTa
      Double Precision TO2,TO2P,TCO2,TCO2P,NLosch,Bw,Bp,Prod,AbO4
      Double Precision Tmixd,TmixdP,Trace,TraceP,Phot,Rhor,Rhos,Roro,AO3
      Double Precision AmO2,AmCO2,tauo2,tauco2,taa,tas,tat
      Double Precision AmH2O,wAmw,wAmp,tauw,Bmw,Bmwp,ww02

      REAL RB0(4),RB1(4),RB2(4),RB3(4),RDHB(4),limit
      REAL VL(515),ETSPCT(2002),Output(54),Xout(54),time(2)
      Real Wvla1(3000),Albdo1(3000),Wvla2(3000),Albdo2(3000)
      REAL C1(4),C2(4),C3(4),C4(4),C5(4),C6(4),D1(4),D2(4),D3(4),D4(4)
      REAL BP00(4),BP01(4),D5(4),D6(4)
      REAL BP02(4),BP10(4),BP11(4),BP12(4),BP20(4),BP21(4),BP22(4)
      REAL BP30(4),BP31(4),BP32(4),BQ00(4),BQ01(4),BQ02(4)
      REAL BQ10(4),BQ11(4),BQ12(4),BQ20(4),BQ21(4),BQ22(4)
      REAL BQ0(7),BQ1(7),BQ2(7),AG41(4),AG42(4),WV(2002)
      REAL BP0(7),BP1(7),BP2(7),BP3(7),AG0(7),AG1(7),AG2(7),AG3(7)
      REAL AG4(7),AG00(4),AG01(4),AG02(4),AG10(4),AG11(4),AG12(4)
      REAL AG20(4),AG21(4),AG22(4),AG30(4),AG31(4),AG32(4),AG40(4) 
      Real Bmx(2),Bmwx(2),intvl,KW
      REAL DECLI(12),RSUN(12)
      
      INTEGER IOUT(54),CIEYr,year,day,DayoYr,DayUT
      
      LOGICAL batch
c
      CHARACTER*2000 Path
      CHARACTER*100 FileIn,FileOut,FileExt,FileScn, Usernm
      CHARACTER*64 AEROS, Spctrm, Comnt
      Character*48 dummy, smart
      Character*24 Filen1, Filen2, Lambr1, Lambr2
      CHARACTER*24 Load
      Character*24 Out(54), Seasn2
      Character*12 Filter
      CHARACTER*6 SEASON, Area
      CHARACTER*4 Atmos, YesNo

      COMMON /SOLAR1/ WV,WLMN,wlmx,WV1,WV2
      COMMON /SOLAR2/ BNORM,GLOBH,GLOBT,DIRX,ETSPCT
      Common /Solar3/ Dir,Aeros,Tauas,Taurl,Rhox,rpd,pinb,pix4,AmR,WVL,
     2   wvln,nx,Znr,Zxr,va,vb,RH
      Common /Number/ NLosch
     
c      DATA O3REF/.34379,.33195,.37707,.34512,.37592,.27761,.3,.28,.33,
c      1 .38,.34379/
c      DATA UNREF/2.0443E-4,2.1841E-4,1.9867E-4,2.1569E-4,1.8678E-4,
c      1 2.1119E-4,2E-4,1E-4,2E-4,1E-4,2.0443E-4/
      DATA A0/-0.897/,A1/4.448/,A2/-2.77/,A3/0.312/,RD0/0.408/
      DATA RB0/-3.364,3.96,-1.909,0./
      DATA RB1/-12.962,34.601,-48.784,27.511/
      DATA RB2/9.164,-18.876,23.776,-13.014/
      DATA RB3/-.217,-.805,.318,0./
      DATA RDHB/-.323,.384,-.17,0./
      DATA C1/.4998,.27999,.049331,.57973/
      DATA C2/45.236,55.642,7.9767,65.559/
      DATA C3/96.233,1382.3,17.726,206.15/
      DATA C4/-13.067,-132.47,-14.555,-26.911/
      DATA C5/55.506,108.73,41.369,78.478/
      DATA C6/83.115,1500.9,-18.384,166.38/
      DATA D1/.86887,.69983,.039871,1.1194/
      DATA D2/43.547,39.689,12.397,76.251/
      DATA D3/-29.719,-26.736,98.641,129.32/
      DATA D4/-1.8192,4.0596,-60.939,-17.537/
      DATA D5/33.783,31.674,128.0,55.211/
      DATA D6/-24.849,-16.936,-34.736,66.192/
      DATA BP0/0.,0.,0.,0.,.84372,.64886,.96635/
      DATA BP1/0.,0.,0.,0.,.30206,.13465,.073464/
      DATA BP2/0.,0.,0.,0.,-.47838,-.30166,-.071847/
      DATA BP3/0.,0.,0.,0.,.15647,.083393,.019774/
      DATA BQ0/0.,0.,0.,0.,1.2853,2.9784,2.0006/
      DATA BQ1/0.,0.,0.,0.,1.486,.61494,7.111/
      DATA BQ2/0.,0.,0.,0.,2.8357,3.3122,3.0136/
      DATA AG0/0.,0.,0.,0.,.75141,.66851,.77876/
      DATA AG1/0.,0.,0.,0.,-.35648,-.20657,-.13625/
      DATA AG2/0.,0.,0.,0.,.29982,.1468,.16092/
      DATA AG3/0.,0.,0.,0.,-.081346,-.040565,-.056749/
      DATA AG4/0.,0.,0.,0.,7.3038E-3,3.8811E-3,6.1178E-3/
      DATA BP00/1.0151,.84946,.94016,.99926/
      DATA BP01/-6.0574E-3,-9.7903E-3,-3.5957E-4,-5.0201E-3/
      DATA BP02/5.5945E-5,1.0266E-4,9.8774E-6,4.8169E-5/
      DATA BP10/-1.2901E-1,-2.0852E-1,1.2843E-1,-5.5311E-2/
      DATA BP11/2.1565E-2,1.2935E-2,1.2117E-3,1.8072E-2/
      DATA BP12/-1.95E-4,-9.4275E-5,-2.7557E-5,-1.693E-4/
      DATA BP20/2.0622E-1,3.9371E-1,-1.4612E-1,9.0412E-2/
      DATA BP21/-3.1109E-2,-2.3536E-2,-8.5631E-4,-2.3949E-2/
      DATA BP22/2.8096E-4,1.8413E-4,2.7298E-5,2.2335E-4/
      DATA BP30/-8.1528E-2,-1.3342E-1,3.9982E-2,-3.9868E-2/
      DATA BP31/1.0582E-2,7.301E-3,3.7258E-4,7.5484E-3/
      DATA BP32/-9.5007E-5,-5.7236E-5,-9.5415E-6,-6.9475E-5/
      DATA BQ00/-3.0306,7.5308,-3.7748,-4.4981/
      DATA BQ01/.12324,-.15526,.13631,.17798/
      DATA BQ02/-6.408E-4,1.0762E-3,-7.6824E-4,-9.9386E-4/
      DATA BQ10/1.0949,-.88621,1.5129,-5.0756/
      DATA BQ11/5.4308E-3,-7.2508E-2,1.5867E-2,.13536/
      DATA BQ12/1.7654E-5,9.8766E-4,-1.2999E-4,-6.7061E-4/
      DATA BQ20/2.5572,2.2092,2.8725,6.6072/
      DATA BQ21/7.2117E-3,2.9849E-2,2.6098E-3,-8.1503E-2/
      DATA BQ22/-2.5712E-5,-2.2029E-4,-9.2133E-6,4.5423E-4/
      DATA AG00/.75831,.65473,.77681,.77544/
      DATA AG01/9.5376E-4,6.0975E-3,-2.7558E-3,-3.1632E-3/
      DATA AG02/-2.3126E-6,-4.3907E-5,3.635E-5,3.577E-5/
      DATA AG10/6.5007E-2,1.0582E-2,-3.07E-1,-2.3927E-3/
      DATA AG11/-1.9238E-2,-2.0473E-2,5.5554E-3,-3.8837E-3/
      DATA AG12/1.6785E-4,1.9499E-4,-4.014E-5,2.8519E-5/
      DATA AG20/-2.5092E-2,7.2283E-2,1.1744E-1,-9.6464E-3/
      DATA AG21/1.5397E-2,1.3209E-2,3.7471E-4,5.8684E-4/
      DATA AG22/-1.3813E-4,-1.3393E-4,-1.5242E-6,-4.3942E-6/
      DATA AG30/-4.7607E-4,-3.3056E-2,-7.4695E-3,0./
      DATA AG31/-4.0963E-3,-3.0744E-3,-1.0596E-3,0./
      DATA AG32/3.6814E-5,3.191E-5,6.5979E-6,0./
      DATA AG40/7.4163E-4,3.6485E-3,-1.381E-3,0./
      DATA AG41/3.5332E-4,2.4708E-4,1.7037E-4,0./
      DATA AG42/-3.146E-6,-2.544E-6,-1.0431E-6,0./
      DATA RSUN /1.032,1.025,1.011,.994,.978,.969,.967,.975,.99,
     1   1.007,1.022,1.031/
      DATA DECLI /-20.71,-12.81,-1.8,9.77,18.83,23.07,21.16,13.65,
     1   2.89,-8.72,-18.37,-22.99/
c
C       Fundamental physical constants (CODATA, 1998)
c
c       h=6.62606876(52)E-34 J*s (Planck constant)
c       c=2.99792458E+08 m*s-1 (speed of light)
c       Avogad= Avogadro number=6.02214199(47)E+23 mol-1
c       NLosch=Loschmidt number=Avogad/Vm (m-3)
c       Vm=22.413996(39)E-03 m3*mol-1
c       NLosch converted here to cm-3
c       evolt=energy of a photon (J)
c       PHOT=1/(h*c)  [J-1*m-1]
c
      PHOT=5.03411762D+24
      evolt=1.602176463D-19
      Avogad=6.02214199D+23
      NLosch=2.6867775D+19
c
c-------------------------------
c
      pinb=3.14159265
      pi2=pinb/2.
      pix4=pinb*4.
      RPD=pinb/180.
c
c===================================
c
c     To obtain the command-line "batch" version, remove the comment sign
c     "c" on line 189!
c
       batch=.FALSE.
c       batch=.TRUE.
c
c===================================
c
c      RANGE1=1./340.85
      epsiln=1e-3
      epsilm=1e-6
      Iwarn1=0
      Iwarn2=0
      Iwarn3=0
      Iwarn5=0
      Iwarn6=0
      Iwarn7=0
      Iwarn8=0
      Iwarn9=0
      FileIn ='smarts295.inp.txt'
      FileOut='smarts295.out.txt'
      FileExt='smarts295.ext.txt'
      FileScn='smarts295.scn.txt'
C
C
C      Files (some with User-defined filenames)
c
c----------------------------------------------------------------------
c
c       
c      
c
c**********************************************************************
c
      smart=' Welcome to SMARTS, version 2.9.5!'
      write(6,314,iostat=Ierr1) smart
 314  format(/,35('*'),/,a48,/,35('*'))
c
      if(batch)goto 313
c
      write(6,3001)
 3001  format('$$$ SMARTS_295> ',
     1 'Use standard mode with default input file?'/,' [If YES (or Y)',
     2 ', execution will start immediately',/,'using the default ',
     3 'input file smarts295.inp.txt]',/,' (Y/N) ==>')
      Read(5,*) YesNo
      If(YesNo.eq.'Y'.or.YesNo.eq.'y'.or.YesNo.eq.'yes'.
     1  or.YesNo.eq.'YES')goto 3003
 312  continue
      Write(6,3140)
 3140 Format('$$$ SMARTS_295> What is the path to the input file?',/,
     1 ' * Type only "." if in the same folder',/,' * Do NOT type ',
     2 'the last "/" of the chain',/,' * 2000 characters max. ==>')
      Read(5,*) Path
      Write(6,315)
 315  Format('$$$ SMARTS_295> Generic name for all input/output ',
     1 'files ',/,' * without any extension',/,' * 100 characters ',
     2 'max.)? ==>')
      Read(5,*) Usernm
      Iname=Index(Usernm,' ') - 1
      FileIn =Usernm(1:Iname)//'.inp.txt'
      FileOut=Usernm(1:Iname)//'.out.txt'
      FileExt=Usernm(1:Iname)//'.ext.txt'
      FileScn=Usernm(1:Iname)//'.scn.txt'
      Write(6,317,iostat=Ierr3) FILEIN,FILEOUT,FILEEXT,filescn
 317  Format('$$$ SMARTS_295> You chose the following filenames:',/,
     1 ' Input: ',A100,/,' Output: ',A100,/,' Spreadsheet-',
     2 'ready: ',A100,/,' Smoothed results: ',A100,/,
     3 '$$$ SMARTS_295> Is this OK? (Y/N) ==>')
      Read(5,*) YesNo
      If(YesNo.eq.'N'.or.YesNo.eq.'n'.or.YesNo.eq.'no'.
     1  or.YesNo.eq.'NO')goto 312
      if(Path.eq.'.')goto 3003
      mname=Index(Path,' ')-1
      FileIn ='/'//FileIn
      FileOut='/'//FileOut
      FileExt='/'//FileExt
      FileScn='/'//FileScn
      FileIn =Path(1:mname)//FileIn
      FileOut=Path(1:mname)//FileOut
      FileExt=Path(1:mname)//FileExt
      FileScn=Path(1:mname)//FileScn
      goto 3003
 313  continue
      numarg = iargc()
      if(numarg.eq.0)goto 3003
      if(numarg.eq.1)goto 3002
      write(6,322)
 322  format('*** ERROR ***',/,' Too many arguments given to SMARTS. '
     1 ,'Only one file name should be given. RUN ABORTED!')
      STOP
 3002  call getarg (1, Usernm)
      Iname=Index(Usernm,' ') - 1
      FileIn =Usernm(1:Iname)//'.inp.txt'
      FileOut=Usernm(1:Iname)//'.out.txt'
      FileExt=Usernm(1:Iname)//'.ext.txt'
      FileScn=Usernm(1:Iname)//'.scn.txt'
c
c**********************************************************************
c
c
 3003  continue
      TotTime    = etime(time)	

      OPEN (UNIT=14,FILE=FileIn,STATUS='OLD')
      OPEN (UNIT=16,FILE=FileOut,STATUS='NEW')
      OPEN (UNIT=22,FILE='Gases/Abs_O2.dat',STATUS='OLD')
      OPEN (UNIT=25,FILE='Gases/Abs_O4.dat',STATUS='OLD')
      OPEN (UNIT=26,FILE='Gases/Abs_N2.dat',STATUS='OLD')
      OPEN (UNIT=27,FILE='Gases/Abs_N2O.dat',STATUS='OLD')
      OPEN (UNIT=28,FILE='Gases/Abs_NO.dat',STATUS='OLD')
      OPEN (UNIT=29,FILE='Gases/Abs_NO2.dat',STATUS='OLD')
      OPEN (UNIT=30,FILE='Gases/Abs_NO3.dat',STATUS='OLD')
      OPEN (UNIT=31,FILE='Gases/Abs_HNO3.dat',STATUS='OLD')
      OPEN (UNIT=32,FILE='Gases/Abs_SO2U.dat',STATUS='OLD')
      OPEN (UNIT=33,FILE='Gases/Abs_SO2I.dat',STATUS='OLD')
      OPEN (UNIT=34,FILE='Gases/Abs_CO.dat',STATUS='OLD')
      OPEN (UNIT=35,FILE='Gases/Abs_CO2.dat',STATUS='OLD')
      OPEN (UNIT=36,FILE='Gases/Abs_CH4.dat',STATUS='OLD')
      OPEN (UNIT=37,FILE='Gases/Abs_NH3.dat',STATUS='OLD')
      OPEN (UNIT=38,FILE='Gases/Abs_BrO.dat',STATUS='OLD')
      OPEN (UNIT=39,FILE='Gases/Abs_CH2O.dat',STATUS='OLD')
      OPEN (UNIT=40,FILE='Gases/Abs_HNO2.dat',STATUS='OLD')
      OPEN (UNIT=41,FILE='Gases/Abs_ClNO.dat',STATUS='OLD')
      read(22,*)dummy
      read(25,*)dummy
      read(26,*)dummy
      read(27,*)dummy
      read(28,*)dummy
      read(29,*)dummy
      read(30,*)dummy
      read(31,*)dummy
      read(32,*)dummy
      read(33,*)dummy
      read(34,*)dummy
      read(35,*)dummy
      read(36,*)dummy
      read(37,*)dummy
      read(38,*)dummy
      read(39,*)dummy
      read(40,*)dummy
      read(41,*)dummy
C
C***      CARD 1
C
      READ(14,*) COMNT
C
C***      CARD 2
C
      READ(14,*) ISPR
      IF(ISPR.EQ.1)GOTO 301
      IF(ISPR.EQ.2)GOTO 302
C
C***      CARD 2a if ISPR=0
C
      READ(14,*) SPR
      if(spr.ge.265.)goto 298
      if(spr.ge.4e-3)goto 299
      spr=4.1e-4
      Altit=0.
      Height=99.9
      Zalt=99.9
      Iwarn1=1
      goto 300
C
C      APPROXIMATE FUNCTION SPR=F(altit,Latit) ACCORDING TO GUEYMARD 
C         (SOLAR ENERGY 1993)--Improved in 2.9.3 for altit>10 km
C
 299  continue
      Zalt=10.+(5.5797-log(spr))/(.14395-.0006335*log(spr))
      Altit=0.
      Height=Zalt
      goto 300
 298  continue
      pp0=SPR/1013.25
      DTA=.014321-.00544*log(pp0)
      Zalt=Max(0.,(DTA**.5-.11963)/.00272)
      Altit=Zalt
      Height=0.
      if(Zalt.le.4.)goto 300
      Altit=0.
      Height=Zalt
      GOTO 300
 301  CONTINUE
C
C***      CARD 2a if ISPR=1 *** "Height" input added in 2.9.3 ***
C
      READ(14,*)SPR,Altit, Height
      Zalt=Altit+Height
      if(Zalt.le.100.)goto 300
      write(16,1599)
 1599 format('*** ERROR #1 ***',/,' The altitude cannot be > 100 km!',
     1 /,' RUN ABORTED!')
      GOTO 998
 302  CONTINUE
C
C***      CARD 2a if ISPR=2 *** Height added in 2.9.3 ***
C
      READ(14,*)Latit,Altit, Height
      Zalt=Altit+Height
      alati=abs(latit)
      if(Zalt.le.100.)goto 281
      write(16,1599)
      GOTO 998
 281  continue
      If(Latit.lt.-90.D00)Latit=45.D00
      if(Zalt.lt.10.)goto 295
      SPR=exp((5.5797-.14395*(Zalt-10.))/(1.-.0006335*(Zalt-10.)))
      goto 300
 295  continue
      PCOR=1.
      IF(abs(alati-45.).lt.epsiln)GOTO 303
      PHI2=Latit*Latit
      PCOR1=.993+2.0783E-04*alati-1.1589E-06*PHI2
      PCOR2=8.855E-03-1.5236E-04*alati-9.2907E-07*PHI2
      PCOR=PCOR1+Zalt*PCOR2
 303  continue
      SPR=1013.25*PCOR*EXP(.00177-.11963*Zalt-.00136*Zalt*Zalt)
 300  CONTINUE
      pp0=SPR/1013.25
      qp=1.-pp0
c      ZAlt2=Zalt*Zalt
C
C***      CARD 3
C
      READ(14,*) iAtmos
C
C***      CARD 3a
C
      IF(iAtmos.EQ.0)READ(14,*)TAIR,RH,SEASON,TDAY
      IF(iAtmos.EQ.1)READ(14,*)Atmos
C
C***      CARD 4
C
      READ(14,*) IH2O
C      
 311  continue
      IF(iAtmos.NE.1)GOTO 320
      IF(Atmos.EQ.'USSA')IREF=1
      IF(Atmos.EQ.'MLS')IREF=2
      IF(Atmos.EQ.'MLW')IREF=3
      IF(Atmos.EQ.'SAS')IREF=4
      IF(Atmos.EQ.'SAW')IREF=5
      IF(Atmos.EQ.'TRL')IREF=6
      IF(Atmos.EQ.'STS')IREF=7
      IF(Atmos.EQ.'STW')IREF=8
      IF(Atmos.EQ.'AS')IREF=9
      IF(Atmos.EQ.'AW')IREF=10
C
C      AVERAGE STRATOSPHERIC TEMPERATURE AND REFERENCE ATMOSPHERIC
C            CONDITIONS
C
      Call RefAtm(Zalt,Pref,TK,TempA,O3ref,RH,Wref,TO3ini,Iref)

      IF(ISPR.EQ.2)SPR=PREF
      TAIR=TK-273.15
      TKair=TK
      Tavg=TK
      TT0=Tk/273.15
      Season='SUMMER'
      if(Iref.eq.3.or.Iref.eq.5.or.Iref.eq.8.or.Iref.eq.10)
     1 Season='WINTER'
      Call Ozon2(Zalt,Ozmin,Ozmax)
      GOTO 321
C
 320  continue
      Atmos='USER'
      IREF=11
      wref=1.4164
      TKair=TAIR+273.15
      TK=TKair
      Tavg=TDAY+273.15
      IF(Height.gt.0.0)TK=Tavg
      TT0=Tk/273.15
c
c      Estimate ozone temperature at sea level
c
      TO3ini=230.87
      if(Season.eq.'WINTER')TO3ini=219.25
c
c      Converts Temperature at given altitude to ozone temperature
c
      Call Ozon(Zalt,TK,Tempa,Tmin,Tmax,Ozmin,Ozmax)
      
      if(tempa.ge.Tmin)goto 397
      Iwarn5=1
      tempa=Tmin
      goto 399
 397  continue
      if(tempa.le.Tmax)goto 396
      Iwarn6=1
      tempa=Tmax
 396  continue
c
 321  continue
      Seasn2='SPRING/SUMMER'
      if(Season.eq.'WINTER')Seasn2='FALL/WINTER'
      w=wref
      IF(IH2O.ne.1.or.iAtmos.eq.1)goto 349
      Iwarn2=1
      Call RefAtm(Zalt,dum1,dum2,dum3,dum4,dum5,W,dum6,1)
 349  continue

      IF(IH2O.NE.2)GOTO 319
C
C      SATURATION VAPOR PRESSURE FROM GUEYMARD (J. Appl. Met. 1993)
C
      TK1=TK/100.
      EVS=EXP(22.329699-49.140396/TK1-10.921853/TK1/TK1-.39015156*TK1)
      EV=EVS*RH/100.
C
C      W=f(T,RH) USING EMPIRICAL MODEL OF GUEYMARD (SOLAR ENERGY 1994)
C
      ROV=216.7*EV/TK
      TT=1.+(TAIR/273.15)
      HV=.4976+1.5265*TT+EXP(13.6897*TT-14.9188*TT**3)
      W=.1*HV*ROV
 319  continue
      IF(IH2O.NE.0)GOTO 328
C
C***      CARD 4a if IH2O=0
C
      READ(14,*)W
C
 328  CONTINUE
      if(w.le.12.)goto 327
      write(16,1027,iostat=Ierr5)w
 1027 format('*** ERROR #2 ***',/,' The value selected or calculated '
     1 ,'for precipitable water, w, is ',f10.3,', which is above the '
     2 ,'allowed maximum value of 12 cm. RUN ABORTED!')
      goto 998      
 327  continue
      if(w.le.0.)goto 776
      OPEN (UNIT=21,FILE='Gases/Abs_H2O.dat',STATUS='OLD')
      read(21,*)dummy
 776  continue
      TEMPO=TEMPA
      TEMPN=TEMPA
C
C***      CARD 5
C
      IALT=0
      Thick=1.
 329  continue
      READ(14,*)IO3
      IF(IO3.ne.1)GOTO 331
      IF(iAtmos.ne.0)goto 348
      Call RefAtm(Zalt,dum1,dum2,dum3,O3ref,dum5,dum6,dum7,1)
      Iwarn3=1
 348  continue
      AbO3=O3REF
      UOC=AbO3
      OPEN (UNIT=23,FILE='Gases/Abs_O3UV.dat',STATUS='OLD')
      OPEN (UNIT=24,FILE='Gases/Abs_O3IR.dat',STATUS='OLD')
      read(23,*)dummy
      read(24,*)dummy
      goto 335
C
C***      CARD 5a if IO3=0
C
 331  continue
      READ(14,*) IALT,AbO3
      if(AbO3.le.0.)goto 335
      OPEN (UNIT=23,FILE='Gases/Abs_O3UV.dat',STATUS='OLD')
      OPEN (UNIT=24,FILE='Gases/Abs_O3IR.dat',STATUS='OLD')
      read(23,*)dummy
      read(24,*)dummy
C
C      OZONE TOTAL COLUMN CORRECTION FITTED FROM REF. ATM.
C      
      IF(IALT.EQ.0)goto 335
      Altit2=Height*Height
      Altit3=Altit2*Height
      Altit5=Height-25.
      Altit9=Height-9.
      Alti92=Altit9*Altit9
      Alti93=Alti92*Altit9
      Alti52=Altit5*Altit5
      Alti53=Alti52*Altit5
      To3=TO3ini
      To32=To3*To3
      rz0=1.0918-.0096156*To3+2.115e-5*to32
      rz1=-.094887+.0008409*To3-1.8611e-6*to32
      Rpz9=1.+rz0*9.+rz1*81.
c      if(Height.gt.25.)goto 333
      if(Height.gt.9.)goto 332
      Rzo3=(1.-.059542*Height+.00060305*Altit2-2.2608e-5*Altit3)
     1  /(1.-.051786*Height)
      Thick=Rzo3*(1.+rz0*Height+rz1*Altit2)
      goto 335
 332  continue
      rz0=-3.1127+.027478*To3-6.0582e-5*to32
      rz1=-.37348+.0032494*To3-7.0588e-6*to32
      Rzo3=(.929881-.066031*Altit9-.00087695*Alti92+9.0424e-5*Alti93)
     1  /(1.-.059421*Altit9)
      Thick=Rzo3*(Rpz9+rz0*Altit9+rz1*Alti92)
      if(Height.le.25.)goto 335
c 333 continue
      Rpz5=Rpz9+rz0*16.+rz1*256.
      Rzo3=exp((-.939032-.11654*Altit5-.015994*Alti52+7.9923e-5*Alti53)
     1  /(1.+.049565*Altit5))
      Thick=Rzo3*(Rpz5+(-3.1127+.027478*To3-6.0582e-5*to32)*Altit5
     1 +(-.37348+.0032494*To3-7.0588e-6*to32)*Alti52)
  335 continue
      UOC=AbO3*THICK
      ApO3=0.
C
C***      CARD 6 - Modified in 2.9! Now refers to gaseous pollution...
C
      Load='STANDARD'
c
      READ(14,*)IGAS
      IF(IGAS.EQ.1)GOTO 340
C
C***CARD 6a if      IGAS=0 - Changed in 2.9! Now inputs gaseous overload
c      in the lower 1-km pollution layer (in ppmv)
C
      READ(14,*)iLoad
      Load='USER-DEFINED'
      ApCH2O=-.003
      ApCH4=0.
      ApCO=-.1
      ApHNO2=-.00099
      ApHNO3=0.
      ApNO=0.
      ApNO2=0.
      ApNO3=-.00049
      ApO3=-.007
      ApSO2=0.
      if(iLoad.ne.1)goto 80
c
c      Default Pristine conditions
c
      Load='PRISTINE ATMOSPHERE'
      ApCH2O=-.003
      ApCH4=0.
      ApCO=-.1
      ApHNO2=-.00099
      ApHNO3=0.
      ApNO=0.
      ApNO2=0.
      ApNO3=-.00049
      ApO3=-.007
      ApSO2=0.
 80   continue
      if(iLoad.ne.2)goto 81
c
c      Default 'LIGHT POLLUTION' conditions
c
      Load='LIGHT POLLUTION'
      ApCH2O=.001
      ApCH4=.2
      ApCO=0.
      ApHNO2=.0005
      ApHNO3=.001
      ApNO=.075
      ApNO2=.005
      ApNO3=.00001
      ApO3=.023
      ApSO2=.01
 81   continue
      if(iLoad.ne.3)goto 82
c
c      Default 'MODERATE POLLUTION' conditions
c
      Load='MODERATE POLLUTION'
      ApCH2O=.007
      ApCH4=.3
      ApCO=.35
      ApHNO2=.002
      ApHNO3=.005
      ApNO=.2
      ApNO2=.02
      ApNO3=.00005
      ApO3=.053
      ApSO2=.05
 82   continue
      if(iLoad.ne.4)goto 83
c
c      Default 'SEVERE POLLUTION' conditions
c
      Load='SEVERE POLLUTION'
      ApCH2O=.01
      ApCH4=.4
      ApCO=9.9
      ApHNO2=.01
      ApHNO3=.012
      ApNO=.5
      ApNO2=.2
      ApNO3=.0002
      ApO3=.175
      ApSO2=.2
 83   continue
c
C***      CARD 6b - If iLoad = 0 --- New in 2.9
c
      if(iLoad.eq.0)Read(14,*)ApCH2O,ApCH4,ApCO,ApHNO2,ApHNO3,
     3 ApNO,ApNO2,ApNO3,ApO3,ApSO2
c
c      Conversion from ppmv to atm-cm
c
      ApCH2O=ApCH2O*.1
      ApCH4=ApCH4*.1
      ApCO=ApCO*.1
      ApHNO2=ApHNO2*.1
      ApHNO3=.012*.1
      ApNO=ApNO*.1
      ApNO2=ApNO2*.1
      ApNO3=ApNO3*.1
      ApO3=ApO3*.1
      ApSO2=ApSO2*.1
 340  CONTINUE
C
C
C***      CARD 7 - Changed in 2.9!! Input CO2 concentration (ppmv)
C
      READ(14,*) qCO2
c
C***      CARD 7a - Changed in 2.9!! Choose ET spectrum
C
      READ(14,*)Ispctr
      
      if(Ispctr.lt.-1.or.Ispctr.gt.8)Ispctr=0
      if(ispctr.eq.-1)OPEN (UNIT=15,FILE='Solar/Spctrm_U.dat',
     1 STATUS='OLD')
      if(ispctr.eq.0)OPEN (UNIT=15,FILE='Solar/Spctrm_0.dat',
     1 STATUS='OLD')
      if(ispctr.eq.1)OPEN (UNIT=15,FILE='Solar/Spctrm_1.dat',
     1 STATUS='OLD')
      if(ispctr.eq.2)OPEN (UNIT=15,FILE='Solar/Spctrm_2.dat',
     1 STATUS='OLD')
      if(ispctr.eq.3)OPEN (UNIT=15,FILE='Solar/Spctrm_3.dat',
     1 STATUS='OLD')
      if(ispctr.eq.4)OPEN (UNIT=15,FILE='Solar/Spctrm_4.dat',
     1 STATUS='OLD')
      if(ispctr.eq.5)OPEN (UNIT=15,FILE='Solar/Spctrm_5.dat',
     1 STATUS='OLD')
      if(ispctr.eq.6)OPEN (UNIT=15,FILE='Solar/Spctrm_6.dat',
     1 STATUS='OLD')
      if(ispctr.eq.7)OPEN (UNIT=15,FILE='Solar/Spctrm_7.dat',
     1 STATUS='OLD')
      if(ispctr.eq.8)OPEN (UNIT=15,FILE='Solar/Spctrm_8.dat',
     1 STATUS='OLD')
c
C
C***      CARD 8
C
      READ(14,*) AEROS
      IF(AEROS.NE.'USER')GOTO 350
C
C***      CARD 8a if AEROS='USER'
C
      READ(14,*) ALPHA1,ALPHA2,OMEGL,GG
      IAER=0
      GOTO 355
C
C      AEROSOL CHARACTERISTICS *** Types 10 & 11 added in 2.9.3 ***
C
 350  CONTINUE
      IF(AEROS.EQ.'S&F_RURAL')IAER=1
      IF(AEROS.EQ.'S&F_URBAN')IAER=2
      IF(AEROS.EQ.'S&F_MARIT')IAER=3
      IF(AEROS.EQ.'S&F_TROPO')IAER=4
      IF(AEROS.EQ.'SRA_CONTL')IAER=5
      IF(AEROS.EQ.'SRA_URBAN')IAER=6
      IF(AEROS.EQ.'SRA_MARIT')IAER=7
      IF(AEROS.EQ.'B&D_C1')IAER=8
      IF(AEROS.EQ.'B&D_C')IAER=9
      if(AEROS.eq.'DESERT_MIN')IAER=10
      if(AEROS.eq.'DESERT_MAX')IAER=11
      if(height.le.2.or.IAER.eq.4)goto 3569
      if(Zalt.le.6)goto 3569
      IAER=4
      Aeros='S&F_TROPO'
      Iwarn7=1
 3569 continue
C      
      if(IAER.ne.10)goto 3570
      alpha1=.996
      alpha2=1.273
      goto 355
 3570 continue
      if(IAER.ne.11)goto 3571
      alpha1=0.
      alpha2=0.
      goto 355
 3571 continue
      IF(IAER.NE.8.AND.IAER.NE.9)GOTO 358
      ALPHA1=-.311
      ALPHA2=.265
      GOTO 355
 358  CONTINUE
      IF(IAER.NE.5)GOTO 360
      ALPHA1=.981
      ALPHA2=1.22
      GOTO 355
 360  continue
      IF(IAER.NE.6)GOTO 362
      ALPHA1=1.117
      ALPHA2=1.358
      GOTO 355
 362  continue
      IF(IAER.NE.7)GOTO 364
      ALPHA1=0.2805
      ALPHA2=0.2004
      GOTO 355
 364  CONTINUE
      IF(RH.GT.100.)RH=50.
c
c Angstrom's alpha =f(RH) for Shettle & Fenn aerosols (modified in 2.9.2)
c
      XRH=COS(0.9*RH*RPD)
      xrh2=xrh*xrh
      xrh3=xrh2*xrh
      ALPHA1=(C1(IAER)+C2(IAER)*XRH+C3(IAER)*XRH2+c4(IAER)*xrh3)/
     1 (1.+C5(IAER)*XRH+c6(iaer)*xrh2)
      ALPHA2=(D1(IAER)+D2(IAER)*XRH+D3(IAER)*XRH2+d4(iaer)*xrh3)/
     1 (1.+D5(IAER)*XRH+d6(iaer)*xrh2)
      if(Iaer.ne.4.or.Height.le.2.0.or.Zalt.lt.6.)goto 3641
      alpha1=1.0514
      alpha2=1.3623
      h2=height*height
      Deltau=Max(95.-20.*height,10.)
      Tauavg=(.13712-.007152*height+
     1 .00011794*h2)/(1.+.12521*height+.072153*h2)
      Taumin=(1.-.01*Deltau)*Tauavg
      Taumax=(1.+.01*Deltau)*Tauavg
      if(Zalt.lt.6.)goto 3642
      t550mn=exp(-3.2755-.15078*Zalt)
      t500mn=1.14*t550mn
      betamn=.441*t550mn
      Bschmn=.495*t550mn
      alpha1=1.055
      alpha2=1.368
      goto 3641
 3642 continue
      if(Season.eq.'SUMMER')goto 3641
      alpha1=1.0588
      alpha2=1.3736
      Tauavg=exp(-3.6752-.13699*height+2.1604/height)
      Taumin=(1.-.01*deltau)*Tauavg
      Taumax=(1.+.01*Deltau)*Tauavg
 3641 continue
      RHC=MAX(50.,RH)
      RHC2=RHC*RHC
C
C      COEFFICIENTS FOR OMEGL (SINGLE SCATTERING ALBEDO)
C
      BP0(IAER)=BP00(IAER)+BP01(IAER)*RHC+BP02(IAER)*RHC2
      BP1(IAER)=BP10(IAER)+BP11(IAER)*RHC+BP12(IAER)*RHC2
      BP2(IAER)=BP20(IAER)+BP21(IAER)*RHC+BP22(IAER)*RHC2
      BP3(IAER)=BP30(IAER)+BP31(IAER)*RHC+BP32(IAER)*RHC2
      BQ0(IAER)=BQ00(IAER)+BQ01(IAER)*RHC+BQ02(IAER)*RHC2
      BQ1(IAER)=EXP(BQ10(IAER)+BQ11(IAER)*RHC+BQ12(IAER)*RHC2)
      BQ2(IAER)=BQ20(IAER)+BQ21(IAER)*RHC+BQ22(IAER)*RHC2
C
C      COEFFICIENTS FOR GG (ASYMMETRY FACTOR)
C
      AG0(IAER)=AG00(IAER)+AG01(IAER)*RHC+AG02(IAER)*RHC2
      AG1(IAER)=AG10(IAER)+AG11(IAER)*RHC+AG12(IAER)*RHC2
      AG2(IAER)=AG20(IAER)+AG21(IAER)*RHC+AG22(IAER)*RHC2
      AG3(IAER)=AG30(IAER)+AG31(IAER)*RHC+AG32(IAER)*RHC2
      AG4(IAER)=AG40(IAER)+AG41(IAER)*RHC+AG42(IAER)*RHC2
C
C***      CARD 9
C
 355  continue
      READ(14,*) ITURB
C
C      SELECT THE APPROPRIATE TURBIDITY INPUT
C
      if(iturb.le.5) goto 374
      write(16,1949)
 1949 format(/,'***** ERROR #3!',/,' Input value for ',
     1 ' ITURB on Card 9 is > 5. Please specify a ',
     2 'smaller value.'/,' RUN ABORTED!')
      goto 998
 374  continue
      IF(ITURB.EQ.1)  GOTO 351
      IF(ITURB.EQ.2)  GOTO 352
      IF(ITURB.EQ.3)  GOTO 353
      IF(ITURB.EQ.4)  GOTO 354
      if(Iturb.eq.5)  goto 3560
C
C***      CARD 9a if ITURB=0
C
      READ(14,*)TAU5
      if(Zalt.ge.6.)tau5=t500mn
      GOTO 359
C
C***      CARD 9a if ITURB=1
C
 351  continue
      READ(14,*) BETA
      TAU5=BETA/(0.5**ALPHA2)
      GOTO 359
 352  CONTINUE
C
C***      CARD 9a if ITURB=2
C
      READ(14,*)BCHUEP
      TAU5=BCHUEP*2.302585
      goto 359
C
C***      CARD 9a if ITURB=5 *** Added in 2.9.3 ***
C
 3560 continue
      READ(14,*) Tau550
      TAU5=Tau550*(1.1**ALPHA2)
 359  CONTINUE
      if(zalt.ge.6.)goto 357
      if(ITURB.ne.5)tau550=Tau5/(1.1**alpha2)
      if(Aeros.eq.'USER'.or.Iaer.gt.4.or.tau5.lt.1e-4)goto 1825 
      Call ALFA(Season,Iaer,Iturb,Iref,alpha1,alpha2,tau5,beta,
     1 alf1,alf2,t550,0)
      alpha1=alf1
      alpha2=alf2
      if(ITURB.ne.5)tau550=t550
      goto 1824
 1825 continue
      beta=Tau5*(.5**alpha2)
 1824 continue
      If(Tau550.lt.5.0)goto 1826
      Write(16,1920,iostat=Ierr6)Tau550
 1920 Format(/,'***** ERROR #4!',/,' Input value for ',
     1 ' turbidity is too large (Tau550 = ',f6.1,'). Please specify a ',
     2 'smaller value.'/,' RUN ABORTED!')
      goto 998
 1826 continue
C
C      FUNCTION RANGE=F(BETA,ALPHA) FROM NEW FIT - Modified in 2.9.2
C
      RANGE=999.
      Visi=764.9
      If(IAer.ne.1.or.IRef.ne.1)goto 344
      If(Iturb.eq.0.or.iturb.eq.2.or.iturb.eq.5)beta=(.5**1.33669)*TAU5
      if(iturb.ne.5)Tau550=Tau5/(1.1**.9884)
 344  continue
      If(tau5.lt.0.001)goto 1355
      Call VISTAU(Season,Range,Tau550,0)
      VISI=RANGE/1.306
 1355 CONTINUE
      GOTO 357
 354  CONTINUE
C
C***      CARD 9a if ITURB=4
C
      READ(14,*)VISI
      RANGE=1.306*VISI
      GOTO 356
 353  CONTINUE
C
C***      CARD 9a if ITURB=3
C
      READ(14,*)RANGE
 356  CONTINUE
C
C      FIT BASED ON MODTRAN4 - Modified in 2.9.2
C
      if(zalt.ge.6.)goto 357
      If(Range.ge.1.0)goto 399
      Write(16,192)
 192  Format(/,'***** ERROR #5!',/,' Input value for ',
     1 ' Meteorological Range is < 1 km. Please specify a larger',
     2 ' value.'/,' RUN ABORTED!')
      goto 998
 399  continue
      Range=Min(Range,999.)
      VISI=RANGE/1.306
c      
      Call VISTAU(Season,Range,Tau550,1)
      TAU5=Tau550*(1.1**alpha2)
      BETA=(0.55**Alpha2)*TAU550
      if(Aeros.eq.'USER'.or.Iaer.gt.4)goto 1827 
      Call ALFA(Season,Iaer,Iturb,Iref,alpha1,alpha2,t5,b,
     1 alf1,alf2,tau550,1)
      alpha1=alf1
      alpha2=alf2
      tau5=t5
      beta=b
 1827 continue
c            
 357  CONTINUE
      IF(ITURB.NE.2)BCHUEP=TAU5/2.302585
      ALPHA=(ALPHA1+ALPHA2)/2.
      if(zalt.lt.6.)goto 3590
      iwarn9=1
      tau550=t550mn
      tau5=t500mn
      beta=betamn
      Bchuep=bschmn
      goto 3591
 3590 continue      
      if(Tau550.lt.Taumin.or.Tau550.gt.Taumax)Iwarn8=1
 3591 continue
C
      WRITE(16,194,iostat=Ierr7) COMNT,Atmos,AEROS
 194  FORMAT(/,'******************   SMARTS, version 2.9.5   *********'
     % ,'**********',//,
     %' Simple Model of the Atmospheric Radiative Transfer of Sunshine'
     % ,/,5X,'Chris A. Gueymard, Solar Consulting Services',/,20x,
     & 'December 2005',//,
     1 4X,'This model is documented in FSEC Report PF-270-95',/,
     2 ' and in a Solar Energy paper, vol. 71, No.5, 325-346 (2001)',
     3 //,' NOTE: These references describe v. 2.8 or earlier!!!',/,
     4 ' See the User''s Manual for details on the considerable ',/,
     5 ' changes that followed...',//,
     %'*************************************************************'
     % ,'***'//,2x,' Reference for this run: ',A64,//,64('-'),//,
     % '* ATMOSPHERE : ',A4,'        AEROSOL TYPE: ',A64,/)
      WRITE(16,100,iostat=Ierr8) SPR,Altit,Height,RH,W,UOC,uoc*1000.,
     % TAU5,Tau550,BETA,BCHUEP,RANGE,VISI,ALPHA1,ALPHA2,ALPHA,Seasn2
 100  FORMAT('* INPUTS:'/,5x,'Pressure (mb) = ',F8.3,'   Ground ', 
     % 'Altitude (km) = ',F8.4,/,5x,'Height above ground (km) = ',f8.4,
     2 /,5X,'Relative Humidity (%) = ',F6.3,3X,
     3 'Precipitable Water (cm) = ',F7.4,/,5x,'Ozone (atm-cm) = ',F6.4,
     1 ' or ',f5.1,' Dobson Units',/,3X,'AEROSOLS:  ','Optical Depth at'
     # ,' 500 nm = ',F6.4,'      Optical depth at 550 nm = ',f6.4,/,
     6 '       Angstrom''s Beta = ',F6.4,'       Schuepp''s'
     %,' B = ',F6.4,/,5x,'Meteorological Range (km) = ',F6.1,'   Visi'
     %,'bility (km) = ',F6.1,/,5x,'Alpha1 = ',F6.4,'  Alpha2 = ',F6.4,
     & '   Mean Angstrom''s Alpha = ',F6.4,/,5x,'Season = ',a24,/)
      WRITE(16,134,iostat=Ierr9)TKair,Tavg,TEMPA
 134  FORMAT('* TEMPERATURES:',/,5x,'Instantaneous at site''s altitude'
     1 ,' = ',F5.1,' K',/,5x,'Daily average (reference) at site''s ',
     2 'altitude = ',F5.1,' K',/,5x,'Stratospheric Ozone and NO2 ',
     3 '(effective) = ',F5.1,' K',/)
      if(Iwarn5.eq.1)write(16,1018,iostat=Ierr10) Tempa, Tmin
 1018 format('** WARNING #1',9('*'),/,'\\ The calculated ozone tempe',
     1 'rature, ',f5.1,' K, was below the most probable minimum of ',
     2 f5.1,'\\ for this altitude. The latter value has been used ',
     4 'for optimum results. Suggestion: double check',
     3 ' the daily temperature on input Card 3a',/)
      if(Iwarn6.eq.1)write(16,1019,iostat=Ierr11) Tempa, Tmax
 1019 format('** WARNING #2',9('*'),/,'\\ The calculated ozone tempe',
     1 'rature, ',f5.1,' K, was above the most probable maximum of ',
     2 f5.1,'\\ for this altitude. The latter value has been used ',
     4 'for optimum results. Suggestion: double check',
     3 ' the daily temperature on input Card 3a',/)
      if(IO3.eq.0.and.(UOC.lt.Ozmin.or.UOC.gt.Ozmax))write(16,1021,
     1 iostat=Ierr12) UOC, Ozmin, Ozmax
 1021 format('** WARNING #3',9('*'),/,'\\ The ozone columnar amount, ',
     1 f6.4,' atm-cm, is outside the most probable limits of ',f6.4,
     2 ' and ',f6.4,/,'\\ for this altitude. This may produce ',
     3 'inconsistent results.',/,'\\ Suggestion: double check the ',
     3 'values of IALT and AbO3 on input Card 5a.',/)
      if(Iwarn1.eq.1)write(16,1301)
 1301 format('** WARNING #4',9('*'),/,'\\ Pressure cannot be < 0.000',
     1 '41 mb and has been increased to this value.',/,'\\ ',/)
      if(Iwarn2.eq.1)write(16,1302)
 1302 format('** WARNING #5',9('*'),/,'\\ Precipitable water was not '
     1 ,'provided and no reference atmosphere was specified!',/,'\\ ',
     2 'USSA conditions have been used here.',/)
      if(Iwarn3.eq.1)write(16,1303)
 1303 format('** WARNING #6',9('*'),/,'\\ The ozone amount was not pro'
     1 ,'vided and no reference atmosphere was specified!',/,'\\ USSA',
     2 ' conditions have been used here.',/)
      if(Iwarn7.eq.1)write(16,1307)
 1307 format('** WARNING #7',9('*'),/,'\\ The aerosol type has been ',
     1 'changed to "S&F_TROPO" because either the receiver''s height ',
     2 'above ground',/,'\\ is > 2 km or its elevation is > 6 km ',
     3 'above sea level.',/)
      if(Iwarn9.eq.1)goto 1311
      if(Iwarn8.ne.1.or.height.le.2.0)goto 1311
      if(Zalt.lt.15.0.or.Zalt.gt.22.)goto 1310
      write(16,1309,iostat=Ierr13)Tau550,Taumin,Taumax
 1309 format('** WARNING #8',9('*'),/,'\\ The aerosol optical depth ',
     1 'at 550 nm, ',f6.4,' is outside the most probable limits of ',
     2 f6.4,' and ',f6.4,/,'\\ for this altitude, assuming a slight ',
     3 'background amount of volcanic aerosols. This may produce ',
     3 'inconsistent results.',/,'\\ Suggestion: double check the ',
     4 'value of your turbidity input on Card 9a.',/)
      goto 1311
 1310 continue
      write(16,1308,iostat=Ierr14)Tau550,Taumin,Taumax
 1308 format('** WARNING #9',9('*'),/,'\\ The aerosol optical depth ',
     1 'at 550 nm, ',f6.4,' is outside the most probable limits of ',
     2 f6.4,' and ',f6.4,/,'\\ for this altitude. This may produce ',
     3 'inconsistent results.',/,'\\ Suggestion: double check the ',
     4 'value of your turbidity input on Card 9a.',/)
 1311 continue
      if(Iwarn9.eq.1)write(16,1312)
 1312 format('** WARNING #20',9('*'),/,'\\ Receiver is at more than 6 ',
     1 'km above sea level, hence the aerosol optical depth has ',
     2 'been fixed to a default value, dependent only on altitude.',/)
c
C***      CARD 10
C
      Read(14,*) Ialbdx
      Rhox=0.2
      If(Ialbdx.lt.0)goto 383
      Call Albdat(Ialbdx,Nwal1,Filen1,Lambr1,Wvla1,Albdo1)
      Goto 384
C
C***      CARD 10a
C
 383  continue
      READ(14,*) Rhox
      
 384  Continue
C
C***      CARD 10b
C      
      Read(14,*)Itilt
      Tilt=0.
      Rhog=0.
      Wazim=0.
      If (Itilt.eq.0)Goto 389
      if(height.gt.0.5)write(16,1314)
 1314 format('** WARNING #21',9('*'),/,'\\ Receiver is at more than ',
     1 '0,5 km above ground, hence the calculation of the reflected ',
     2 'irradiance from the ground to the tilted plane is not',
     3 ' accurate.',/)
C
C***      CARD 10c
C      
      Read(14,*)Ialbdg,TILT,WAZIM
c      
      Rhog=Rhox
      If(Ialbdg.ge.0)Goto 385
C
C***      CARD 10d
C      
      Read(14,*)Rhog
c      
      Goto 389
 385  Continue
      Filen2=Filen1
      Lambr2=Lambr1
      If(Ialbdg.ne.Ialbdx)Call Albdat(Ialbdg,Nwal2,Filen2,
     2   Lambr2,Wvla2,Albdo2)
 389  Continue
C
C***      CARD 11 - Modified in 2.9
C
      READ(14,*)WLMN,WLMX,Suncor,SolarC
      
      If(Ialbdx.ge.0.and.Ialbdx.ne.2)
     2  Call Albchk(Nwal1,Filen1,Wvla1,Albdo1,.001*wlmn,.001*wlmx)
      If(Ialbdg.ge.0.and.Ialbdg.ne.2.and.Ialbdg.ne.Ialbdx.and.
     2 Itilt.ne.0) Call Albchk(Nwal2,Filen2,Wvla2,Albdo2,
     3 .001*wlmn,.001*wlmx)
C
C***      CARD 12
C
      READ(14,*) IPRT
      IF(IPRT.EQ.0) GOTO 392
C
C***      CARD 12a if IPRT=1 TO 3 - Modified in 2.9
C      
      READ(14,*)WPMN,WPMX,INTVL
      
      IF(INTVL.LT.0.5)WRITE(16,198)
 198  FORMAT(' *** WARNING #18 ***',/,'  Parameter INTVL on Card 12a',
     & ' is too low and will be defaulted to 0.5 nm.')
      IF(IPRT.lt.2)goto 392
      OPEN(UNIT=17,FILE=FileExt,STATUS='NEW')
C
C***      CARDS 12b if IPRT=2 TO 3
C      
      READ(14,*)IOTOT
C
C***      CARDS 12c if IPRT=2 TO 3
C      
      READ(14,*)(IOUT(i),i=1,IOTOT)
c
c=======================================         
      Out(1) ='Extraterrestrial_spectrm'
      Out(2) ='Direct_normal_irradiance'
      Out(3) ='Difuse_horizn_irradiance'
      Out(4) ='Global_horizn_irradiance'
      Out(5) ='Direct_horizn_irradiance'
      Out(6) ='Direct_tilted_irradiance'
      Out(7) ='Difuse_tilted_irradiance'
      Out(8) ='Global_tilted_irradiance'
      Out(9) ='Beam_normal_+circumsolar'
      Out(10)='Difuse_horiz-circumsolar'
      Out(11)='Circumsolar___irradiance'
      Out(12)='Global_tilt_photon_irrad'
      Out(13)='Beam_normal_photon_irrad'
      Out(14)='Difuse_horiz_photn_irrad'
      Out(15)='RayleighScat_trnsmittnce'
      Out(16)='Ozone_totl_transmittance'
      Out(17)='Trace_gas__transmittance'
      Out(18)='WaterVapor_transmittance'
      Out(19)='Mixed_gas__transmittance'
      Out(20)='Aerosol_tot_transmittnce'
      Out(21)='Direct_rad_transmittance'
      Out(22)='RayleighScat_optic_depth'
      Out(23)='Ozone_totl_optical_depth'
      Out(24)='Trace_gas__optical_depth'
      Out(25)='WaterVapor_optical_depth'
      Out(26)='Mixed_gas__optical_depth'
      Out(27)='Aeros_spctrl_optic_depth'
      Out(28)='Single_scattering_albedo'
      Out(29)='Aerosol_asymmetry_factor'
      Out(30)='Zonal_ground_reflectance'
      Out(31)='Local_ground_reflectance'
      Out(32)='Atmosph_back_reflectance'
      Out(33)='Global_tilt_reflectd_rad'
      Out(34)='Upward_reflctd_radiation'
      Out(35)='Glob_horiz_PAR_phot_flux'
      Out(36)='Dir_norml_PAR_photn_flux'
      Out(37)='Dif_horiz_PAR_photn_flux'
      Out(38)='Glob_tilt_PAR_photn_flux'
      Out(39)='Spectral_photonic_energy'
      Out(40)='Globl_horizn_photon_flux'
      Out(41)='Dirct_normal_photon_flux'
      Out(42)='Dif_horizntl_photon_flux'
      Out(43)='Global_tiltd_photon_flux'
c      
      Out(46)='DRay'
      Out(47)='Daer'
      Out(48)='Dif0'
      Out(49)='Fda00'
      Out(50)='Fdazt'
      Out(51)='Rhoa'
c
c=======================================      
c    
      Write(16,147,iostat=Ierr15)FileExt
 147  Format(/,'The following spectral variables will be output to ',
     2 'file: ',A24,/)
      Do 390 i=1,IOTOT
      j=IOUT(i)
      Write(16,148,iostat=Ierr16) Out(j)
 148  Format(' * ',A24)
 390  continue
 392  continue
C
C***      CARD 13
C
      READ(14,*) ICIRC
      
      IF(ICIRC.EQ.0)goto 401
C
C***      CARD 13a if ICIRC=1
C
      READ(14,*)slope,apert,limit
      Icirc=-2
      if(apert.le.0.0.and.slope.le.0.0)goto 401
      if(apert.le.0.0.and.limit.le.0.0)goto 401
      Icirc=-1
      if(slope.gt.10.0.or.apert.gt.10.0.or.limit.gt.10.0)goto 401
      if(slope.le.0.0.and.limit.le.0.0)goto 402
      if(limit.le.0.0)limit=2.*apert-slope
      If(slope.le.0.0)slope=2.*apert-limit
      if(apert.le.0.0)apert=.5*(slope+limit)
      nx=1+ifix(limit*10)
      Zxr=limit*rpd
      Znr=slope*rpd
      Icirc=2      
      Zdif=tan(Zxr)-tan(Znr)
      va=(tan(Zxr)+tan(Znr))/Zdif
      vb=2./Zdif
      goto 401
 402  continue
      Zxr=0.
      Znr=0.
      va=0.
      vb=0.
      nx=1+ifix(apert*10)
      Icirc=1
 401  continue
C
C***      CARD 14
C
      READ(14,*) ISCAN
C
C***      CARD 14a if ISCAN=1 - Modified in 2.9
C
      FWHM=0.
      IF(ISCAN.ne.1)goto 379
      READ(14,*)IFILT,WV1,WV2,step,FWHM
      OPEN (UNIT=18,FILE=FileScn,STATUS='NEW')
 379  continue
C
C***      CARD 15
C
      READ(14,*)ILLUM
      IF(ILLUM.EQ.0)GOTO 408
      If(Illum.ge.1)Goto 403
      OPEN (UNIT=19,FILE='CIE_data/VLambda.dat',STATUS='OLD')
      Read(19,*)dummy
C
C      PHOTOMETRIC DATA
C
      DO 409 IV=1,513
      READ(19,*)wvli,VL(IV)
 409  CONTINUE
      CIEYr=24
      Close (Unit=19,status='keep')
      Goto 408
 403  continue
      OPEN (UNIT=19,FILE='CIE_data/VMLambda.dat',STATUS='OLD')
      Read(19,*)dummy
      Do 404 IV=1,513
      READ(19,*)wvli,VL(IV)
 404  CONTINUE
      CIEYr=88
      Close (Unit=19,status='keep')
C
C***      CARD 16
C
 408  continue
      READ(14,*)IUV
C
C      WAVELENGTH LIMITS MANIPULATION
C
      IF(wlmn.LE.280.)wlmn=280.
      IF(WLMX.GE.4000.)WLMX=4000.
 400  continue
      IF(ILLUM.eq.0)GOTO 406
      IF(ILLUM.lt.0)GOTO 405
      wlmn=280.
      wlmx=4000.
      goto 406
 405  CONTINUE
      if(ILLUM.ne.-1.and.ILLUM.ne.-2)goto 406
      wlmn=MIN(379.,wlmn)
      wlmx=MAX(782.,wlmx)
 406  CONTINUE
      IF(IUV.NE.1)GOTO 419
      wlmn=280.
      WPMN=280.
      wlmx=MAX(400.,wlmx)
      WPMX=MAX(400.,WPMX)
 419  CONTINUE
c
      Area='ZONAL'
      If(Ialbdx.ge.0)goto 450
      WRITE(16,171,iostat=Ierr17) Area,RHOX
 171  FORMAT(//,3x,A6,' ALBEDO (constant and Lambertian) = ',F5.3,//)
      Goto 451
 450  Continue
      Write(16,173,iostat=Ierr18) Area,Filen1,Lambr1
 173  Format(//,'      Spectral ',A5,' albedo data: ',A24,/,
     2  '      with a reflection process: ',A24,//)
 451  Continue
      IF(ICIRC.ge.1)WRITE(16,115,iostat=Ierr19)slope,APERT,limit
 115  FORMAT('* GEOMETRY (half-angles) OF THE SIMULATED RADIOMETER ',
     2 '(deg.):',/,'   Slope = ',F5.2,'      Aperture = ',F5.2,
     3 '   Limit = ',F5.2,/)
      If (Icirc.eq.-2) Write(16,151)
 151  Format('** WARNING #10',9('*'),/,'\\ The values of both Apert ',
     2 'and Slope or Limit for the circumsolar',/,'\\ correction are ',
     3 'incorrectly less than or equal to 0.',/,'\\ This calculation ',
     4 'is therefore skipped!',/)
      If(Icirc.eq.1) Write(16,152)
 152  Format('** WARNING #11',9('*'),/'\\ The radiometer''s Slope ',
     2 'and Limit angles are not provided.',/,'\\ Circumsolar calcula',
     3 'tions will therefore be performed for',/,'\\ an average geome',
     4 'try corresponding to the Aperture angle.',/)
      If(Icirc.eq.-1) Write(16,153)
 153  Format('** WARNING #12',9('*'),/,'\\ Circumsolar calculations ',
     1 'cannot be done for this geometry.',/,'\\ All half angles must',
     2 ' be < 10 deg.',/)
      If(Itilt.eq.0)Goto 464
      Area='LOCAL'
      If(Ialbdg.ge.0)goto 454
      WRITE(16,171) Area,RHOG
      Goto 464
 454  continue
      Write(16,173) Area, Filen2,Lambr2
 464  Continue
C
C      PRELIMINARY CALCULATIONS FOR SPECTRAL FUNCTIONS
C
      BETAC=BETA*(2.**(ALPHA2-ALPHA1))
      EPSIRP=0.1686
C
C***      CARD 17
C
      READ(14,*) IMASS
      
C
C      SOLAR POSITION LONG DO-LOOP
C
      nread=0
      jday=0
 898  CONTINUE
      nread=nread+1
      iday=1
C
      IF(IMASS.NE.1)GOTO 11
C
C***      CARD 17a if IMASS=1
C
      READ(14,*,END=998)ELEV,Azim
      Zenit=90.-ELEV
      GOTO 3
 11   CONTINUE
      IF(IMASS.EQ.2)GOTO 4
      IF(IMASS.EQ.3)GOTO 12
      IF(IMASS.EQ.4)GOTO 19
C
C***      CARD 17a if IMASS=0
C
      READ(14,*,END=998)Zenit,Azim
      GOTO 3
 19   continue
C
C***      CARD 17a if IMASS=4
C
      READ(14,*,END=998) month,Latit,Dstep
      iscan=0
      if(iprt.eq.2)iprt=0
      if(iprt.eq.3)iprt=1
      jday=jday+1
      dsun=decli(month)
      testx=abs(dsun-Latit)
      if(testx.le.85.)goto 897
      write(16,1099,iostat=Ierr20)testx
 1099 format(/,90('*'),//,'*** ERROR #6 *** Sun is too low for the',
     1 ' specified date.',/,'[This condition must be ful',
     2 'filled: |declination - latitude| < 85 deg.',/,'The input data',
     3 ' for this day are such that |declination - latitude| = ',f6.1,
     4 ' deg.]',/,' RUN ABORTED!')
      goto 898
 897  continue
c 
      suncor=rsun(month)
      phir=Latit*rpd
      cphi=cos(phir)
      sphi=sin(phir)
      tphi=sphi/cphi
      DECR=dsun*rpd
      cdec=cos(decr)
      sdec=sin(decr)
      tdec=sdec/cdec
      q=cphi*cdec
      cosom0=-tphi*tdec
      om0=0.
      if(cosom0.lt.-1.0)om0=pinb
      if(cosom0.gt.-1.0.and.cosom0.lt.1.0)om0=acos(cosom0)
      sinom0=SIN(om0)
      sq=sinom0-om0*cosom0
      srise=12.-12.*om0/pinb
      daylth=2.*(12.-srise)
c
c      MEAN SOLAR ELEVATION ON THE HORIZONTAL, NO ATMOS. EFFECT
c
      sinh0=0.
      if(om0.gt.0.)SINH0=q*sq/om0
C
C      EXTRATERRESTRIAL IRRADIATION IN MJ/m2
C
      H0nd=.027502*SolarC*suncor*om0
      H0hd=H0nd*sinh0
c
c      Do-loop for incremental zenith angle
c
      iday=0
      Hbnx=0.
      Hbhx=0.
      Hdx=0.
      Hglob=0.
      Hglobs=0.
c      Hiext=0.
      Hibx=0.
      Hidx=0.
      Hig=0.
      His=0.
      Hdose=0.
      cinteg=Dstep/30.
      cstep=.0036*cinteg
      cstepi=cinteg/daylth
      dtime=Dstep/120.
 899  continue
      iday=iday+1
      omr=pinb*float(2*iday-1)*dtime/12.
      if(omr.le.(om0-dtime*pinb/12.))goto 895
c
c      Daily results
c      
      write(16,1002,iostat=Ierr21)H0hd,cstep*Hbhx,cstep*Hdx,cstep*Hglob,
     1 cstep*Hglobs,cstep*Hbnx,cstep*Hglob/H0hd
 1002 Format('* Monthly-average daily-total irradiations (MJ/m2)',/,
     1  '   - Horizontal ',
     3 'surface',/,'   Extraterrestrial: ',f8.3,'   Direct: ',f8.3,
     4 '   Diffuse: ',f8.3,'   Global: ',f8.3,//,'   - Tilted surface',
     5 /,'   Global: ',f8.3,'   Direct normal: ',f8.3,//,'   - Clear',
     6 'ness index (Kt, dimensionless): ',f6.4)
      if(IUV.ne.0)write(16,1003,iostat=Ierr22)1000.*cstep*Hdose
 1003 format(//,'* Monthly-average daily-total UV dose (kJ/m2): ',
     1 f8.3)
      if(iLLUM.ne.0)write(16,1004,iostat=Ierr23)
     1 cstepi*Hibx,cstepi*Hidx,cstepi*Hig,cstepi*His,Hiext
 1004 format(//,'* Monthly-average illuminances (klux)',/,
     +  '   - Horizontal surface',/,'   Direct: ',f8.3,'   Diffuse: ',
     1 f8.3,'   Global: ',f8.3,//,'   - Tilted surface',/,'   Global: '
     2 ,f8.3,'   Extraterrestrial normal: ',f8.3,/)
      goto 898
c
 895  continue
      cosom=cos(omr)
      zcos=q*(cosom-cosom0)
      zr=acos(zcos)
      Zenit=zr/rpd
      zsin=sin(zr)
      cosaz=1.
      prod=cphi*zsin
      if(abs(prod).gt.1d-10)cosaz=(sphi*zcos-sdec)/prod
      if(abs(Latit-90.D00).lt.epsiln)cosaz=cphi*cosom/zsin
      azr=acos(cosaz)
      if(omr.lt.0.)azr=-azr
      Azim=180.-azr/rpd
      AmR=AMZ(Zenit)
      ITER=0
      GOTO 222 
 12   CONTINUE
C
C***      CARD 17a if IMASS=3 - Modified in 2.9
C
      READ(14,*,END=998)YEAR,month,DAY,HOUR,Latit,Longit,ZONE
c
      HourUT=Hour-Zone
      DayUT=Day
      if(HourUT.le.24.)goto 601
      HourUT=HourUT-24.
      DayUT=Day+1
 601  continue
      if(HourUT.ge.0.)goto 602
      HourUT=HourUT+24.
      DayUT=Day-1
 602  continue
      CALL SunPSA(HourUT,Latit,Longit,dec,
     1 Zenit,Azim,Julian,Radius,EOT,SPR,TK,Year,Month,DayUT)
 3    CONTINUE
      IF(Zenit.LE.90.)GOTO 13
      WRITE(16,103,iostat=Ierr24)Zenit
 103  FORMAT(//,'** ERROR #7 *** Value of Zenit = ',F6.2,' is > 90 deg.'
     1 ,' RUN ABORTED!')
      GOTO 898
 13   CONTINUE
      AmR=AMZ(Zenit)
      DayoYr=Nday(Year,Month,Day)
c
c      Solar Time
c
      SolarH=Hour-(Zone-Longit/15.)+EOT/60.
c
      ITER=0
      GOTO 2
C
C***      CARD 17a if IMASS=2
C
 4    continue
      READ(14,*,END=998) AMASS
      Azim=180.
      IF(AMASS.GT.1.0)GOTO 6
      Zenit=0.
      ITER=0
      GOTO 9
 6    CONTINUE
      Zenit=90.
      zx=89.75
      IF(AMASS.GE.35.0)GOTO 99
C
C      FAST ITERATION METHOD TO OBTAIN Zenit FROM AIR MASS
C
      ZXR=ACOS((((1225/AMASS/AMASS)-1)/1224)**0.5)
      ZX=ZXR/RPD
 99   continue
      If(Amass.le.38.2)goto 98
      Write(16,109,iostat=Ierr25)Amass
 109  Format(//,'** ERROR #8 *** Value of AMASS = ',f6.2,' is > 38.2. ',
     1 'RUN ABORTED!')
      Goto 898
 98   continue
      ITER=0
      SignD=1.
      DLTZ=0.5
 20   continue
      DLTZ=DLTZ/5.
      ZX=ZX+SignD*DLTZ
      AMX=AMZ(ZX)
      ITER=ITER+1
      EPSI=AMX-AMASS
      IF(ABS(EPSI).LT.0.0005) GOTO 5
      IF(ITER.GE.100) GOTO 5
      IF(EPSI.LT.0.0) GOTO 7
 8    continue
      ZX=ZX-DLTZ
      AMX=AMZ(ZX)
      EPSI=AMX-AMASS
      SignD=1.
      IF(EPSI) 20,5,8
 7    continue
      ZX=ZX+DLTZ
      AMX=AMZ(ZX)
      EPSI=AMX-AMASS
      SignD=-1.
      IF(EPSI) 7,5,20
 5    CONTINUE
      Zenit=ZX
 9    CONTINUE
      AmR=AMASS
 2    CONTINUE
      ZR=Zenit*RPD
      ZCOS=COS(ZR)
 222  continue
C
C      NEW PARAMETERIZATIONS FOR THE OPTICAL MASSES
C      (GUEYMARD, SOLAR ENERGY 1993). Revised & augmented for 2.9
C
C      AmR=AMZ(Zenit)   [ALREADY COMPUTED]
      AmH2O=1./(ZCOS+.10648*(Zenit**0.11423)*(93.781-Zenit)**(-1.9203))
      AmO3=1./(ZCOS+1.0651*(Zenit**.6379)*(101.8-Zenit)**(-2.2694))
      AmNO2=1./(ZCOS+1.1212*(Zenit**1.6132)*(111.55-Zenit)**(-3.2629))
      AmNO=1./(ZCOS+.77738*(Zenit**.11075)*(100.34-Zenit)**(-1.5794))
      AmN2=1./(ZCOS+.38155*(Zenit**8.871e-05)*(95.195-Zenit)**(-1.8053))
      AmCO2=1./(ZCOS+.65786*(Zenit**.064688)*(96.974-Zenit)**(-1.8083))
      AmCO=1./(ZCOS+.505*(Zenit**.063191)*(95.899-Zenit)**(-1.917))
      AmCH4=1./(ZCOS+.49381*(Zenit**.35569)*(98.23-Zenit)**(-2.1616))
      AmN2O=1./(ZCOS+.61696*(Zenit**.060787)*(96.632-Zenit)**(-1.8279))
      AmO2=1./(ZCOS+.65779*(Zenit**.064713)*(96.974-Zenit)**(-1.8084))
      AmNH3=1./(ZCOS+.32101*(Zenit**.010793)*(94.337-Zenit)**(-2.0548))
      AmSO2=1./(ZCOS+.63454*(Zenit**.0099198)*(95.804-Zenit)**(-2.0573))
      AmHNO3=1./(ZCOS+1.044*(Zenit**.78456)*(103.15-Zenit)**(-2.4794))
c      AmHNO4=1./(ZCOS+.92716*(Zenit**.080263)*(96.303-Zenit)**(-1.6709))
c      AmN2O5=1./(ZCOS+1.0497*(Zenit**.48545)*(99.455-Zenit)**(-2.1117))
      AmAER=1./(ZCOS+.16851*(Zenit**.18198)*(95.318-Zenit)**(-1.9542))
      AmCH2O=AmN2O
      AmClNO=AmNO2
      AmHNO2=AmHNO3
      AmNO3=AmNO2
      AmBrO=AmO3
      AmPOL=1./(.0001569+.9998431*zcos*zcos)**.5
      Amdif=1.732
c
      if(imass.ne.4)WRITE(16,172,iostat=Ierr26)Zenit,Azim,AmR,
     1 Real(AmH2O),AMO3,AMNO2,AmAER
 172  FORMAT(2(/,100('=')),//,'* SOLAR POSITION (deg.):',/,4x,
     1 'Zenith Angle (apparent) = ',F6.3,'  Azimuth (from North) = ',
     2 F7.2,//,'      RELATIVE OPTICAL MASSES:',/,'  - Rayleigh',
     #' = ',F6.3,/,'  - Water Vapor = ',F6.3,/,'  - Ozone'
     4,' = ',F6.3,/,'  - NO2 = ',F6.3,/,'  - Aerosols = ',F6.3,/)
      IF(IMASS.EQ.3)WRITE(16,170,iostat=Ierr27)YEAR,Month,DAY,Hour,
     1 DayoYr,DayUT,HourUT,Real(Julian),Real(Dec),RADIUS,EOT,SolarH
 170      FORMAT('  Results below are for this specific day:',/,' Year',
     1 ' = ',I4,'   Month = ',i2,'  Day = ',I2,'   Hour (LST) = ',f6.3,
     2 '   Day of Year = ',i3,/,'   In Universal Time:',/,
     3 '   Day (UT) = ',i2,'   Hour (UT) = ',F6.3,/,
     3 '   Julian Day = ',f12.3,'  Declination = ',F7.3,' deg.',
     5 '  Radius vector = ',F7.5,'   Equation of Time (min) = ',f7.3,
     5 /,'   Local Apparent Time (or Solar Time): ',f7.3,//)
c
      if(imass.eq.4)goto 223
      Zenith=Zenit
      Zenit=Min(90.,Zenith)
      ELEV=90.-Zenit
      ZR=Zenit*RPD
      ZCOS=COS(ZR)
 223  continue
      ZSIN=SIN(ZR)
      EPSIR=0.17*(1.-EXP(-8.*ZCOS))
      SIGMAR=3.65-2.3*EXP(-4.*ZCOS)
      EXPR=0.72+ZCOS
c      ZCOS1=MAX(.07,ZCOS)
c      EXPA=-.5+EXP(.24/(ZCOS1**1.24))
      FHTcy=(-11.012+12.392*AMO3)/(1.+.23644*AMO3)
      FHTcx=3.2656*(1.-EXP(-.46464*AMO3**1.25))-.965936*FHTcy
      FHTcz=2.*FHTcx+1.93187*FHTcy
      FHTdx=EXP(.31045+.001684*AMO3-.28549/AMO3**4)
c
      IF(IMASS.EQ.3)SUNCOR=1./RADIUS**2
c
c      Gas Abundances for total column in atm-cm
c
      if(nread.gt.1)goto 837
      AbCO2=.802685*qCO2*pp0
      AbO2=1.67766E5*pp0
      AbO4=1.8171d4*NLosch*NLosch*(pp0**1.7984)/(TT0**.344)
      AbBrO=2.5E-06
      AbNO3=.00005
      AbCH2O=.0003
      AbHNO2=.0001
      AbClNO=.00012
      AbO3=uoc
      pp0x2=pp0*pp0
      pln=log(pp0)
      pln2=pln*pln
      pln3=pln2*pln
      if(iatmos.eq.1.and.iref.le.6)goto 287
      AbCH4=1.31195*(pp0*1.1245)*(TT0**.047283)
      AbCO=.31491*(pp0**2.6105)*exp(.82546-2.9753*pp0+.88437*pp0x2)
      AbN2O=.24344*(pp0**1.1625)
      AbN2=3.8298*(pp0**1.8643)/(TT0**.50342)
      AbHNO3=3.6739e-4*(pp0**.13568)/(TT0**.0714)
      AbNO2=1e-4*Min(1.864+.20314*pp0,41.693*pp0)
c      UN=2.0671e-4
c      UNC=AbNO2
      AbNO=1e-4*Min(.74039+2.4154*pp0,57.314*pp0)
      AbSO2=1.114e-5*(pp0**.81319)*exp(.81356+3.0448*pp0x2
     1  -1.5652*pp0x2*pp0)
c      AbNH3=.0068778*(pp0**5.0426)*exp(-1.7876-.96817*pp0-.91191*pp0x2)
      AbNH3=exp(-8.6472+2.239*pLN-2.3822*PLN2-1.4408*PLN3
     1 -.46322*pln3*pln)
      goto 288
 287  continue
      Call Gases(Iref,pp0,AbCH4,AbCO,AbN2O,AbN2,AbHNO3,AbNO2,AbNO,
     1 AbSO2,AbNH3)
 288  continue
c
      if(iday.ne.1.or.jday.gt.1)goto 5000
      write(16,174,iostat=ierr28)qCO2
 174  format(/,' CO2 Mixing Ratio (ppmv): ',f6.1,//)
 5000 continue
      if(iday.ne.1.or.jday.gt.1.or.nread.ne.1)goto 5002
      Write(16,101,iostat=ierr29)
     3 AbBrO,AbCH2O,AbCH4,AbClNO,AbCO,AbCO2,AbHNO2,AbHNO3,AbNH3,AbNO,
     2 AbNO2,AbNO3,AbN2*1e5,AbN2O,AbO2,AbO3,AbO2,AbSO2
 101  Format(/,' Total column abundances (atm-cm) for all gases except',
     1 ' H2O, and for normal/standard conditions:',//,
     2 '    BrO',7x,'CH2O',8x,'CH4',6x,'ClNO3',9x,'CO',8x,'CO2',7x,
     3 'HNO2',7x,'HNO3',8x,'NH3',//,9(e10.4,1x),///,
     3 '     NO',8x,'NO2',8x,'NO3',9x,'N2',8x,'N2O',9x,
     4 'O2',9x,'O3',9x,'O4',8x,'SO2',//,9(e10.4,1x),///)
 5002 continue
      if(iGas.eq.1)goto 837
      AtCH4=AbCH4+ApCH4
      AtCO=Max(0.,AbCO+ApCO)
      AtHNO3=AbHNO3+ApHNO3
      AtNO2=AbNO2+ApNO2
      AtNO=AbNO+ApNO
      AtSO2=AbSO2+ApSO2
      AtNO3=Max(0.,AbNO3+ApNO3)
      AtO3=Max(0.,AbO3+ApO3)
      AtCH2O=Max(0.,AbCH2O+ApCH2O)
      AtHNO2=Max(0.,AbHNO2+ApHNO2)
      AtO2=AbO2
      AtN2O=AbN2O
      AtCO2=AbCO2
      AtN2=AbN2
      AtNH3=AbNH3
      AtBrO=AbBrO
      AtClNO=AbClNO
c      
      if(iday.eq.1.and.jday.le.1)Write(16,102,iostat=ierr30)Load,
     2 AtBrO,AtCH2O,AtCH4,AtClNO,AtCO,AtCO2,AtHNO2,AtHNO3,AtNH3,AtNO,
     3 AtNO2,AtNO3,AtN2*1e5,AtN2O,AtO2,AtO3,AtO2,AtSO2
 102  Format(' Corrected total column abundances for all gases '
     1 ,'(atm-cm)',/,' with these realistic conditions: ',A24,//,
     2 '    BrO',7x,'CH2O',8x,'CH4',6x,'ClNO3',9x,'CO',8x,'CO2',7x,
     3 'HNO2',7x,'HNO3',8x,'NH3',//,9(e10.4,1x),///,
     3 '     NO',8x,'NO2',8x,'NO3',9x,'N2',8x,'N2O',9x,
     4 'O2',9x,'O3',9x,'O4',8x,'SO2',//,9(e10.4,1x),///)
 837  continue
C
C
C      "CDRS" DIFFUSE TILT MODEL ACCORDING TO GUEYMARD
C            (SOLAR ENERGY 1987)
C
      If(Itilt.eq.0)Goto 453
      Tilt0=Tilt
      Wazim0=Wazim
      If(Tilt.lt.-400.)Tilt=Zenit
      If(Wazim.lt.-400.)Wazim=Azim
      SR=TILT*RPD
      Swa=(Azim-Wazim)*RPD
      Rdi=1.
      Rd=1.
      Tetas=Zenit
      H1=ELEV/100.
      CSR=COS(SR)
      SSR=SIN(SR)
      CS1=1.0
      If(Tilt0.ge.0.)CS1=CSR*ZCOS+SSR*ZSIN*COS(Swa)
      TETAS=ACOS(CS1)/RPD
      CS2=CS1*CS1
      CS3=CS2*CS1
      RBN=MAX(0.,CS1)
      RDI=(1.+CSR)/2.
      RG=(1.-CSR)/2.
      DA0=A0
      DA1=A1
      DA2=A2
      DA3=A3
      RDH0=RD0
      DO 10 K=1,4
      HK1=H1**K
      DA0=DA0+RB0(K)*HK1
      DA1=DA1+RB1(K)*HK1
      DA2=DA2+RB2(K)*HK1
      DA3=DA3+RB3(K)*HK1
      RDH0=RDH0+RDHB(K)*HK1
 10   CONTINUE
      RDC=EXP(DA0+DA1*CS1+DA2*CS2+DA3*CS3)
      RDS=(1.-.2249*SSR**2+.1231*SIN(2.*SR)-.0342*SIN(4.*SR))/.7751
      RD=RDC+RDH0*RDS
      if(imass.ne.4) WRITE(16,112,iostat=ierr31)TILT,WAZIM,TETAS,Rdi,Rd
 112  FORMAT(/,'* ANGLES (deg.) FOR TILTED SURFACE CALCULATIONS: ',/,
     %'   Surface Tilt = ',F7.3,
     2 '   Surface Azimuth (from North) = ',F7.3,/,1X,
     #'  Incidence Angle = ',F7.3,//,
     5'  Diffuse irradiance ratios (tilted plane/horizontal):',/,
     6 5x,F7.4,'  (isotropic approximate conversion--for reference)',
     7 /,5x,F7.4,'  (anisotropic conversion model--used here)',//)
      Tilt=Tilt0
      Wazim=Wazim0
 453  Continue
c
c      Z=zenit
c
c     Turbidity dependent coefficients for diffuse algorithm
c
      t5=tau550
      t52=t5*t5
      t53=t5*t52
      t54=t5*t53
      t505=t5**0.5
      t515=t5**1.5
      t503=t5-.03
      t5032=t503*t503
      t5033=t503*t5032
      t5034=t503*t5033
      t5035=t503**.5
      t56=t5-.6
      t562=t56*t56
      
      Ama1=Min(Amaer,20.)
      Ama11=Ama1-1.
      Ama12=Ama11*Ama11
      Ama15=Ama11**1.5
      Ama05=Ama11**0.5
      Ama2=Ama1*Ama1
      Ama3=Ama1*Ama2
      Ama4=Ama1*Ama3
      Ama5=Ama1**.5
      tra0=(.79996+5.2399*t5-.45849*t52)/(1.+5.6621*t5+.67258*t52)
      tra1=(-.25538+5.4972*t5-2.484*t52)/(1.+3.0475*t5+3.1615*t52)
      tra2=(1.7845+8.5655*t5-2.8046*t52)/(1.+1.9911*t5+1.0593*t52)
      trb0=(.98+4.2022*t5-.36341*t52)/(1.+4.1566*t5-.24198*t52)
      trb1=(-2.927-3.5797*t5+19.036*t52)/(1.+28.266*t5+1.4545*t52)
      trb2=(.017854+6.9189*t5+3.1496*t52)/(1.+7.1728*t5-.75729*t52)
      tzb0=1.
      tzb1=0.
      tzb2=0.
      if(Zenit.ge.45.)goto 7100
      fdifa0=(-405.89+669.51*Ama1-421.72*Ama3+163.26*Ama4)/
     1 (1.-1.7483*Ama1+.86666*Ama2)
      fdifa1=(1375.2-2268.7*Ama1+1430.2*Ama3-553.82*Ama4)/
     1 (1.-1.7459*Ama1+.86337*Ama2)
      goto 7101
 7100 continue
      fdifa0=(-81.197-42.369*Ama1+1.6385*Ama2+117.14*Ama5)/
     1 (1.+.0048737*Ama2)
      fdifa1=(287.15+148.6*Ama1-5.6994*Ama2-410.8*Ama5)/
     1 (1.+.005139*Ama2)
      fdifb0=(3.1461+4.3549*t5-.023703*t52-5.3845*t505)/
     1 (1.+1.8418*t52)
      fdifb1=(-.34571-.1714*t5-.0022115*t52+.55363*t505)/
     1 (1.+.046034*t52)
      fdifb2=(.017953+.087827*t5-.016289*t52-.065605*t505)/
     1 (1.+3.231*t52)
 7101 continue
      if(zenit.le.0.0.or.t5.le.0.03)goto 7029
      if(zenit.gt.80.)goto 7001
      tzc1b0=(1.0303-.29853*t5+3.0145*t52-5.1787*t53+.75487*t54)/
     1 (1.+48.21*t5-11.627*t505)
      tzc2b0=(.0087736-.023675*t5+.038433*t52-.02068*t53+.0025949*t54)/
     1 (1.+.75681*t5-1.102*t505)
      tzc3b0=(-1.6395+4.6978*t5-9.2494*t52+5.9966*t53-.78497*t54)/
     1 (1.+37.007*t5-3.1734*t505)
      tzd1b1=(.79647-.34098*t56+.23205*t562)/(1.-.6048*t56+.16386*t562)
      tzd2b1=(-.059924+.084758*t56-.025479*t562)
     1 /(1.-.60223*t56+.12697*t562)
      tzd3b1=(-.042678+.34131*t56-.28495*t562)/(1.+.14978*t562)
      tzd4b1=(-.013436+.23576*t56+.016945*t562)
     1 /(1.-.61716*t56+.13362*t562)
      tze1b2=-1.4472+.67069*t5-.18447*t52+.031889*t53
      tze2b2=.10793-.054098*t5+.072644*t52-.015276*t53
      tze3b2=.4919-1.0133*t5+.35268*t52-.044266*t53
      tze4b2=.073577-.036235*t5+.066935*t52-.014448*t53
      if(tau550.gt.1.75)goto 7009
      tze1b2=(-1.7064-3.3834*t5-.47597*t54+2.6748*t505)/
     1 (1.+.92521*t52)
      tze2b2=(.16239+.5601*t5+.13857*t54-.39887*t505)/(1.+1.1706*t52)
      tze3b2=(-.14892-.084698*t5+.020901*t54+.19723*t505)/
     1 (1.+.074613*t52)
      tze4b2=(.11187+.36411*t5+.19632*t54-.168*t505)/(1.+.42997*t52)
      if(tau550.gt.0.6)goto 7009
      tzc1b0=(.068343-1.4788*t5+23.173*t52-50.108*t53+42.158*t54)/
     1 (1.+14.968*t5-7.0445*t505)
      tzc2b0=(.010684-.10861*t5-4.3737*t52+9.6237*t53-8.1427*t54)/
     1 (1.+70.477*t5-19.077*t505)
      tzc3b0=(.0055635+.15348*t5-2.3326*t52+6.547*t53-5.2644*t54)/
     1 (1.+26.745*t5-9.9257*t505)
      tzc4b0=(.030341-1.0782*t5+23.098*t52-49.809*t53+44.96*t54)/
     1 (1.+18.655*t5-7.7781*t505)
      tzd1b1=(2.4+24.478*t503-3.8623*t5032-10.047*t5035)
     1 /(1.+14.505*t503)
      tzd2b1=(-.068+.68664*t503-.40106*t5032-.26178*t5035)
     1 /(1.-1.6208*t503)
      tzd3b1=(-1.2-15.514*t503+10.004*t5032+8.1483*t5035)
     1 /(1.+19.234*t503)
      tzd4b1=(.5-.12601*t503+7.0083*t5032-3.805*t5035)
     1 /(1.+34.177*t503)
      goto 7009
 7001 continue
      tzc1b0=(-.21837+.42655*t5-.25804*t52+.053587*t53-.0034933*t54)/
     1 (1.-1.0045*t5)
      tzc2b0=(.007183-.011421*t5+.0048386*t52-.00060429*t53
     1 -1.1538e-5*t54)/(1.-1.0044*t5)
      tzc3b0=(.41391-.94615*t5+.66482*t52-.14276*t53+.010187*t54)/
     1 (1.-1.0012*t5)
      tzd1b1=(-1.28-10.727*t5-.84523*t52+1.6956*t53-.26123*t54)/
     1 (1.+6.48*t5)
      tzd2b1=(-.014423+5.2179*t5+.10566*t52-.52195*t53+.08956*t54)/
     1 (1.+51.341*t5)    
      tzd3b1=3.3705+.91411*t5-1.1374*t52+.27451*t53-.020812*t54
      tze1b2=(4.8107+9.1856*t5-2.4024*t52+.29673*t53-10.278*t505)/
     1 (1.+.16684*t52)
      tze2b2=(-.18328-.39604*t5+.098351*t52-.011915*t53+.43926*t505)/
     1 (1.+.17865*t52)
      tze3b2=(-9.9946-14.963*t5+4.0066*t52-.50372*t53+18.124*t505)/
     1 (1.+.13084*t52)
      if(tau550.gt.0.6)goto 7009
      tzc1b0=(-.076486-2.9836*t5-3.6388*t52+5.2983*t53+1.0036*t505)/
     1 (1.+31.865*t5-11.358*t505)
      tzc2b0=(.0075+.060214*t503+.20831*t5032-.40488*t5033+
     1 .20894*t5034)/(1.+15.819*t503+2.2681*t5035)
      tzc3b0=(.072803+7.6593*t5+3.2585*t52-11.177*t53-1.9115*t505)/
     1 (1.+37.274*t5-12.44*t505)      
      tze1b2=(-.09282+3.6079*t5+72.469*t52-42.168*t53-.81503*t505)/
     1 (1.+34.684*t5-11.93*t505)
      tze2b2=(-.051779+.36296*t5-2.8752*t52)/(1.+52.058*t52)
      tze3b2=(-.94044-51.232*t5-120.6*t52+88.994*t53+15.858*t505)/
     1 (1.+35.032*t5-11.999*t505)
      if(tau550.gt.0.1)goto 7009
      tzd1b1=(-3.6546-15.903*t5+15.443*t505)/(1.-7.4089*t5)
      tzd2b1=(.2838+3.3062*t5-1.3909*t505)/(1.+11.041*t5)
      tzd3b1=(8.0948+25.437*t5-31.606*t505)/(1.-8.1072*t5)
 7009 continue     
c      icase=0
      if(zenit.gt.80.)goto 7021
c      icase=1
      tzb0=1.+tzc1b0*Ama11+tzc2b0*Ama12+tzc3b0*Ama15
      tzb1=(tzd1b1*Ama11+tzd2b1*Ama12+tzd3b1*Ama05)/(1.+tzd4b1*Ama11)
      tzb2=(tze1b2*Ama11+tze2b2*Ama12+tze3b2*Ama05)/(1.+tze4b2*Ama12)
      if(tau550.gt.1.75)goto 7029
c      icase=2
      tzb2=(tze1b2*Ama11+tze2b2*Ama12+tze3b2*Ama05)/(1.+tze4b2*Ama11)
      if(tau550.gt.0.6)goto 7029
c      icase=3
      tzb0=(1.+tzc1b0*Ama11+tzc2b0*Ama12+tzc3b0*Ama05)/(1.+tzc4b0*Ama11)
      tzb1=(tzd1b1*Ama11+tzd2b1*Ama12+tzd3b1*Ama05)/(1.+tzd4b1*Ama11)
      goto 7029
 7021 continue
      tzb0=1.+tzc1b0*Ama11+tzc2b0*Ama12+tzc3b0*Ama05
      tzb1=tzd1b1*Ama11+tzd2b1*Ama12+tzd3b1*Ama05
      tzb2=tze1b2*Ama11+tze2b2*Ama12+tze3b2*Ama05
c      icase=4
 7029 continue
      
      alba00=21.712-74.917*t5
      alba01=-79.895+263.33*t5
      alba0=(-13.126+331.3*t5+48.496*t52)/(1.+4.5503*t515)
      alba1=(113.53-2152.3*t5-280.*t52)/(1.+4.7907*t515)
      alba2=(-260.19+3516.8*t5+438.98*t52)/(1.+5.1594*t515)
      albb0=(-169.74+366.27*t5-1497.4*t52+417.66*t53)/(1.+211.44*t52)
      albb1=(695.55-2080.1*t5+5470.9*t52-1385.8*t53)/(1.+178.71*t52)
      albb2=(-724.21+2368.7*t5-5331.3*t52+1335.4*t53)/(1.+158.89*t52)
      albc0=(.030827+2.1215*t5+.23068*t52)/(1.+.84396*t5)
      albc1=(-.03264-2.1233*t5-.29271*t52)/(1.+1.1121*t5)
      albc2=(.015472+.7404*t5+.12473*t52)/(1.+1.1626*t5)
      albc3=(-.002198-.084679*t5-.015984*t52)/(1.+1.1628*t5)
      albc4=(1.019+1.6635*t5+.47646*t52)/(1.+80.333*t5)
C
c     Ozone-dependent coefficients for diffuse algorithm
c
      Amo32=Amo3*Amo3
      Amo33=Amo32*Amo3
      FHTa0=(-.47724+2.4591*Amo3-1.5553*Amo32+.36215*Amo33)/
     1 (1.-1.5041*Amo3+.58992*Amo32)
      FHTa1=(.018787-.010397*Amo3-.017601*Amo32+.0052716*Amo33)/
     1 (1.-1.4903*Amo3+.56957*Amo32)
      FHTb0=(15.376-4.3125*Amo3+.83276*Amo32-.040636*Amo33)/
     1 (1.-4.6253*Amo3+2.8311*Amo32)
      FHTb1=(-.13145+.035853*Amo3-.0076174*Amo32+.00038038*Amo33)/
     1 (1.-1.9406*Amo3+.93498*Amo32)
      FHTc0=(-62.912+49.558*Amo3)/(1.-.027447*Amo3)
      FHTc1=(3.3372-2.4508*Amo3)/(1.-.035041*Amo3)
      FHTd0=(30.875-4.5802*Amo3+.50428*Amo32)/
     1 (1.-2.5567*Amo3+1.8116*Amo32)
      FHTd1=(-.23641+.020167*Amo3-.004264*Amo32)/
     1 (1.-1.3537*Amo3+.59902*Amo32)
      FHTe0=(2.5689-4.824*Amo3+2.07*Amo32)/
     1 (1.-1.1905*Amo3+.3643*Amo32)
      FHTe1=(-.066605+.21397*Amo3-.10567*Amo32)/
     1 (1.-1.2116*Amo3+.37579*Amo32)
      FHTf0=(2.2348-.44161*Amo3+.3772*Amo32-.020273*Amo33)/
     1 (1.-.97556*Amo3+.28047*Amo32)
      FHTf1=(-.012594-.012008*Amo3-.015456*Amo32+.00085387*Amo33)/
     1 (1.-.96938*Amo3+.26425*Amo32)
      ra00=(1.8973-2.8609*Amo3+1.4498*Amo32-.18485*Amo33)/
     1 (1.-.95212*Amo3+.24444*Amo32)
      ra01=(.35236-.2446*Amo3+.24659*Amo32-.013065*Amo33)/
     1 (1.-.88635*Amo3+.22055*Amo32)
      ra10=(-.58215+.31643*Amo3-.023724*Amo32+.00068713*Amo33)/
     1 (1.-.1444*Amo3-.11746*Amo32)
      ra11=(18.015-121.17*Amo3+81.105*Amo32-13.644*Amo33)/
     1 (1.-7.7047*Amo3)
      ra12=(.092338+1.1519*Amo3-2.3328*Amo32+1.1325*Amo33)/
     1 (1.-1.4379*Amo3+.7014*Amo32)
      ra02=(1.4738-.90914*Amo3+.14322*Amo32)/
     1 (1.-.30469*Amo3+.027331*Amo32)
      ra13=(-.20733+.19451*Amo3-.029374*Amo32)/
     2 (1.-.51985*Amo3+.081935*Amo32)
      ra03=(-8.1831+3.2169*Amo3-.18812*Amo32)/(1.+.32473*Amo3)
      ra14=(2.1533-.57263*Amo3+.03416*Amo32)/(1.+.0027812*Amo3)
      ra04=(1.9915-.58894*Amo3+.05611*Amo32
     1 -.0013311*Amo33)/(1.-.26987*Amo3+.02113*Amo32)
      ra15=(1.7389-3.3761*Amo3+2.1498*Amo32-.40236*Amo33)/
     1 (1.-.71275*Amo3+.16324*Amo32)
      ra16=(1.4282-.61201*Amo3+.10302*Amo32
     1 -.0055147*Amo33)/(1.-.6277*Amo3+.12961*Amo32)
      ra05=(18.214+65.305*Amo3-8.6308*Amo32+1.1603*Amo33)/
     1 (1.0+69.727*Amo3-.34374*Amo32)
      ra17=(-.12865-.18023*Amo3+.084979*Amo32-.0068614*Amo33)/
     1 (1.-.18029*Amo3+.020348*Amo32)
      ra06=(1.0122-.18077*Amo3+.43678*Amo32)/
     1 (1.-.15562*Amo3+.47075*Amo32)
      ra18=(1.9286-2.0616*Amo3+.24389*Amo32)/
     1 (1.+.2096*Amo3+.030102*Amo32)
c-----------------------------------------------
c      
C      Start reading the selected E.T. SPECTRUM file
C
      if(nread.gt.1)goto 787
      if(imass.eq.4.and.iday.gt.1)goto 787
      READ(15,*)Spctrm
      READ(15,*)ESC
      ESCC=SUNCOR*SolarC
      Scor=Escc/Esc
      Scor2=SolarC/Esc
c 
      if(iday.eq.1.and.nread.le.1)WRITE(16,126,iostat=ierr32)ESCC,
     1 SUNCOR,SolarC,Spctrm
 126  FORMAT(/,33('*',2x),//,'** SPECTRUM:',/,'   Total (0-100 µm) ',
     1 'Extraterrestrial Irradiance used here = ',F7.2,' W/m2',/,
     #'  (i.e., ',F6.4,' times the selected solar constant, ',f7.2,
     4 ' W/m2, due to the actual Sun-Earth distance.)',/,'   Source'
     5 ,' for selected solar spectrum: ',A64,/)
      If(abs(Scor2-1.).gt.1e-4.and.iday.eq.1.and.jday.le.1.and.
     1 nread.le.1)write(16,127,iostat=ierr33)Scor2
 127  format(' To account for the chosen Solar Constant value, the ',
     2 'selected solar spectrum has been uniformly multiplied',/,
     3 ' by this scaling coefficient = ',f6.4,/)
c
      if(imass.ne.4.or.iday.ne.1)goto 5007
      write(16,1001,iostat=ierr34)month,
     1 Real(Latit),srise,daylth,Dstep
 1001 Format(//,' Mean daily results for the average day of'
     1 ,' month ',i2,' at latitude: ',f7.3,' deg.',/,' Solar time of '
     2 ,'sunrise (hr): ',f6.3,'   Daylength (hr): ',f6.3,'   Time ',
     3 'integration step (min): ',f5.1,/)
 5007	continue
c
c      Rewind data files for new runs
c
      goto 788
 787  continue
      Rewind 15
      Rewind 22
      Rewind 25
      Rewind 26
      Rewind 27
      Rewind 28
      Rewind 29
      Rewind 30
      Rewind 31
      Rewind 32
      Rewind 33
      Rewind 34
      Rewind 35
      Rewind 36
      Rewind 37
      Rewind 38
      Rewind 39
      Rewind 40
      Rewind 41
      if(w.gt.0.)Rewind 21
      if(AbO3.gt.0.)Rewind 23
      if(AbO3.gt.0.)Rewind 24
      read(22,*)dummy
      read(25,*)dummy
      read(26,*)dummy
      read(27,*)dummy
      read(28,*)dummy
      read(29,*)dummy
      read(30,*)dummy
      read(31,*)dummy
      read(32,*)dummy
      read(33,*)dummy
      read(34,*)dummy
      read(35,*)dummy
      read(36,*)dummy
      read(37,*)dummy
      read(38,*)dummy
      read(39,*)dummy
      read(40,*)dummy
      read(41,*)dummy
      if(w.gt.0.)read(21,*)dummy
      if(AbO3.gt.0.)read(23,*)dummy
      if(AbO3.gt.0.)read(24,*)dummy
c      
C      Start reading the selected E.T. SPECTRUM file
C
      READ(15,*)Spctrm
      READ(15,*)ESC
      ESCC=SUNCOR*SolarC
      Scor=Escc/Esc
      Scor2=SolarC/Esc
c 
      if(iday.eq.1)WRITE(16,126)ESCC,
     1 SUNCOR,SolarC,Spctrm
      If(abs(Scor2-1.).gt.1e-4.and.iday.eq.1.and.jday.le.1)
     1 write(16,127)Scor2
c
      if(imass.eq.4.and.iday.eq.1)write(16,1001)month,
     1 Real(Latit),srise,daylth,Dstep
c
 788  continue
C
c-----------------------------------------------
c
C      INITIALIZATION FOR BROADBAND RESULTS
C
      SUMB=0.
      SUMBX=0.D00
      SUMD=0.D00
      SUMD0=0.D00
      SUMDX=0.D00
      SUMG=0.
      SUMBN=0.D00
      SUM0=0.
      SUMBS=0.
      SUMDS=0.
      SUMRS=0.
      SUMGS=0.
      IWVL=0
      BILLUM=0.
      DILLUM=0.
      SILLUM=0.
      XILLUM=0.
      BILLX=0.
      DILLX=0.
      SERY0=0.
      SERY1=0.
      SERY2=0.
      SERY3=0.
      SERY4=0.
      SDNA=0.
      SECAL=0.
      SPHO=0.
      SACG=0.
      SPOL=0.
      SSIS=0.
      SPRT=0.
      SSCUPH=0.
      SSCUPM=0.
      SUVA1=0.
      SUVB1=0.
      SUVA2=0.
      SUVB2=0.
      PARg=0.
      PARgs=0.
      PARb=0.
      PARd=0.
      PPFDg=0.
      PPFDgs=0.
      PPFDb=0.
      PPFDd=0.
C
      IF(IPRT.le.0.or.IPRT.eq.2.or.imass.eq.4)goto 65
      If(Itilt.ne.0.or.Icirc.ne.0)goto 60
      WRITE(16,110)
 110  FORMAT('*** IRRADIANCES IN W/(m2 nm) AND TRANSMITTANCES FOR A ',
     2  'GIVEN WAVELENGTH (in nm)',
     3  //,' WVLGTH',2X,'SPCTRM',5X,'BEAM',8X,'BEAM',8X,'DIFF',
     4  8X,'GLOB',/,
     7  18X,' NORMAL',3X,'------------ HORIZONTAL -----------',/)
      goto 65
 60   continue
      IF(Itilt.ne.0.or.Icirc.ne.1)goto 61
      WRITE(16,114)
 114  FORMAT('*** IRRADIANCES IN W/(m2 nm) FOR A ',
     2  'GIVEN WAVELENGTH (in nm)',
     3  //,'WVLGTH',2X,'SPCTRM',6X,'BEAM',8X,'DIFF',
     4  8X,'GLOB',8X,'BEAM_EXP',4X,'DIF_EXP',/,
     5  18X,' NORMAL',3X,'------ HORIZONTAL -----    ------- NORMAL',
     6  ' -------',/)
      goto 65
 61   continue
      IF(Itilt.ne.1.or.Icirc.ne.0)goto 62
      WRITE(16,111)
 111  FORMAT('*** IRRADIANCES IN W/m2 nm FOR A ',
     2  'GIVEN WAVELENGTH (in nm)',//
     3  ,'WVLGTH',2X,'SPCTRM',6X,'BEAM',8X,'DIFF',
     4  8X,'GLOB',8X,'DIFF',7X,'GLOB',/,
     7  18X,' NORMAL',3X,'------ HORIZONTAL -----',
     8  3X,'-------- TILTED -------',/)
      GOTO 65
 62   CONTINUE
      WRITE(16,116)
 116  FORMAT('*** IRRADIANCES IN W/m2 nm FOR A ',
     2  'GIVEN WAVELENGTH (in nm)',//
     3  ,'WVLGTH',2X,'SPCTRM',6X,'BEAM',8X,'DIFF',
     4  8X,'GLOB',8X,'BEAM_EXP',8X,'DIFF',8X,'GLOB',/,
     7  18X,' NORMAL',3X,'------ HORIZONTAL -----   -- NORMAL--',
     8  '       ---- TILTED PLANE ---',/)
 65   CONTINUE
      If(IPRT.lt.2) goto 5008
      Write(17,113,iostat=ierr35) (Out(Iout(i)),i=1,IOTOT)
 113  Format('Wvlgth',50(1x,a24))
 5008	continue
C
c-----------------------------------------------
c-----------------------------------------------
c
C***      START SPECTRAL CALCULATIONS
C
      IWVL=0
      WVOLD=wlmn-1.
      IF(wlmn.GE.1705.)WVOLD=wlmn-5.
      iF(wlmn.le.400.)WVOLD=wlmn-0.5
c
 15   continue
      READ (15,*,END=999) IWVLN1,H0
c 
      WVLn=FLOAT(IWVLN1)/10.
      WVL=wvln/1000.
      wvl2=wvl*wvl
      wvl3=wvl*wvl2
      WVL4=WVL*WVL3
      IF(WVLN.LE.WVOLD)GOTO 15
      IF(WVLN.GT.wlmx)GOTO 999
      H0=H0*Scor
      WVOLD=WVLN
      IWVL=IWVL+1
      wv(IWVL)=WVLN
C
c-----------------------------------------------
c
C**      RAYLEIGH SCATTERING FUNCTION - Revised in 2.9
C
      TAURL=pp0/(117.3405*WVL4-1.5107*WVL2 + .017535
     % -8.7743E-4/WVL2)
      TR=EXP(-AmR*TAURL)
C
c-----------------------------------------------
c
C**      WATER VAPOR ABSORPTION - Fundamentally revised in 2.9
C
      TH2o=1.D00
      TH2oP=1.D00
      Tauw=0.D00
c
      if(wvln.lt.440.0.or.w.le.0.0)goto 17
 73   continue
      READ (21,*,END=17) wvlw,AW,iband,ifitw,bwa0,bwa1,bwa2,
     1 ifitm,bma0,bma1,bma2,ifitmw,bmwa0,bmwa1,bmwa2,bpa1,bpa2
      if(abs(wvln-wvlw).gt.epsilm)goto 73
      IF(AW.le.0.0)GOTO 17
c      
      Bw=1.
      w0=4.11467
      if(iband.eq.2)w0=2.92232
      if(iband.eq.3)w0=1.41642
      if(iband.eq.4)w0=.41612
      if(iband.eq.5)w0=.05663
      ww0=w-w0
      ww02=ww0*ww0
      Bw=1.+bwa0*ww0+bwa1*ww02
      if(ifitw.eq.1)Bw=Bw/(1.+bwa2*ww0)
      if(ifitw.eq.2)Bw=Bw/(1.+bwa2*ww02)
      if(ifitw.eq.6)Bw=bwa0+bwa1*ww0
c      
      Bm=1.
      Bmp=1.
      do 72 im=1,2
      xamw=AmH2O
      if(im.eq.2)xamw=amdif
      xamw1=xamw-1.
      xamw11=xamw1**.1
      xamw12=xamw1*xamw1
      xamw15=xamw1**.5
      xamw25=xamw1**.25
c      
      if(ifitm.eq.0)Bmx(im)=bma1*(xamw**bma2)
      if(ifitm.eq.1)Bmx(im)=(1.+bma0*xamw1+bma1*xamw12)/
     1 (1.+bma2*xamw1)
      if(ifitm.eq.2)Bmx(im)=(1.+bma0*xamw1+bma1*xamw12)/
     1 (1.+bma2*xamw12)
      if(ifitm.eq.3)Bmx(im)=(1.+bma0*xamw1+bma1*xamw12)/
     1 (1.+bma2*xamw15)
      if(ifitm.eq.5)Bmx(im)=(1.+bma0*xamw25)/(1.+bma2*xamw11)
 72      continue
c      
      Bm=Bmx(1)
      Bmp=Bmx(2)
      Bm=max(Bm,.05)
      Bm=min(Bm,7.0)
      Bmp=max(Bmp,.1)
      Bmp=min(Bmp,6.0)
      Bw=max(Bw,.05D00)
      Bw=min(Bw,7.0D00)
      Bmw=Bm*Bw
      Bmwp=Bmp*Bw 
      if(abs(Bw-1.).lt.epsilm)goto 75
      if(ifitm.ne.0.and.abs(Bm-1.).lt.epsilm)goto 75
      if(ifitm.eq.0.and.Bm.gt.0.968.and.Bm.lt.1.0441)goto 75
      if(ifitmw.eq.-1)goto 75
      Bmw=1.
      wfw0=w/w0
      do 74 im=1,2
      xamw=AmH2O
      if(im.eq.2)xamw=Amdif
      yamw=xamw*wfw0
      yamw1=yamw-1.
      yamw12=yamw1*yamw1
      if(ifitmw.eq.0)Bmwx(im)=bmwa1*(yamw**bmwa2)
      if(ifitmw.eq.1)Bmwx(im)=(1.+bmwa0*yamw1+bmwa1*yamw12)/
     1 (1.+bmwa2*yamw1)
      if(ifitmw.eq.2)Bmwx(im)=(1.+bmwa0*yamw1+bmwa1*yamw12)/
     1 (1.+bmwa2*yamw12)
 74   continue
      Bmw=Bmwx(1)
      Bmwp=Bmwx(2)
      Bmw=max(Bmw,.05D00)
      Bmw=min(Bmw,7.0D00)
      Bmwp=max(Bmwp,.1D00)
      Bmwp=min(Bmwp,6.D00)
 75   continue
c
      Bp=1.
      wamw=w*AmH2O
      if(abs(qp).lt.1e-5)goto 78
      qp1=Min(.35,qp)
      pp01=Max(.65,pp0)
      pp02=pp01*pp01
      qp2=qp1*qp1
      Bp=1.+.1623*qp
      if(iband.eq.2)Bp=1.+.08721*qp1
      if(iband.eq.3)Bp=1.-bpa1*qp1-bpa2*qp2
      if(iband.eq.4)Bp=(1.-exp(-.63486+6.9149*pp01-13.853*pp02)
     1  *wamw)*(1.-bpa1*qp1-bpa2*qp2)
      if(iband.eq.5)Bp=(1.-wamw*exp(8.9243-18.197*pp01+2.4141*pp02))
     1 *(1.-bpa1*qp1-bpa2*qp2)
      Bp=max(Bp,.3D00)
      Bp=min(Bp,1.7D00)
 78   continue
c
      wAmw=(w*AmH2O)**.9426
      wAmp=(amdif*w)**.9426
c
      Tauw=Bmw*Bp*Aw*wAmw
      TH2O=EXP(-Tauw)
      TH2OP=EXP(-Bmwp*Bp*Aw*wAmp)
      Tauw=Tauw/AmH2O
 17   CONTINUE
C
c-----------------------------------------------
c
c      Absorption from all other gases - Fundamentally revised in 2.9
c
c      Those in variable quantities due to pollution are treated in 
c      subroutines
c
      Tauz3=0.D00
      tautrc=0.
      taumix=0.
c
C**      Uniformly mixed gases
C
c      1. Oxygen (O2)
c
      TO2=1.
      TO2P=1.
      if(wvln.lt.627.0.or.WVLN.GT.1581.0)goto 801
 800  continue
      READ (22,*,END=801) wvlo,AO2
      if(abs(wvln-wvlo).gt.epsilm)goto 800
      IF(AO2.le.0.0)GOTO 801
      tauo2=AO2*AbO2
      TO2=exp(-(tauo2*AmO2))
      TO2P=exp(-(tauo2*Amdif))
 801  continue
C
c      2. Methane (CH4)
c
      TCH4=1.
      TCH4P=1.
      If(AbCH4.le.0.)goto 853
      if(wvln.lt.1617.0)goto 853
 852  continue
      READ (36,*,END=853) wvlo,ACH4
      if(abs(wvln-wvlo).gt.epsilm)goto 852
      IF(ACH4.le.0.0)GOTO 853
      Call GSCH4(ACH4,AbCH4,AmCH4,TCH4,TCH4P,amdif)
 853  continue
C
c      3. Carbon Monoxide (CO)
c
      TCO=1.
      TCOP=1.
      If(AbCO.le.0.)goto 855
      if(wvln.lt.2310.0.or.wvln.gt.2405.)goto 855
 854  continue
      READ (34,*,END=855) wvlo,ACO
      if(abs(wvln-wvlo).gt.epsilm)goto 854
      Call GSCO(ACO,AbCO,AmCO,TCO,TCOP,amdif)
 855  continue
C
c      4. Nitrous oxide (N2O)
c
      TN2O=1.
      TN2OP=1.
      if(wvln.lt.1950.0)goto 807
 806  continue
      READ (27,*,END=807) wvlo,AN2O
      if(abs(wvln-wvlo).gt.epsilm)goto 806
      IF(AN2O.le.0.0)GOTO 807
      TN2O=exp(-(AN2O*AbN2O*AmN2O))
      TN2OP=exp(-(AN2O*AbN2O*Amdif))
 807  continue
C
c      5. Carbon Dioxide (CO2)
c
      TCO2=1.
      TCO2P=1.
      if(wvln.lt.1036.0)goto 809
 808  continue
      READ (35,*,END=809) wvlo,ACO2
      if(abs(wvln-wvlo).gt.epsilm)goto 808
      IF(ACO2.le.0.0)GOTO 809
      tauco2=ACO2*AbCO2
      TCO2=exp(-(tauco2*AmCO2))
      TCO2P=exp(-(tauco2*Amdif))
 809  continue
C
c      6. Nitrogen (N2)
c
      TN2=1.
      TN2P=1.
      if(wvln.lt.3645.0)goto 811
 810  continue
      READ (26,*,END=811) wvlo,AN2
      if(abs(wvln-wvlo).gt.epsilm)goto 810
c      IF(AN2.le.0.0)GOTO 811
      TN2=exp(-(AN2*AbN2*AmN2))
      TN2P=exp(-(AN2*AbN2*Amdif))
 811  continue
C
c      7. Oxygen-oxygen (O2-O2 or O4) 
c
c      [Collision-induced absorption; also includes O2-N2]
c
      TO4=1.
      TO4P=1.
      if(WVLN.GT.1593.0)goto 827
 826  continue
      READ (25,*,END=827) wvlo,xsO4
      if(abs(wvln-wvlo).gt.epsilm)goto 826
      IF(xsO4.le.0.0)GOTO 827
      AO4=xsO4*1d-46
      TO4=exp(-AO4*AbO4*AmO2)
      TO4P=exp(-AO4*AbO4*Amdif)
 827  continue
c
c
C**      Misc. Trace gases
C
c      1. Nitric acid (HNO3)
c
      THNO3=1.
      THNO3P=1.
      If(AbHNO3.le.0.)goto 863
      if(wvln.gt.350.0)goto 863
 862  continue
      READ (31,*,END=863) wvlo,xsHNO3,athno3
      if(abs(wvln-wvlo).gt.epsilm)goto 862
      Call GSHNO3(234.2,xsHNO3,athno3,AbHNO3,AmHNO3,THNO3,
     1 THNO3P,amdif)
 863  continue
C
c      2. Nitrogen dioxide (NO2)
c
      TNO2=1.
      TNO2P=1.
      if(AbNO2.le.0.)goto 865
      if(wvln.gt.926.0)goto 865
 864  continue
      READ (29,*,END=865) wvlo,xsNO2,atno2
      if(abs(wvln-wvlo).gt.epsilm)goto 864
      Call GSNO2(TempN,xsNO2,atno2,AbNO2,AmNO2,TNO2,TNO2P,amdif)
 865  continue
C
c      3. Nitrogen trioxide (NO3)
c
      TNO3=1.
      TNO3P=1.
      if(AbNO3.le.0.)goto 867
      if(wvln.lt.400.0.or.wvln.gt.703.0)goto 867
 866  continue
      READ (30,*,END=867) wvlo,xsNO3,atno3
      if(abs(wvln-wvlo).gt.epsilm)goto 866
      Call GSNO3(TempN,xsNO3,atno3,AbNO3,AmNO3,TNO3,TNO3P,amdif)
 867  continue
C
c      4. Nitric oxide (NO)
c
      TNO=1.
      TNOP=1.
      If(AbNO.le.0.)goto 869
      if(wvln.lt.2645.0.or.wvln.gt.2745.)goto 869
 868  continue
      READ (28,*,END=869) wvlo,ANO
      if(abs(wvln-wvlo).gt.epsilm)goto 868
      Call GSNO(ANO,AbNO,AmNO,TNO,TNOP,amdif)
 869  continue
C
c      5a. Sulfur Dioxide (SO2) [UV band]
c
      TSO2=1.
      TSO2P=1.
      If(AbSO2.le.0.)goto 823
      if(wvln.gt.420.0)goto 821
 820  continue
      READ (32,*,END=821) wvlo,xsSO2,atso2
      if(abs(wvln-wvlo).gt.epsilm)goto 820
c      IF(xsSO2.le.0.0)GOTO 821
      Call GSSO2U(247.1,xsSO2,atso2,AbSO2,AmSO2,TSO2,TSO2P,amdif)
 821  continue
C
c      5b. Sulfur Dioxide (SO2) [IR band]
c
      if(wvln.lt.3955.0)goto 823
 822  continue
      READ (33,*,END=823) wvlo,ASO2
      if(abs(wvln-wvlo).gt.epsilm)goto 822
c      IF(ASO2.le.0.0)GOTO 823
      Call GSSO2I(ASO2,AbSO2,AmSO2,TSO2,TSO2P,amdif)      
 823      continue
C
c      6a. Ozone (O3) [UV and VIS bands]
c
      TO3=1.0D+00
      TXO3=TO3
      TAUZ3=0.D00
      IF(AbO3.le.0.0)GOTO 875
      if(wvln.gt.1091.)goto 872
 870  continue
      read(23,*,end=872)wvlo,xso3,a0o3,a1o3
      if(abs(wvln-wvlo).gt.epsilm)goto 870
c      if(xso3.le.0.)goto 872
      Tref=223.
      if(wvl.lt.345.)Tref=228.
      Call GSO3U(Tref,Tempo,xso3,a0o3,a1o3,Abo3,AmO3,tauz3,TxO3,xo3,
     1 AO3)
      goto 875
 872  continue
c
c      6b. Ozone (O3) [IR bands]
c
      if(wvln.lt.2470.)goto 875
 874  continue
      read(24,*,end=875)wvlo,AO3
      if(abs(wvln-wvlo).gt.epsilm)goto 874
      TxO3=exp(-AO3*AbO3*AmO3)
 875  continue
      TO3=Txo3
c
c      7. Ammonia (NH3)
c
      TNH3=1.
      TNH3P=1.
      if(wvln.lt.1900.0)goto 825
 824  continue
      READ (37,*,END=825) wvlo,ANH3
      if(abs(wvln-wvlo).gt.epsilm)goto 824
      IF(ANH3.le.0.0)GOTO 825
      TNH3=exp(-ANH3*AbNH3*AmNH3)
      TNH3P=exp(-ANH3*AbNH3*Amdif)
 825  continue
C
c      8. Bromine monoxide (BrO)
c
      TBrO=1.
      TBrOP=1.
      if(wvln.lt.296.5.or.WVLN.GT.384.5)goto 829
 828  continue
      READ (38,*,END=829) wvlo,xsBrO
      if(abs(wvln-wvlo).gt.epsilm)goto 828
      ABrO=xsBrO*NLosch
c      IF(ABrO.le.0.0)GOTO 829
      TBrO=exp(-ABrO*AbBrO*AmBrO)
      TBrOP=exp(-ABrO*AbBrO*Amdif)
 829  continue
C
c      9. Formaldehyde (CH2O)
c
      TCH2O=1.
      TCH2OP=1.
      If(AbCH2O.le.0.)goto 831
      if(WVLN.GT.400.0)goto 831
 830  continue
      READ (39,*,END=831) wvlo,xsCH2O,atCH2O
      if(abs(wvln-wvlo).gt.epsilm)goto 830
      Call GSCH2O(TK-24.,xsCH2O,atCH2O,AbCH2O,AmCH2O,
     1 TCH2O,TCH2OP,amdif)
 831  continue
C
c      10. Nitrous acid (HNO2)
c
      THNO2=1.
      THNO2P=1.
      If(AbHNO2.le.0.)goto 833
      if(wvln.lt.300.5.or.WVLN.GT.396.5)goto 833
 832  continue
      READ (40,*,END=833) wvlo,xsHNO2
      if(abs(wvln-wvlo).gt.epsilm)goto 832
      Call GSHNO2(xsHNO2,AbHNO2,AmHNO2,THNO2,THNO2P,amdif)
 833  continue
C
c      11. Chlorine nitrate (ClNO3)
c
      TClNO=1.
      TClNOP=1.
      TCl=230.
      if(WVLN.GT.432.0)goto 835
 834  continue
      READ (41,*,END=835) wvlo,xsClNO,a1tCl,a2tCl
      if(abs(wvln-wvlo).gt.epsilm)goto 834
      AClNO=xsClNO*(1.+a1tCl*(TCl-296.)+a2tCl*(TCl-296.)*(TCl-296.))*
     2 NLosch
c      IF(AClNO.le.0.0)GOTO 835
      TClNO=exp(-AClNO*AbClNO*AmClNO)
      TClNOP=exp(-AClNO*AbClNO*Amdif)
 835  continue
c
      if(iGas.ne.0) goto 891
c
c      Supplemental absorption due to non-standard conditions 
c      (e.g., pollution...)
c
c      2. Methane (CH4)
c
      if(wvln.lt.1617.0)goto 880
      Call GSCH4(ACH4,ApCH4,AmPOL,Tcor,TcorP,amdif)
      TCH4=Min(TCH4*tcor,1.)
      TCH4P=Min(TCH4P*tcorp,1.)
 880  continue
C
c      3. Carbon Monoxide (CO)
c
      if(wvln.lt.2310.0.or.wvln.gt.2405.)goto 881
      if(ApCO.le.0.0.and.AbCO.le.0.0)goto 881
      Call GSCO(ACO,ApCO,AmPOL,Tcor,TcorP,amdif)
      TCO=Min(TCO*tcor,1.)
      TCOP=Min(TCOP*tcorp,1.)
 881  continue
C
C**      Misc. Trace gases
C
c      1. Nitric acid (HNO3)
c
      if(wvln.gt.350.0)goto 882
      Call GSHNO3(TK,xsHNO3,athno3,ApHNO3,AmPOL,Tcor,TcorP,amdif)
      THNO3=Min(THNO3*tcor,1.)
      THNO3P=Min(THNO3P*tcorp,1.)
 882  continue
C
c      2. Nitrogen dioxide (NO2)
c
      if(wvln.gt.926.0)goto 883
      Call GSNO2(TK,xsNO2,atno2,ApNO2,AmPOL,Tcor,TcorP,amdif)
      TNO2=Min(TNO2*tcor,1.)
      TNO2P=Min(TNO2P*tcorp,1.)
 883  continue
C
c      3. Nitrogen trioxide (NO3)
c
      if(wvln.lt.400.0.or.wvln.gt.703.0)goto 884
      if(ApNO3.le.0.0.and.AbNO3.le.0.0)goto 884
      Call GSNO3(TK,xsno3,atno3,ApNO3,AmPOL,Tcor,TcorP,amdif)
      TNO3=Min(TNO3*tcor,1.)
      TNO3P=Min(TNO3P*tcorp,1.)
 884  continue
C
c      4. Nitric oxide (NO)
c
      if(wvln.lt.2645.0.or.wvln.gt.2745.)goto 885
      Call GSNO(ANO,ApNO,AmPOL,Tcor,TcorP,amdif)
      TNO=Min(TNO*tcor,1.)
      TNOP=Min(TNOP*tcorp,1.)
 885  continue
C
c      5a. Sulfur Dioxide (SO2) [UV band]
c
      if(wvln.gt.420.0)goto 886
      Call GSSO2U(TK,xsSO2,atso2,ApSO2,AmPOL,Tcor,TcorP,amdif)
      TSO2=Min(TSO2*tcor,1.)
      TSO2P=Min(TSO2P*tcorp,1.)
 886  continue
C
c      5b. Sulfur Dioxide (SO2) [IR band]
c
      if(wvln.lt.3955.0)goto 887
      Call GSSO2I(ASO2,ApSO2,AmPOL,Tcor,TcorP,amdif)
      TSO2=Min(TSO2*tcor,1.)
      TSO2P=Min(TSO2P*tcorp,1.)
 887  continue
C
c      6a. Ozone (O3) [UV and VIS bands]
c
      if(ApO3.le.0.0.and.AbO3.le.0.0)goto 889
      if(wvln.gt.1091.)goto 888
      Call GSO3U(Tref,TK,xso3,a0o3,a1o3,Apo3,AmPOL,tz3,Tcoro3,xo3,AO3)
      TO3=Min(TxO3*tcoro3,1.D+00)
      tauz3=Max(tauz3+tz3,0.D00)
 888  continue
c
c      6b. Ozone (O3) [IR band]
c
      if(wvln.lt.2470.)goto 889
      TO3=Min(TxO3*exp(-AO3*ApO3*AmPOL),1.D+00)
c      tauz3=Max(tauz3+ApO3*AO3,0.)
 889  continue
C
c      9. Formaldehyde (CH2O)
c
      if(WVLN.gt.400.0)goto 890
      if(ApCH2O.le.0.0.and.AbCH2O.le.0.0)goto 890
      Call GSCH2O(TK,xsCH2O,atCH2O,ApCH2O,AmPOL,Tcor,TcorP,amdif)
      TCH2O=Min(TCH2O*tcor,1.)
      TCH2OP=Min(TCH2OP*tcorp,1.)
 890  continue
C
c      10. Nitrous acid (HNO2)
c
      if(wvln.lt.300.5.or.WVLN.GT.396.5)goto 891
      if(ApHNO2.le.0.0.and.AbHNO2.le.0.0)goto 891
      Call GSHNO2(xsHNO2,ApHNO2,AmPOL,Tcor,TcorP,amdif)
      THNO2=Min(THNO2*tcor,1.)
      THNO2P=Min(THNO2P*tcorp,1.)
 891  continue
c
C
c      Total gaseous absorption excluding H2O and O3
c
c      Mixed gases
c
      Tmixd=TO2*TO4*TN2*TN2O*TCO*TCO2*TCH4
      TmixdP=TO2P*TO4P*TN2P*TN2OP*TCOP*TCO2P*TCH4P
      Trace=TNO*TNO2*TNO3*THNO3*TSO2*TNH3*TBrO*TCH2O*THNO2*TClNO
      TraceP=TNOP*TNO2P*TNO3P*THNO3P*TSO2P
     2 *TNH3P*TBrOP*TCH2OP*THNO2P*TClNOP 
      taumix=-log(Tmixd)/AmR
      tautrc=-log(Trace)/AmR
C
c-----------------------------------------------
c
C**      AEROSOL EXTINCTION
C
      TAUA=0.
      TAUAS=0.
      TAA=1.0D00
      TAAp=1.0D00
      TAS=1.0D00
      TAT=1.0D00
      IF(IAER.LE.0.OR.IAER.GT.7)GOTO 33
      OMEGL=BP0(IAER)+BP1(IAER)*WVL+BP2(IAER)*WVL2+BP3(IAER)*WVL3
      IF(wvln.LE.1999.)GOTO 33
      BQ=EXP(BQ1(IAER)*(WVL-BQ2(IAER)))
      OMEGL=1.-(BQ0(IAER)*BQ)/(1.+BQ)**2
      GOTO 34
 33   CONTINUE
C
C      BRASLAU & DAVE AEROSOL MODEL C1 OR D1
C
      IF(IAER.NE.8)GOTO 35
      OMEGL=.9441-.08817*EXP(1.-3.3815E-3*wvln)
      IF(wvln.GE.2001.)OMEGL=.8569+.0436E-3*wvln
 35   CONTINUE
C
C      BRASLAU & DAVE AEROSOL MODEL C OR D
C
      if(Iaer.ne.9)goto 36
      IF(IAER.EQ.9)OMEGL=1.0
      goto 34
c
c      Desert Aerosol (moderate) *** New in 2.9.3 ***
c
 36   continue
      if(Iaer.ne.10)goto 39
      omegl=.9
      if(wvln.gt.500.)omegl=.96
      goto 34
c
c      Desert Aerosol (dust storm) *** New in 2.9.3 ***
c
 39   continue
      if(Iaer.ne.11)goto 34
      omegl=.6
      if(wvln.gt.500.)omegl=.7
c
 34   CONTINUE
C
      IF(BETA.GT.0.0)GOTO 37
      OMEGL=1.0
      GOTO 38
 37   continue
      TAUA=BETA/WVL**ALPHA2
      IF(wvln.LE.499.) TAUA=BETAC/(WVL**ALPHA1)
      IF(wvln.GE.1001.0.AND.IAER.EQ.8)TAUA=BETA/WVL**.825
      if(iaer.ne.1.or.iref.ne.1)goto 777
      alpha=1.1696-4.6814*wvl+12.96*wvl2
      if(abs(wvl-0.3).lt.1e-4)alpha=.93178
      if(wvl.gt.0.3.and.wvl.lt.0.337)alpha=1.443-4.6051*wvl+9.6723*wvl2
      if(abs(wvl-0.337).lt.1e-4)alpha=.989
      if(wvl.gt.0.337.and.wvl.lt.0.55)alpha=.98264+.032539*wvl
     1 -.040251*wvl2
      if(abs(wvl-0.55).lt.1e-4)alpha=.98839
      if(wvl.gt.0.55.and.wvl.lt.0.694)alpha=-32.0108+151.02*wvl-229.75
     1 *wvl2+116.83*wvl3
      if(abs(wvl-0.694).lt.1e-4)alpha=1.192
      if(wvl.gt.0.694.and.wvl.lt.1.06)alpha=-1.9669+9.576*wvl-9.4345
     1 *wvl2+3.1621*wvl3
      if(abs(wvl-1.06).lt.1e-4)alpha=1.3485
      if(wvl.gt.1.06.and.wvl.le.1.536)alpha=-.25628+3.0677*wvl-1.9011
     1 *wvl2+.41005*wvl3
      if(wvl.gt.1.536.and.wvl.le.2.0)alpha=-1.3018+3.7405*wvl-1.6633
     1 *wvl2+.25856*wvl3
      if(wvl.gt.2.0.and.wvl.le.2.25)alpha=2.1665-.40189*wvl+.057873
     1 *wvl2
      if(wvl.gt.2.25.and.wvl.le.2.5)alpha=2.1188-.35073*wvl+.044553*wvl2
      if(wvl.gt.2.5.and.wvl.le.2.7)alpha=4.3108-1.5493*wvl+.17324*wvl2
      if(wvl.gt.2.7.and.wvl.le.3.0)alpha=2.1947-.33892*wvl+.015213*wvl2
      if(wvl.gt.3.0.and.wvl.le.3.39)alpha=-2.993+3.3795*wvl-.86713*wvl2
     1 +.073101*wvl3
      if(wvl.gt.3.39.and.wvl.le.3.75)alpha=1.6801-.12171*wvl+.0068994
     1 *wvl2
      if(wvl.gt.3.75)alpha=2.0473-.27977*wvl+.022939*wvl2
      Taua=Tau5/(2.*wvl)**alpha
 777  continue
      TAUAS=OMEGL*TAUA
      Tauaa=Taua-Tauas
      TAS=EXP(-TAUAS*AmAER)
      TAT=EXP(-TAUA*AmAER)
      TAA=EXP(-TAUAA*AmAER)
 38   CONTINUE
C
c-----------------------------------------------
c
C      BEAM RADIATION
C
      TSCAT=TR*TAS
      TABS0=TH2O*Tmixd*Trace*TAA
      TAAp=EXP(-TAUAA*Amdif)
      TABS0P=TH2OP*TmixdP*TraceP*TAAP
      TABS=TABS0*TO3
      TDIR=TABS*TSCAT
      DIR=H0*TDIR
      DIRH=DIR*ZCOS
      H0H=H0*ZCOS
      FHTO=1.
      FHT1=1.D00
      IF(TAUZ3.gt.5d-6)FHTO=EXP(-FHTcz-FHTdx*(TAUZ3-2.D00))
c
c      corrected in 2.9.2
c      
      IF(tauz3.LE.2.D00)FHTO=EXP(-FHTcx*TAUZ3-FHTcy*(TAUZ3**.95))
c
c      Improved multiple scattering algorithm--New in 2.9.3, revised in 2.9.5
c
      
      Fda00=1.
      Fdazt=1.
c      Fdif=1.
      if(t5.le.0.03)goto 3909
      ssaro=Taurl/(Taurl+Tauz3)
      Taurf=(ssaro**.5)*Taurl*Taurl
      Fda00=(trb0+trb1*Taurf)/(1.+trb2*(Taurf**.5))
      if(wvln.gt.294.)Fdazt=tzb0+tzb1*Taurf+tzb2*Taurf**.5
      
 3909 continue
      if(wvln.gt.400.)goto 3900
      Fda00=1.
      if(wvln.le.294.0.or.t5.le.0.03)goto 3908     
      Fda00=(tra0+tra1*Taurf)/(1.+tra2*Taurf)
 3908 continue
      FHT1=.962-9.1*tauz3
      if(Tauz3.le.0.01D00)goto 3900
      if(Tauz3.le.22.5D00)goto 3907
      FHT1=Min(12.D00,FHTa0+Tauz3*FHTa1)
      if(Amo3.le.2.0)goto 3900
      FHT1=Min(12.D00,FHTb0+Tauz3*FHTb1)
      goto 3900
 3907 continue
      if(Tauz3.le.15.5D00)goto 3906
      FHT1=Min(7.D00,FHTc0+Tauz3*FHTc1)
      if(Amo3.le.1.6)goto 3900
      FHT1=Min(6.D00,FHTd0+Tauz3*FHTd1)
      goto 3900
 3906 continue
      if(Tauz3.le.10.D00)goto 3905
      FHT1=Min(9.D00,FHTe0+Tauz3*FHTe1)
      if(Amo3.le.1.9)goto 3900
      FHT1=Min(8.D00,FHTf0+Tauz3*FHTf1)
      goto 3900
 3905 continue
      if(Tauz3.le.6.0D00)goto 3904
      ra0=ra00
      if(Amo3.gt.2.2)ra0=ra01
      if(Amo3.le.2.6)goto 3910
      ra1=ra10
      goto 3912
 3910 continue
      if(Amo3.le.1.72)goto 3914
      ra1=ra11
      goto 3912
 3914 continue
      ra1=ra12
 3912 continue
      FHT1=Min(10.D00,ra0+(Tauz3-6.D00)*ra1)
      goto 3900
 3904 continue
c 
      if(Tauz3.le.1.0D00)goto 3902
      if(Amo3.gt.3.2)goto 3915
      ra0=ra02
      ra1=ra13
      FHT1=Min(2.D00,ra0+Tauz3*ra1)
      FHT1x=Min(2.,ra0+2.505*ra1)
      goto 3903
 3915 continue
      ra0=ra03
      ra1=ra14
      FHT1=Min(2.D00,ra0+Tauz3*ra1)
      FHT1x=Min(2.,ra0+2.505*ra1)
 3903 continue 
      if(Tauz3.le.2.505D00)goto 3900
      ra0=FHT1x
      if(Amo3.gt.3.5)ra0=ra04
      ra1=ra15
      if(Amo3.gt.2.4)ra1=ra16
      FHT1=Min(7.5D00,ra0+(Tauz3-2.505D00)*ra1)
       goto 3900
 3902 continue 
      if(Tauz3.le.0.1D00)goto 3901
      xlim=1.D00
      if(Amo3.gt.2.)xlim=1.6D00
      FHT1=Min(xlim,ra05+Tauz3*ra17)
      goto 3900
 3901 continue
      FHT1=Min(1.D00,ra06+Tauz3*ra18)
 3900 continue
      FHT1=Max(FHT1,2.D-03)
      if(Tauz3.lt.5.0D00)FHT1=Max(FHT1,5.D-01)
      FHTO=FHTO/FHT1
      HT=H0H
      IF(Zenit.ge.89.)HT=H0/AmR
      HTa=HT*TABS0p*FHTO
C
c--------------------------------------------------
c
C      Diffuse radiation (improved in 2.9.5)
C
c--------------------------------------------------
c
C      Asymmetry and forward scatterance
C
      if(IAER.ne.10)goto 40
      GG=.71
      if(wvln.gt.500.)gg=.675
 40   continue
      if(IAER.ne.11)goto 44
      GG=.89
      if(wvln.gt.500.)gg=.85
 44   continue
      IF(IAER.NE.8.AND.IAER.NE.9)GOTO 42
      GG=0.8042
      GOTO 43
 42   CONTINUE
      IF(IAER.LE.0.OR.IAER.GT.7)GOTO 43
      GG=AG0(IAER)+AG1(IAER)*WVL+AG2(IAER)*WVL2+AG3(IAER)
     % *WVL3+AG4(IAER)*WVL4
 43   CONTINUE
      GG=MIN(0.99,GG)
      ALG=log(1.-GG)
      AFS=ALG*(1.459+ALG*(.1595+ALG*.4129))
      BFS=ALG*(.0783+ALG*(-.3824-ALG*.5874))
      FA1=1.-.5*EXP((AFS+BFS*ZCOS)*ZCOS)
      FA1P=1.-.5*EXP((AFS+BFS*.6)*.6)
C
C**      1. DIFFUSE RADIATION FROM RAYLEIGH SCATTERING
C
      DRAY=0.D00
      FR=0.5
      FRP=0.5
      IF(TAURL.GE.EPSIR)FR=.5*EXP(-((TAURL-EPSIR)/SIGMAR)**EXPR)
      IF(TAURL.GE.EPSIRP)FRP=.5*EXP(-.1957*(TAURL-0.0648)**1.32)
 41   CONTINUE
      DRAY=HTa*FR*(1.-TR)*(Max(Tas,1.d-10)**.167)
c
C
C      2. DIFFUSE RADIATION FROM AEROSOL SCATTERING
C
      DAER=0.D00
      IF(BETA.gt.0.0)DAER=HTa*FA1*(TR**.167)*(1.-TAS)*Fda00*Fdazt
C
C      Sky diffuse before backscattering
C
      Fdifz=1.
      Fdiftz=1.
      if(wvln.gt.294.)goto 7200
      Fdifz=fdifa0+fdifa1*wvl
      if(zenit.ge.45.)Fdiftz=fdifb0+fdifb1*Ama1+fdifb2*Ama2
 7200 continue
      DIF0=Fdifz*Fdiftz*(DRAY+DAER)
      Glob0=Dirh+Dif0
C
C      3. BACKSCATTERING - reflection from ground to space and back
C
      TRP=EXP(-Amdif*TAURL)
      TAUAP=TAUA*Amdif
      TASP=EXP(-OMEGL*TAUAP)
      TTp5=Tabs0p**.5
      GAMOZ=1.
      IF(wvln.LE.379.5)GAMOZ=EXP(-1D+5*(4.8344+23.088*(AbO3+ApO3))
     1 *(.38-WVL)**5.8)
       Rhob0=Rhox
      Rhob=Rhox
      Rhod=Rhox
      Rhor=0.D00
      Rhoa=0.
      Rhos=0.D00
      Roro=0.
      Dgrnd=0.D00
      If(Ialbdx.lt.0)goto 411
      rocb=0.
      if(Glob0.gt.0.D00)rocb=Dirh/Glob0
      Call Albdos(Ialbdx,Nwal1,Albdo1,Wvla1,Wvl,Zenit,Zcos,Rhob0,Rhob,
     1 Rhod)
 411  continue
      Rho=Max(Rhob,Rhod)
      If(TTP5.le.1D-12) goto 413
      RHOR=TTP5*((1.-FRP)**.85)*(TASP**.05)*(1.-TRP)*GAMOZ
c
c     New in 2.9.4/revised in 2.9.5
c
      Fatau=0.
      if(tau550.lt.0.03)goto 3999      
      if(wvl.gt.0.35)goto 3992
      
      Fatau=exp(alba00+alba01*wvl)
      if(t5.le.0.2)goto 3998
      
      Fatau=exp(alba0+alba1*wvl+alba2*wvl2)
      goto 3998
 3992 continue
      if(wvl.gt.0.5)goto 3994
      
      Fatau=exp(albb0+albb1*wvl+albb2*wvl2)
      goto 3998
 3994 continue
      
      Fatau=(albc0+albc1*wvl+albc2*wvl2+albc3*wvl3)/(1.+albc4*wvl)
 3998 continue
c
      RHOA=TTP5*((1.-FA1P)**.85)*GAMOZ*Fatau
 3999 continue
      RHOS=RHOR+RHOA
      If(Ialbdx.ge.0)Rho=Rhob*rocb+Rhod*(1.-rocb)
      RORO=RHO*RHOS
      Upward=Rho*Glob0
      DGRND=Upward*RHOS/(1.-RORO)
 413  continue
c
      DIF=DIF0+DGRND
      GLOB=DIRH+DIF
C
c-----------------------------------------------
c
c      UV calculations
c
      IF(IUV.EQ.1.AND.wvln.LE.400.)CALL UVDAT(ER0,ER1,ER2,
     # ER3,ER4,DNA,PHO,ECAL,ACG,POL,SIS,PRT,SCUPH,SCUPM,wvln)
      GERY0=GLOB*ER0
      GERY1=GLOB*ER1
      GERY2=GLOB*ER2
      GERY3=GLOB*ER3
      GERY4=GLOB*ER4
      GDNA=GLOB*DNA
      GECAL=GLOB*ECAL
      GPHO=GLOB*PHO
      GACG=GLOB*ACG
      GPOL=GLOB*POL
      GSIS=GLOB*SIS
      GPRT=GLOB*PRT
      GSCUPH=GLOB*SCUPH
      GSCUPM=GLOB*SCUPM
C
c-----------------------------------------------
c
C**      CIRCUMSOLAR CORRECTION FOR A SIMULATED RADIOMETER
C
      direxp=dir
      difexp=dif
      difcc=0.
      IF(ICIRC.le.0)GOTO 77
      call Circum (Difccs,Iaer,Icirc)
c
c      Correction for the diminished aureole close to the horizon
c
      cexp=1.
      if(Zenit.le.(90.-apert))goto 76
      zexp=2.*acos((pi2-zr)/(apert*rpd))
      cexp=1.-.5*(zexp-sin(zexp))/pinb
 76   continue
      difexp=Max(0.1D00*dif,dif-cexp*difccs*zcos)
      difcc=(Dif-Difexp)/(cexp*zcos)
      direxp=dir+difcc
 77   CONTINUE
C
c-----------------------------------------------
C
C**            CALCULATION ON INCLINED PLANES
C
C
      Dirs=Dirh
      Difs=Dif
      Globs=Glob
      If(Itilt.eq.0)goto 421
      DIRS=RBN*DIR
      RDD=RD
      AerRay=0.5
      if(TauRl.gt.0.)AerRay=TauAs/TauRl
      IF(AerRay.le.0.5)RDD=RDI
      IF(AerRay.gt.0.5.and.AerRay.lt.1.5)RDD=
     2 (AerRay-.5)*RD+(1.5-AerRay)*RDI
      DIFS=RDD*DIF
      Rog=Rhog
      If(Ialbdg.lt.0)goto 429
      If(Ialbdg.ne.Ialbdx)goto 427
      Call Albdos(Ialbdg,Nwal1,Albdo1,Wvla1,Wvl,Zenit,Zcos,Rob0,Rob,Rod)
      Goto 428
 427  Continue

      Call Albdos(Ialbdg,Nwal2,Albdo2,Wvla2,Wvl,Zenit,Zcos,Rob0,Rob,Rod)
c      
 428  Continue
      Rog=Max(Rob,Rod)
      If(TTP5.le.1D-12) goto 429      
      if(Glob.gt.0.D00)Rog=(Rod*Dif+(Rob0+(Rob-Rob0)*Abs(cos(swa)))
     1 *Dirh)/Glob
 429  Continue
      GRNS=Rog*RG*GLOB
      GLOBS=DIRS+DIFS+GRNS
c      Corrected in 2.9.3
      DIFS=GLOBS-DIRS
c
 421  continue
C
c-----------------------------------------------
C
C*      PHOTON FLUX per wavelength (cm-2 s-1 nm-1) [modified in 2.9.3]
C
      WPHT=1.0E-10*WVL*PHOT
      PFG=GLOB*WPHT
      PFB=DIR*WPHT
      PFD=DIF*WPHT
      PFGS=GlobS*WPHT
C
c-----------------------------------------------
C
C*      PHOTON FLUX per eV energy (cm-2 s-1 eV-1) [new in 2.9.3]
C
      PhoteV=1.0D+06/(PHOT*WVL*evolt)
      Wvphot=1000.*WVL/PhoteV
      PFGeV=PFG*WvPhot
      PFBeV=PFB*WvPhot
      PFDeV=PFD*WvPhot
      PFGSeV=PFGS*WvPhot
c
c------------------------------------------------
c
c     Photosynthetic photon flux density [new in 2.9.3]
c
      PPFcst=Wvl*Phot/Avogad
      PPFG=PPFcst*Glob
      PPFGS=PPFcst*GlobS
      PPFB=PPFcst*Dir
      PPFD=PPFcst*Dif
C
C================================================
C
C      TOTAL IRRADIANCE (W/m2) using a trapezoidal rule
C
      BNORM(IWVL)=DIR
      GLOBH(IWVL)=GLOB
      GLOBT(IWVL)=GLOBS
      DIRX(IWVL)=DIREXP
      Etspct(IWVL)=h0
      CALL STEPS(wvln,wlmn,wlmx,DWVL)
      DIRWL=DIR*DWVL
      difwl=Dif*DWVL
      dirxwl=direxp*dwvl
      difxwl=difexp*dwvl
      glbswl=Globs*Dwvl
      H0WL=H0*DWVL
      SUM0=SUM0+H0WL
      SUMBN=SUMBN+DIRWL
      SUMD=SUMD+(DIF*DWVL)
      SUMD0=SUMD0+(Dif0*DWVL)
      SUMG=SUMG+(GLOB*DWVL)
      SUMBS=SUMBS+(DIRS*DWVL)
      SUMDS=SUMDS+(DIFS*DWVL)
      SUMRS=SUMRS+(GRNS*DWVL)
      SUMGS=SUMGS+glbswl
      sumbx=sumbx+dirxwl
      sumdx=sumdx+difxwl
C
c-----------------------------------------------
C
C      ILLUMINANCES and PAR [revised in 2.9.3]
C
      IF(ILLUM.EQ.0)GOTO 857
      IF(wvln.LT.359.0.OR.wvln.GT.830.)GOTO 857
      JV=Int(WVLN)-317
      if(wvln.le.400.)JV=Int(2.*(WVLN-358.45))
      XILLUM=XILLUM+vl(jv)*h0wl
      BILLUM=BILLUM+VL(JV)*DIRwl
      DILLUM=DILLUM+VL(JV)*DIFwl
      SILLUM=SILLUM+VL(JV)*glbswl
      billx=billx+vl(jv)*dirxwl
      dillx=dillx+vl(jv)*difxwl
      IF(wvln.LT.400.0.OR.wvln.GT.700.)GOTO 857
      PARg=PARg+Glob*DWVL
      PARgs=PARgs+Globs*DWVL
      PARb=PARb+Dir*DWVL
      PARd=PARd+Dif*DWVL
      PPFDg=PPFDg+PPFG*DWVL
      PPFDgs=PPFDgs+PPFGs*DWVL
      PPFDb=PPFDb+PPFB*DWVL
      PPFDd=PPFDd+PPFD*DWVL
C
 857  CONTINUE
C
c-----------------------------------------------
C
C      UV IRRADIANCES AND DOSES
C
      IF(IUV.NE.1)GOTO 861
      IF(wvln.GT.400.)GOTO 861
      DWVLU=0.5
      IF(wvln.LE.280.0.OR.wvln.GE.400.)DWVLU=0.25
      SERY0=SERY0+(GERY0*DWVLU)
      SERY1=SERY1+(GERY1*DWVLU)
      SERY2=SERY2+(GERY2*DWVLU)
      SERY3=SERY3+(GERY3*DWVLU)
      SERY4=SERY4+(GERY4*DWVLU)
      SDNA=SDNA+(GDNA*DWVLU)
      SECAL=SECAL+(GECAL*DWVLU)
      SPHO=SPHO+(GPHO*DWVLU)
      SACG=SACG+(GACG*DWVLU)
      SPOL=SPOL+(GPOL*DWVLU)
      SSIS=SSIS+(GSIS*DWVLU)
      SPRT=SPRT+(GPRT*DWVLU)
      SSCUPH=SSCUPH+(GSCUPH*DWVLU)
      SSCUPM=SSCUPM+(GSCUPM*DWVLU)
      IF(wvln.GT.320.)GOTO 858
      IF(abs(wvln-320.).lt.0.01)DWVLU=0.25
      SUVB2=SUVB2+(GLOB*DWVLU)
      GOTO 859
 858  continue
      SUVA2=SUVA2+(GLOB*DWVLU)
 859  CONTINUE
      IF(wvln.GT.315.)GOTO 860
      IF(abs(wvln-315.).lt.0.01)DWVLU=0.25
      SUVB1=SUVB1+(GLOB*DWVLU)
      GOTO 861
 860  continue
      IF(abs(wvln-320.).lt.0.01)DWVLU=0.5
      SUVA1=SUVA1+(GLOB*DWVLU)
 861  CONTINUE
C
c-----------------------------------------------
c
C      OUTPUT TO FILE 16 (formatted text)
C
      NWMX=IWVL
      IF(IPRT.EQ.0)GOTO 953
      DWP=WVLN-WPMN
      KW=0.
      IF(DWP.GT.0.)KW=MOD(DWP,INTVL)
      IF(abs(KW).gt.1e-4)GOTO 953
      IF(wvln.LT.WPMN.OR.wvln.GT.WPMX)GOTO 953
      IF(IPRT.ne.1.and.IPRT.ne.3)goto 426
      If(Itilt.ne.0.or.Icirc.ne.0)goto 422
      WRITE(16,128,iostat=ierr6)WVLN,H0,real(DIR),Real(DirH),Real(Dif),
     1 Real(Glob),Real(DRay),Real(DAer)
 128  FORMAT(f7.1,1X,F7.3,2X,7(E11.5,1X))
      goto 426
 422  continue
      IF(Itilt.ne.0.or.Icirc.ne.1)goto 423
      WRITE(16,120,iostat=ierr37)WVLN,H0,real(DIR),Real(Dif),Real(Glob),
     1 Real(DirExp),Real(DifExp),Real(DRay),Real(DAer)
 120  FORMAT(f7.1,1X,F7.3,2X,3(E11.5,1X),2X,5(E11.5,1X))
      goto 426
 423  continue
      IF(Itilt.ne.1.or.Icirc.ne.0)goto 424
      WRITE(16,124,iostat=ierr38)WVLN,H0,real(DIR),Real(Dif),Real(Glob),
     1 Real(DifS),Real(GlobS)
 124  FORMAT(f7.1,1X,F7.3,2X,3(E11.5,1X),2X,2(E11.5,1X))
      GOTO 426
 424  CONTINUE
      WRITE(16,125,iostat=ierr39)WVLN,H0,real(DIR),Real(Dif),Real(Glob),
     1 Real(DirExp),Real(DifS),Real(GlobS)
 125  FORMAT(f7.1,1X,F7.3,2X,3(E11.5,1X),2X,E11.5,3X,2(E11.5,1X))
 426  continue
      IF(IPRT.ne.2.and.IPRT.ne.3)goto 953
c
c===============================================
C
C      OUTPUT TO FILE 17 (text file in spreadsheet format)
C
      Output(1)=h0
      Output(2)=real(dir)
      Output(3)=Real(Dif)
      Output(4)=Real(Glob)
      Output(5)=Real(DirH)
      Output(6)=Real(DirS)
      Output(7)=Real(DifS)
      Output(8)=Real(GlobS)
      Output(9)=Real(DirExp)
      Output(10)=Real(DifExp)
      Output(11)=difcc
      Output(12)=Real(Pfgs)
      Output(13)=Real(Pfb)
      Output(14)=Real(Pfd)
      Output(15)=tr
      Output(16)=real(TO3)
      Output(17)=Real(Trace)
      Output(18)=Real(TH2O)
      Output(19)=Real(Tmixd)
      Output(20)=Real(Tat)
      Output(21)=Real(TDir)
      Output(22)=taurl
      Output(23)=real(tauz3)
      Output(24)=tautrc
      Output(25)=Real(Tauw)
      Output(26)=taumix
      Output(27)=taua
      Output(28)=omegl
      Output(29)=gg
      Output(30)=rho
      Output(31)=rog
      Output(32)=Real(RhoS)
      Output(33)=Real(Grns)
      Output(34)=Upward
      Output(35)=Real(PPFg)
      Output(36)=Real(PPFb)
      Output(37)=Real(PPFd)
      Output(38)=Real(PPFgs)
      Output(39)=Real(PhoteV)
      Output(40)=Real(PFGeV)
      Output(41)=Real(PFbeV)
      Output(42)=Real(PFdeV)
      Output(43)=Real(PFGseV)
c      
      Output(46)=Real(DRay)
      Output(47)=Real(DAer)
      Output(48)=Real(Dif0)
      Output(49)=Real(Dgrnd)
      Output(50)=Real(rhor)
      Output(51)=rhoa
c
      do 457 io=1,IOTOT
      jo=IOUT(io)
      Xout(io)=Output(jo)
 457  continue
      WRITE(17,121,iostat=ierr40)WVLN,(Xout(io),io=1,IOTOT)
 121  FORMAT(e9.4,50(1X,e9.4))
 953  CONTINUE
      GOTO 15
 999  CONTINUE
C
c-----------------------------------------------
c
C***      END OF SPECTRAL CALCULATIONS
C
c-----------------------------------------------
c-----------------------------------------------
c
      SUMB=SUMBN*ZCOS
C
c
c      Illuminance, Luminous efficacy
C
      IF(ILLUM.EQ.0)GOTO 960
      XILLM=.683*XILLUM
      BILLM=.683*BILLUM
      DILLM=.683*DILLUM
      GILLM=BILLM*ZCOS+DILLM
      SILLM=.683*SILLUM
      BILLMX=.683*BILLX
      DILLMX=.683*DILLX
      EFF0=1000.*XILLM/ESCC
      EFFB=0.
      EFFD=0.
      EFFG=0.
      EFFS=0.
      EFFBX=0.
      EFFDX=0.
      IF(SUMBN.GT.0.D00)EFFB=1000.*BILLM/SUMBN
      IF(SUMD.GT.0.D00)EFFD=1000.*DILLM/SUMD
      IF(SUMG.GT.0.)EFFG=1000.*GILLM/SUMG
      IF(SUMGS.GT.0.)EFFS=1000.*SILLM/SUMGS
      IF(SUMBX.GT.0.D00)EFFBX=1000.*BILLMX/SUMBX
      IF(SUMDX.GT.0.D00)EFFDX=1000.*DILLMX/SUMDX      
 960   CONTINUE
 968   continue
C
c-----------------------------------------------
C
c      UV dose & index
C
C      UV DOSE IN MED/hour TO SIMULATE A ROBERTSON-BERGER MEASUREMENT
C      (1 MED = 210 J/m2 or 1 MED/HOUR = 58.33E-3 W/m2 with the
C      CIE Erythema action curve convolved to the irradiance)
C
      DOSE=SERY0/.05833
C
c      UV index, defined as 40*Erythemally weighted irradiance
c
      UVindx=Sery0*40.
c
c-----------------------------------------------
c
c      Daily calculations for Imass=4
c
      if(imass.ne.4)goto 894
      Hbnx=Hbnx+Sumbx
      Hbhx=Hbhx+(Sumbx*zcos)
      Hdx=Hdx+Sumdx
      Hglob=Hglob+Sumg
      Hglobs=Hglobs+Sumgs
      if(iUV.eq.1)Hdose=Hdose+dose
      if(illum.eq.0)goto 899
      Hiext=xillm
      Hibx=Hibx+billmx
      Hidx=Hidx+dillmx
      Hig=Hig+gillm
      His=His+sillm
      goto 899
 894  continue
c
c-----------------------------------------------
c-----------------------------------------------
c
c      Print results
c
c-----------------------------------------------
C      
      WRITE(16,122,iostat=ierr60) wlmn,wlmx,NWMX,SUM0,Real(SumBn),
     1 Real(SumBn)/sum0,SUMB,Real(SumD),SUMG,sumg/Escc
 122  FORMAT(/,'Wavelength Range = '
     % ,f6.1,' to ',f6.1,' nm;',2X,'Number of Wavelengths = ',I5,
     % //,'*** BROADBAND IRRADIANCES (W/m2):',//,'* DIRECT BEAM AT ',
     3 'NORMAL INCIDENCE:',/,2x,'Extraterrestrial = ',F7.2,3X,
     % 'Terrestrial = ',F7.2,3x,'Atmospheric Transmittance = ',f6.4,//,
     2 '* FOR THE HORIZONTAL PLANE:',/,2x,
     6 'Direct Beam = ',F7.2,'   Diffuse = ',F6.2,'   Global = ',F7.2,
     7 '   Clearness index, KT = ',f6.4)
      WRITE(16,135,iostat=ierr62)Real(SUMD0),Real(SumD-SUMD0)
 135  FORMAT('  Diffuse irradiance origination details:',/,
     1 '   Sky diffuse = ',F6.2,'   Back-scattered diffuse = ',F6.2,/)
C
      IF(ITILT.le.0)goto 6010
      WRITE(16,123,iostat=ierr64)SUMBS,SUMDS,SUMRS,SUMGS
 123  FORMAT( '* FOR THE TILTED PLANE: ',/,'  Direct Beam = ',F7.2,
     % '   Sky Diffuse = ',F6.2,'   Ground Reflected = ',F6.2,
     # '   Global = ',F7.2,/)
 6010 continue
c
      IF(ICIRC.le.0)goto 6012
      WRITE(16,137,iostat=ierr66)Real(SumBx),Real(SumDx),sumg
 137  FORMAT('* EXPERIMENTAL (WITH CIRCUMSOLAR CORRECTION):',/,2x,
     # 'Direct Beam, Normal Incidence = ',f7.2,'   Diffuse Horizontal ',
     2 '= ',F6.2,'   Global Horizontal = ',F7.2,/)
 6012 continue
c
      IF(ILLUM.eq.0)goto 779
      WRITE(16,188,iostat=ierr68)CIEYr,XILLM,BILLM,DILLM,GILLM,SILLM,
     2  PARb,PARd,PARg,PARgs,PPFDb,PPFDd,PPFDg,PPFDgs
 188  FORMAT(///,'*** ILLUMINANCES (klux) obtained with the Vlambda'
     2 ,' curve from CIE 19',i2,':',/,' E.T. = ',F6.2,2x,
     % 'BEAM NORMAL = ',F6.2,'  DIFFUSE HORIZONTAL = ',F7.2,'  GLOBAL'
     4,' HORIZONTAL = ',F7.2,'  GLOBAL TILT = ',F7.2,//,'*** PHOTOSY',
     5 'NTHETIC IRRADIANCE (W/m2) for 400-700 nm:',/,' BEAM NORMAL =',
     6 1x,f6.2,'  DIFFUSE HORIZONTAL = ',f6.2,'  GLOBAL HORIZONTAL =',
     7 1x,f6.2,'  GLOBAL TILT = ',f6.2,//,'*** PHOTOSYNTHETIC PHOTON '
     8 ,'FLUX DENSITY (µmol m-2 s-1) for 400-700 nm:',/,' BEAM NORMAL'
     6 ,' = ',f6.1,'  DIFFUSE HORIZONTAL = ',f6.1,
     7 '  GLOBAL HORIZONTAL = ',f6.1,'  GLOBAL TILT = ',f6.1,/)
      IF(ICIRC.ne.1.and.ICIRC.ne.2)goto 779
      WRITE(16,189,iostat=ierr70)BILLMX,DILLMX
 189  FORMAT(6x,'ILLUMINANCES WITH CIRCUMSOLAR CORRECTION (klux):',/,
     & 7x,'BEAM NORMAL = ',F6.2,'   DIFFUSE = ',F7.2,/)
 779  continue
      IF(abs(ILLUM).ne.2)goto 6014
      WRITE(16,180,iostat=ierr72)EFF0,EFFB,EFFD,EFFG,EFFS
 180  FORMAT('*** LUMINOUS EFFICACY (lm/W):',/,
     & ' E.T. = ',F6.2,'   BEAM = ',F7.3, '   DIFFUSE = ',
     % F7.3,'   GLOBAL = ',F7.3,'   GLOBAL TILT = ',F7.3,/)
      IF(ICIRC.EQ.1.or.ICIRC.EQ.2)WRITE(16,181,iostat=ierr74)
     1 EFFBX,EFFDX
 181  FORMAT(6x,'LUMINOUS EFFICACY WITH CIRCUMSOLAR CORRECTION (lm/W)'
     & ,':',/,7x,'BEAM = ',F7.3, '   DIFFUSE = ',F7.3,/)
 6014 continue
c
      IF(IUV.ne.1)goto 778
      WRITE(16,143,iostat=ierr76)SUVA1,SUVB1,SUVA2,SUVB2,SERY0,SERY1,
     @ SERY2,SERY4,SDNA,SPHO,SACG,SECAL,SPOL,SSIS,SPRT,SSCUPH,
     & SSCUPM
      WRITE(16,144,iostat=ierr78)DOSE,UVindx
 778  continue
 143  FORMAT(///,'*** UV IRRADIANCES (W/m2):',//,' TOTAL UV-A IRRADIAN'
     1,'CE (315-400 nm) = ',F9.5,/,' TOTAL UV-B IRRADIANCE (280-315 nm'
     #,') = ',F8.5,//,' TOTAL UV-A IRRADIANCE (320-400 nm) = ',F9.5,/,
     #' TOTAL UV-B IRRADIANCE (280-320 nm) = ',F8.5,///,'*** ACTION-WE'
     % ,'IGHTED DOSE RATES (W/m2) USING SELECTED ACTION CURVES',//,' E'
     % ,'RYTHEMAL C.I.E. (McKinley & Diffey, 1987) = ',E10.4,/,' ERYT',
     % 'HEMAL from Green et al. (1974) = ',E10.4,/,' ERYTHEMAL from ',
     # 'Green et al. (1975) = ',E10.4,/,' ERYTHEMAL from ',
     #'Diffey (1982) modified by Bjorn (1989) = ',E10.4,/,' DNA DAMA',
     9 'GE from Setlow (1974) = ',E10.4,/,' PHOTOSYNTHESIS INHIBITION'
     5 ,' from Caldwell et al. (1986) = ',E10.4,/,' ACGIH SAFETY ',
     %'SPECTRUM from Wester (1981) = ',E10.4,/,' BIOLOGICAL ACTION '
     # ,'from Caldwell (1971) and Green (1974) = ',E10.4,/,' POLYCHRO'
     %,'MATIC ACTION FOR HIGHER PLANTS from Caldwell et al. (1986) = ',
     # E10.4,/,' SYSTEMIC IMMUNOSUPPRESSION from deFabo et al. (1990)
     % = ',E10.4,/,' DNA TO PROTEIN CROSSLINKS from Peak & Peak ',
     %'(1986) = ',E10.4,/,' SKIN CARCINOGENESIS from deGruijl & Van'
     & ,'derLeun (1994) = ',E10.4,' (humans); ',E10.4,' (mice)',//)
 144  FORMAT('*** DOSE RATE IN MED/h from an ideal Robertson-Berger',
     % ' instrument = ',E10.4,///,'*** UV Index = ',f7.3,//)
C
C-----------------------------------
c
c      Postprocessor: scan and smooth irradiance results if ISCAN=1
c
      IF(ISCAN.EQ.0)GOTO 898
      filter='Gaussian'
      if(ifilt.eq.0)FILTER='Triangular'
      WRITE(18,155,iostat=ierr80)FWHM,step,filter
 155  FORMAT(' SMOOTHED RESULTS TO SIMULATE A ',F5.2,' nm FWHM',
     # ' INSTRUMENT AND A WAVELENGTH STEP OF ',f4.1,' nm,',/,
     2 ' Shape selected: ',A12)
      WRITE(18,193)
 193  FORMAT('   WVLGTH',2X,'ET_SPCTRUM  BEAM_NORMAL',
     2 ' BEAM_NORM+  GLOB_HORIZ  GLOBL_TILT',/)
      IF(IFILT.NE.1)GOTO 91
      sigma2=fwhm*fwhm/5.5451774
      fk=fwhm*2.
      CALL ScanG(step,FWHM,fk,sigma2,Nwmx)
      GOTO 92
 91   CONTINUE
      fk=fwhm
      CALL ScanT(step,FWHM,fk,Nwmx)
 92   CONTINUE
C
      GOTO 898
 998  CONTINUE
c
      CLOSE (UNIT=14,STATUS='KEEP')
      CLOSE (UNIT=15,STATUS='KEEP')
      CLOSE (UNIT=16,STATUS='KEEP')
      IF(IPRT.ge.2)CLOSE (UNIT=17,STATUS='KEEP')
      IF(Iscan.eq.1)CLOSE (UNIT=18,STATUS='KEEP')
      CLOSE (UNIT=20,STATUS='KEEP')
      IF(w.gt.0.)CLOSE (UNIT=21,STATUS='KEEP')
      CLOSE (UNIT=22,STATUS='KEEP')
      IF(AbO3.gt.0.)CLOSE (UNIT=23,STATUS='KEEP')
      IF(AbO3.gt.0.)CLOSE (UNIT=24,STATUS='KEEP')
      CLOSE (UNIT=25,STATUS='KEEP')
      CLOSE (UNIT=26,STATUS='KEEP')
      CLOSE (UNIT=27,STATUS='KEEP')
      CLOSE (UNIT=28,STATUS='KEEP')
      CLOSE (UNIT=29,STATUS='KEEP')
      CLOSE (UNIT=30,STATUS='KEEP')
      CLOSE (UNIT=31,STATUS='KEEP')
      CLOSE (UNIT=32,STATUS='KEEP')
      CLOSE (UNIT=33,STATUS='KEEP')
      CLOSE (UNIT=34,STATUS='KEEP')
      CLOSE (UNIT=35,STATUS='KEEP')
      CLOSE (UNIT=36,STATUS='KEEP')
      CLOSE (UNIT=37,STATUS='KEEP')
      CLOSE (UNIT=38,STATUS='KEEP')
      CLOSE (UNIT=39,STATUS='KEEP')
      CLOSE (UNIT=40,STATUS='KEEP')
      CLOSE (UNIT=41,STATUS='KEEP')
c
		TotTime    = etime(time) - TotTime
		write(6, 9910) TotTime
 9910	format ("Total CPU time: ",f7.3," sec")
c
      STOP
      END
c
c
c-----------------------------------------------
c      SUBROUTINES
c-----------------------------------------------
c
      Subroutine ALBCHK(N,Name,Wvla,Albdo,Wlmin,Wlmax)
c
c      Sends a Warning message if the reflectance data range is less
c      than the simulated spectrum range.
c
      Real Wvla(3000),Albdo(3000),Minalb,Maxalb
      Character*24 Name 
c
      Minalb=Albdo(1)
      Maxalb=Albdo(N)
      dwvl=.0005
      if(Wlmin.gt.0.4)dwvl=.001
      if(Wlmin.gt.1.702)dwvl=.005
      wvl1=Wvla(1)-dwvl
      wvln=Wvla(N)+dwvl
      If(wvl1.gt.Wlmin.or.Wvln.lt.Wlmax)Write(16,195)Name,
     2  Wvla(1),Wvla(N),Wlmin,Wlmax,Minalb,Wvla(1),Maxalb,Wvla(N)
 195  Format('** WARNING #13 ',9('*'),/,'\\ Ground reflectance data ',
     2 'for ', A24,/,'\\ extend only from ',f6.4,' to ',
     3  f6.4,' µm,',/,'\\ whereas the wavelength limits for this run ',
     4 'are ',f6.4,' and ',f6.4,' µm.',/,'\\ Consequently, reflect',
     5 'ance is fixed at ',F5.3,' below ',f6.4,' µm and at ',F5.3,
     6 ' above ',f6.4,' µm.',//)
      Return
      End
c
c
c
      Subroutine ALBDAT(Ialbd,Ntot,Name,Lamber,Wvla,Albd)
c
c      Reads the selected albedo data file
c
      Character*24 Name, Lamber
      Real Wvla(3000),Albd(3000)
c
c      Nfile=Max(19,18+Ialbd)
 3    continue
      If(Ialbd.ne.3)goto 4
      Open (UNIT=20,FILE='Albedo/SNOW.DAT',STATUS='OLD')
      Name='FRESH_SNOW'
      Goto 300
 4    continue
      If(Ialbd.ne.4)goto 5
      Open (UNIT=20,FILE='Albedo/NEVE.DAT',STATUS='OLD')
      Name='MOUNTAIN_NEVE'
      Goto 300
 5    continue
      If(Ialbd.ne.5)goto 6
      Open (UNIT=20,FILE='Albedo/BASALT.DAT',STATUS='OLD')
      Name='BASALT_ROCK'
      Goto 300
 6    continue
      If(Ialbd.ne.6)goto 7
      Open (UNIT=20,FILE='Albedo/DRY_SAND.DAT',STATUS='OLD')
      Name='DRY_SAND'
      Goto 300
 7    continue
      If(Ialbd.ne.7)goto 8
      Open (UNIT=20,FILE='Albedo/WITESAND.DAT',STATUS='OLD')
      Name='WHITE_SANDS'
      Goto 300
 8    continue
      If(Ialbd.ne.8)goto 9
      Open (UNIT=20,FILE='Albedo/SOIL.DAT',STATUS='OLD')
      Name='SOIL'
      Goto 300
 9    continue
      If(Ialbd.ne.9)goto 10
      Open (UNIT=20,FILE='Albedo/Dry_clay.dat',STATUS='OLD')
      Name='DRY_CLAY_SOIL'
      Goto 300
 10   continue
      If(Ialbd.ne.10)goto 11
      Open (UNIT=20,FILE='Albedo/WETCLAY.DAT',STATUS='OLD')
      Name='WET_CLAY_SOIL'
      Goto 300
 11   continue
      If(Ialbd.ne.11)goto 12
      Open (UNIT=20,FILE='Albedo/ALFALFA.DAT',STATUS='OLD')
      Name='ALFALFA'
      Goto 300
 12   continue
      If(Ialbd.ne.12)goto 13
      Open (UNIT=20,FILE='Albedo/GRASS.DAT',STATUS='OLD')
      Name='GRASS'
      Goto 300
 13   continue
      If(Ialbd.ne.13)goto 14
      Open (UNIT=20,FILE='Albedo/RYEGRASS.DAT',STATUS='OLD')
      Name='RYE_GRASS'
      Goto 300
 14   continue
      If(Ialbd.ne.14)goto 15
      Open (UNIT=20,FILE='Albedo/MEADOW1.DAT',STATUS='OLD')
      Name='ALPINE_MEADOW'
      Goto 300
 15   continue
      If(Ialbd.ne.15)goto 16
      Open (UNIT=20,FILE='Albedo/MEADOW2.DAT',STATUS='OLD')
      Name='LUSH_MEADOW'
      Goto 300
 16   continue
      If(Ialbd.ne.16)goto 17
      Open (UNIT=20,FILE='Albedo/WHEAT.DAT',STATUS='OLD')
      Name='WHEAT'
      Goto 300
 17   continue
      If(Ialbd.ne.17)goto 18
      Open (UNIT=20,FILE='Albedo/PINETREE.DAT',STATUS='OLD')
      Name='PONDEROSA_PINE_TREE'
      Goto 300
 18   continue
      If(Ialbd.ne.18)goto 19
      Open (UNIT=20,FILE='Albedo/CONCRETE.DAT',STATUS='OLD')
      Name='CONCRETE'
      Goto 300
 19   continue
      If(Ialbd.ne.19)goto 20
      Open (UNIT=20,FILE='Albedo/BlckLoam.dat',STATUS='OLD')
      Name='BLACK_LOAM'
      Goto 300
 20   continue
      If(Ialbd.ne.20)goto 21
      Open (UNIT=20,FILE='Albedo/BrwnLoam.dat',STATUS='OLD')
      Name='BROWN_SANDY_LOAM'
      Goto 300
 21   continue
      If(Ialbd.ne.21)goto 22
      Open (UNIT=20,FILE='Albedo/BrwnSand.dat',STATUS='OLD')
      Name='BROWN_LOAMY_FINE_SAND'
      Goto 300
 22   continue
      If(Ialbd.ne.22)goto 23
      Open (UNIT=20,FILE='Albedo/Conifers.dat',STATUS='OLD')
      Name='CONIFER_TREES'
      Goto 300
 23   continue
      If(Ialbd.ne.23)goto 24
      Open (UNIT=20,FILE='Albedo/DarkLoam.dat',STATUS='OLD')
      Name='BROWN_SILT_LOAM'
      Goto 300
 24   continue
      If(Ialbd.ne.24)goto 25
      Open (UNIT=20,FILE='Albedo/DarkSand.dat',STATUS='OLD')
      Name='BROWN_LOAMY_SAND'
      Goto 300
 25   continue
      If(Ialbd.ne.25)goto 26
      Open (UNIT=20,FILE='Albedo/Decidous.dat',STATUS='OLD')
      Name='DECIDUOUS_TREES'
      Goto 300
 26   continue
      If(Ialbd.ne.26)goto 27
      Open (UNIT=20,FILE='Albedo/DryGrass.dat',STATUS='OLD')
      Name='DRY_GRASS'
      Goto 300
 27   continue
      If(Ialbd.ne.27)goto 28
      Open (UNIT=20,FILE='Albedo/DuneSand.dat',STATUS='OLD')
      Name='WHITE_DUNE_SAND'
      Goto 300
 28   continue
      If(Ialbd.ne.28)goto 29
      Open (UNIT=20,FILE='Albedo/FineSnow.dat',STATUS='OLD')
      Name='FINE_SNOW'
      Goto 300
 29   continue
      If(Ialbd.ne.29)goto 30
      Open (UNIT=20,FILE='Albedo/GrnGrass.dat',STATUS='OLD')
      Name='GREEN_GRASS'
      Goto 300
 30   continue
      If(Ialbd.ne.30)goto 31
      Open (UNIT=20,FILE='Albedo/GrnlSnow.dat',STATUS='OLD')
      Name='GRANULAR_SNOW'
      Goto 300
 31   continue
      If(Ialbd.ne.31)goto 32
      Open (UNIT=20,FILE='Albedo/LiteClay.dat',STATUS='OLD')
      Name='LIGHT_BROWN_CLAY'
      Goto 300
 32   continue
      If(Ialbd.ne.32)goto 33
      Open (UNIT=20,FILE='Albedo/LiteLoam.dat',STATUS='OLD')
      Name='LIGHT_BROWN_LOAM'
      Goto 300
 33   continue
      If(Ialbd.ne.33)goto 34
      Open (UNIT=20,FILE='Albedo/LiteSand.dat',STATUS='OLD')
      Name='LIGHT_BROWN_LOAMY_SAND'
      Goto 300
 34   continue
      If(Ialbd.ne.34)goto 35
      Open (UNIT=20,FILE='Albedo/PaleLoam.dat',STATUS='OLD')
      Name='PALE_BROWN_LOAM'
      Goto 300
 35   continue
      If(Ialbd.ne.35)goto 36
      Open (UNIT=20,FILE='Albedo/Seawater.dat',STATUS='OLD')
      Name='SEA_WATER'
      Goto 300
 36   continue
      If(Ialbd.ne.36)goto 37
      Open (UNIT=20,FILE='Albedo/SolidIce.dat',STATUS='OLD')
      Name='SOLID_ICE'
      Goto 300
 37   continue
      If(Ialbd.ne.37)goto 38
      Open (UNIT=20,FILE='Albedo/Dry_Soil.dat',STATUS='OLD')
      Name='DRY_SOIL'
      Goto 300
 38   continue
      If(Ialbd.ne.38)goto 39
      Open (UNIT=20,FILE='Albedo/LiteSoil.dat',STATUS='OLD')
      Name='LIGHT_SANDY_SOIL'
      Goto 300
 39   continue
      If(Ialbd.ne.39)goto 40
      Open (UNIT=20,FILE='Albedo/RConcrte.dat',STATUS='OLD')
      Name='OLD_RUNWAY_CONCRETE'
      Goto 300
 40   continue
      If(Ialbd.ne.40)goto 41
      Open (UNIT=20,FILE='Albedo/RoofTile.dat',STATUS='OLD')
      Name='TERRACOTA_ROOFING_TILE'
      Goto 300
 41   continue
      If(Ialbd.ne.41)goto 42
      Open (UNIT=20,FILE='Albedo/RedBrick.dat',STATUS='OLD')
      Name='RED_BRICK'
      Goto 300
 42   continue
      If(Ialbd.ne.42)goto 43
      Open (UNIT=20,FILE='Albedo/Asphalt.dat',STATUS='OLD')
      Name='OLD_RUNWAY_ASPHALT'
      Goto 300
 43   continue
      If(Ialbd.ne.43)goto 44
      Open (UNIT=20,FILE='Albedo/TallCorn.dat',STATUS='OLD')
      Name='TALL_GREEN_CORN'
      Goto 300
 44   continue
      If(Ialbd.ne.44)goto 45
      Open (UNIT=20,FILE='Albedo/SndGrvl.dat',STATUS='OLD')
      Name='SAND_&_GRAVEL'
      Goto 300
 45   continue
      If(Ialbd.ne.45)goto 46
      Open (UNIT=20,FILE='Albedo/Fallow.dat',STATUS='OLD')
      Name='FALLOW_FIELD'
      Goto 300
 46   continue
      If(Ialbd.ne.46)goto 47
      Open (UNIT=20,FILE='Albedo/WetClay2.dat',STATUS='OLD')
      Name='WET_RED_CLAY'
      Goto 300
 47   continue
      If(Ialbd.ne.47)goto 48
      Open (UNIT=20,FILE='Albedo/WetSSoil.dat',STATUS='OLD')
      Name='WET_SANDY_SOIL'
      Goto 300
 48   continue
      If(Ialbd.ne.48)goto 49
      Open (UNIT=20,FILE='Albedo/Gravel.dat',STATUS='OLD')
      Name='GRAVEL'
      Goto 300
 49   continue
      If(Ialbd.ne.49)goto 50
      Open (UNIT=20,FILE='Albedo/WetClay2.dat',STATUS='OLD')
      Name='WET_RED_CLAY'
      Goto 300
 50   continue
      If(Ialbd.ne.50)goto 51
      Open (UNIT=20,FILE='Albedo/WetSilt.dat',STATUS='OLD')
      Name='WET_SILT'
      Goto 300
 51   continue
      If(Ialbd.ne.51)goto 52
      Open (UNIT=20,FILE='Albedo/LngGrass.dat',STATUS='OLD')
      Name='DRY_LONG_GRASS'
      Goto 300
 52   continue
      If(Ialbd.ne.52)goto 53
      Open (UNIT=20,FILE='Albedo/LwnGrass.dat',STATUS='OLD')
      Name='GENERIC_LAWN_GRASS'
      Goto 300
 53   continue
      If(Ialbd.ne.53)goto 54
      Open (UNIT=20,FILE='Albedo/OakTree.dat',STATUS='OLD')
      Name='DECIDOUS_OAK_TREE_LEAVES'
      Goto 300
 54   continue
      If(Ialbd.ne.54)goto 55
      Open (UNIT=20,FILE='Albedo/Pinion.dat',STATUS='OLD')
      Name='PINON_PINETREE_NEEDLES'
      Goto 300
 55   continue
      If(Ialbd.ne.55)goto 56
      Open (UNIT=20,FILE='Albedo/MeltSnow.dat',STATUS='OLD')
      Name='MELTING_SNOW=SLUSH'
      Goto 300
 56   continue
      If(Ialbd.ne.56)goto 57
      Open (UNIT=20,FILE='Albedo/Plywood.dat',STATUS='OLD')
      Name='PLYWOOD_SHEET'
      Goto 300
 57   continue
      If(Ialbd.ne.57)goto 58
      Open (UNIT=20,FILE='Albedo/WiteVinl.dat',STATUS='OLD')
      Name='WHITE_VINYL_COVER_SHEET'
      Goto 300
 58   continue
      If(Ialbd.ne.58)goto 59
      Open (UNIT=20,FILE='Albedo/FibrGlss.dat',STATUS='OLD')
      Name='CLEAR_FIBERGLASS_COVER'
      Goto 300
 59   continue
      If(Ialbd.ne.59)goto 60
      Open (UNIT=20,FILE='Albedo/ShtMetal.dat',STATUS='OLD')
      Name='GALVANIZED_SHEET_METAL'
      Goto 300
 60   continue
      If(Ialbd.ne.60)goto 61
      Open (UNIT=20,FILE='Albedo/Wetland.dat',STATUS='OLD')
      Name='WETLAND_CANOPY'
      Goto 300
 61   continue
      If(Ialbd.ne.61)goto 62
      Open (UNIT=20,FILE='Albedo/SageBrsh.dat',STATUS='OLD')
      Name='SAGEBRUSH_CANOPY'
      Goto 300
 62   continue
      If(Ialbd.ne.62)goto 63
      Open (UNIT=20,FILE='Albedo/FirTrees.dat',STATUS='OLD')
      Name='FIR_TREES_COLORADO'
      Goto 300
 63   continue
      If(Ialbd.ne.63)goto 64
      Open (UNIT=20,FILE='Albedo/CSeaWatr.dat',STATUS='OLD')
      Name='COASTAL_PACIFIC_SEAWATER'
      Goto 300
 64   continue
      If(Ialbd.ne.64)goto 65
      Open (UNIT=20,FILE='Albedo/OSeaWatr.dat',STATUS='OLD')
      Name='OPEN_ATLANTIC_SEAWATER'
      Goto 300
 65   continue
      If(Ialbd.ne.65)goto 66
      Open (UNIT=20,FILE='Albedo/GrazingField.dat',STATUS='OLD')
      Name='GRAZING_FIELD'
      Goto 300
 66   continue
      If(Ialbd.ne.66)goto 2
      Open (UNIT=20,FILE='Albedo/Spruce.dat',STATUS='OLD')
      Name='SPRUCE_TREE'
      Goto 300
c      
 2    continue
      If(Ialbd.ne.2)goto 1
      Ntot=2
      Lamber='NON_LAMBERTIAN'
      Wvla(1)=.28
      Albd(1)=.035
      Name='WATER'
      Return
 1    continue
      If(Ialbd.LE.1)Open (UNIT=20,FILE='Albedo/ALBEDO.DAT',STATUS='OLD')
      Name='USER_DEFINED'
 300  Continue
      Ial=1
 371  continue
      Read(20,*,End=370)Wvla(Ial),Albd(Ial)
      Ial=Ial+1
      Goto 371
 370  Continue
      Ntot=Ial-1
      if(Ntot.gt.3000)write(16,911)Ialbd,Ntot
 911  format('** WARNING #19 ** Error in albedo file # ',i2,': Number ',
     1 'of data rows is ',i8,/,' but should be <= 3000.',/)
      Lamber='NON_LAMBERTIAN'
      If(Ialbd.le.0)Lamber='LAMBERTIAN'
      Close (Unit=20,status='keep')
      Return
      End
C
C
C
      Subroutine ALBDOS (Ialbd,Nwal,Albdo,Wvla,Wvl,Z,Zcos,Rhob0,
     2   Rhob,Rhod)
c
c      Calculates and/or interpolates spectral albedo and provides
c      non-Lambertian reflectances for direct and diffuse radiation
c
c      Modified in 2.9.3:
c      Albedo files must have no more than 3000 lines of data 	
c
c      Double Precision Z
      Real Wvla(3000), Albdo(3000)
c
      If(Ialbd.eq.2)goto 734
c
c      Interpolation in wavelength for albedo data
c
      Do 730 Ial=1,Nwal
      If(abs(Wvla(Ial)-Wvl).gt.1e-4)goto 731
      Rhox=Albdo(Ial)
      Goto 730
 731  continue
      If(Wvl.ge.Wvla(1))goto 732
      Rhox=Albdo(1)
      Goto 730
 732  continue
      If(Wvl.le.Wvla(Nwal))Goto 733
      Rhox=Albdo(Nwal)
      Goto 730
 733  Continue
      If(Wvla(Ial).lt.Wvl.and.Wvla(Ial+1).gt.Wvl)Call Interp(1,
     2  Wvla(Ial),Wvla(Ial+1),Wvl,Albdo(Ial),Albdo(Ial+1),Rhox)
 730  Continue
c
      Rhod=Rhox
      Rhob0=Rhox
      Rhob=Rhox
c
C      Non-Lambertian reflection
c
      If(Ialbd.le.0)goto 738
      If(Ialbd.ne.3.and.Ialbd.ne.4.and.Ialbd.ne.28.and.Ialbd.ne.30)
     1 goto 741
c
c      Fresh snow case, based on Larsen & Barkstrom 1977
c
      Rhod=Rhob0*.939
      Rhob=Rhob0*(1.-.176*Zcos)/.94
      Goto 738
 741  continue
c
c      All other natural albedos, except water, based on the 
c      "land albedo" formula of Larsen & Barkstrom
c
      Rhob=Rhob0/.35
      Rhod=Rhob0*1.167
      If(Zcos.gt.1.0E-6)Rhob=Rhob*(1.-Zcos*log(1.+1./Zcos))
      Goto 738
c
c      Water albedo, as in oceans, fitted from Fresnel formula used with
c      spectral refractive indices of air and water, and taking the
c       underlying reflectance into account
c
 734  Continue
      Wvl2=Wvl*Wvl
      Wvl3=Wvl2*Wvl
      Rhod=.0803-.00365*Wvl
      Rhob0=.04059-.015565*Wvl+.013033*Wvl2-.0036053*Wvl3
      If(Wvl.gt.2.8)Rhob0=-4.264+3.6959*Wvl-1.0508*Wvl2+.099003*Wvl3
      Rhob=Rhob0*(1.+(.001209-.00010748*Wvl)*exp((.11087+.0017729*Wvl)
     2  *Z))
 738  Continue
      Rhob0=Min(Rhob0,1.)
      Rhob=Min(Rhob,1.)
      Rhod=Min(Rhod,1.)
c
      Return
      End
c
c
c
      Subroutine ALFA(Season,Iaer,Iturb,Iref,alpha1,alpha2,tau5,
     1 beta,al1,al2,tau550,index)
c
      Character*6 Season
c
c      Overall alpha1, alpha2; considering TROPO aerosols in the free
c      atmosphere for Shettle & Fenn models. New in 2.9.2
c
      alfa1s=.999
      alfa1w=.999
      alfa2s=1.565
      alfa2w=1.584
      frctn=0.
      alfa1=0.
      alfa2=0.
      T55=Tau5/(1.1**alpha2)
      if(index.eq.1)T55=tau550
      frctns=1.
      frctnw=1.
      if(Iaer.eq.4)goto 1
      t552=t55*t55
      frctns=Min(1.,exp((-1.6435+1.4011*t55-8.2491*t552+.065552/t55)/
     1 (1.+1.936*t552)))
      frctnw=Min(1.,exp((-1.8181+.84983*t55-8.641*t552+.0436/t55)/
     1 (1.+1.8959*t552)))
 1    continue
      frctn=frctns
      alfa1=alfa1s
      alfa2=alfa2s
      if(Season.eq.'SUMMER')goto 1825
      frctn=frctnw
      alfa1=alfa1w
      alfa2=alfa2w
 1825 continue
      al1=alpha1*(1.-frctn)+alfa1*frctn
      al2=alpha2*(1.-frctn)+alfa2*frctn
      if(index.eq.1)goto 10
c
c      Recalculate Tau550 from Tau5 and the new value of alpha2
c 
      if(iTurb.eq.1)TAU5=BETA/(0.5**AL2)
      if(iTurb.eq.0.or.iTurb.eq.2.or.Iturb.eq.5)BETA=(0.5**AL2)*TAU5
      if(iTurb.ne.5)Tau550=Tau5/(1.1**al2)
      If(IAer.ne.1.or.IRef.ne.1)goto 1827
      if(iTurb.eq.1)TAU5=BETA/(0.5**1.336688)
      if(iTurb.eq.0.or.iTurb.eq.2.or.Iturb.eq.5)BETA=(0.5**1.33669)*TAU5
      if(iTurb.ne.5)Tau550=Tau5/(1.1**.988415)
      goto 1827
 10   continue
c
c      Recalculate Tau5 from Tau550 and the new value of alpha2
c 
      TAU5=Tau550*(1.1**al2)
      BETA=(0.55**AL2)*TAU550
      If(IAer.ne.1.or.IRef.ne.1)goto 1827
      TAU5=Tau550*(1.1**.9883)
      Beta=Tau550*(.55**1.39223)
 1827 continue
c
      return
      end
c
c
c
      FUNCTION AMZ(X)
C
c      New coefficients in 2.9
c
c      Double Precision X,XR,Xcos
      XR=X*0.017453293
      XCOS=COS(XR)
      AMZ=1./(XCOS+.48353*(X**.095846)*(96.741-X)**(-1.754))
      RETURN
      END
C
c
C
      Subroutine Circum (Ecircl,Jaer,Icirc)
c
c      Circumsolar calculations based on typical aerosol phase functions
c      and an improved single-scattering approximation for the aureole
c      radiance. The original data for the SRA aerosol phase functions
c      were supplied by Maria Putsay, Hungarian Met. Service (1995).
c
      REAL WVLref(5),RHref(4),PHFNA(8)
      Double precision Ecircl,SKY,Ebnl
      Character*64 Aeros
      Common /Solar3/Ebnl,Aeros,Tauasl,Taurl,Rognd,Degrad,pinb,pi4,AmR,
     2   WVL,wvln,nx,Znr,Zxr,va,vb,RH

      Data Wvlref /.3,.55,.694,1.06,1.536/
      Data RHref /0.,70.,80.,99./

      PHFNR0=1.4794/pi4
C
C      SOURCE OF THE BASIC DATA POINTS FOR THE AEROSOL PHASE FUNCTIONS
C            [Fits = f(scatt. angle, wvlgth) by C. Gueymard]
C
C      JAER            AEROS                           SOURCE
C      ______      ________________                 ____________
C      1            S&F_RURAL                        MODTRAN2
C      2            S&F_URBAN                              "
C      3            S&F_MARIT                              "
C      4            S&F_TROPO                              "
C      5            SRA_CONTL                        Putsay 1995
C      6            SRA_URBAN                              "
C      7            SRA_MARIT                              "
C      8            B&D_C  (HAZE L)                  Lenoble 1985
C      9            B&D_C1 (HAZE L)                        "
c     10            Desrt_Min                Assumed same as SRA_CONTL
c     11            Desrt_Max                Assumed same as SRA_MARIT
c
      ja=jaer
      if(jaer.eq.9)ja=8
      if(jaer.eq.0)ja=5
      if(jaer.eq.10)ja=5
      if(jaer.eq.11)ja=7
      if(ja.le.4)Call Phase1(wvl,RH,ja,IRH0,NN,Iwv0,Inter1,Inter2)
      if(ja.ge.5.and.ja.le.7)Call Phase2(wvl,ja,A0,A1,A2,A3,B0,B1,B2)
c
      TAUTSL=TAURL+TAUASL
      TAURM=1.38*TAUTSL*TAUTSL
      TAUGM=ROGND*(TAURL+TAURM)
      ECIRCL=0.0D+00
c
c      Main loop for x, the incremental scattering angle
c
      DO 20 ix=1,nx
      x=.1*(ix-1)
      x2=x*x
      DX=.1*DEGRAD
      XR=X*DEGRAD
      SIN2X=SIN(2.*XR)
      PHFNR=(.76032+.71904*(COS(XR)**2))/pi4
      if(ja.gt.4)goto 25
c      
c      Call PhF1 to pick the correct Shettle & Fenn phase function, then
c      interpolate in wavelength and/or humidity if necessary
c
      PF11=PhF1(NN,IWV0,x)
      If(Inter2.ne.0)PF12=PhF1(NN,IWV0+1,x)
      If(Inter1.ne.0)PF21=PhF1(NN+1,IWV0,x)
      If(Inter1.ne.0.and.Inter2.ne.0)PF22=PhF1(NN+1,IWV0+1,x)
      PF=PF11
      PF1=0.
      PF2=0.
      If(Inter1.eq.0.and.Inter2.eq.0)goto 49
      If(Inter2.eq.0)goto 41
      Call Interp(2,Wvlref(Iwv0),Wvlref(Iwv0+1),Wvl,PF11,PF12,PF2)
      PF=PF2
 41   continue
      If(Inter1.eq.0)goto 49
      Call Interp(1,RHref(Irh0),RHref(Irh0+1),RH,PF11,PF21,PF1)
      PF=PF1
      If(PF1.le.0.0.or.PF2.le.0.0)goto 49
      Call Interp(2,Wvlref(Iwv0),Wvlref(Iwv0+1),Wvl,PF21,PF22,PF3)
      Call Interp(1,RHref(Irh0),RHref(Irh0+1),RH,PF2,PF3,PF)
 49   continue
c
      PHFNA(ja)=PF
      goto 24
 25   continue
c
      IF(JA.GT.7)GOTO 23
      IF(X.GT.1.)GOTO 22
      PHFNA(JA)=B0+B1*X+B2*X2
      GOTO 24
 22   CONTINUE
      XLN=log(X+1.)
      XLN2=XLN*XLN
      PHFNA(JA)=EXP(A0+A1*XLN+A2*XLN2+A3*XLN2*XLN)
      GOTO 24
 23   CONTINUE
      PHFNA(8)=(2.4148+.59238*X-.032609*X2)/(1.+.23295*X)
 24   CONTINUE
C
C      Global sky scattering function
C
      PHFNT=(TAURL+TAURM)*PHFNR+TAUGM*PHFNR0+TAUASL*PHFNA(JA)
C
C      SKY RADIANCE AT DISTANCE X DEGREES FROM THE SUN'S CENTER ALONG
C            THE ALMUCANTAR (W/m2 nm sr)
C
      SKY=EBNL*AmR*PHFNT
C
C      NUMERICAL INTEGRATION (SIMPSON'S RULE) TO OBTAIN THE CIRCUMSOLAR
C         SPECTRAL IRRADIANCE WITHIN THE APERTURE ANGLE
C         and taking the geometric penumbra function into account
C         if the radiometer's geometry (slope and limit angles) is known
C
      DXI=2.*DX
      IF(MOD(ix,2).EQ.0)DXI=4.*DX
      IF(ix.EQ.1.OR.ix.EQ.nx)DXI=DX
      Penumb=1.
      if(Icirc.lt.2.or.xr.le.Znr)goto 21
      Penumb=0.
      if(Icirc.lt.2.or.xr.ge.Zxr)goto 21
      Z1=.5*vb*tan(xr)
      Z2=.5*(va*va - 1.)/(vb*tan(xr))
      if(abs(Z1-Z2).lt.1.)vu=2.*acos(Z1-Z2)
      if(abs((Z1+Z2)/va).lt.1.)vl=2.*acos((Z1+Z2)/va)
      Penumb=2.*(va*va*(vl-sin(vl))+vu-sin(vu))/pi4
 21   continue
      ECIRCL=ECIRCL+SKY*Penumb*SIN2X*DXI*pinb
 20   CONTINUE
c 
      ECIRCL=ECIRCL/3.
c
 999  CONTINUE
      Return
      END
c
c
c
      Subroutine Gases(iatm,pr,g1CH4,g2CO,g3N2O,g4N2,g5HNO3,g6NO2,g7NO,
     1  g8SO2,g9NH3)
c
      Real g1a0(6),g2a0(6),g3a0(6),g4a0(6),g5a0(6),g6a0(6),g7a0(6),
     1 g8a0(6),g9a0(6)
      Real g1a1(6),g2a1(6),g3a1(6),g4a1(6),g5a1(6),g6a1(6),g7a1(6),
     1 g8a1(6),g9a1(6)
      Real g2a2(6),g6a2(6),g7a2(6),g8a2(6),g9a2(6)
      Real g2a3(6),g8a3(6),g9a3(6)
      Real g2a4(6),g8a4(6),g9a4(6)
c
c      Data g9a0 /.0033129,.0037671,.0023213,.0035662,.001539,.0042842/
c      Data g9a1 /4.3135,4.552,4.2953,4.4027,4.2145,4.6732/
c      Data g9a2 /-2.3852,-2.4465,-1.8702,-2.3822,-1.2973,-2.4793/
c      Data g9a3 /1.0182,1.0036,.75466,.95602,.49091,.88047/
c      Data g9a4 /-1.5742,-1.6510,-1.4515,-1.5888,-1.3194,-1.6303/
      Data g9a0 /-8.6499,-8.6733,-8.6310,-8.6499,-8.6007,-8.6806/
      Data g9a1 /2.1947,2.2598,2.1654,2.1954,2.0830,2.2869/
      Data g9a2 /-2.5936,-2.7965,-2.4300,-2.6350,-2.3099,-2.8974/
      Data g9a3 /-1.8190,-2.0213,-1.6139,-1.8122,-1.4907,-2.1547/
      Data g9a4 /-.65854,-.76521,-.56694,-.64901,-.50188,-.82552/
      Data g8a0 /.11133,.10577,.11349,.11018,.11764,.10467/
      Data g8a1 /.81200,.74538,.83797,.80736,.88324,.72267/
      Data g8a2 /.81319,.75908,.83480,.80245,.87543,.74782/
      Data g8a3 /3.0557,3.1972,3.0427,3.1073,2.9868,3.1973/
      Data g8a4 /-1.5780,-1.6285,-1.5935,-1.6075,-1.5909,-1.6097/
      Data g7a0 /.74307,.82452,.68649,.83589,.62899,.75964/
      Data g7a1 /2.4015,2.4068,2.4028,2.4011,2.3996,2.4132/
      Data g7a2 /57.079,57.874,56.504,57.682,56.140,57.933/
      Data g6a0 /1.8599,1.9992,1.8015,1.9733,1.6838,1.9265/
      Data g6a1 /.18453,.18498,.18444,.18424,.18403,.18548/
      Data g6a2 /41.771,41.357,42.382,41.029,42.686,41.546/
      Data g5a0 /3.6370,3.8402,3.5665,3.7505,3.3853,3.8093/
      Data g5a1 /.12319,.11963,.12376,.11934,.12671,.12212/
      Data g4a0 /3.8269,3.7082,3.9369,3.7903,4.0671,3.6869/
      Data g4a1 /1.8374,1.8384,1.8651,1.8536,1.8808,1.8232/
      Data g3a0 /.24730,.23972,.24142,.22167,.24227,.24772/
      Data g3a1 /1.0791,1.1561,1.1397,1.2029,1.1344,1.0839/
      Data g2a0 /.29625,.26917,.29291,.31369,.26476,.27585/
      Data g2a1 /2.4480,2.5408,2.4273,2.5201,2.2604,2.5267/
      Data g2a2 /.54669,.72959,.57135,.59633,.45707,.65861/
      Data g2a3 /-2.4114,-2.5384,-2.4406,-2.5759,-2.1216,-2.4641/
      Data g2a4 /.65756,.68593,.68198,.71193,.59399,.65748/
      Data g1a0 /1.3255,1.2756,1.2818,1.2692,1.2808,1.3288/
      Data g1a1 /1.0574,1.1270,1.1171,1.1391,1.1231,1.0611/
c
      pr2=pr*pr
      x=log(pr)
      x2=x*x
      x3=x2*x
      g1CH4=g1a0(iatm)*pr**g1a1(iatm)
      g2CO=g2a0(iatm)*(pr**g2a1(iatm))*exp(g2a2(iatm)+g2a3(iatm)*pr
     1  +g2a4(iatm)*pr2)
      g3N2O=g3a0(iatm)*pr**g3a1(iatm)
      g4N2=g4a0(iatm)*pr**g4a1(iatm)
      g5HNO3=g5a0(iatm)*1e-4*pr**g5a1(iatm)
      g6NO2=1E-4*Min(g6a0(iatm)+g6a1(iatm)*pr,g6a2(iatm)*pr)
      g7NO=1e-4*Min(g7a0(iatm)+g7a1(iatm)*pr,g7a2(iatm)*pr)
      g8SO2=g8a0(iatm)*1e-4*(pr**g8a1(iatm))*exp(g8a2(iatm)+g8a3(iatm)
     1  *pr2+g8a4(iatm)*pr2*pr)
c      g9NH3=g9a0(iatm)*(pr**g2a1(iatm))*exp(g9a2(iatm)+g9a3(iatm)
c      1  *pr+g9a4(iatm)*pr2)
      g9NH3=exp(g9a0(iatm)+g9a1(iatm)*x+g9a2(iatm)*x2+g9a3(iatm)*x3
     1  +g9a4(iatm)*x3*x)
      return
      end
c
c
c
      Subroutine GSCH4(ACH4,AbCH4,AmCH4,TCH4,TCH4P,amdif)
C
      TCH4=exp(-(ACH4*AbCH4*AmCH4))
      TCH4P=exp(-(ACH4*AbCH4*Amdif))
 803  continue
      return
      end
c
c
c
      Subroutine GSCH2O(T,xsCH2O,atCH2O,AbCH2O,AmCH2O,TCH2O,TCH2OP,
     1 amdif)
C
      Double Precision NLosch
c      
      Common /Number/ NLosch
c
      ACH2O=(xsCH2O+atCH2O*(T-293.))*NLosch
      TCH2O=exp(-ACH2O*AbCH2O*AmCH2O)
      TCH2OP=exp(-ACH2O*AbCH2O*Amdif)
      return
      end
c
c
c
      Subroutine GSCO(ACO,AbCO,AmCO,TCO,TCOP,amdif)
C
      TCO=exp(-ACO*AbCO*AmCO)
      TCOP=exp(-ACO*AbCO*Amdif)
      return
      end
c
c
c
      Subroutine GSHNO2(xsHNO2,AbHNO2,AmHNO2,THNO2,THNO2P,amdif)
C
      Double Precision NLosch
c      
      Common /Number/ NLosch
c
      AHNO2=xsHNO2*NLosch
      THNO2=exp(-AHNO2*AbHNO2*AmHNO2)
      THNO2P=exp(-AHNO2*AbHNO2*Amdif)
      return
      end
c
c
c
      Subroutine GSHNO3(T,xsHNO3,athno3,AbHNO3,AmHNO3,
     1 THNO3,THNO3P,amdif)
C
      Double Precision NLosch
c      
      Common /Number/ NLosch
c
      xnl=NLosch*1e-19
      AHNO3=xsHNO3*0.1*exp(athno3*1e-3*(T-298.))*xnl
      THNO3=exp(-AHNO3*AbHNO3*AmHNO3)
      THNO3P=exp(-AHNO3*AbHNO3*Amdif)
      return
      end
c
c
c
      Subroutine GSNO(ANO,AbNO,AmNO,TNO,TNOP,amdif)
C
      TNO=exp(-ANO*AbNO*AmNO)
      TNOP=exp(-ANO*AbNO*Amdif)
      return
      end
c
c
c
      Subroutine GSNO2(T,xsNO2,atno2,AbNO2,AmNO2,TNO2,TNO2P,amdif)
C
      Double Precision NLosch
c      
      Common /Number/ NLosch
c
      ANO2=(xsNO2+atno2*(T-220.))*NLosch
      TNO2=exp(-ANO2*AbNO2*AmNO2)
      TNO2P=exp(-ANO2*AbNO2*Amdif)
      return
      end
c
c
c
      Subroutine GSNO3(T,xsNO3,atno3,AbNO3,AmNO3,TNO3,TNO3P,amdif)
C
      Double Precision NLosch
c      
      Common /Number/ NLosch
c
      ANO3=(xsNO3+atno3*(T-230.))*NLosch
      TNO3=exp(-ANO3*AbNO3*AmNO3)
      TNO3P=exp(-ANO3*AbNO3*Amdif)
      return
      end
c
c
c
      Subroutine GSO3U(Tref,T,xs,a0o3,a1o3,Abo3,Amo3,tauz3,TO3,xso3,
     1 AO3)
C
      DOUBLE PRECISION AO3,TO3,xcheck,tauz3, NLosch
c      
      Common /Number/ NLosch
c
      fto3=1.-T/Tref
      xso3=xs*(1.+a0o3*fto3)/(1.+a1o3*fto3)
      AO3=NLosch*xso3
      TAUZ3=AbO3*AO3
      XCHECK=AMO3*TAUZ3
      TO3=0.D+000
      IF(XCHECK.le.499.)TO3=EXP(-XCHECK)
      return
      end
c
c
c
      Subroutine GSSO2U(T,xsSO2,atso2,AbSO2,AmSO2,TSO2,TSO2P,amdif)
C
      Double Precision NLosch
c      
      Common /Number/ NLosch
c
c      UV band
c
      ASO2=(xsSO2+atso2*(T-213.))*NLosch
      TSO2=exp(-ASO2*AbSO2*AmSO2)
      TSO2P=exp(-ASO2*AbSO2*Amdif)
      return
      end
c
C
c
      Subroutine GSSO2I(ASO2,AbSO2,AmSO2,TSO2,TSO2P,amdif)
c
c      IR band
c
      TSO2=exp(-(ASO2*AbSO2*AmSO2))
      TSO2P=exp(-(ASO2*AbSO2*Amdif))
      return
      end
c
c
c
      Subroutine Interp (IntTyp,X1,X2,X,Y1,Y2,Y)
c
C      Interpolation routine using either a linear interpolation 
C      (IntTyp=1) or logarithmic interpolation (IntTyp=2)
C      The log interpolation returns to a linear interpolation if the
C      gradient is very small
C
      it=IntTyp
      If(y1.le.1e-5.or.y2.le.1e-5)it=1
      If(it.eq.1)goto 1
c
c      Log interpolation
c
      a1=log(y1)
      a2=log(y2)
      Grad=(a2-a1)/(x2-x1)
      If(Abs(Grad).le.1E-2)goto 1
      a=a1+(x-x1)*Grad
      y=exp(a)
      Return
C      
C      Linear interpolation
c
 1    continue
      y=y1+(x-x1)*(y2-y1)/(x2-x1)
      Return
      End
C
c
c
      Function Nday(Year,Month,Day)
c
c      To obtain the calendar day (Day of year) from (Month, Day of mnth)
c
      Integer Day,Year
c
      Leap1=Mod(Year,4)
      leap2=Mod(year,100)
      leap3=Mod(year,400)
      leap=leap1-leap2+leap3
      Xdm=32.8
      If(Month.le.2)Xdm=30.6
      If(Leap.eq.0.and.Month.gt.2)Xdm=31.8
      Cday=Int(30.6*Month + Day - Xdm + 0.5)
      Nday=Ifix(Cday)
      Return
      End
c
c
c
      Subroutine Ozon(Z,Tz,Tm,Tmin,Tmax,Ozmin,Ozmax)
c
      Real zref(41),a0(41),a1(41)
      Real O3Min(41),O3Max(41),TO3Min(41),TO3Max(41)
c
      Data zref/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,
     1 16.,17.,18.,19.,20.,21.,22.,23.,24.,25.,30.,35.,40.,45.,50.,55.,
     2 60.,65.,70.,75.,80.,85.,90.,95.,100./
      Data a0/-128.24,-118.47,-99.894,-99.881,-102.06,-106.79,-112.54,
     1 -115.88,-117.69,-115.02,-91.984,-18.072,68.685,7.1018,-83.208,
     2 -145.22,-165.41,-166.81,-145.86,-113.39,-74.301,-26.34,23.488,
     3 54.405,60.253,51.106,7.4044,-16.679,-21.216,-43.058,-44.575,
     4 -55.374,-99.21,44.285,43.945,32.81,-19.095,-16.587,-9.3195,
     5 -6.7324,-10.863/
      Data a1/0.65139,0.61388,0.54085,0.53352,0.5325,0.54076,0.55315,
     1 0.55586,0.55201,0.52965,0.41801,0.078528,-0.32603,-0.058176,
     2 0.34991,0.6326,0.72425,0.72921,0.63055,0.47874,0.2971,0.075867,
     3 -0.15234,-0.29358,-0.31898,-0.27613,-0.073547,0.034212,0.060706,
     4 0.1574,0.18328,0.23388,0.42174,-0.15026,-0.14293,-0.096462,
     5 0.13032,0.087694,0.029694,0.0187,0.028993/
      Data O3Min /0.2434,0.2416,0.2389,0.2364,0.2339,0.2319,0.2297,
     1 0.2276,0.2259,0.2247,0.2245,0.2248,0.2243,0.2232,0.2236,0.2239,
     2 0.226,0.2279,0.2256,0.211,0.1866,0.1607,0.1362,0.1145,0.0961,
     3 0.0807,0.0409,0.0201,7.99E-03,2.63E-03,8.89E-04,3.36E-04,
     4 9.51E-05,3.31E-05,1.37E-05,7.50E-06,4.38E-06,2.37E-06,8.94E-07,
     5 2.59E-07,8.00E-08/
      Data O3Max /0.4444,0.4428,0.4406,0.4383,0.4359,0.4336,0.4308,
     1 0.4278,0.4235,0.4182,0.4092,0.3979,0.3856,0.3727,0.3552,0.336,
     2 0.3132,0.289,0.2669,0.2537,0.2464,0.239,0.2302,0.219,0.2048,
     3 0.1887,0.113,0.0482,1.69E-02,5.43E-03,1.91E-03,8.24E-04,
     4 3.03E-04,9.99E-05,2.93E-05,1.04E-05,5.92E-06,4.39E-06,1.95E-06,
     5 6.75E-07,1.50E-07/
      Data TO3Min /211.,210.8,210.5,210.3,210.1,209.9,209.7,209.6,209.4,
     1 209.3,209.2,209.1,208.9,208.8,208.7,208.6,208.5,208.4,208.4,
     2 208.4,208.6,208.9,209.2,209.6,210.1,210.7,214.8,223.1,234.,244.9,
     3 250.3,246.5,237.7,218.8,188.2,163.1,151.6,149.2,154.5,166.6,173./
      Data TO3Max /241.4,241.1,240.5,240.,239.6,239.3,238.9,238.7,238.5,
     1 238.3,238.3,238.4,238.6,238.8,239.1,239.5,240.,240.5,241.1,241.8,
     2 242.7,243.8,244.9,246.3,247.8,249.3,257.2,267.5,277.3,282.3,
     3 277.3,268.1,253.1,241.5,235.6,226.9,219.7,216.8,213.5,218.4,230./
c
c      Ozone temperature at altitude z vs air temperature at z: 
c            To3(z)=(1.-a1)*T(z)-a0
c      
      INTRP=0
      N=41
      DO 10 IZ=1,N
c      TO3(IZ)=(1.-a1(IZ))*Tz-a0(IZ)
      if(INTRP.ne.0)goto 10
      IF(abs(Z-ZREF(IZ)).le.1e-4)goto 89
      if(Z.gt.ZREF(IZ))goto 10
      if(IZ.le.2)goto 20
      if(IZ.ge.(N-1))goto 21
      INTRP=1
      J1=IZ-1
      goto 10
 20   continue
      INTRP=1
      J1=Max(Iz-1,1)
      goto 10
 21   continue
      INTRP=1
      J1=Min(iz-1,(N-1))
      goto 10
 89   continue
      INTRP=2
      Tm=(1.-a1(IZ))*Tz-a0(IZ)
      Tmin=TO3min(IZ)
      Tmax=TO3max(IZ)
      Ozmin=O3min(IZ)
      Ozmax=O3max(IZ)
 10   CONTINUE
      if(INTRP.eq.2)goto 99
      xp=zref(j1)
      xp1=zref(j1+1)
      Tm0=(1.-a1(j1))*Tz-a0(j1)
      Tm1=(1.-a1(j1+1))*Tz-a0(j1+1)
      Tmin0=TO3min(j1)
      Tmin1=TO3min(j1+1)
      Tmax0=TO3max(j1)
      Tmax1=TO3max(j1+1)
      Ozmin0=O3min(j1)
      Ozmin1=O3min(j1+1)
      Ozmax0=O3max(j1)
      Ozmax1=O3max(j1+1)
c
      Call Interp (1,Xp,Xp1,Z,Tm0,Tm1,Tm)
      Call Interp (1,Xp,Xp1,Z,Tmin0,Tmin1,Tmin)
      Call Interp (1,Xp,Xp1,Z,Tmax0,Tmax1,Tmax)
      Call Interp (1,Xp,Xp1,Z,Ozmin0,Ozmin1,Ozmin)
      Call Interp (1,Xp,Xp1,Z,Ozmax0,Ozmax1,Ozmax)
      
 99   CONTINUE
      return
      end
c
c
c
      Subroutine Ozon2(z,Ozmin,Ozmax)
c
      Real zref(41),O3Min(41),O3Max(41)
      
      Data zref/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,
     1 16.,17.,18.,19.,20.,21.,22.,23.,24.,25.,30.,35.,40.,45.,50.,55.,
     2 60.,65.,70.,75.,80.,85.,90.,95.,100./
      Data O3Min /0.2434,0.2416,0.2389,0.2364,0.2339,0.2319,0.2297,
     1 0.2276,0.2259,0.2247,0.2245,0.2248,0.2243,0.2232,0.2236,0.2239,
     2 0.226,0.2279,0.2256,0.211,0.1866,0.1607,0.1362,0.1145,0.0961,
     3 0.0807,0.0409,0.0201,7.99E-03,2.63E-03,8.89E-04,3.36E-04,
     4 9.51E-05,3.31E-05,1.37E-05,7.50E-06,4.38E-06,2.37E-06,8.94E-07,
     5 2.59E-07,8.00E-08/
      Data O3Max /0.4444,0.4428,0.4406,0.4383,0.4359,0.4336,0.4308,
     1 0.4278,0.4235,0.4182,0.4092,0.3979,0.3856,0.3727,0.3552,0.336,
     2 0.3132,0.289,0.2669,0.2537,0.2464,0.239,0.2302,0.219,0.2048,
     3 0.1887,0.113,0.0482,1.69E-02,5.43E-03,1.91E-03,8.24E-04,
     4 3.03E-04,9.99E-05,2.93E-05,1.04E-05,5.92E-06,4.39E-06,1.95E-06,
     5 6.75E-07,1.50E-07/
c      
      INTRP=0
      N=41
      DO 10 IZ=1,N
      if(INTRP.ne.0)goto 10
      IF(abs(Z-ZREF(IZ)).le.1e-4)goto 89
      if(Z.gt.ZREF(IZ))goto 10
      if(IZ.le.2)goto 20
      if(IZ.ge.(N-1))goto 21
      INTRP=1
      J1=IZ-1
      goto 10
 20   continue
      INTRP=1
      J1=Max(Iz-1,1)
      goto 10
 21   continue
      INTRP=1
      J1=Min(iz-1,(N-1))
      goto 10
 89   continue
      INTRP=2
      Ozmin=O3min(IZ)
      Ozmax=O3max(IZ)
 10   CONTINUE
      if(INTRP.eq.2)goto 99
      xp=zref(j1)
      xp1=zref(j1+1)
      Ozmin0=O3min(j1)
      Ozmin1=O3min(j1+1)
      Ozmax0=O3max(j1)
      Ozmax1=O3max(j1+1)
c
      Call Interp (1,Xp,Xp1,Z,Ozmin0,Ozmin1,Ozmin)
      Call Interp (1,Xp,Xp1,Z,Ozmax0,Ozmax1,Ozmax)
      
 99   CONTINUE
      return
      end
c
c
c
      Subroutine Phase1(Wvl,RH,ja,IRH0,NN,IWV0,Inter1,Inter2)
c
c      Determines if interpolation in wavelength and/or humidity is
c      necessary for the phase functions of the four Shettle & Fenn 
c      aerosol models.
c
      Real Wvlref(5),RHref(4)
c
      Data Wvlref /.3,.55,.694,1.06,1.536/
      Data RHref /0.,70.,80.,99./
c
c      Determine the boundary humidity indices
c
      Do 10 IRH=1,3
      If(RH.lt.99.)goto 11
      IRH0=4
      IRH1=4
      goto 10
 11   continue
      if(abs(RH-RHref(irh)).gt.1e-3)goto 12
      IRH0=irh
      IRH1=irh
      goto 10
 12   continue
      If(RH.lt.RHref(irh).or.RH.gt.RHref(irh+1))goto 10
      IRH0=irh
      IRH1=irh+1
 10   continue
c
c      Determine the boundary wavelength indices
c
      Do 20 IWL=1,4
      If(Wvl.gt.0.3)goto 21
      IWV0=1
      IWV1=1
      goto 20
 21   continue
      If(Wvl.lt.1.536)goto 22
      IWV0=5
      Iwv1=5
      goto 20
 22   continue
      If(abs(Wvl-Wvlref(iwl)).gt.5e-5)goto 23
      IWV0=iwl
      IWV1=iwl
      goto 20
 23   continue
      If(Wvl.lt.Wvlref(iwl).or.Wvl.gt.Wvlref(iwl+1))goto 20
      IWV0=iwl
      IWV1=iwl+1
 20   continue
c
c      Determine the (aerosol type, humidity) combination to select the
c      appropriate phase function
c
      NN=4*(JA-1)+IRH0
      Inter1=0
      Inter2=0
      If(IWV1.ne.IWV0)Inter2=1
      If(IRH1.ne.IRH0)Inter1=1
      If(IWV1.eq.IWV0.or.IRH1.eq.IRH0)goto 32
      Inter1=1
      Inter2=1
 32   continue
      Return
      End
c
c
c
      Subroutine Phase2(wvl0,ja,a0,a1,a2,A3,B0,B1,B2)
c
c      Calculates the spectral coefficients for the SRA aerosols, based 
c      on fits of Putsay's data. These phase functions are normalized to
c      1.0.
c
      REAL C01(6),C02(6),C03(6),C11(6),C12(6),C13(6)
      REAL C21(6),C22(6),C23(6),C31(6),C32(6),C33(6)
      REAL D01(6),D11(6),D21(6),D02(6),D12(6),D22(6)
      REAL D03(6),D13(6),D23(6)
c
      DATA C01 /4.1983,-4.5782,1.8095,.023009,-1.4302,.81463/
      DATA C02 /.84772,.19722,-.26051,.13604,-1.4856,.85833/
      DATA C03 /5.3667,-2.6223,.36658,0.,.024301,0./
      DATA C11 /-6.3223,6.8886,-1.5299,.19363,-1.2267,.63892/
      DATA C12 /-.045872,-4.8712,3.927,-.49406,-1.2365,.70588/
      DATA C13 /-1.9614,.24074,1.5583,-.34084,-.053584,.45265/
      DATA C21 /3.7043,-5.0188,1.2353,-.13367,-1.1047,.53007/
      DATA C22 /-.13977,3.3633,-3.1214,.48134,-1.1779,.62786/
      DATA C23 /-1.3035,3.7942,-3.9983,.61291,-1.5559,2.0313/
      DATA C31 /-.74424,1.1329,-.29929,.020114,-1.0218,.48272/
      DATA C32 /-6.0261E-4,-.70206,.75497,-.16314,-.87101,.36801/
      DATA C33 /.37894,-1.1823,1.2189,-.24736,-2.5674,3.1382/
      DATA D01 /-5.0313,60.924,377.11,-.56641,-10.092,35.529/
      DATA D11 /-179.83,299.04,-171.04,31.246,-3.9319,18.244/
      DATA D21 /120.84,-320.92,182.49,-33.577,-3.4496,12.734/
      DATA D02 /787.06,267.02,531.65,0.,490.27,0./
      DATA D12 /-16.651,26.858,-16.459,3.4092,-3.8694,14.497/
      DATA D22 /11.956,-31.224,16.935,-3.598,-3.4317,10.448/
      DATA D03 /4184.9,92707.,-26720.,2893.,-254.94,4683.5/
      DATA D13 /-3236.7,6653.4,-4421.3,927.19,-8.8844,208.63/
      DATA D23 /258.04,-852.99,647.57,-144.5,-6.9472,31.93/
c      
      WVL=MIN(2.5,WVL0)
      WVL2=WVL*WVL
      WVL3=WVL2*WVL
c      
      IF(JA.NE.5)GOTO 11
      A0=(C01(1)+C01(2)*WVL+C01(3)*WVL2+C01(4)*WVL3)/(1.+C01(5)*WVL+
     2  C01(6)*WVL2)
      A1=(C11(1)+C11(2)*WVL+C11(3)*WVL2+C11(4)*WVL3)/(1.+C11(5)*WVL+
     2  C11(6)*WVL2)
      A2=(C21(1)+C21(2)*WVL+C21(3)*WVL2+C21(4)*WVL3)/(1.+C21(5)*WVL+
     2  C21(6)*WVL2)
      A3=(C31(1)+C31(2)*WVL+C31(3)*WVL2+C31(4)*WVL3)/(1.+C31(5)*WVL+
     2  C31(6)*WVL2)
      B0=(D01(1)+D01(2)*WVL+D01(3)*WVL2+D01(4)*WVL3)/(1.+D01(5)*WVL+
     2  D01(6)*WVL2)
      B1=(D11(1)+D11(2)*WVL+D11(3)*WVL2+D11(4)*WVL3)/(1.+D11(5)*WVL+
     2  D11(6)*WVL2)
      B2=(D21(1)+D21(2)*WVL+D21(3)*WVL2+D21(4)*WVL3)/(1.+D21(5)*WVL+
     2  D21(6)*WVL2)
      GOTO 19
 11   continue
      IF(JA.NE.6)GOTO 12
      A0=(C02(1)+C02(2)*WVL+C02(3)*WVL2+C02(4)*WVL3)/(1.+C02(5)*WVL+
     2  C02(6)*WVL2)
      A1=(C12(1)+C12(2)*WVL+C12(3)*WVL2+C12(4)*WVL3)/(1.+C12(5)*WVL+
     2  C12(6)*WVL2)
      A2=(C22(1)+C22(2)*WVL+C22(3)*WVL2+C22(4)*WVL3)/(1.+C22(5)*WVL+
     2  C22(6)*WVL2)
      A3=(C32(1)+C32(2)*WVL+C32(3)*WVL2+C32(4)*WVL3)/(1.+C32(5)*WVL+
     2  C32(6)*WVL2)
      B0=(D02(1)+D02(2)*WVL+D02(3)*WVL2+D02(4)*WVL3)/(1.+D02(5)*WVL+
     2  D02(6)*WVL2)
      B1=(D12(1)+D12(2)*WVL+D12(3)*WVL2+D12(4)*WVL3)/(1.+D12(5)*WVL+
     2  D12(6)*WVL2)
      B2=(D22(1)+D22(2)*WVL+D22(3)*WVL2+D22(4)*WVL3)/(1.+D22(5)*WVL+
     2  D22(6)*WVL2)
      GOTO 19
 12   continue
      IF(JA.NE.7)GOTO 19
      A0=(C03(1)+C03(2)*WVL+C03(3)*WVL2+C03(4)*WVL3)/(1.+C03(5)*WVL+
     2  C03(6)*WVL2)
      A1=(C13(1)+C13(2)*WVL+C13(3)*WVL2+C13(4)*WVL3)/(1.+C13(5)*WVL+
     2  C13(6)*WVL2)
      A2=(C23(1)+C23(2)*WVL+C23(3)*WVL2+C23(4)*WVL3)/(1.+C23(5)*WVL+
     2  C23(6)*WVL2)
      A3=(C33(1)+C33(2)*WVL+C33(3)*WVL2+C33(4)*WVL3)/(1.+C33(5)*WVL+
     2  C33(6)*WVL2)
      B0=(D03(1)+D03(2)*WVL+D03(3)*WVL2+D03(4)*WVL3)/(1.+D03(5)*WVL+
     2  D03(6)*WVL2)
      B1=(D13(1)+D13(2)*WVL+D13(3)*WVL2+D13(4)*WVL3)/(1.+D13(5)*WVL+
     2  D13(6)*WVL2)
      B2=(D23(1)+D23(2)*WVL+D23(3)*WVL2+D23(4)*WVL3)/(1.+D23(5)*WVL+
     2  D23(6)*WVL2)
 19   CONTINUE
      Return
      End
c
c
c
      BLOCK DATA PhData
c
c      Contains all the data to calculate the phase functions
c      (normalized to 1.0) of the Shettle & Fenn aerosols.
c      MnPhFn is a large look-up table to select the index for the
c      appropriate phase function to be used. These have been fitted as
c      a function of scattering angle from the MODTRAN2 block data.
c      They are then interpolated in wavelength and/or humidity in 
c      subroutine PHASE1.
c
      Common /MnPhFn/ Mum1(5),Mum2(5),Mum3(5),Mum4(5),
     2                Mum5(5),Mum6(5),Mum7(5),Mum8(5),
     3                Mum9(5),Mum10(5),Mum11(5),Mum12(5),
     4                Mum13(5),Mum14(5),Mum15(5),Mum16(5)
      Common /COEF/ coef1(4),coef2(4),coef3(4),coef4(4),coef5(4),
     2  coef6(4),coef7(4),coef8(4),coef9(4),coef10(4),coef11(4),
     3  coef12(4),coef13(4),coef14(4),coef15(4),coef16(4),coef17(4),
     4  coef18(4),coef19(4),coef20(4),coef21(4),coef22(4),coef23(4),
     5  coef24(4),coef25(4),coef26(4)
      
      Data Mum1 /3,2,2,2,4/
      Data Mum2 /3,2,2,2,4/
      Data Mum3 /3,3,3,13,4/
      Data Mum4 /21,23,23,23,24/
      Data Mum5 /11,12,13,13,4/
      Data Mum6 /1,12,12,13,4/
      Data Mum7 /1,12,12,13,4/
      Data Mum8 /21,23,23,23,23/
      Data Mum9 /7,7,7,14,14/
      Data Mum10 /22,7,8,8,15/
      Data Mum11 /5,9,9,9,10/
      Data Mum12 /20,26,26,26,25/
      Data Mum13 /11,17,17,18,18/
      Data Mum14 /11,17,17,17,18/
      Data Mum15 /16,19,19,17,18/
      Data Mum16 /15,16,16,19,19/
      Data coef1 /75.076,15.489,-.63666,13.223/
      Data coef2 /14.112,-1.5781,.11803,1.2411/
      Data coef3 /31.334,.078401,.15612,4.8454/
      Data coef4 /13.636,-1.9645,.10512,.3839/
      Data coef5 /232.3,-48.726,3.135,4.8947/
      Data coef6 /286.85,-63.482,3.9161,3.8698/
      Data coef7 /15.367,-1.8062,.091857,.49924/
      Data coef8 /8.9154,-1.2369,.055255,.05388/
      Data coef9 /41.211,-6.5633,.35612,.75109/
      Data coef10 /7.991,-.91351,.03869,.070524/
      Data coef11 /1.525,678.18,-29.405,428.76/
      Data coef12 /26.742,-2.6073,.23645,2.5487/
      Data coef13 /16.769,-2.4267,.15085,.84414/
      Data coef14 /3.3641,-.37048,.013674,-.013413/
      Data coef15 /4.1232,-.5204,.021062,-.025236/
      Data coef16 /1.993,17273.,-809.84,8574.4/
      Data coef17 /.79497,.080383,-.002775,.09951/
      Data coef18 /.72947,.47274,-.012227,.62264/
      Data coef19 /1.1549,.23963,-.0088949,.204/
      Data coef20 /1698.,-425.03,28.967,30.9/
      Data coef21 /525.35,131.03,-9.2447,67.011/
      Data coef22 /56.571,-8.8617,.48769,1.3827/
      Data coef23 /101.15,-11.792,1.0288,7.0975/
      Data coef24 /48.028,-8.692,.5231,1.0421/
      Data coef25 /37.104,-5.8167,.31294,.56116/
      Data coef26 /256.51,-62.645,4.4611,3.2847/
      End
c
c
c
      Function PhF1 (model,iwv,xs)
c
c      Selects the proper phase function for the Shettle & Fenn aerosols
c
c      Stores the coefficients for 26 phase functions for the Shettle & 
c      Fenn aerosols.
c      (Note that only 26 functions are used from a total of 70 available
c      functions in MODTRAN2; all these functions are normalized to 1) 
c      These functions have been fitted to the data in MODTRAN2,
c      so that they are now continuous functions of the scattering angle,
c      xs, for xs<10 deg.      
c
      Real C(4)
      Common /MnPhFn/ Mnum(5,16)
      Common /COEF/ Coeff(4,26)
c
      M=Mnum(iwv,model)
      Do 1 I=1,4
      C(I)=Coeff(I,M)
 1    continue
      PhF1=(C(1)+C(2)*xs+C(3)*xs*xs)/(1.+C(4)*xs)
      Return
      End
C
C
      SUBROUTINE RefAtm(Z,Pm,Tm,TO3m,O3m,RHm,Wm,TO3ini,k)
      
      REAL Zref(50),Tref(50,10),Pref(50,10)
      Real RHref(50,10),O3ref(50,10),Wref(50,10),TO3(50,10)
c
      Data Zref /0.,0.5,1.,1.5,2.,2.5,3.,3.5,4.,5.,6.,7.,8.,9.,10.,
     1 11.,12.,13.,14.,15.,16.,17.,18.,19.,20.,21.,22.,23.,24.,25.,
     2 27.5,30.,32.5,35.,37.5,40.,42.5,45.,47.5,50.,55.,60.,65.,70.,
     3 75.,80.,85.,90.,95.,100./
c
      Data (Tref(i,1),i=1,50) /288.15,284.90,281.65,278.40,275.15,
     1 271.91,268.66,265.41,262.17,255.68,249.19,242.70,236.22,229.73,
     2 223.25,216.77,216.65,216.65,216.65,216.65,216.65,216.65,216.65,
     3 216.65,216.65,217.58,218.57,219.57,220.56,221.55,224.03,226.51,
     4 229.58,236.51,243.44,250.35,257.25,264.16,270.65,270.65,260.77,
     5 247.02,233.29,219.59,208.40,198.64,188.89,186.87,188.42,195.08/
      Data (Tref(i,2),i=1,50) /294.15,291.90,289.65,287.40,285.15,
     1 282.15,279.15,276.15,273.15,267.15,261.15,254.65,248.15,241.65,
     2 235.15,228.80,222.30,215.80,215.70,215.70,215.70,215.70,216.80,
     3 217.90,219.20,220.40,221.60,222.80,223.90,225.10,228.50,233.70,
     4 239.00,245.20,251.30,257.50,263.70,269.90,275.20,275.70,269.30,
     5 257.10,240.10,218.10,196.10,174.10,165.10,165.00,178.30,190.50/
      Data (Tref(i,3),i=1,50) /272.15,270.40,268.65,266.90,265.15,
     1 263.40,261.65,258.65,255.65,249.65,243.65,237.65,231.65,225.65,
     2 219.65,219.20,218.70,218.20,217.70,217.20,216.70,216.20,215.70,
     3 215.20,215.20,215.20,215.20,215.20,215.20,215.20,215.50,217.40,
     4 220.40,227.90,235.50,243.20,250.80,258.50,265.10,265.70,260.60,
     5 250.80,240.90,230.70,220.40,210.10,199.80,199.50,208.30,218.60/
      Data (Tref(i,4),i=1,50) /287.15,284.45,281.75,279.05,276.35,
     1 273.65,270.95,268.25,265.55,260.15,253.15,246.15,239.15,232.15,
     2 225.15,225.15,225.15,225.15,225.15,225.15,225.15,225.15,225.15,
     3 225.15,225.15,225.15,225.15,225.15,226.60,228.10,231.00,235.10,
     4 240.00,247.20,254.60,262.10,269.50,273.60,276.20,277.20,274.00,
     5 262.70,239.70,216.60,193.60,170.60,161.70,161.60,176.80,190.40/
      Data (Tref(i,5),i=1,50) /257.15,258.15,259.15,257.55,255.95,
     1 254.35,252.75,251.15,247.75,240.95,234.15,227.30,220.55,217.15,
     2 217.15,217.15,217.15,217.15,217.15,217.15,216.60,216.00,215.40,
     3 214.80,214.20,213.60,213.00,212.40,211.80,211.20,213.60,216.00,
     4 218.50,222.30,228.50,234.70,240.80,247.00,253.20,259.30,259.10,
     5 250.90,248.40,245.40,234.70,223.90,213.10,202.30,211.00,218.50/
      Data (Tref(i,6),i=1,50) /299.65,296.65,293.65,290.65,287.65,
     1 286.95,283.65,280.32,277.00,270.30,263.60,257.00,250.30,243.60,
     2 237.00,230.10,223.60,217.00,210.30,203.70,197.00,194.80,198.80,
     3 202.70,206.70,210.70,214.60,217.00,219.20,221.40,227.00,232.30,
     4 237.70,243.10,248.50,254.00,259.40,264.80,269.60,270.20,263.40,
     5 253.10,236.00,218.90,201.80,184.80,177.10,177.00,184.30,190.70/
      Data (Tref(i,7),i=1,50) /301.15,297.40,293.65,290.90,288.15,
     1 285.40,282.65,279.90,277.15,271.65,266.15,259.15,252.15,245.15,
     2 238.15,231.15,224.15,217.15,210.15,203.50,203.15,205.20,207.38,
     3 209.57,211.75,213.93,215.94,217.92,219.90,221.88,226.85,231.80,
     4 237.70,243.10,248.50,254.00,259.40,264.80,269.60,270.20,263.40,
     5 253.10,236.00,218.90,201.80,184.80,177.10,177.00,184.30,190.70/
      Data (Tref(i,8),i=1,50) /287.15,285.65,284.15,282.65,281.15,
     1 277.90,274.65,271.40,268.15,261.65,255.15,248.65,242.15,235.65,
     2 229.15,222.65,216.15,213.55,210.95,208.35,205.75,203.15,203.15,
     3 205.65,208.15,210.65,213.15,215.15,217.15,219.15,224.15,229.15,
     4 234.48,239.80,245.13,250.56,255.88,261.21,265.94,266.54,259.83,
     5 249.67,232.80,215.93,199.06,182.29,174.70,174.60,181.80,188.11/
      Data (Tref(i,9),i=1,50) /278.15,276.85,275.55,274.25,272.95,
     1 271.65,268.40,265.15,261.45,255.51,248.90,242.40,235.90,229.36,
     2 226.66,227.66,228.65,229.65,230.15,230.15,230.15,230.15,230.15,
     3 230.15,230.15,230.15,230.15,230.15,230.71,231.90,234.88,237.86,
     4 240.00,247.20,254.60,262.10,269.50,273.60,276.20,277.20,274.00,
     5 262.70,239.70,216.60,193.60,170.60,161.70,161.60,176.80,190.40/
      Data (Tref(i,10),i=1,50) /249.15,250.65,252.15,253.65,250.90,
     1 248.15,245.40,242.65,239.90,234.38,228.87,223.36,217.86,214.90,
     2 214.40,213.90,213.25,212.45,211.65,210.85,210.05,209.26,208.46,
     3 207.66,207.65,207.65,207.65,207.65,207.65,207.65,207.65,207.65,
     4 210.50,214.30,220.50,226.70,232.80,239.00,245.20,251.30,251.10,
     5 242.90,240.40,237.40,226.70,215.90,205.10,194.30,203.00,210.50/
c
      Data (Pref(i,1),i=1,50) /1013.25,954.61,898.76,845.59,795.01,
     1 746.91,701.21,657.8,616.6,540.48,472.17,411.05,356.51,308.,
     2 264.99,226.99,193.99,165.79,141.7,121.11,103.52,88.5,75.65,
     3 64.67,55.29,47.29,40.47,34.67,29.72,25.49,17.43,11.97,8.01,
     4 5.746,4.15,2.871,2.06,1.491,1.09,0.7978,0.425,0.219,0.109,
     5 0.0522,0.024,0.0105,0.00446,0.00184,0.00076,0.00032/
      Data (Pref(i,2),i=1,50) /1013.5,956.5,902.2,850.6,801.6,754.9,
     1 710.5,668.2,628.,553.6,486.6,426.4,372.4,324.,280.9,242.6,208.6,
     2 178.6,152.5,130.3,111.3,95,81.2,69.5,59.5,51,43.7,37.6,32.2,
     3 27.7,19.07,13.2,9.3,6.52,4.64,3.33,2.41,1.76,1.29,0.951,0.515,
     4 0.272,0.139,0.067,0.03,0.012,0.00448,0.00164,0.000625,0.000258/
      Data (Pref(i,3),i=1,50) /1018.,956.,897.4,842.,789.7,740.4,693.8,
     1 649.8,608.1,531.3,462.7,401.6,347.3,299.3,256.8,219.9,188.2,
     2 161.1,137.8,117.8,100.7,86.1,73.6,62.8,53.7,45.8,39.1,33.4,28.6,
     3 24.4,16.46,11.1,7.56,5.18,3.6,2.53,1.8,1.29,0.94,0.683,0.362,
     4 0.188,0.095,0.047,0.0222,0.0103,0.00456,0.00198,0.000877,
     5 0.0004074/
      Data (Pref(i,4),i=1,50) /1010,951.6,896.,843.2,793.,745.3,700.,
     1 657.1,616.4,541.4,474.,413.4,359.2,310.8,267.7,230.1,197.8,170.,
     2 146.1,125.6,108,92.85,79.83,68.64,59,50.7,43.6,37.5,32.28,27.8,
     3 19.23,13.4,9.4,6.61,4.72,3.4,2.48,1.82,1.34,0.987,0.537,0.288,
     4 0.147,0.071,0.032,0.0125,0.00451,0.00161,0.000606,0.000248/
      Data (Pref(i,5),i=1,50) /1013.5,948.4,887.8,831.,777.5,727.1,
     1 679.8,635.2,593.2,515.8,446.7,385.3,330.8,282.9,241.8,206.7,
     2 176.6,151.,129.1,110.3,94.31,80.58,68.82,58.75,50.14,42.77,
     3 36.47,31.09,26.49,22.56,15.13,10.2,6.91,4.701,3.23,2.243,1.57,
     4 1.113,0.79,0.5719,0.299,0.155,0.079,0.04,0.02,0.00966,0.0045,
     5 0.002022,0.000907,0.000423/
      Data (Pref(i,6),i=1,50) /1013.25,957.5,904.2,853.3,804.8,758.6,
     1 714.8,673.,633.2,559.2,492.4,432.1,377.9,329.3,285.9,247.2,212.8,
     2 182.4,155.6,132.1,111.5,93.7,78.9,66.6,56.5,48.,40.9,35.,30.,
     3 25.7,17.63,12.2,8.52,6,4.26,3.05,2.2,1.59,1.16,0.854,0.456,0.239,
     4 0.121,0.058,0.026,0.011,0.0044,0.00172,0.000688,0.000289/
      Data (Pref(i,7),i=1,50) /1013.5,957.9,904.6,853.6,805.1,758.8,
     1 714.8,672.9,633.1,559.3,492.9,433.1,379.1,330.7,287.3,248.6,
     2 214.1,183.6,156.6,132.9,112.5,95.3,80.8,68.7,58.5,49.9,42.6,36.4,
     3 31.2,26.8,18.45,14.,8.52,6.,4.26,3.05,2.2,1.59,1.16,0.854,0.456,
     4 0.239,0.121,0.058,0.026,0.011,0.0044,0.00172,0.000688,0.000289/
      Data (Pref(i,8),i=1,50) /1021.,962.1,906.4,853.5,803.5,756.,710.7,
     1 667.7,626.8,551.,482.8,421.6,366.8,317.9,274.4,235.9,201.9,172.2,
     2 146.6,124.6,105.6,89.4,75.5,63.9,54.2,46.,39.2,33.4,28.5,24.4,
     3 16.65,12.6,8.52,6.,4.26,3.05,2.2,1.59,1.16,0.854,0.456,0.239,
     4 0.121,0.058,0.026,0.011,0.0044,0.00172,0.000688,0.000289/
      Data (Pref(i,9),i=1,50) /1012.5,952.1,895.,841.1,790.2,742.2,
     1 696.7,653.5,612.5,536.7,468.6,407.8,353.5,305.2,262.6,226.,194.6,
     2 167.7,144.6,124.7,107.5,92.7,80.,69.,59.5,51.3,44.3,38.2,33.,
     3 28.5,19.85,13.8,9.4,6.61,4.72,3.4,2.48,1.82,1.34,0.987,0.537,
     4 0.288,0.147,0.071,0.032,0.0125,0.00451,0.00161,0.000606,0.000248/
      Data (Pref(i,10),i=1,50) /1013.5,946.4,884.1,826.3,772.1,721.,
     1 672.7,627.2,584.3,505.8,436.4,375.2,321.4,274.3,234.,199.5,170.1,
     2 144.9,123.4,105.,89.3,75.9,64.49,54.75,46.48,39.45,33.49,28.43,
     3 24.14,20.5,13.67,9.05,6.91,4.701,3.23,2.243,1.57,1.113,0.79,
     4 0.5719,0.299,0.155,0.079,0.04,0.02,0.00966,0.0045,0.002022,
     5 0.000907,0.000423/
c
      Data (RHref(i,1),i=1,50) /46.04,47.61,49.18,50.67,52.16,51.53,
     1 50.91,50.56,50.21,48.49,49.28,48.17,50.50,36.98,28.87,27.54,
     2 12.61,6.134,2.864,2.065,1.394,1.162,0.987,0.849,0.735,0.572,
     3 0.444,0.349,0.272,0.214,1.38E-01,6.10E-02,3.58E-02,1.06E-02,
     4 6.04E-03,1.47E-03,8.61E-04,2.51E-04,5.36E-04,8.20E-04,9.15E-05,
     5 1.43E-04,2.90E-04,4.37E-04,6.43E-04,8.48E-04,6.38E-04,4.28E-04,
     6 2.19E-04,8.68E-06/
      Data (RHref(i,2),i=1,50) /76.18,71.10,66.02,60.61,55.20,50.25,
     1 45.29,42.17,39.05,31.42,29.98,30.31,29.63,30.15,29.44,19.48,
     2 10.69,5.423,2.933,1.695,1.404,1.166,0.856,0.651,0.492,0.382,
     3 0.297,0.238,0.186,0.147,8.90E-02,3.10E-02,1.81E-02,5.23E-03,
     4 3.08E-03,9.37E-04,5.68E-04,1.99E-04,1.35E-04,7.13E-05,5.99E-05,
     5 7.75E-05,3.93E-04,7.08E-04,3.66E-02,7.24E-02,5.43E-02,3.62E-02,
     6 1.81E-02,1.46E-05/
      Data (RHref(i,3),i=1,50) /77.07,73.79,70.50,67.97,65.45,61.10,
     1 56.74,53.32,49.89,47.17,44.02,31.03,23.01,19.66,17.93,5.505,
     2 3.001,2.274,1.984,1.765,1.57,1.396,1.27,1.153,0.986,0.841,0.723,
     3 0.62,0.537,0.463,3.13E-01,1.64E-01,9.36E-02,2.33E-02,1.29E-02,
     4 2.45E-03,1.39E-03,3.28E-04,2.12E-04,9.67E-05,7.51E-05,8.27E-05,
     5 9.44E-05,1.06E-04,1.34E-04,1.62E-04,1.22E-04,8.12E-05,4.08E-05,
     6 4.38E-07/
      Data (RHref(i,4),i=1,50) /75.23,72.64,70.04,69.96,69.89,67.51,
     1 65.12,62.80,60.47,53.46,50.43,49.11,41.19,23.59,14.17,3.819,
     2 1.481,0.944,0.729,0.629,0.539,0.469,0.428,0.385,0.339,0.298,
     3 0.261,0.226,0.167,0.123,7.60E-02,2.90E-02,1.67E-02,4.46E-03,
     4 2.55E-03,6.45E-04,3.95E-04,1.44E-04,1.02E-04,5.99E-05,4.01E-05,
     5 4.69E-05,4.25E-04,8.04E-04,7.49E-02,1.49E-01,1.12E-01,7.45E-02,
     6 3.73E-02,1.43E-05/
      Data (RHref(i,5),i=1,50) /80.46,74.88,69.30,69.62,69.93,67.79,
     1 65.64,63.02,60.40,54.09,50.71,55.97,23.73,26.84,15.42,6.589,
     2 3.378,2.142,1.852,1.6,1.488,1.384,1.287,1.197,1.113,1.035,0.963,
     3 0.895,0.832,0.766,4.77E-01,1.88E-01,1.14E-01,4.09E-02,2.30E-02,
     4 5.05E-03,2.91E-03,7.65E-04,4.50E-04,1.35E-04,7.01E-05,6.76E-05,
     5 4.43E-05,2.10E-05,2.45E-05,2.79E-05,2.10E-05,1.42E-05,7.32E-06,
     6 4.61E-07/
      Data (RHref(i,6),i=1,50) /75.59,74.21,72.83,73.71,74.58,61.44,
     1 48.29,41.61,34.94,37.74,34.82,32.01,29.49,25.37,19.52,13.16,
     2 9.26,5.886,7.416,9.931,16.78,19.25,8.328,3.755,1.82,0.923,0.504,
     3 0.332,0.24,0.161,9.46E-02,2.82E-02,1.68E-02,5.45E-03,3.31E-03,
     4 1.17E-03,7.24E-04,2.78E-04,1.91E-04,1.04E-04,9.36E-05,1.15E-04,
     5 3.96E-04,6.77E-04,4.68E-03,8.68E-03,6.51E-03,4.35E-03,2.18E-03,
     6 1.58E-05/
      Data (RHref(i,7),i=1,50) /80.00,70.00,65.00,62.50,60.00,60.00,
     1 60.00,55.00,50.00,45.00,40.00,40.00,40.00,35.00,30.00,15.69,
     2 9.83,5.701,5.623,6.637,10.630,12.016,5.339,2.513,1.289,0.707,
     3 0.421,0.294,0.218,0.155,9.24E-02,2.93E-02,1.73E-02,5.36E-03,
     4 3.22E-03,1.08E-03,6.62E-04,2.46E-04,1.69E-04,9.09E-05,8.01E-05,
     5 1.00E-04,3.95E-04,6.89E-04,1.74E-02,3.42E-02,2.56E-02,1.71E-02,
     6 8.55E-03,1.53E-05/
      Data (RHref(i,8),i=1,50) /80.00,75.00,70.00,60.00,50.00,47.50,
     1 45.00,40.00,35.00,32.50,30.00,30.00,30.00,30.00,30.00,13.16,
     2 9.26,5.886,7.416,9.931,16.780,19.250,8.328,3.755,1.820,0.923,
     3 0.504,0.332,0.240,0.161,9.46E-02,2.82E-02,1.68E-02,5.45E-03,
     4 3.31E-03,1.17E-03,7.24E-04,2.78E-04,1.91E-04,1.04E-04,9.36E-05,
     5 1.15E-04,3.96E-04,6.77E-04,4.68E-03,8.68E-03,6.51E-03,4.35E-03,
     6 2.18E-03,1.58E-05/
      Data (RHref(i,9),i=1,50) /85.00,80.00,75.00,70.00,65.00,65.00,
     1 60.00,57.50,55.00,50.00,45.00,40.00,35.00,32.00,20.00,9.22,
     2 4.73,2.999,2.593,2.240,2.083,1.938,1.802,1.676,1.558,1.449,
     3 1.348,1.253,1.165,1.072,6.68E-01,2.63E-01,1.60E-01,5.73E-02,
     4 3.22E-02,7.07E-03,4.07E-03,1.07E-03,6.30E-04,1.89E-04,9.81E-05,
     5 9.46E-05,6.20E-05,2.94E-05,3.42E-05,3.91E-05,2.95E-05,1.99E-05,
     6 1.02E-05,6.45E-07/
      Data (RHref(i,10),i=1,50) /80.00,70.00,65.00,60.00,60.00,57.50,
     1 55.00,52.50,50.00,47.50,45.00,42.50,40.00,21.23,12.75,3.44,
     2 1.33,0.850,0.656,0.566,0.485,0.422,0.385,0.347,0.305,0.268,
     3 0.235,0.203,0.150,0.111,6.84E-02,2.61E-02,1.51E-02,4.01E-03,
     4 2.30E-03,5.81E-04,3.55E-04,1.30E-04,9.18E-05,5.39E-05,3.61E-05,
     5 4.22E-05,3.83E-04,7.24E-04,6.74E-02,1.34E-01,1.01E-01,6.71E-02,
     6 3.35E-02,1.29E-05/
c
      Data (O3ref(i,1),i=1,50) /0.3438,0.3425,0.3413,0.3400,0.3387,
     1 0.3375,0.3363,0.3352,0.3341,0.3319,0.3298,0.3276,0.3253,0.3224,
     2 0.3186,0.3135,0.3067,0.2990,0.2906,0.2819,0.2708,0.2586,0.2446,
     3 0.2290,0.2120,0.1942,0.1762,0.1583,0.1410,0.1247,8.981E-02,
     4 6.308E-02,4.291E-02,2.795E-02,1.743E-02,1.045E-02,6.049E-03,
     5 3.430E-03,1.921E-03,1.082E-03,3.459E-04,1.059E-04,3.238E-05,
     6 1.253E-05,5.770E-06,2.578E-06,1.101E-06,4.354E-07,1.353E-07,
     7 3.028E-07/
      Data (O3ref(i,2),i=1,50) /0.3320,0.3306,0.3292,0.3278,0.3263,
     1 0.3249,0.3235,0.3220,0.3206,0.3175,0.3144,0.3110,0.3074,0.3036,
     2 0.2995,0.2948,0.2894,0.2832,0.2757,0.2674,0.2592,0.2509,0.2417,
     3 0.2302,0.2164,0.2016,0.1864,0.1709,0.1555,0.1397,1.037E-01,
     4 7.417E-02,5.066E-02,3.248E-02,1.959E-02,1.132E-02,6.370E-03,
     5 3.659E-03,2.142E-03,1.271E-03,4.590E-04,1.554E-04,4.782E-05,
     6 1.565E-05,5.579E-06,2.206E-06,9.322E-07,3.650E-07,1.124E-07,
     74.303E-07/
      Data (O3ref(i,3),i=1,50) /0.3771,0.3757,0.3744,0.3732,0.3720,
     1 0.3709,0.3697,0.3686,0.3674,0.3649,0.3621,0.3588,0.3549,0.3500,
     2 0.3435,0.3348,0.3239,0.3108,0.2970,0.2836,0.2701,0.2557,0.2399,
     3 0.2226,0.2038,0.1840,0.1644,0.1459,0.1284,0.1122,7.872E-02,
     4 5.412E-02,3.623E-02,2.335E-02,1.441E-02,8.445E-03,4.757E-03,
     5 2.672E-03,1.503E-03,8.540E-04,2.750E-04,8.590E-05,2.758E-05,
     6 1.037E-05,4.523E-06,2.090E-06,9.869E-07,4.275E-07,1.438E-07,
     7 2.633E-07/
      Data (O3ref(i,4),i=1,50) /0.3451,0.3439,0.3427,0.3414,0.3402,
     1 0.3388,0.3375,0.3361,0.3347,0.3318,0.3287,0.3253,0.3217,0.3173,
     2 0.3117,0.3045,0.2954,0.2855,0.2751,0.2646,0.2539,0.2428,0.2310,
     3 0.2178,0.2035,0.1878,0.1710,0.1541,0.1378,0.1223,8.979E-02,
     4 6.476E-02,4.518E-02,2.959E-02,1.821E-02,1.064E-02,6.019E-03,
     5 3.454E-03,2.020E-03,1.211E-03,4.470E-04,1.540E-04,4.918E-05,
     6 1.600E-05,5.447E-06,2.110E-06,8.941E-07,3.504E-07,1.081E-07,
     7 4.893E-07/
      Data (O3ref(i,5),i=1,50) /0.3759,0.3750,0.3740,0.3730,0.3721,
     1 0.3711,0.3701,0.3691,0.3681,0.3659,0.3637,0.3609,0.3571,0.3513,
     2 0.3432,0.3342,0.3253,0.3148,0.3015,0.2861,0.2691,0.2507,0.2306,
     3 0.2086,0.1855,0.1631,0.1426,0.1240,0.1074,0.0929,6.423E-02,
     4 4.378E-02,2.916E-02,1.867E-02,1.146E-02,6.736E-03,3.824E-03,
     5 2.156E-03,1.214E-03,6.885E-04,2.260E-04,7.800E-05,2.770E-05,
     6 8.960E-06,2.905E-06,1.320E-05,7.201E-07,3.566E-07,1.351E-07,
     7 2.399E-07/
      Data (O3ref(i,6),i=1,50) /0.2776,0.2763,0.2750,0.2737,0.2724,
     1 0.2712,0.2700,0.2688,0.2677,0.2655,0.2635,0.2615,0.2597,0.2578,
     2 0.2560,0.2542,0.2522,0.2501,0.2480,0.2459,0.2437,0.2410,0.2367,
     3 0.2298,0.2204,0.2098,0.1981,0.1845,0.1692,0.1528,1.131E-01,
     4 7.803E-02,5.034E-02,3.095E-02,1.819E-02,1.036E-02,5.802E-03,
     5 3.305E-03,1.909E-03,1.110E-03,3.700E-04,1.151E-04,3.629E-05,
     6 1.449E-05,6.783E-06,2.951E-06,1.182E-06,4.409E-07,1.302E-07,
     7 3.135E-07/
      Data (O3ref(i,7),i=1,50) /0.3104,0.3090,0.3077,0.3064,0.3050,
     1 0.3037,0.3024,0.3011,0.2999,0.2980,0.2955,0.2930,0.2905,0.2879,
     2 0.2852,0.2824,0.2792,0.2757,0.2717,0.2669,0.2621,0.2572,0.2518,
     3 0.2445,0.2341,0.2219,0.2091,0.1953,0.1800,0.1640,1.346E-01,
     4 9.576E-02,6.165E-02,3.954E-02,2.394E-02,1.382E-02,7.734E-03,
     5 4.362E-03,2.541E-03,1.534E-03,6.398E-04,2.198E-04,6.960E-05,
     6 2.053E-05,8.388E-06,5.192E-06,2.962E-06,1.194E-06,3.920E-07,
     7 1.001E-07/
      Data (O3ref(i,8),i=1,50) /0.2798,0.2784,0.2770,0.2757,0.2744,
     1 0.2731,0.2718,0.2706,0.2694,0.2678,0.2656,0.2636,0.2616,0.2598,
     2 0.2580,0.2562,0.2543,0.2523,0.2503,0.2484,0.2463,0.2443,0.2414,
     3 0.2364,0.2284,0.2186,0.2080,0.1961,0.1818,0.1666,1.383E-01,
     4 9.922E-02,6.476E-02,4.063E-02,2.428E-02,1.397E-02,7.819E-03,
     5 4.400E-03,2.554E-03,1.540E-03,6.327E-04,2.069E-04,6.500E-05,
     6 1.946E-05,8.597E-06,5.428E-06,2.744E-06,1.047E-06,3.565E-07,
     7 1.015E-07/
      Data (O3ref(i,9),i=1,50) /0.3296,0.3287,0.3278,0.3268,0.3259,
     1 0.3249,0.3239,0.3229,0.3218,0.3203,0.3180,0.3156,0.3127,0.3093,
     2 0.3039,0.2974,0.2899,0.2819,0.2722,0.2614,0.2496,0.2371,0.2237,
     3 0.2088,0.1924,0.1752,0.1581,0.1414,0.1257,0.1111,8.823E-02,
     4 6.321E-02,4.452E-02,2.994E-02,1.911E-02,1.153E-02,6.658E-03,
     5 3.854E-03,2.253E-03,1.380E-03,5.915E-04,2.161E-04,7.877E-05,
     6 2.699E-05,9.977E-06,4.957E-06,3.631E-06,1.375E-06,3.911E-07,
     6 7.327E-08/
      Data (O3ref(i,10),i=1,50)/0.4496,0.4484,0.4472,0.4459,0.4447,
     1 0.4435,0.4423,0.4410,0.4397,0.4377,0.4349,0.4321,0.4280,0.4228,
     2 0.4137,0.4027,0.3917,0.3810,0.3644,0.3456,0.3207,0.2926,0.2623,
     3 0.2315,0.2006,0.1731,0.1493,0.1289,0.1113,0.0963,7.345E-02,
     4 5.127E-02,3.600E-02,2.339E-02,1.466E-02,8.741E-03,5.074E-03,
     5 2.938E-03,1.714E-03,1.041E-03,4.194E-04,1.305E-04,4.881E-05,
     6 2.002E-05,8.666E-06,4.742E-06,3.958E-06,1.740E-06,6.179E-07,
     7 1.362E-07/
c
      Data (Wref(i,1),i=1,50) /1.416E+00,1.145E+00,9.163E-01,
     1 7.247E-01,5.655E-01,4.365E-01,3.349E-01,2.552E-01,1.928E-01,
     2 1.079E-01,5.802E-02,2.937E-02,1.329E-02,5.580E-03,2.596E-03,
     3 1.350E-03,7.853E-04,5.217E-04,3.958E-04,3.242E-04,2.741E-04,
     4 2.367E-04,2.053E-04,1.784E-04,1.552E-04,1.351E-04,1.176E-04,
     5 1.023E-04,8.884E-05,7.707E-05,5.387E-05,3.762E-05,2.631E-05,
     6 1.851E-05,1.310E-05,9.338E-06,6.685E-06,4.788E-06,3.424E-06,
     7 2.438E-06,1.193E-06,5.385E-07,2.252E-07,8.616E-08,2.959E-08,
     8 9.154E-09,2.513E-09,6.605E-10,1.440E-10,2.737E-11/
      Data (Wref(i,2),i=1,50) /2.922E+00,2.289E+00,1.774E+00,
     1 1.358E+00,1.027E+00,7.709E-01,5.796E-01,4.355E-01,3.262E-01,
     2 1.860E-01,1.071E-01,5.916E-02,3.093E-02,1.485E-02,5.951E-03,
     3 2.020E-03,7.893E-04,4.701E-04,3.632E-04,3.041E-04,2.636E-04,
     4 2.300E-04,2.020E-04,1.782E-04,1.575E-04,1.392E-04,1.229E-04,
     5 1.081E-04,9.490E-05,8.307E-05,5.926E-05,4.220E-05,3.008E-05,
     6 2.153E-05,1.550E-05,1.123E-05,8.161E-06,5.905E-06,4.253E-06,
     7 3.046E-06,1.512E-06,7.007E-07,3.008E-07,1.155E-07,3.861E-08,
     8 1.118E-08,2.734E-09,6.447E-10,1.278E-10,2.429E-11/
      Data (Wref(i,3),i=1,50) /8.517E-01,6.907E-01,5.547E-01,
     1 4.394E-01,3.417E-01,2.603E-01,1.938E-01,1.419E-01,1.035E-01,
     2 5.281E-02,2.416E-02,1.034E-02,4.709E-03,2.283E-03,1.162E-03,
     3 7.322E-04,5.733E-04,4.783E-04,4.056E-04,3.452E-04,2.946E-04,
     4 2.521E-04,2.162E-04,1.854E-04,1.590E-04,1.365E-04,1.173E-04,
     5 1.007E-04,8.649E-05,7.418E-05,5.051E-05,3.446E-05,2.360E-05,
     6 1.628E-05,1.131E-05,7.919E-06,5.579E-06,3.950E-06,2.802E-06,
     7 1.983E-06,9.629E-07,4.361E-07,1.853E-07,7.327E-08,2.650E-08,
     8 8.642E-09,2.505E-09,6.931E-09,1.580E-10,3.002E-11/
      Data (Wref(i,4),i=1,50) /2.081E+00,1.671E+00,1.337E+00,
     1 1.063E+00,8.329E-01,6.446E-01,4.936E-01,3.732E-01,2.776E-01,
     2 1.457E-01,7.110E-02,3.092E-02,1.098E-02,3.529E-03,1.368E-03,
     3 7.591E-04,5.689E-04,4.770E-04,4.129E-04,3.606E-04,3.157E-04,
     4 2.769E-04,2.423E-04,2.110E-04,1.832E-04,1.587E-04,1.372E-04,
     5 1.184E-04,1.022E-04,8.832E-05,6.128E-05,4.260E-05,2.975E-05,
     6 2.095E-05,1.487E-05,1.064E-05,7.649E-06,5.510E-06,3.966E-06,
     7 2.842E-06,1.418E-06,6.625E-07,2.880E-07,1.126E-07,3.849E-08,
     8 1.122E-08,2.710E-09,6.314E-10,1.239E-10,2.354E-11/
      Data (Wref(i,5),i=1,50) /4.161E-01,3.562E-01,2.962E-01,
     1 2.398E-01,1.898E-01,1.465E-01,1.096E-01,7.955E-02,5.624E-02,
     2 2.699E-02,1.270E-02,5.324E-03,2.622E-03,1.658E-03,1.013E-03,
     3 6.885E-04,5.381E-04,4.532E-04,3.908E-04,3.369E-04,2.902E-04,
     4 2.498E-04,2.148E-04,1.845E-04,1.583E-04,1.357E-04,1.161E-04,
     5 9.920E-05,8.460E-05,7.207E-05,4.831E-05,3.252E-05,2.198E-05,
     6 1.494E-05,1.021E-05,7.031E-06,4.874E-06,3.393E-06,2.369E-06,
     7 1.656E-06,7.932E-07,3.571E-07,1.517E-07,6.133E-08,2.316E-08,
     8 7.862E-09,2.371E-09,6.804E-10,1.601E-10,3.042E-11/
      Data (Wref(i,6),i=1,50) /4.115E+00,3.250E+00,2.534E+00,
     1 1.936E+00,1.430E+00,1.036E+00,7.557E-01,5.603E-01,4.266E-01,
     2 2.439E-01,1.295E-01,6.546E-02,3.064E-02,1.293E-02,4.942E-03,
     3 1.884E-03,8.286E-04,4.799E-04,3.438E-04,2.679E-04,2.222E-04,
     4 1.889E-04,1.620E-04,1.411E-04,1.242E-04,1.100E-04,9.765E-05,
     5 8.682E-05,7.701E-05,6.820E-05,5.048E-05,3.726E-05,2.746E-05,
     6 2.026E-05,1.495E-05,1.101E-05,8.085E-06,5.922E-06,4.317E-06,
     7 3.122E-06,1.578E-06,7.346E-07,3.082E-07,1.126E-07,3.520E-08,
     8 1.012E-08,2.633E-09,6.586E-10,1.375E-10,2.613E-11/
      Data (Wref(i,7),i=1,50) /4.229E+00,3.302E+00,2.630E+00,
     1 2.106E+00,1.679E+00,1.326E+00,1.028E+00,7.877E-01,6.045E-01,
     2 3.504E-01,1.955E-01,1.034E-01,4.968E-02,2.106E-02,7.606E-03,
     3 2.394E-03,9.766E-04,5.661E-04,4.431E-04,3.886E-04,3.412E-04,
     4 2.699E-04,2.043E-04,1.648E-04,1.393E-04,1.216E-04,1.086E-04,
     5 9.785E-05,8.824E-05,7.954E-05,5.833E-05,4.666E-05,3.006E-05,
     6 2.156E-05,1.559E-05,1.134E-05,8.339E-06,6.128E-06,4.506E-06,
     7 3.334E-06,2.024E-06,1.311E-06,8.065E-07,5.496E-07,2.235E-07,
     8 5.156E-08,2.538E-08,1.649E-08,4.038E-09,4.038E-09/
      Data (Wref(i,8),i=1,50) /2.096E+00,1.649E+00,1.269E+00,
     1 9.573E-01,7.184E-01,5.303E-01,3.851E-01,2.822E-01,2.098E-01,
     2 1.171E-01,6.459E-02,3.496E-02,1.794E-02,8.485E-03,3.434E-03,
     3 1.371E-03,8.733E-04,6.892E-04,5.747E-04,4.675E-04,3.500E-04,
     4 2.365E-04,1.650E-04,1.295E-04,1.065E-04,9.062E-05,7.937E-05,
     5 7.040E-05,6.252E-05,5.568E-05,4.034E-05,3.214E-05,2.301E-05,
     6 1.690E-05,1.243E-05,9.125E-06,6.685E-06,4.847E-06,3.462E-06,
     7 2.451E-06,1.309E-06,6.930E-07,3.030E-07,1.209E-07,3.859E-08,
     8 8.343E-09,4.117E-09,2.706E-09,6.839E-10,6.839E-10/
      Data (Wref(i,9),i=1,50) /1.484E+00,1.215E+00,9.843E-01,
     1 7.863E-01,6.177E-01,4.695E-01,3.471E-01,2.562E-01,1.888E-01,
     2 1.007E-01,5.207E-02,2.719E-02,1.495E-02,9.138E-03,6.382E-03,
     3 5.053E-03,4.343E-03,3.902E-03,3.551E-03,3.239E-03,2.960E-03,
     4 2.700E-03,2.459E-03,2.235E-03,2.026E-03,1.831E-03,1.652E-03,
     5 1.484E-03,1.325E-03,1.163E-03,7.841E-04,5.275E-04,3.574E-04,
     6 2.442E-04,1.481E-04,8.482E-05,4.661E-05,2.287E-05,1.181E-05,
     7 6.077E-06,2.165E-06,5.152E-07,4.169E-08,1.743E-09,9.137E-11,
     8 9.077E-12,7.235E-12,6.812E-12,3.317E-12,3.317E-12/
      Data (Wref(i,10),i=1,50)/2.164E-01,1.858E-01,1.545E-01,
     1 1.216E-01,9.159E-02,6.824E-02,5.057E-02,3.734E-02,2.751E-02,
     2 1.459E-02,7.450E-03,3.626E-03,1.650E-03,7.449E-04,3.564E-04,
     3 1.847E-04,1.372E-04,1.170E-04,1.044E-04,9.512E-05,8.790E-05,
     4 8.225E-05,7.772E-05,7.402E-05,7.089E-05,6.814E-05,6.573E-05,
     5 6.363E-05,6.194E-05,6.070E-05,5.862E-05,5.755E-05,5.717E-05,
     6 5.684E-05,5.660E-05,5.641E-05,5.628E-05,5.616E-05,5.604E-05,
     7 5.593E-05,5.573E-05,5.561E-05,5.512E-05,5.424E-05,3.369E-05,
     8 1.103E-05,2.561E-06,1.068E-06,2.766E-07,2.766E-07/
c
      Data (TO3(i,1),i=1,50) /225.3,225.2,225.1,224.8,224.6,224.4,224.3,
     1 224.1,224.0,223.7,223.5,223.3,223.2,223.1,223.0,223.0,223.2,
     2 223.3,223.5,223.7,224.0,224.3,224.7,225.2,225.8,226.6,227.4,
     3 228.4,229.4,230.5,232.9,236.4,240.8,246.2,251.8,257.3,262.2,
     4 265.7,266.9,264.2,254.0,240.3,226.3,209.8,199.5,192.7,188.6,
     5 188.2,190.6,198.3/      
      Data (TO3(i,2),i=1,50) /231.9,231.8,231.7,231.4,231.2,230.9,230.7,
     1 230.5,230.3,229.9,229.5,229.2,228.9,228.7,228.5,228.4,228.4,
     2 228.5,228.8,229.3,229.7,230.1,230.6,231.2,231.9,232.8,233.7,
     3 234.8,235.9,237.1,239.8,244.1,248.5,253.3,258.3,263.3,267.9,
     4 271.0,271.8,269.6,261.5,248.3,229.7,204.7,180.7,169.5,167.1,
     5 170.1,182.2,196.9/
      Data (TO3(i,3),i=1,50) /220.4,220.4,220.3,220.1,219.9,219.8,219.7,
     1 219.5,219.4,219.2,218.9,218.7,218.6,218.4,218.3,218.3,218.2,
     2 218.2,218.2,218.2,218.3,218.4,218.5,218.7,219.0,219.4,219.9,
     3 220.4,221.1,222.0,223.9,227.6,232.3,238.6,244.8,250.9,256.6,
     4 261.2,263.2,261.8,255.8,245.4,233.6,221.2,211.0,204.5,202.0,
     5 204.2,212.4,225.1/
      Data (TO3(i,4),i=1,50) /233.4,233.4,233.3,233.1,232.9,232.7,232.6,
     1 232.4,232.3,232.0,231.7,231.5,231.4,231.3,231.3,231.4,231.6,
     2 231.8,232.0,232.3,232.6,232.9,233.2,233.7,234.2,234.9,235.8,
     3 236.9,238.2,239.6,242.4,246.7,251.3,256.7,262.3,267.6,271.9,
     4 273.6,273.6,271.9,265.3,250.9,228.6,202.3,177.8,165.8,163.8,
     5 166.8,180.7,197.5/
      Data (TO3(i,5),i=1,50) /217.3,217.2,217.2,217.1,217.0,216.9,216.8,
     1 216.7,216.6,216.4,216.3,216.2,216.0,216.0,216.0,215.9,215.9,
     2 215.9,215.8,215.8,215.7,215.6,215.6,215.6,215.7,215.9,216.2,
     3 216.7,217.3,218.2,220.3,223.3,226.8,231.2,236.4,241.8,247.1,
     4 252.0,255.9,257.8,255.4,248.0,243.1,235.5,222.5,212.4,210.1,
     5 206.3,213.5,222.1/
      Data (TO3(i,6),i=1,50) /229.6,229.4,229.2,228.9,228.6,228.3,228.1,
     1 227.8,227.6,227.2,226.8,226.6,226.3,226.2,226.1,226.0,226.0,
     2 226.0,226.1,226.2,226.4,226.6,227.1,227.7,228.6,229.6,230.6,
     3 231.6,232.8,234.1,236.8,240.9,245.4,250.0,254.6,259.1,263.1,
     4 265.9,266.8,264.9,257.3,244.9,226.9,205.7,189.0,181.6,178.4,
     5 180.5,187.4,195.2/
      Data (TO3(i,7),i=1,50) /229.6,229.3,229.0,228.7,228.4,228.2,227.9,
     1 227.7,227.5,227.2,226.8,226.5,226.2,225.9,225.8,225.6,225.6,
     2 225.6,225.7,226.0,226.4,226.8,227.3,227.9,228.7,229.6,230.6,
     3 231.6,232.8,234.0,236.7,240.7,245.6,250.1,254.6,259.1,263.1,
     4 265.9,266.7,264.7,257.1,244.9,227.4,206.7,189.0,181.1,178.4,
     5 180.3,187.1,195.0/
      Data (TO3(i,8),i=1,50) /227.7,227.4,227.1,226.8,226.6,226.3,226.1,
     1 225.8,225.6,225.4,225.1,224.9,224.7,224.6,224.5,224.4,224.5,
     2 224.5,224.6,224.7,224.9,225.0,225.3,225.7,226.4,227.3,228.1,
     3 229.0,230.1,231.3,233.8,237.6,242.1,246.6,251.1,255.6,259.5,
     4 262.3,263.2,261.3,253.9,241.6,223.9,202.9,186.5,179.1,176.0,
     5 178.1,184.9,192.8/
      Data (TO3(i,9),i=1,50) /235.4,235.3,235.2,235.1,234.9,234.8,234.7,
     1 234.6,234.5,234.4,234.2,234.1,234.0,234.0,234.1,234.3,234.4,
     2 234.6,234.8,235.0,235.2,235.5,235.8,236.2,236.7,237.3,238.1,
     3 239.0,240.2,241.4,243.9,247.4,251.4,257.0,262.5,267.8,271.9,
     4 273.6,273.6,272.0,265.1,249.7,227.1,202.9,179.7,165.6,163.7,
     5 167.0,180.7,188.5/
      Data (TO3(i,10),i=1,50)/211.6,211.5,211.4,211.3,211.2,211.0,210.9,
     1 210.8,210.7,210.6,210.5,210.3,210.2,210.1,210.0,209.9,209.8,
     2 209.7,209.6,209.5,209.3,209.3,209.3,209.4,209.7,210.0,210.3,
     3 210.8,211.3,211.8,213.1,215.5,218.8,223.3,228.6,234.1,239.4,
     4 244.3,248.0,249.8,247.6,240.0,235.1,227.5,214.5,204.4,202.1,
     5 198.3,205.5,213.3/
C
      TO3ini=TO3(1,k)
      INTRP=0
      N=50
      DO 10 IZ=1,N
      if(INTRP.ne.0)goto 10
      IF(abs(Z-ZREF(IZ)).le.1e-4)goto 89
      if(Z.gt.ZREF(IZ))goto 10
      if(IZ.le.2)goto 20
      if(IZ.ge.(N-1))goto 21
      INTRP=1
      J1=IZ-1
      goto 10
 20   continue
      INTRP=1
      J1=Max(Iz-1,1)
      goto 10
 21   continue
      INTRP=1
      J1=Min(iz-1,(N-1))
      goto 10
 89   continue
      INTRP=2
      RHm=RHref(iz,k)
      Tm=Tref(iz,k)
      Pm=Pref(iz,k)
      O3m=O3ref(iz,k)
      TO3m=TO3(iz,k)
      Wm=Wref(iz,k)
 10   CONTINUE
      if(INTRP.eq.2)goto 99      
      xp=zref(j1)
      xp1=zref(j1+1)
c            
      Call Interp (1,Xp,Xp1,Z,RHref(j1,k),RHref(j1+1,k),RHm)
      Call Interp (1,Xp,Xp1,Z,Tref(j1,k),Tref(j1+1,k),Tm)
      Call Interp (1,Xp,Xp1,Z,Pref(j1,k),Pref(j1+1,k),Pm)
      Call Interp (1,Xp,Xp1,Z,O3ref(j1,k),O3ref(j1+1,k),O3m)
      Call Interp (1,Xp,Xp1,Z,Wref(j1,k),Wref(j1+1,k),Wm)
      Call Interp (1,Xp,Xp1,Z,TO3(j1,k),TO3(j1+1,k),TO3m)
c
 99   CONTINUE
c
      return
      end
c
c
c
      Subroutine ScanG(step,FWHM,fk,sigma2,N)
c
c      Smoothes irradiance with a Gaussian filter
c
      Real wv(2002),ET(2002),limit1,limit2
      DOUBLE PRECISION TB,TG,TT,TX
      DOUBLE PRECISION BNORM(2002),GLOBH(2002),GLOBT(2002),DIRX(2002)
      COMMON /SOLAR1/ WV,WvLMN,WvLMX,WV1,WV2
      COMMON /SOLAR2/ BNORM,GLOBH,GLOBT,DIRX,ET
c
      if(wv1.gt.wvlmn+fwhm)goto 10
      if(wv1.gt.wvlmn+.5*fwhm)goto 11
      write(18,100)
 100  format(' ** WARNING #14',/,'Lower limit for scans needs to',
     1  'be > WV1 + 0.5*FWHM!',/)
      goto 999
 11   continue
      write(18,101)
 101  format(' ** WARNING #15 ',9('*'),/,'\\ Lower limit for scans is',
     1 ' not > WV1 + FWHM.',/,'\\ This will reduce accuracy in the ',
     2 'results for the first wavelengths.',/)
 10   continue
      if(wv2.lt.wvlmx-fwhm)goto 20
      if(wv2.lt.wvlmx-.5*fwhm)goto 21
      write(18,102)
 102  format(' ** WARNING #16',/,'Upper limit for scans needs to',
     1 ' be < WV2 - 0.5*FWHM!',/)
      goto 999
 21   continue
      write(18,103)
 103  format('** WARNING #17',9('*'),/,'\\ Upper limit for scans is',
     1 ' not < WV2 - FWHM.',/,'\\ This will reduce accuracy in the ',
     2 'results for the last wavelengths.',/)
 20   continue
c
c      Find the limits for spectral integration
c
      wvc=wv1
 40   continue
      dw1=1.
      d1=aint(wvc-fk)
      if(d1.le.400.)dw1=0.5
      if(d1.gt.1705)dw1=5.
      limit1=Max(wvlmn,d1-dw1)
      dw2=1.
      d2=aint(wvc+fk+1.)
      if(d2.le.400.)dw2=0.5
      if(d2.gt.1705)dw2=5.
      limit2=Min(wvlmx,d2+dw2)
      Nw=int((limit2-limit1)/dw1)+1
c
      do 30 j=1,N
      if(abs(wv(j)-limit1).gt.1e-3)goto 30
       index=j
 30   continue
c
      T0=0.
      TB=0.
      TG=0.
      TT=0.
      TX=0.
      Totwi=0.
c
c      Perform integration & convolution
c            
      do 32 k=1,Nw
      kx=index+k-1
      wvx=wv(kx)
      dw=1.
      if(wvx.lt.400.)dw=0.5
      if(abs(wvx-400.).lt.0.01)dw=0.75
      if(wvx.gt.1705)dw=5.
      if(abs(wvx-1700.).lt.0.01)dw=1.75
      if(abs(wvx-1702.).lt.0.01)dw=2.5
      if(abs(wvx-1705.).lt.0.01)dw=3.75
      wt=exp(-(wvc-wvx)*(wvc-wvx)/2./sigma2)
      T0=T0+ET(kx)*wt*dw
      TB=TB+Bnorm(kx)*wt*dw
      TG=TG+Globh(kx)*wt*dw
      TT=TT+Globt(kx)*wt*dw
      TX=TX+Dirx(kx)*wt*dw
      Totwi=Totwi+wt*dw
 32   continue
      TB=TB/Totwi
      TG=TG/Totwi
      TT=TT/Totwi
      T0=T0/Totwi
      TX=TX/Totwi
c
      write(18,200)wvc,T0,TB,TX,TG,TT
 200  FORMAT(3x,f6.1,2X,5(E10.4,2X))
      wvc=wvc+step
      if(wvc.le.wv2)goto 40
 999  continue
      return
      end
c
c
c
      Subroutine ScanT(step,FWHM,fk,N)
c
c      Smoothes irradiance with a filter with 'triangular-shape' 
c      transmittance 
c
      Real wv(2002),ET(2002),limit1,limit2
      DOUBLE PRECISION TB,TG,TT,TX
      DOUBLE PRECISION BNORM(2002),GLOBH(2002),GLOBT(2002),DIRX(2002)
      COMMON /SOLAR1/ WV,WvLMN,WvLMX,WV1,WV2
      COMMON /SOLAR2/ BNORM,GLOBH,GLOBT,DIRX,ET
c
      if(wv1.gt.wvlmn+fwhm)goto 10
      if(wv1.gt.wvlmn+.5*fwhm)goto 11
      write(18,100)
 100  format(' ** WARNING #14',/,'Lower limit for scans needs to',
     1 ' be > WV1 + 0.5*FWHM!',/)
      goto 999
 11   continue
      write(18,101)
 101  format(' ** WARNING #15 ',9('*'),/,'\\ Lower limit for scans is',
     1 ' not > WV1 + FWHM.',/,'\\ This will reduce accuracy in the ',
     2 'results for the first wavelengths.',/)
 10   continue
      if(wv2.lt.wvlmx-fwhm)goto 20
      if(wv2.lt.wvlmx-.5*fwhm)goto 21
      write(18,102)
 102  format(' ** WARNING #16',/,'Upper limit for scans needs to be < ',
     1 'WV2 - 0.5*FWHM!',/)
      goto 999
 21   continue
      write(18,103)
 103  format('** WARNING #17',9('*'),/,'\\ Upper limit for scans is',
     1 ' not < WV2 - FWHM.',/,'\\ This will reduce accuracy in the ',
     2 'results for the last wavelengths.',/)
 20   continue
c
c      Find the limits for spectral integration
c
      wvc=wv1
 40   continue
      dw1=1.
      d1=aint(wvc-fk+.5)
      if(d1.le.400.)dw1=0.5
      if(d1.gt.1705)dw1=5.
      limit1=Max(wvlmn,d1-dw1)
      dw2=1.
      d2=aint(wvc+fk+.5)
      if(d2.le.400.)dw2=0.5
      if(d2.gt.1705)dw2=5.
      limit2=Min(wvlmx,d2+dw2)
      Nw=int((limit2-limit1)/dw1)+1
c
      do 30 j=1,N
      if(abs(wv(j)-limit1).gt.1e-3)goto 30
      index=j
 30   continue
c
      T0=0.
      TB=0.
      TG=0.
      TT=0.
      TX=0.
      Totwi=0.
c
c      Perform integration & convolution
c
      do 32 k=1,Nw
      kx=index+k-1
      wvx=wv(kx)
      dw=1.
      if(wvx.lt.400.)dw=0.5
      if(abs(wvx-400.).lt.0.01)dw=0.75
      if(wvx.gt.1705)dw=5.
      if(abs(wvx-1700.).lt.0.01)dw=1.75
      if(abs(wvx-1702.).lt.0.01)dw=2.5
      if(abs(wvx-1705.).lt.0.01)dw=3.75
      wt=Max(0.,1.-abs(wvc-wvx)/fwhm)
      T0=T0+ET(kx)*wt*dw
      TB=TB+Bnorm(kx)*wt*dw
      TG=TG+Globh(kx)*wt*dw
      TT=TT+Globt(kx)*wt*dw
      TX=TX+Dirx(kx)*wt*dw
      Totwi=Totwi+wt*dw
 32   continue
      TB=TB/Totwi
      TG=TG/Totwi
      TT=TT/Totwi
      T0=T0/Totwi
      TX=TX/Totwi
c      
      write(18,200)wvc,T0,TB,TX,TG,TT
 200  FORMAT(3x,f6.1,2X,5(E10.4,2X))
      wvc=wvc+step
      if(wvc.le.wv2)goto 40
 999  continue
      return
      end
C
C
c
      SUBROUTINE STEPS (WVLN,WLMN,WLMX,DWVL)
C
      DWVL=1.0
      IF(wvln.lt.400.)DWVL=0.5
      IF(abs(wvln-400.).lt.0.01)DWVL=0.75
      IF(abs(wvln-1700.).lt.0.01)DWVL=1.75
      IF(abs(wvln-1702.).lt.0.01)DWVL=2.5
      IF(abs(wvln-1705.).lt.0.01)DWVL=3.75
      IF(wvln.GE.1706.)DWVL=5.
      IF(abs(wvln-WLMN).lt.0.01.or.abs(wvln-WLMX).lt.0.01)DWVL=DWVL/2.
      RETURN
      END
C
C
C
      Subroutine SunPSA(dHour,dLat,dLong,decli,
     1 Zenit,Azim,Julian,R,EOT,P,T,Year,Month,Day)
c
c      'PSA Algorithm' by Blanco-Muriel et al. (Solar Energy 2001)
c      Translated to Fortran from their original code in C++
c
c***      Inputs  ***
c
c      Year
c      Month
c      Day
c      dHour: decimal Hour (Universal Time, UT)
c      dLat: decimal Latitude (deg., positive North)
c      dLong: decimal Longitude (deg., positive East)
c
      Integer Year,Day
      Double Precision Julian,pinb,twopi,rad,LMST,GMST,dRLat,HrAngl,
     1 cosHA,Elapsd,dX,dY,EclipL,Paralx,Decli,dLong,dLat,AUnit,Radius,
     2 EclipO,RightA,Anomly,Omega,sinELg,SunLng,zenith,azimu,cosLat,
     3 sinLat
c
c
c       Declaration of some constants
c
      pinb=3.14159265358979323846
      twopi=2.*pinb
      rad=pinb/180.
      Radius=6371.01
      AUnit=149597890.0
c
c      Calculate current Julian Day
c
      liAux1 =(Month-14)/12
      LiAux2=(1461*(Year + 4800 + liAux1))/4 
     1 + (367*(Month- 2-12*liAux1))/12
     2 - (3*((Year + 4900+ liAux1)/100))/4+Day-32075
      Julian=Float(LiAux2)-0.5+dHour/24.
c
c      Calculate difference in days between the current Julian Day 
c      and JD 2451545.0, which is noon 1 January 2000 Universal Time 
c
      Elapsd = Julian-2451545.0
c 
c      Calculate ecliptic coordinates (ecliptic longitude and obliquity of 
c      the ecliptic in radians) but without limiting the angle to be less 
c      than 2*Pi (i.e., the result may be greater than 2*Pi)
c
      Omega=2.1429-0.0010394594*Elapsd
      SunLng = 4.8950630+ 0.017202791698*Elapsd
      Anomly = 6.2400600+ 0.0172019699*Elapsd
      EclipL = SunLng + 0.03341607*sin(Anomly) + 0.00034894*
     1 sin(2.*Anomly)-0.0001134-0.0000203*sin(Omega)
      EclipO = 0.4090928 - 6.2140e-9*Elapsd+0.0000396*cos(Omega)
c 
c      Calculate celestial coordinates (right ascension and declination)       
c      in radians but without limiting the angle to be less than 2*Pi 
c      (i.e., the result may be greater than 2*Pi)
c
      SinELg= sin(EclipL)
      dY = cos(EclipO) * SinELg
      dX = cos(EclipL)
      RightA = atan2(dY,dX)
      if(RightA.lt.0.0) RightA = RightA + twopi
      Decli = asin(sin(EclipO)*SinELg)
c 
c      Calculate local coordinates (azimuth and zenith angle) in degrees
c            
      GMST = 6.6974243242 + 0.0657098283*Elapsd + dHour
      LMST = (GMST*15. + dLong)*rad
      HrAngl = LMST - RightA
      dRLat = dLat*rad
      cosLat = cos(dRLat)
      sinLat = sin(dRLat)
      cosHA= cos(HrAngl)
      Zenith = acos(cosLat*cosHA*cos(Decli) + sin(Decli)*sinLat)
      dY = -sin(HrAngl)
      dX = tan(Decli)*cosLat - sinLat*cosHA
      Azimu = atan2(dY, dX)
      if (Azimu.lt. 0.0)Azimu = Azimu + twopi
      Azimu = Azimu/rad
c
c      Parallax Correction
c
      Paralx=(Radius/AUnit)*sin(Zenith)
      Zenith=(Zenith + Paralx)/rad
c
c      Sun-Earth actual distance in AU (from Michalsky's paper)
c
      R=1.00014-.01671*cos(Anomly)-.00014*cos(2.*Anomly)
c
c      Equation of Time (in min, from Michalsky's paper)
c
      RightA=RightA/rad
      SunLng=SunLng/rad
      xsun=-aint(abs(SunLng)/360.)
      if(Sunlng.lt.0.)xsun=-xsun+1.
      SunLng=SunLng+xsun*360.
      EOT=(SunLng-RightA)*4.
C
C      REFRACTION CORRECTION FOR ACTUAL ATMOSPHERIC CONDITIONS (P,T)
C
      ELD=90.-Zenith
      ELD2=ELD*ELD
      REFR=0.
      PT=P/T
      IF(ELD.LT.15.0.AND.ELD.GE.-2.5)REFR=PT*(.1594+.0196*ELD+
     # 2E-5*ELD2)/(1.+.505*ELD+.0845*ELD2)
      IF(ELD.GE.15.0.AND.ELD.LT.90.)REFR=.00452*PT/TAN(ELD*rad)
      Zenith=90.-(ELD+REFR)
      zenit=real(zenith)
      azim=real(azimu)
c
c      Declination in degrees
c
      Decli=Decli/rad
      return
      end
C
C
c
      SUBROUTINE UVDAT(ERY0,ERY1,ERY2,ERY3,ERY4,DNA,PHO,ECAL,ACG,
     % POL,SIS,PRT,SCUPH,SCUPM,wvln)
C
C      Calculates different erythema and DNA action/damage curves
C
      REAL A(6),B(6)
      DATA B/-1.3448,1.2203E4,5.2729E5,-1.33E7,7.4736E7,54.81/
      DATA A/41.791,1.3853E4,3.6663E5,-1.1993E7,7.5816E7,53.426/
C
      DW=wvln-300.
      DW2=DW*DW
      DW3=DW*DW2
      DW4=DW*DW3
      DW5=DW*DW4
      DW6=DW*DW5
      DW7=DW*DW6
      DW8=DW*DW7
      DW325=wvln-325.
      wvl=wvln/1000.
      XL=-log(wvl)
      XL2=XL*XL
      XL3=XL*XL2
      XL4=XL2*XL2
C
C      ERYTHEMA SPECTRUM OF CIE 1987 (MCKINLAY & DIFFEY)
C
      ERY0=1.0
      IF(wvln.GE.299.0.AND.wvln.Lt.329.)ERY0=10.**(.094*(298.-wvln))
      IF(wvln.GE.329.)ERY0=10.**(.015*(139.-wvln))
C
C      ERYTHEMA SPECTRUM OF KOMHYR AND MACHTA, fitted by GREEN ET AL.
C            (1974)
C
      EXPL=EXP((DW+3.5)/2.692)
      ERY1=.04485/(1.+EXP((DW-11.4)/3.13))+3.9796*EXPL/(1.+EXPL)**2
C
C      ERYTHEMA SPECTRUM OF COBLENTZ AND STAIR (1934), fitted by
C            GREEN ET AL. (1975)
C
      EXPL2=EXP((DW+3.)/3.21)
      ERY2=0.
      IF(wvln.LE.325.)ERY2=4.*EXPL2/(1.+EXPL2)**2
C
C      ERYTHEMA SPECTRUM OF PARRISH ET AL. (1982), AS FITTED BY
C            BJORN (1989)
C
      ERY3=EXP(-.4232-.1413*DW-.0105*DW2+2E-4*DW3+8.982E-6*DW4-
     # 3.921E-7*DW5+5.623E-9*DW6-3.603E-11*DW7+8.759E-14*DW8)
C
C      ERYTHEMA SPECTRUM OF DIFFEY (1982) MODIFIED BY BJORN (1989)
C
      ERY4=0.
      IF(wvln.GE.326.0.OR.wvln.LE.284.)GOTO 90
      ERY4=.98-.0957*DW
      IF(wvln.gt.310.)ERY4=EXP(-5.0188-.118*DW325+9.382E-4*DW325*DW325)
 90   CONTINUE
C
C      PLANT/DNA SPECTRUM OF SETLOW (1974) AS FITTED BY
C            GREEN & MO (1975)
C
      DNA=0.
      IF(wvln.GE.366.)GOTO 99
      DNA=EXP(13.82*(-1.+1./(1.+EXP((wvln-310.)/9.))))
 99   CONTINUE
C
C      PHOTOSYNTHESIS INHIBITION SPECTRUM OF CALDWELL ET AL. (1986)
C
      PHO=0.
      IF(wvln.LE.340.)PHO=13.42*EXP(106.219-.6122*wvln
     1 +.0008316*wvln*wvln) 
C
C      CALDWELL (1971) BIOLOGICAL ACTION CURVE, AS FITTED BY 
C      GREEN ET AL. (1974)
C
      ECAL=0.
      IF(wvln.LE.313.)ECAL=2.618*(1.-(wvln/313.3)**2)*EXP(-DW/31.08)
C
C      ACGIH (1978) SAFETY SPECTRUM, AS FITTED BY WESTER (1981, 1984)
C
      ACG=0.
      IF(wvln.Lt.300.)ACG=1.-0.36*((wvln-270.)/20.)**1.64
      IF(wvln.GE.300.0.AND.wvln.LE.315.)ACG=0.3*0.74**DW
C
C      POLYCHROMATIC ACTION FOR HIGHER PLANTS,data fom Caldwell et al.
C      (1986); fit by Gueymard
C
      POL=EXP(40.355-106.88*XL+59.307*XL2)
C
C      SYSTEMATIC IMMUNOSUPPRESSION, data from deFabo et al. (1990);
C            fit by Gueymard
C      
      SIS=EXP(-42.826+45.056*XL-9.3345*XL2)
C
C      DNA TO PROTEIN CROSSLINKS, data from Peak & Peak (1986);
C            fit by Gueymard
C
      PRT=EXP(-1305.8+5287.4*XL-7917.5*XL2+5154.1*XL3-1228.3*XL4)
C
C      SKIN CARCINOGENESIS FOR MICE AND HUMANS, data from
C            de Gruijl & Van der Leun (1994); fit by Gueymard
C
      H=WVL-.299
      X=WVL-.293
      SH=0.
      SX=0.
      H1=1.
      X1=1.
      DO 10 I=1,5
      H1=H1*H
      X1=X1*X
      SH=SH+A(I)*H1
      SX=SX+B(I)*X1
 10   CONTINUE
      SCUPH=EXP(-SH/(1.+A(6)*H))
      SCUPM=EXP(-SX/(1.+B(6)*X))
C
 999  continue
      RETURN
      END
c
C
c
      Subroutine VISTAU(Season,Range,Tau,index)
c
      Real vs1(5),vw1(5),vs2(3),vw2(3),vs3(2),vw3(2)
      Character*6 Season
      Data vs1 /-3.2998,-5.37,156.14,42.389,48.957/
      Data vs2 /.026483,7.133,-6.6238/
      Data vs3 /.039987,.43928/
      Data vw1 /-3.6629,-6.5109,165.85,44.857,51.968/
      Data vw2 /.010149,6.7705,-1.7703/
      Data vw3 /.023339,.27928/
c
      if(index.eq.1)goto 1
      tln=log(Tau)
      Range=999.
c
c***      (Index=0) Calculate Range from Tau
c
      if(season.eq.'WINTER')goto 345
c
c      Calculations for SPRING/SUMMER conditions
c
      if(Tau.lt.0.0402)goto 346
      if(Tau.le.0.0416)goto 3442
      if(Tau.lt.0.0901)goto 3440
      delta1=(vs1(4)*tln-vs1(2))*(vs1(4)*tln-vs1(2))-
     1 4.*(vs1(5)*tln-vs1(3))*(tln-vs1(1))
      Range=1./(.001+.5*(vs1(2)-vs1(4)*tln-(delta1**.5))/
     1 (vs1(5)*tln-vs1(3)))
      goto 346
 3440 continue
      delta2=vs2(2)*vs2(2)+4.*vs2(3)*(Tau-vs2(1))
      Range=Min(999.,1./(.001+.5*(vs2(2)-(delta2**.5))/(-vs2(3))))
      goto 346
 3442 continue
      Range=1./(.001+(Tau-vs3(1))/vs3(2))
 346  continue
      Range=Min(Range,999.)
      goto 30
 345  continue
c
c      Calculations for FALL/WINTER conditions
c
      if(Tau.lt.0.0235)goto 346
      if(Tau.le.0.0245)goto 3446
      if(Tau.lt.0.0709)goto 3444
      delta1=(vw1(4)*tln-vw1(2))*(vw1(4)*tln-vw1(2))-
     1 4.*(vw1(5)*tln-vw1(3))*(tln-vw1(1))
      Range=1./(.001+.5*(vw1(2)-vw1(4)*tln-(delta1**.5))/
     1 (vw1(5)*tln-vw1(3)))
      goto 346
 3444 continue
      delta2=vw2(2)*vw2(2)+4.*vw2(3)*(Tau-vw2(1))
      Range=Min(999.,1./(.001+.5*(vw2(2)-(delta2**.5))/(-vw2(3))))
      goto 346
 3446 continue
      Range=1./(.001+(Tau-vw3(1))/vw3(2))
      goto 346
c
 1      continue
c
c***      (Index=1) Calculate Tau from Range
c
      YVIS=(1./RANGE)-.001
      Yvis2=Yvis*Yvis
      if(Range.lt.100.)goto 3447
      if(Range.gt.320.)goto 3448
      Tau=vs2(1)+vs2(2)*Yvis+vs2(3)*Yvis2
      if(Season.eq.'WINTER')Tau=vw2(1)+vw2(2)*Yvis+vw2(3)*Yvis2
      goto 30
 3447 continue
      Tau=exp((vs1(1)+vs1(2)*Yvis+vs1(3)*Yvis2)/
     1 (1.+vs1(4)*Yvis+vs1(5)*Yvis2))
      if(Season.eq.'WINTER')Tau=exp((vw1(1)+vw1(2)*Yvis+      
     1 vw1(3)*Yvis2)/(1.+vw1(4)*Yvis+vw1(5)*Yvis2))
      goto 30
 3448 continue
      Tau=vs3(1)+vs3(2)*Yvis
      if(Season.eq.'WINTER')Tau=vw3(1)+vw3(2)*Yvis
 30   continue
c
      Return
      End
C
C*********************************************************************
