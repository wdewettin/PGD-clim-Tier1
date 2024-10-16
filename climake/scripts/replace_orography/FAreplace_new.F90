PROGRAM FAREPLACE
!
!
! program for blending two aladin files
! On output fname1 will contain some (spectral/gridpoint) fields of fname2
!     
USE PARKIND1  ,ONLY : JPIM     ,JPRB
!
       IMPLICIT NONE
!

 CHARACTER(len=14)         ::   FNAME1,FNAME2
		 
 INTEGER(KIND=JPIM)                 ::   IREP,INMAX,INUM1,INUM2
 INTEGER(KIND=JPIM)                 ::   I_NGRID,JVARA,REP_TYPE(20)
 REAL(KIND=JPRB)                    ::   zspec(2621440),zsurf1(2621440),zsurf2(2621440),zsurf(2621440)
 REAL(KIND=JPRB)                    ::   ZLSM(2621440)
 CHARACTER (LEN = 16) ::  CL_CGRID(20)
 CHARACTER :: CL_CPREF*4,CL_CVARA*12 
 namelist/namblend/FNAME1,FNAME2,CL_CGRID,I_NGRID,REP_TYPE
 LOGICAL :: LDEXIS,LDCOSP
 INTEGER(KIND=JPIM) :: KNGRIB,KNBITS,KSTRON,KPUILA
!
!      -----------------------------------------------------------------		 
INUM1=11
INUM2=12
I_NGRID=0
CL_CGRID(:)='                '
REP_TYPE=0
FNAME1='original'
FNAME2='newfields'
! reading namelist 
  read (4,namblend)
WRITE(*,*) 'Replacing/adding ', I_NGRID, ' fields'
WRITE(*,*) 'Opening file ',FNAME1
 inmax=1
 CALL faitou(IREP,INUM1,.true.,FNAME1,'OLD',.true.,&
&.false.,0,inmax,inmax,'CADRE LECTURE1  ')
       
	 
WRITE(*,*) 'Opening file ',FNAME2
 inmax=1
 CALL faitou(IREP,INUM2,.true.,FNAME2,'OLD',.true.,&
&.false.,0,inmax,inmax,'CADRE LECTURE2  ')

CALL FACILE(IREP,INUM2,'SURF',1,'IND.TERREMER',ZLSM,.FALSE.)
!call fanion(IREP,INUM1,CL_CPREF,1,CL_CVARA,LDEXIS,LDCOSP,KNGRIB,KNBITS,KSTRON,KPUILA)
!call fagote(IREP,INUM1,KNGRIB,KNBITS,KNBITS,KSTRON,KPUILA,0)

DO JVARA = 1,I_NGRID
  IF (REP_TYPE(JVARA) == 3) THEN
      WRITE(*,*) ' --- adding ', CL_CGRID(JVARA)
      CL_CPREF=CL_CGRID(JVARA)(1:4)
      CL_CVARA=CL_CGRID(JVARA)(5:16)
      ! to write replacement data with exactly the same encoding:
      ! read details for the chosen field:
      call fanion(IREP,INUM2,CL_CPREF,1,CL_CVARA,LDEXIS,LDCOSP,KNGRIB,KNBITS,KSTRON,KPUILA)
      ! set these as defaults for writing:
      ! note that we (for now) fix the degree of the laplacian to stay equal (KDMOPL=0)
      ! surface fields are in grid point, so it doesn't make any difference anyway
      call fagote(IREP,INUM1,KNGRIB,KNBITS,KNBITS,KSTRON,KPUILA,0)
      call facile(IREP,INUM2,CL_CPREF,1,CL_CVARA,ZSURF,.FALSE.)
      call faienc(IREP,INUM1,CL_CPREF,1,CL_CVARA,ZSURF,.FALSE.)
  ELSE
      WRITE(*,*) ' --- replacing ', CL_CGRID(JVARA)
      CL_CPREF=CL_CGRID(JVARA)(1:4)
      CL_CVARA=CL_CGRID(JVARA)(5:16)
      ! to write replacement data with exactly the same encoding:
      ! read details for the chosen field:
      call fanion(IREP,INUM1,CL_CPREF,1,CL_CVARA,LDEXIS,LDCOSP,KNGRIB,KNBITS,KSTRON,KPUILA)
      ! set these as defaults for writing:
      ! note that we (for now) fix the degree of the laplacian to stay equal (KDMOPL=0)
      ! surface fields are in grid point, so it doesn't make any difference anyway
      call fagote(IREP,INUM1,KNGRIB,KNBITS,KNBITS,KSTRON,KPUILA,0)
      call facile(IREP,INUM2,CL_CPREF,1,CL_CVARA,ZSURF2,.FALSE.)
      call facile(IREP,INUM1,CL_CPREF,1,CL_CVARA,ZSURF1,.FALSE.)
          IF (REP_TYPE(JVARA) == 0) ZSURF=ZSURF2*REAL(1-NINT(ZLSM(:))) + REAL(NINT(ZLSM(:)))*ZSURF1 
          IF (REP_TYPE(JVARA) == 1) ZSURF=ZSURF1*REAL(1-NINT(ZLSM(:))) + REAL(NINT(ZLSM(:)))*ZSURF2 
          IF (REP_TYPE(JVARA) == 2) ZSURF=ZSURF2 
      call faienc(IREP,INUM1,CL_CPREF,1,CL_CVARA,ZSURF,.FALSE.)
  END IF
ENDDO

! finished
 CALL fairme(IREP,INUM1,'KEEP')
 CALL fairme(IREP,INUM2,'KEEP')

END
