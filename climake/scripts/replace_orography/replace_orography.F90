
PROGRAM MODIFY_OROGRAPHY
!
! Program to replace the SFX.ZS field (orography) in the PGD file
! using the SURFGEOPOTENTIEL field from a climatology (clim) file,
! divided by g = 9.80665 m/s².
!
USE PARKIND1, ONLY: JPIM, JPRB
IMPLICIT NONE

! Declare variables
CHARACTER(LEN=14) :: FNAME1, FNAME2
INTEGER(KIND=JPIM) :: IREP, INUM1, INUM2
INTEGER(KIND=JPIM) :: INGRID, JVARA, INMAX
REAL(KIND=JPRB) :: ZSURF1(2621440), ZSURF2(2621440), ZSURF(2621440)
REAL(KIND=JPRB) :: ZGEOPOT(2621440), G
! CHARACTER(LEN=16) :: CL_CGRID(20)
LOGICAL :: LDEXIS, LDCOSP
NAMELIST /namelist_vars/ FNAME1, FNAME2
!
INUM1=11
INUM2=12
!
! Read namelist
READ(4, namelist_vars)

! Open the files
WRITE(*,*) 'Opening PGD file ', FNAME1
INMAX = 1
!
CALL faitou(IREP, INUM1, .TRUE., FNAME1, 'OLD', .TRUE., .TRUE., 2, INMAX, INMAX, 'CADRE LECTURE1  ')

WRITE(*,*) 'Opening clim file ', FNAME2
INMAX = 1
CALL faitou(IREP, INUM2, .TRUE., FNAME2, 'OLD', .TRUE., .TRUE., 2, INMAX, INMAX, 'CADRE LECTURE2  ')

! Read surface geopotential from the clim file (SURFGEOPOTENTIEL)
CALL facile(IREP, INUM2, 'SURF', 1, 'GEOPOTENTIEL', ZGEOPOT, .FALSE.)

! Modify the geopotential to get the orography (divide by g)
ZSURF = ZGEOPOT / 9.80665
!
! What does this step do??
call fagote(IREP, INUM1, 0, 24, 24, 10, 1, 5)
!
! Replace the orography field (SFX.ZS) in the PGD file with the modified geopotential
CALL faienc(IREP, INUM1, 'SFX.', 0, 'ZS', ZSURF, .FALSE.)

! Finalize and close the files
CALL fairme(IREP, INUM1, 'KEEP')
CALL fairme(IREP, INUM2, 'KEEP')

END PROGRAM MODIFY_OROGRAPHY
