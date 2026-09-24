PROGRAM MODIFY_OROGRAPHY
!
! Replace SFX.ZS (orography) in the PGD file
! using SURFGEOPOTENTIEL from climatology file divided by g.
!
USE PARKIND1, ONLY: JPIM, JPRB
IMPLICIT NONE

! ---- config ----
INTEGER(KIND=JPIM), PARAMETER :: NPTS = 4621440
REAL(KIND=JPRB),    PARAMETER :: G = 9.80665_JPRB

! ---- variables ----
CHARACTER(LEN=256) :: FNAME1, FNAME2
INTEGER(KIND=JPIM) :: IREP, INUM1, INUM2
INTEGER(KIND=JPIM) :: INMAX
REAL(KIND=JPRB) :: ZSURF(NPTS)
REAL(KIND=JPRB) :: ZGEOPOT(NPTS)
! INTEGER(KIND=JPIM), PARAMETER :: NPTS = 4621440
REAL(KIND=JPRB),    PARAMETER :: SENT = -HUGE(1.0_JPRB)

INTEGER(KIND=JPIM) :: N1, N2
REAL(KIND=JPRB) :: ZSURF1(NPTS)


LOGICAL :: LDEXIS, LDCOSP
NAMELIST /namelist_vars/ FNAME1, FNAME2

! ---- local stats vars ----
REAL(KIND=JPRB) :: zmin, zmax, zmean
INTEGER(KIND=JPIM) :: n_nan, n_inf
INTEGER(KIND=JPIM) :: i
INTEGER(KIND=JPIM) :: dbg_unit

! ============================================================
WRITE(*,*) "============================================================"
WRITE(*,*) "  MODIFY_OROGRAPHY - DEBUG / INSTRUMENTED VERSION"
WRITE(*,*) "============================================================"
WRITE(*,*) "Compiled with: extensive debug output"
WRITE(*,*) "NPTS = ", NPTS
WRITE(*,*) "g    = ", G
WRITE(*,*) "============================================================"

INUM1 = 11
INUM2 = 12

! ---------------------------
! Read namelist
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[1] Reading namelist_vars from unit 4 ..."
READ(4, namelist_vars)

FNAME1 = TRIM(FNAME1)
FNAME2 = TRIM(FNAME2)

WRITE(*,*) "[1] Namelist read OK."
WRITE(*,*) "[1] FNAME1 = <", TRIM(FNAME1), ">"
WRITE(*,*) "[1] FNAME2 = <", TRIM(FNAME2), ">"
WRITE(*,*) "[1] len_trim(FNAME1) = ", LEN_TRIM(FNAME1)
WRITE(*,*) "[1] len_trim(FNAME2) = ", LEN_TRIM(FNAME2)

IF (LEN_TRIM(FNAME1) == 0 .OR. LEN_TRIM(FNAME2) == 0) THEN
  WRITE(*,*) "ERROR: one of the filenames is empty after trimming."
  STOP 2
END IF

! ---------------------------
! Open PGD file
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[2] Opening PGD file (unit ", INUM1, "): ", TRIM(FNAME1)
INMAX = 1

CALL faitou(IREP, INUM1, .TRUE., FNAME1, 'OLD', .TRUE., .TRUE., 2, INMAX, INMAX, 'CADRE LECTURE1  ')
WRITE(*,*) "[2] faitou(PGD) returned IREP = ", IREP
IF (IREP /= 0) THEN
  WRITE(*,*) "ERROR: failed to open PGD file."
  STOP 10
END IF

! ---------------------------
! Open climatology file
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[3] Opening clim file (unit ", INUM2, "): ", TRIM(FNAME2)
INMAX = 1

CALL faitou(IREP, INUM2, .TRUE., FNAME2, 'OLD', .TRUE., .TRUE., 2, INMAX, INMAX, 'CADRE LECTURE2  ')
WRITE(*,*) "[3] faitou(CLIM) returned IREP = ", IREP
IF (IREP /= 0) THEN
  WRITE(*,*) "ERROR: failed to open clim file."
  STOP 11
END IF

! --- Read PGD SFX.ZS and count filled points ---
ZSURF1 = SENT
CALL facile(IREP, INUM1, 'SFX.', 0, 'ZS', ZSURF1, .FALSE.)
IF (IREP /= 0) THEN
  WRITE(*,*) "ERROR reading PGD SFX.ZS, IREP=", IREP
  STOP 100
END IF
N1 = COUNT(ZSURF1 /= SENT)

WRITE(*,*) "PGD: effective filled points in SFX.ZS = ", N1, " / ", NPTS

! --- Read clim SURF.GEOPOTENTIEL and count filled points ---
ZGEOPOT = SENT
CALL facile(IREP, INUM2, 'SURF', 1, 'GEOPOTENTIEL', ZGEOPOT, .FALSE.)
IF (IREP /= 0) THEN
  WRITE(*,*) "ERROR reading CLIM SURFGEOPOTENTIEL, IREP=", IREP
  STOP 101
END IF
N2 = COUNT(ZGEOPOT /= SENT)

WRITE(*,*) "CLIM: effective filled points in SURFGEOPOTENTIEL = ", N2, " / ", NPTS

! --- Compare ---
IF (N1 /= N2) THEN
  WRITE(*,*) "ERROR: PGD and CLIM fields do NOT have same effective length!"
  WRITE(*,*) "       PGD filled =", N1, " CLIM filled =", N2
  STOP 102
END IF


! ---------------------------
! Read geopotential
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[4] Reading clim field: SURF.GEOPOTENTIEL ..."
ZGEOPOT = -HUGE(1.0_JPRB)   ! sentinel initialization
CALL facile(IREP, INUM2, 'SURF', 1, 'GEOPOTENTIEL', ZGEOPOT, .FALSE.)
WRITE(*,*) "[4] facile(SURF.GEOPOTENTIEL) returned IREP = ", IREP
IF (IREP /= 0) THEN
  WRITE(*,*) "ERROR: failed to read SURF.GEOPOTENTIEL"
  STOP 12
END IF

! ---------------------------
! Stats: ZGEOPOT
! ---------------------------
CALL field_stats("ZGEOPOT (raw geopotential)", ZGEOPOT, NPTS)

! sanity range checks
WRITE(*,*) ""
WRITE(*,*) "[4] Sanity check on geopotential magnitude:"
WRITE(*,*) "    Typical surface geopotential should be O(0..~1e5) m^2/s^2"
WRITE(*,*) "    (Since 10 km height => ~1e5 m^2/s^2)"
WRITE(*,*) "    If values are huge (1e9) or tiny nonsense, read/order mismatch is likely."

! ---------------------------
! Convert to orography (m)
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[5] Converting: ZSURF = ZGEOPOT / g"
ZSURF = ZGEOPOT / G

CALL field_stats("ZSURF (derived orography in m)", ZSURF, NPTS)

WRITE(*,*) ""
WRITE(*,*) "[5] Sample values ZGEOPOT -> ZSURF at a few indices:"
DO i = 1, 5
  WRITE(*,'(A,I8,A,ES16.6,A,ES16.6)') "    i=", i, "  ZGEOPOT=", ZGEOPOT(i), "  ZSURF=", ZSURF(i)
END DO
DO i = NPTS-4, NPTS
  WRITE(*,'(A,I8,A,ES16.6,A,ES16.6)') "    i=", i, "  ZGEOPOT=", ZGEOPOT(i), "  ZSURF=", ZSURF(i)
END DO

! ---------------------------
! Possibly set encoding/packing options
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[6] Calling fagote(...)  (packing/encoding options)"
WRITE(*,*) "    NOTE: this does NOT align domains; it modifies output encoding."
WRITE(*,*) "    If you get artefacts, try removing this call."
CALL fagote(IREP, INUM1, 0, 24, 24, 10, 1, 5)
WRITE(*,*) "[6] fagote returned IREP = ", IREP
IF (IREP /= 0) THEN
  WRITE(*,*) "WARNING: fagote returned nonzero IREP. Continuing anyway."
END IF

! ---------------------------
! Write into PGD: SFX.ZS
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[7] Writing to PGD: field SFX.ZS ..."
CALL faienc(IREP, INUM1, 'SFX.', 0, 'ZS', ZSURF, .FALSE.)
WRITE(*,*) "[7] faienc(SFX.ZS) returned IREP = ", IREP
IF (IREP /= 0) THEN
  WRITE(*,*) "ERROR: failed to write SFX.ZS to PGD"
  STOP 13
END IF

! ---------------------------
! Finalize
! ---------------------------
WRITE(*,*) ""
WRITE(*,*) "[8] Closing files (KEEP) ..."
CALL fairme(IREP, INUM1, 'KEEP')
WRITE(*,*) "[8] fairme(PGD) returned IREP = ", IREP
IF (IREP /= 0) THEN
  WRITE(*,*) "WARNING: fairme(PGD) returned nonzero IREP."
END IF

CALL fairme(IREP, INUM2, 'KEEP')
WRITE(*,*) "[8] fairme(CLIM) returned IREP = ", IREP
IF (IREP /= 0) THEN
  WRITE(*,*) "WARNING: fairme(CLIM) returned nonzero IREP."
END IF

WRITE(*,*) ""
WRITE(*,*) "============================================================"
WRITE(*,*) "DONE."
WRITE(*,*) "============================================================"

!=======================================================================
CONTAINS
!=======================================================================

SUBROUTINE field_stats(label, arr, n)
  USE PARKIND1, ONLY: JPIM, JPRB
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: label
  INTEGER(KIND=JPIM), INTENT(IN) :: n
  REAL(KIND=JPRB), INTENT(IN) :: arr(n)

  REAL(KIND=JPRB) :: zmin, zmax, zsum
  REAL(KIND=JPRB) :: x
  INTEGER(KIND=JPIM) :: i
  INTEGER(KIND=JPIM) :: n_nan, n_inf

  zmin = HUGE(1.0_JPRB)
  zmax = -HUGE(1.0_JPRB)
  zsum = 0.0_JPRB
  n_nan = 0
  n_inf = 0

  DO i = 1, n
    x = arr(i)

    ! NaN check: NaN is the only float where x /= x
    IF (x /= x) THEN
      n_nan = n_nan + 1
      CYCLE
    END IF

    ! Inf check (crude): abs(x) > huge/10
    IF (ABS(x) > HUGE(1.0_JPRB) / 10.0_JPRB) THEN
      n_inf = n_inf + 1
      CYCLE
    END IF

    IF (x < zmin) zmin = x
    IF (x > zmax) zmax = x
    zsum = zsum + x
  END DO

  WRITE(*,*) ""
  WRITE(*,*) "------------------------------------------------------------"
  WRITE(*,*) "Field stats: ", TRIM(label)
  WRITE(*,*) "  n points = ", n
  WRITE(*,*) "  min      = ", zmin
  WRITE(*,*) "  max      = ", zmax
  WRITE(*,*) "  mean(*)  = ", zsum / REAL(n - n_nan - n_inf, KIND=JPRB)
  WRITE(*,*) "  NaNs     = ", n_nan
  WRITE(*,*) "  Infs     = ", n_inf
  WRITE(*,*) "  first 5  = ", arr(1), arr(2), arr(3), arr(4), arr(5)
  WRITE(*,*) "  last  5  = ", arr(n-4), arr(n-3), arr(n-2), arr(n-1), arr(n)
  WRITE(*,*) "------------------------------------------------------------"
END SUBROUTINE field_stats

END PROGRAM MODIFY_OROGRAPHY
