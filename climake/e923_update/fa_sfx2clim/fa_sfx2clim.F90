PROGRAM FA_SFX2CLIM

! -----
! PURPOSE:
!   Updates selected fields in e923 clim file by their counterparts
!   from sfx file. Updated fields are controlled by logical keys:
!
!     L_FRAC   - fractions of land and town
!     L_Z0     - surface roughness (orographic, vegetation, effective, thermal)
!     L_VEG    - vegetation fraction, leaf area index, min. stomatal resistance
!     L_VEGDOM - dominant vegetation index (suboption of L_VEG):
!                  1 - sea, lake, river
!                  2 - ice cap (unused)
!                  3 - desert, low vegetation
!                  4 - high vegetation
!                  5 - lake (if distinction, unused)
!     L_SOIL   - percentge of clay and sand
!     L_DEPTH  - soil depth
!     L_RAD    - albedos and emissivity
!
! FRACTIONS AND LAND-SEA MASK.
!   SFX.FRAC_SEA + SFX.FRAC_WATER + SFX.FRAC_NATURE + SFX.FRAC_TOWN = 1.
!   SFX.FRAC_WATER is fraction of lake and river, while SFX.COVER002 is
!   fraction of lake only. SFX.COVER001 is fraction of sea. Land-sea mask
!   in e923 procedure with input PGD file is defined as 1 (land) for points
!   where SFX.COVER001 + SFX.COVER002 <= 0.5.
!
! ROUGHNESS TYPES:
!   Orographic roughness z0rel and vegetation roughness z0veg are provided
!   directly by .sfx file. Effectvive mechanical roughness z0 and thermal
!   roughness z0h are obtained as:
!
!     z0  = sqrt(z0rel^2 + z0veg^2)
!
!           / STHER.z0   ; LZ0THER=.T.
!     z0h = |
!           \ STHER.z0veg; LZ0THER=.F.
!
! VEGETATION FRACTION AND LEAF AREA INDEX:
!   Vegetation fraction in .sfx file is area shaded by vegetation per
!   nature area, in e923 file per gridbox area.
!   LAI in .sfx file is one sided leaf area per nature area, in e923 file
!   per vegetation shaded area.
!
! ALBEDOS AND EMISSIVITY:
!   These are determined only approximately over the land, according to
!   fractions of town, clay, sand, rock and vegetation.
!
! SMOOTHING:
!   Smoothing is done only over land points.
!
! INPUT FA FIELDS:
!   SFX.FRAC_NATURE  - fraction of nature  (within gridbox)     [1]
!   SFX.FRAC_TOWN    - fraction of town    (within gridbox)     [1]
!   SFX.Z0REL        - mechanical orographic roughness z0rel    [m]
!   X001Z0VEG        - mechanical vegetation roughness z0veg    [m]
!   X001VEG          - vegetation fraction (within nature)      [1]
!   X001LAI          - leaf area index     (within nature)      [1]
!   X001RSMIN        - minimum stomatal resistance              [s/m]
!   SFX.CLAY         - clay fraction       (within nature)      [1]
!   SFX.SAND         - sand fraction       (within nature)      [1]
!   X001DG2          - root depth / hydrological soil depth     [m]
!   SURFIND.TERREMER - land-sea mask       (0 - sea, 1 - land)
!   SURFIND.VEG.DOMI - dominant vegetation index
!                      (needed for vegetation albedo)
!
! OUTPUT FA FIELDS:
!   SURFPROP.TERRE   - fraction of land    (unused)             [1]
!   SURFPROP.URBANIS - fraction of town    (unused)             [1]
!   SURFZ0REL.FOIS.G - mechanical orographic roughness g.z0rel  [J/kg]
!   SURFZ0VEG.FOIS.G - mechanical vegetation roughness g.z0veg  [J/kg]
!   SURFZ0.FOIS.G    - mechanical effective  roughness g.z0     [J/kg]
!   SURFGZ0.THERM    - thermal roughness               g.z0h    [J/kg]
!   SURFPROP.VEGETAT - vegetation fraction (within gridbox)     [1]
!   SURFIND.FOLIAIRE - leaf area index     (within shaded area) [1]
!   SURFIND.VEG.DOMI - dominant vegetation index
!   SURFRESI.STO.MIN - minimum stomatal resistance              [s/m]
!   SURFPROP.ARGILE  - percentage of clay                       [%]
!   SURFPROP.SABLE   - percentage of sand                       [%]
!   SURFEPAIS.SOL    - root depth                               [m]
!   SURFEPAI.SOL.MAX - hydrological soil depth                  [m]
!                      (unused, set equal to root depth)
!   SURFALBEDO.SOLNU - bare ground albedo                       [1]
!   SURFALBEDO.COMPL - unused, set to bare ground albedo        [1]
!   SURFALBEDO.VEG   - vegetation albedo                        [1]
!   SURFALBEDO       - albedo              (without snow)       [1]
!   SURFEMISSIVITE   - emissivity          (without snow)       [1]
!
! REMARK:
!   Packing in sfx file must be switched off by setting NVGRIBSFX=0 in
!   EXSEG1.nam namelist NAMSFXCMP. Undefined values over the sea/water
!   will be replaced by proper e923 values.
!
! ORIGINAL:
!   30-Jan-2020, J. Masek
!
! MODIFICATIONS:
!   13-Feb-2020, J. Masek
!   Implemented smoothing applied on log(z0rel) or log(z0veg). Added
!   scaling factor FACZ0_VEG for z0veg.
!
!   12-Jun-2020, J. Masek
!   Added vegetation fraction and leaf area index.
!
!   18-Jun-2020, J. Masek
!   Added minimum stomatal resistance and soil characteristics.
!
!   07-Aug-2020, J. Masek
!   Added smoothing of non-roughness fields (namelist parameter NLISS).
!   Updated fractions of land and town. Correct conversion of vegetation
!   fraction and leaf area index between SURFEX and ISBA. Updated albedos
!   and emissivities.
!
!   06-Sep-2021, J. Masek
!   Consistency of roughness lengths with land-sea mask.
! -----
     
IMPLICIT NONE

! -----
! parameters
! -----

INTEGER,PARAMETER :: JPULNAM=81  ! logical unit to read namelist
INTEGER,PARAMETER :: JPULFA =81  ! logical unit to read/write FA files

REAL,PARAMETER :: RG   =9.80665  ! gravity acceleration                [m/s^2]
REAL,PARAMETER :: SZZ0M=0.001    ! sea roughness                           [m]
REAL,PARAMETER :: SZZ0N=0.001    ! minimum mechanical roughness            [m]
REAL,PARAMETER :: STHER=0.1      ! thermal to mechanical roughness (land)  [1]
REAL,PARAMETER :: SRSMN=1.0      ! minimum min. surface resistance       [s/m]
REAL,PARAMETER :: SRSMX=5000.    ! maximum min. surface resistance       [s/m]
REAL,PARAMETER :: SDEPN=0.1      ! minimum soil depth                      [m]
REAL,PARAMETER :: SDEPX=8.0      ! maximum soil depth                      [m]
REAL,PARAMETER :: SALBN=0.05     ! minimum albedo                          [1]
REAL,PARAMETER :: SALBX=0.80     ! maximum albedo                          [1]
REAL,PARAMETER :: SALBM=0.07     ! sea/water albedo                        [1]
REAL,PARAMETER :: SALBC=0.1      ! clay albedo                             [1]
REAL,PARAMETER :: SALBS=0.3      ! sand albedo                             [1]
REAL,PARAMETER :: SALBR=0.2      ! rock albedo                             [1]
REAL,PARAMETER :: SALBU=0.2      ! town albedo                             [1]
REAL,PARAMETER :: SALBL=0.2      ! low vegetaton albedo                    [1]
REAL,PARAMETER :: SALBH=0.1      ! high vegetaton albedo                   [1]
REAL,PARAMETER :: SEMIN=0.90     ! minimum emissivity                      [1]
REAL,PARAMETER :: SEMIX=1.00     ! maximum emissivity                      [1]
REAL,PARAMETER :: SEMIM=0.96     ! sea/water emissivity                    [1]
REAL,PARAMETER :: SEMIC=0.939    ! clay emissivity                         [1]
REAL,PARAMETER :: SEMIS=0.939    ! sand emissivity                         [1]
REAL,PARAMETER :: SEMIR=0.940    ! rock emissivity                         [1]
REAL,PARAMETER :: SEMIU=0.916    ! town emissivity                         [1]
REAL,PARAMETER :: SEMIV=0.970    ! vegetation emissivity                   [1]
REAL,PARAMETER :: SVEG =0.02     ! gridbox fraction below which vegetation
                                 ! is not taken into account               [1]
REAL,PARAMETER :: SLAIX=10.      ! maximum LAI value in ISBA               [1]
REAL,PARAMETER :: SLAIH=3.5      ! summer LAI treshold for high vegetation [1]
REAL,PARAMETER :: UNDEF=1.E+10   ! treshold for undefined values

! -----
! scalars and ordinary arrays
! -----

INTEGER :: JPXPAH,JPXIND,JPXGEO,JPXNIV

INTEGER :: IERR
INTEGER :: INBARI,INIMES,INBARP,ITRONC
INTEGER :: INLATI,INXLON,INIVER,INBITS
INTEGER :: INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL
INTEGER :: INIVAU,IREP,ITYPTR
INTEGER :: IX1,IX2,IY1,IY2
INTEGER :: I,J,JFILE,JX,JY
INTEGER :: NDGL,NDLON,NDLUN,NDLUX,NDGUN,NDGUX
INTEGER :: NBITS,NLISS,NLISSZ,NLISSZ_VEG

REAL :: FACZ0,FACZ0_VEG
REAL :: ZCODIL,ZCLOPO,ZSLOPO,ZSLAPO,ZREFER
REAL :: ZFM,ZFC0,ZFC,ZFS0,ZFS,ZFR,ZFU,ZFV

CHARACTER(LEN= 16) :: CLNMCA
CHARACTER(LEN=256) :: CLFILE(0:3)

LOGICAL :: L_FRAC,L_Z0,L_VEG,L_VEGDOM,L_SOIL,L_DEPTH,L_RAD
LOGICAL :: LZ0THER
LOGICAL :: LLERFA,LLGARD,LLIMST,LLNOMM,LLSPEC

! -----
! allocatable arrays
! -----

INTEGER,ALLOCATABLE :: INLOPA (:)
INTEGER,ALLOCATABLE :: INLOPA0(:)
INTEGER,ALLOCATABLE :: INOZPA (:)

REAL,ALLOCATABLE :: ZAHYBR (:)
REAL,ALLOCATABLE :: ZBHYBR (:)
REAL,ALLOCATABLE :: ZSINLA (:)
REAL,ALLOCATABLE :: ZSINLA0(:)

REAL,ALLOCATABLE :: ZNATURE(:,:)  ! fraction of nature (SFX.FRAC_NATURE)
REAL,ALLOCATABLE :: ZTOWN  (:,:)  ! fraction of town   (SFX.FRAC_TOWN)
REAL,ALLOCATABLE :: ZLAND  (:,:)  ! fraction of land
REAL,ALLOCATABLE :: ZLSM   (:,:)  ! land-sea mask (0 - sea, 1 - land)
REAL,ALLOCATABLE :: Z0VEG  (:,:)  ! mechanical vegetation roughness
REAL,ALLOCATABLE :: Z0REL  (:,:)  ! mechanical orographic roughness
REAL,ALLOCATABLE :: Z0     (:,:)  ! effective mechanical roughness
REAL,ALLOCATABLE :: Z0H    (:,:)  ! thermal roughness
REAL,ALLOCATABLE :: ZVEG0  (:,:)  ! fraction of nature  shaded by vegetation
REAL,ALLOCATABLE :: ZVEG   (:,:)  ! fraction of gridbox shaded by vegetation
REAL,ALLOCATABLE :: ZVEGDOM(:,:)  ! dominant vegetation index
REAL,ALLOCATABLE :: ZLAI0  (:,:)  ! leaf area index (per nature area)
REAL,ALLOCATABLE :: ZLAI   (:,:)  ! leaf area index (per shaded area)
REAL,ALLOCATABLE :: ZRSMIN (:,:)  ! minimum stomatal resistance
REAL,ALLOCATABLE :: ZCLAY  (:,:)  ! percentage of clay (within nature)
REAL,ALLOCATABLE :: ZSAND  (:,:)  ! percentage of sand (within nature)
REAL,ALLOCATABLE :: ZDEPTH (:,:)  ! root depth / hydrological soil depth
REAL,ALLOCATABLE :: ZALBG  (:,:)  ! ground albedo
REAL,ALLOCATABLE :: ZALBV  (:,:)  ! vegetation albedo
REAL,ALLOCATABLE :: ZALB   (:,:)  ! albedo
REAL,ALLOCATABLE :: ZEMIS  (:,:)  ! emissivity
REAL,ALLOCATABLE :: ZMASK  (:,:)

! -----
! namelists
! -----

NAMELIST /NAM/ L_FRAC,L_Z0,L_VEG,L_VEGDOM,L_SOIL,L_DEPTH,L_RAD, &
 &             NBITS,FACZ0,FACZ0_VEG,NLISS,NLISSZ,NLISSZ_VEG,LZ0THER

! -----
! read input arguments
! -----

! get input arguments
IF (IARGC() == 3) THEN
  CALL GETARG(1,CLFILE(1))
  CALL GETARG(2,CLFILE(2))
  CALL GETARG(3,CLFILE(3))
ELSEIF (IARGC() == 4) THEN
  CALL GETARG(1,CLFILE(0))
  CALL GETARG(2,CLFILE(1))
  CALL GETARG(3,CLFILE(2))
  CALL GETARG(4,CLFILE(3))
ELSE
  CALL HELP
  CALL EXIT(1)
ENDIF

! -----
! process namelist
! -----

! set default values
L_FRAC    =.FALSE.  ! update fractions of land and town
L_Z0      =.FALSE.  ! update surface roughness
L_VEG     =.FALSE.  ! update vegetation
L_VEGDOM  =.FALSE.  ! update dominant vegetation index
L_SOIL    =.FALSE.  ! update percentage of clay and sand
L_DEPTH   =.FALSE.  ! update root depth / hydrological soil depth
L_RAD     =.FALSE.  ! update albedos and emissivity
NBITS     =24       ! output precision (not applied to roughness)
FACZ0     = 1.      ! scaling factor for orographic roughness z0rel
FACZ0_VEG = 1.      ! scaling factor for vegetation roughness z0veg
NLISS     = 0       ! number of smoothings applied on soil/vegetation fields
NLISSZ    = 0       ! number of smoothings applied on log(z0rel)
NLISSZ_VEG= 0       ! number of smoothings applied on log(z0veg)
LZ0THER   =.FALSE.  ! thermal roughness without orographic component

! read namelist
IF ( IARGC() == 4 ) THEN
  OPEN(JPULNAM,FILE=CLFILE(0),STATUS='OLD')
  READ(JPULNAM,NAM)
  CLOSE(JPULNAM)
ENDIF

! check values
IF (L_RAD.AND.L_VEGDOM) THEN
  CALL HELP
  WRITE(0,'(A)') 'fa_sfx2clim: Error:'
  WRITE(0,'(A)') &
   & '  Namelist variable L_RAD=.T. requires L_VEGDOM=.F.!'
  WRITE(0,*)
  CALL EXIT(1)
ELSEIF ( FACZ0 < 0. ) THEN
  CALL HELP
  WRITE(0,'(A)') 'fa_sfx2clim: Error:'
  WRITE(0,'(A)') '  Namelist variable FACZ0 must not be negative!'
  WRITE(0,*)
  CALL EXIT(1)
ELSEIF ( FACZ0_VEG < 0. ) THEN
  CALL HELP
  WRITE(0,'(A)') 'fa_sfx2clim: Error:'
  WRITE(0,'(A)') '  Namelist variable FACZ0_VEG must not be negative!'
  WRITE(0,*)
  CALL EXIT(1)
ENDIF

! -----
! get FA dimensions and global packing defaults, allocate FA arrays
! -----

! determine FA dimensions valid for used xrd library
CALL FA_LIMITS(JPXPAH,JPXIND,JPXGEO,JPXNIV)

! get global packing defaults:
!   IPUILA - starting value for searching optimal Laplacian power
!   IDMOPL - search depth (in both directions)
CALL FAVORI(INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)

! allocate FA arrays
ALLOCATE(INLOPA  (JPXPAH))
ALLOCATE(INLOPA0 (JPXPAH))
ALLOCATE(INOZPA  (JPXIND))
ALLOCATE(ZSINLA  (JPXGEO))
ALLOCATE(ZSINLA0 (JPXGEO))
ALLOCATE(ZAHYBR(0:JPXNIV))
ALLOCATE(ZBHYBR(0:JPXNIV))

! -----
! set up FA package
! -----

! switch FA package to English
CALL LFIFRA(.FALSE.)

! set verbosity level to minimum
INIVAU=0
CALL FANMSG(INIVAU)

LLNOMM=.TRUE.         ! FA file name is associated with fortran unit
LLERFA=.TRUE.         ! issue fatal error when FA open fails
LLIMST=.FALSE.        ! do not produce statistics on FA close
LLSPEC=.FALSE.        ! gridpoint representation of fields
INIMES=0              ! level of diagnostic messages (0, 1 or 2)
INBARP=0              ! expected number of articles to be written
CLNMCA='FA_SFX2CLIM'  ! name of framework

! -----
! check consistency of FA files, read roughness fields
! -----

! loop through FA files
DO JFILE=1,3

  ! open FA file
  CALL FAITOU(IREP,JPULFA,LLNOMM,CLFILE(JFILE),'OLD',LLERFA,LLIMST, &
   &          INIMES,INBARP,INBARI,CLNMCA)

  ! get content of framework
  CALL FACIES(CLNMCA,ITYPTR,ZSLAPO,ZCLOPO,ZSLOPO,ZCODIL,ITRONC, &
   &          INLATI,INXLON,INLOPA,INOZPA,ZSINLA,INIVER,ZREFER, &
   &          ZAHYBR,ZBHYBR,LLGARD)

  ! store/check horizontal geometry
  IF ( JFILE == 1 ) THEN

    ! store dimensions and geographic parameters
    INLOPA0(:)=INLOPA(:)
    ZSINLA0(:)=ZSINLA(:)
    NDGL =INLATI
    NDLON=INXLON
    NDLUN=INLOPA(3)
    NDLUX=INLOPA(4)
    NDGUN=INLOPA(5)
    NDGUX=INLOPA(6)

    ! allocate data arrays
    ALLOCATE(ZNATURE(NDLON,NDGL))
    ALLOCATE(ZTOWN  (NDLON,NDGL))
    ALLOCATE(ZLAND  (NDLON,NDGL))
    ALLOCATE(ZLSM   (NDLON,NDGL))
    IF (L_Z0) THEN
      ALLOCATE(Z0VEG(NDLON,NDGL))
      ALLOCATE(Z0REL(NDLON,NDGL))
      ALLOCATE(Z0   (NDLON,NDGL))
      ALLOCATE(Z0H  (NDLON,NDGL))
    ENDIF
    IF (L_VEG.OR.L_RAD) ALLOCATE(ZVEG0(NDLON,NDGL))
    IF (L_VEG) THEN
      ALLOCATE(ZVEG  (NDLON,NDGL))
      ALLOCATE(ZLAI0 (NDLON,NDGL))
      ALLOCATE(ZLAI  (NDLON,NDGL))
      ALLOCATE(ZRSMIN(NDLON,NDGL))
    ENDIF
    IF ((L_VEG.AND.L_VEGDOM).OR.L_RAD) ALLOCATE(ZVEGDOM(NDLON,NDGL))
    IF (L_SOIL.OR.L_RAD) THEN
      ALLOCATE(ZCLAY(NDLON,NDGL))
      ALLOCATE(ZSAND(NDLON,NDGL))
    ENDIF
    IF (L_DEPTH) ALLOCATE(ZDEPTH(NDLON,NDGL))
    IF (L_RAD) THEN
      ALLOCATE(ZALBG(NDLON,NDGL))
      ALLOCATE(ZALBV(NDLON,NDGL))
      ALLOCATE(ZALB (NDLON,NDGL))
      ALLOCATE(ZEMIS(NDLON,NDGL))
    ENDIF

  ELSE

    ! check dimensions and geographic parameters
    IERR=0
    IF ( INLATI /= NDGL  ) THEN
      WRITE(0,*) '  NDGL =',NDGL,INLATI
      IERR=IERR+1
    ENDIF
    IF ( INXLON /= NDLON ) THEN
      WRITE(0,*) '  NDLON=',NDLON,INXLON
      IERR=IERR+1
    ENDIF
    DO J=3,6
      IF ( INLOPA(J) /= INLOPA0(J) ) THEN
        WRITE(0,*) '  INLOPA(',J,')=',INLOPA0(J),INLOPA(J)
        IERR=IERR+1
      ENDIF
    ENDDO
    DO J=1,8
      IF ( ABS(ZSINLA(J)-ZSINLA0(J)) > 1.E-06*ABS(ZSINLA0(J)) ) THEN
        WRITE(0,*) '  ZSINLA(',J,')=',ZSINLA0(J),ZSINLA(J)
        IERR=IERR+1
      ENDIF
    ENDDO
    IF ( IERR > 0 ) THEN
      WRITE(0,*)
      WRITE(0,'(A)')  'fa_sfx2clim: Error:'
      WRITE(0,'(A)')  '  Horizontal geometry of FA files is not the same!'
      WRITE(0,'(5A)') '  (',TRIM(CLFILE(1)),' versus ',TRIM(CLFILE(JFILE)),')'
      WRITE(0,*)
      CALL EXIT(1)
    ENDIF

  ENDIF

  ! read necessary fields from input FA file
  IF ( JFILE == 1 ) THEN

    ! nature and town fractions within gridbox
    CALL FACILE(IREP,JPULFA,'SFX.',0,'FRAC_NATURE',ZNATURE,LLSPEC)
    CALL FACILE(IREP,JPULFA,'SFX.',0,'FRAC_TOWN'  ,ZTOWN  ,LLSPEC)

    ! clay and sand fractions within nature
    IF (L_SOIL.OR.L_RAD) THEN
      CALL FACILE(IREP,JPULFA,'SFX.',0,'CLAY',ZCLAY,LLSPEC)
      CALL FACILE(IREP,JPULFA,'SFX.',0,'SAND',ZSAND,LLSPEC)
    ENDIF

    ! close input PGD file
    CALL FAIRME(IREP,JPULFA,'KEEP')

  ELSEIF ( JFILE == 2 ) THEN

    ! orographic and vegetation roughness
    IF (L_Z0) THEN
      CALL FACILE(IREP,JPULFA,'SFX.',0,'Z0REL',Z0REL,LLSPEC)
      CALL FACILE(IREP,JPULFA,'X001',0,'Z0VEG',Z0VEG,LLSPEC)
    ENDIF

    ! vegetation fraction, leaf area index, minimum stomatal resistance
    IF (L_VEG.OR.L_RAD) THEN
      CALL FACILE(IREP,JPULFA,'X001',0,'VEG'  ,ZVEG0 ,LLSPEC)
    ENDIF
    IF (L_VEG) THEN
      CALL FACILE(IREP,JPULFA,'X001',0,'LAI'  ,ZLAI0 ,LLSPEC)
      CALL FACILE(IREP,JPULFA,'X001',0,'RSMIN',ZRSMIN,LLSPEC)
    ENDIF

    ! root depth / hydrological soil depth
    IF (L_DEPTH) THEN
      CALL FACILE(IREP,JPULFA,'X001',0,'DG2',ZDEPTH,LLSPEC)
    ENDIF

    ! close input SURFEX file
    CALL FAIRME(IREP,JPULFA,'KEEP')

  ELSEIF ( JFILE == 3 ) THEN

    ! land-sea mask and dominant vegetation index
    CALL FACILE(IREP,JPULFA,'SURF',0,'IND.TERREMER',ZLSM   ,LLSPEC)
    IF (L_RAD) THEN
      CALL FACILE(IREP,JPULFA,'SURF',0,'IND.VEG.DOMI',ZVEGDOM,LLSPEC)
    ENDIF

  ENDIF

ENDDO

! -----
! treatment of selected fields
! -----

! set fractions of nature and town to zero where undefined (E zone)
DO JY=1,NDGL
  DO JX=1,NDLON
    IF ( ZNATURE(JX,JY) >= UNDEF ) ZNATURE(JX,JY)=0.
    IF ( ZTOWN  (JX,JY) >= UNDEF ) ZTOWN  (JX,JY)=0.
  ENDDO
ENDDO

! determine fraction of land and land-sea mask (zero in E zone)
DO JY=1,NDGL
  DO JX=1,NDLON
    ZLAND(JX,JY)=ZNATURE(JX,JY)+ZTOWN(JX,JY)
  ENDDO
ENDDO

IF (L_FRAC) THEN

  ! overwrite fractions of land and town in output FA file
  CALL FAIENC(IREP,JPULFA,'SURF',0,'PROP.TERRE'  ,ZLAND,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'PROP.URBANIS',ZTOWN,LLSPEC)

ENDIF

! surface roughness
IF (L_Z0) THEN

  ! scale roughness, replace undefined values by values over the sea
  DO JY=1,NDGL
    DO JX=1,NDLON
      IF ( Z0REL(JX,JY) < UNDEF ) THEN
        Z0REL(JX,JY)=FACZ0*Z0REL(JX,JY)*RG
      ELSE
        Z0REL(JX,JY)=0.
      ENDIF
      IF ( Z0VEG(JX,JY) < UNDEF ) THEN
        Z0VEG(JX,JY)=MAX(FACZ0_VEG*Z0VEG(JX,JY),SZZ0N)*RG
      ELSE
        Z0VEG(JX,JY)=SZZ0M*RG
      ENDIF
    ENDDO
  ENDDO

  ! -----
  ! smooth roughness
  ! -----

  ! smooth orographic roughness z0rel
  IF ( NLISSZ > 0 ) THEN

    ALLOCATE(ZMASK(NDLON,NDGL))
    ZMASK(:,:)=0.

    ! convert positive z0rel to log(z0rel)
    DO JY=NDGUN,NDGUX
      DO JX=NDLUN,NDLUX
        IF ( Z0REL(JX,JY) > 0. ) THEN
          Z0REL(JX,JY)=LOG(Z0REL(JX,JY))
          ZMASK(JX,JY)=1.
        ENDIF
      ENDDO
    ENDDO

    ! perform smoothing
    CALL SMOOTH(NLISSZ,NDLON,NDGL,ZMASK,Z0REL)

    ! convert log(z0rel) to z0rel
    DO JY=NDGUN,NDGUX
      DO JX=NDLUN,NDLUX
        IF ( ZMASK(JX,JY) == 1. ) THEN
          Z0REL(JX,JY)=EXP(Z0REL(JX,JY))
        ENDIF
      ENDDO
    ENDDO

    DEALLOCATE(ZMASK)

  ENDIF

  ! smooth vegetation roughness z0veg
  IF ( NLISSZ_VEG > 0 ) THEN

    ! convert z0veg over the land to log(z0veg)
    DO JY=NDGUN,NDGUX
      DO JX=NDLUN,NDLUX
        IF ( ZLSM(JX,JY) >= 0.5 ) THEN
          Z0VEG(JX,JY)=LOG(Z0VEG(JX,JY))
        ENDIF
      ENDDO
    ENDDO

    ! perform smoothing
    CALL SMOOTH(NLISSZ_VEG,NDLON,NDGL,ZLSM,Z0VEG)

    ! convert log(z0veg) over the land back to z0veg
    DO JY=NDGUN,NDGUX
      DO JX=NDLUN,NDLUX
        IF ( ZLSM(JX,JY) >= 0.5 ) THEN
          Z0VEG(JX,JY)=EXP(Z0VEG(JX,JY))
        ENDIF
      ENDDO
    ENDDO

  ENDIF

  ! -----
  ! update mechanical roughness z0 and thermal roughness z0h
  ! -----

  ! initialize roughness lengths with sea value
  Z0 (:,:)=SZZ0M*RG
  Z0H(:,:)=SZZ0M*RG

  ! loop over C+I zone
  IF ( LZ0THER ) THEN
    DO JY=NDGUN,NDGUX
      DO JX=NDLUN,NDLUX
        Z0 (JX,JY)=SQRT(Z0VEG(JX,JY)*Z0VEG(JX,JY)+Z0REL(JX,JY)*Z0REL(JX,JY))
        Z0H(JX,JY)=STHER*Z0(JX,JY)
      ENDDO
    ENDDO
  ELSE
    DO JY=NDGUN,NDGUX
      DO JX=NDLUN,NDLUX
        Z0 (JX,JY)=SQRT(Z0VEG(JX,JY)*Z0VEG(JX,JY)+Z0REL(JX,JY)*Z0REL(JX,JY))
        Z0H(JX,JY)=STHER*Z0VEG(JX,JY)
      ENDDO
    ENDDO
  ENDIF

  ! -----
  ! ensure consistency with land-sea mask
  ! -----

  ! loop over C+I zone
  DO JY=NDGUN,NDGUX
    DO JX=NDLUN,NDLUX
      IF ( ZLSM(JX,JY) < 0.5 ) THEN
        Z0REL(JX,JY)=0.
        Z0VEG(JX,JY)=SZZ0M*RG
        Z0   (JX,JY)=SZZ0M*RG
        Z0H  (JX,JY)=SZZ0M*RG
      ENDIF
    ENDDO
  ENDDO

  ! -----
  ! write updated roughness to output FA file
  ! -----

  ! switch off packing
  INGRIB=0
  CALL FAGOTE(IREP,JPULFA,INGRIB,INBPDG,INBCSP,ISTRON,IPUILA,IDMOPL)

  ! overwrite roughness fields in output FA file
  CALL FAIENC(IREP,JPULFA,'SURF',0,'Z0REL.FOIS.G',Z0REL,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'Z0VEG.FOIS.G',Z0VEG,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'Z0.FOIS.G'   ,Z0   ,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'GZ0.THERM'   ,Z0H  ,LLSPEC)

ENDIF

! set output packing
INGRIB=2
CALL FAGOTE(IREP,JPULFA,INGRIB,NBITS,NBITS,ISTRON,IPUILA,IDMOPL)

! vegetation fraction, leaf area index and minimum stomatal resistance
! (optionally also dominant vegetation index)
IF (L_VEG) THEN

  ! treat points without nature; change conventions from SURFEX to ISBA
  DO JY=1,NDGL
    DO JX=1,NDLON
      IF ( ZNATURE(JX,JY)*ZVEG0(JX,JY) < SVEG ) THEN
        ZVEG0 (JX,JY)=0.
        ZVEG  (JX,JY)=0.
        ZLAI  (JX,JY)=0.
        ZRSMIN(JX,JY)=SRSMX
      ELSE
        ZVEG(JX,JY)=ZNATURE(JX,JY)*ZVEG0(JX,JY)  ! shaded area per gridbox area
        ZLAI(JX,JY)=ZLAI0  (JX,JY)/ZVEG0(JX,JY)  ! leaf area per shaded area
      ENDIF
      ZLAI  (JX,JY)=          MIN(SLAIX,ZLAI  (JX,JY))
      ZRSMIN(JX,JY)=MAX(SRSMN,MIN(SRSMX,ZRSMIN(JX,JY)))
    ENDDO
  ENDDO

  ! smooth fields
  IF ( NLISS > 0 ) THEN
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZVEG0 )
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZVEG  )
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZLAI0 )
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZLAI  )
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZRSMIN)
  ENDIF

  ! overwrite fields in output FA file
  CALL FAIENC(IREP,JPULFA,'SURF',0,'PROP.VEGETAT',ZVEG  ,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'IND.FOLIAIRE',ZLAI  ,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'RESI.STO.MIN',ZRSMIN,LLSPEC)

  ! dominant vegetation index
  IF (L_VEGDOM) THEN

    DO JY=1,NDGL
      DO JX=1,NDLON
        IF ( ZLSM(JX,JY) == 0. ) THEN
          ZVEGDOM(JX,JY)=1.  ! sea, lake, river
        ELSEIF ( ZLAI0(JX,JY) <= SLAIH ) THEN
          ZVEGDOM(JX,JY)=3.  ! desert, low vegetation
        ELSE
          ZVEGDOM(JX,JY)=4.  ! high vegetation
        ENDIF
      ENDDO
    ENDDO
    CALL FAIENC(IREP,JPULFA,'SURF',0,'IND.VEG.DOMI',ZVEGDOM,LLSPEC)

  ENDIF

ENDIF

! percentage of clay and sand
IF (L_SOIL.OR.L_RAD) THEN

  ! replace undefined values by zero, change units
  DO JY=1,NDGL
    DO JX=1,NDLON
      IF ( ZCLAY(JX,JY) < UNDEF ) THEN
        ZCLAY(JX,JY)=ZLSM(JX,JY)*ZCLAY(JX,JY)*100.
      ELSE
        ZCLAY(JX,JY)=0.
      ENDIF
      IF ( ZSAND(JX,JY) < UNDEF ) THEN
        ZSAND(JX,JY)=ZLSM(JX,JY)*ZSAND(JX,JY)*100.
      ELSE
        ZSAND(JX,JY)=0.
      ENDIF
    ENDDO
  ENDDO

  ! smooth fields
  IF ( NLISS > 0 ) THEN
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZCLAY)
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZSAND)
  ENDIF

  ! overwrite fields in output FA file
  IF (L_SOIL) THEN
    CALL FAIENC(IREP,JPULFA,'SURF',0,'PROP.ARGILE',ZCLAY,LLSPEC)
    CALL FAIENC(IREP,JPULFA,'SURF',0,'PROP.SABLE' ,ZSAND,LLSPEC)
  ENDIF
  
ENDIF

! root depth / hydrological soil depth
IF (L_DEPTH) THEN

  ! replace undefined values outside nature by e923 maximum value,
  ! truncate values to min/max range
  DO JY=1,NDGL
    DO JX=1,NDLON
      IF ( ZNATURE(JX,JY) == 0. ) ZDEPTH(JX,JY)=SDEPX
      ZDEPTH(JX,JY)=MAX(SDEPN,MIN(SDEPX,ZDEPTH(JX,JY)))
    ENDDO
  ENDDO
  
  ! smooth field
  IF ( NLISS > 0 ) CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZDEPTH)

  ! overwrite fields in output FA file
  CALL FAIENC(IREP,JPULFA,'SURF',0,'EPAIS.SOL'   ,ZDEPTH,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'EPAI.SOL.MAX',ZDEPTH,LLSPEC)

ENDIF

! albedos and emissivity
IF (L_RAD) THEN

  ! prepare vegetation fraction within nature
  IF (.NOT.L_VEG) THEN
    DO JY=1,NDGL
      DO JX=1,NDLON
        IF ( ZNATURE(JX,JY)*ZVEG0(JX,JY) < SVEG ) ZVEG0(JX,JY)=0.
      ENDDO
    ENDDO
    IF ( NLISS > 0 ) CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZVEG0)
  ENDIF

  ! determine albedos and emissivity
  DO JY=1,NDGL
    DO JX=1,NDLON

      ! nature fractions of clay and sand
      ZFC0=ZCLAY(JX,JY)/100.
      ZFS0=ZSAND(JX,JY)/100.

      ! gridbox fractions: water, unshaded clay/sand/rock, urban, vegetation
      ZFM=1.-ZLAND(JX,JY)
      ZFC=ZNATURE(JX,JY)*    ZFC0      *(1.-ZVEG0(JX,JY))
      ZFS=ZNATURE(JX,JY)*         ZFS0 *(1.-ZVEG0(JX,JY))
      ZFR=ZNATURE(JX,JY)*(1.-ZFC0-ZFS0)*(1.-ZVEG0(JX,JY))
      ZFU=ZTOWN  (JX,JY)
      ZFV=ZNATURE(JX,JY)*                   ZVEG0(JX,JY)

      ! bare ground albedo
      ZALBG(JX,JY)=(ZFM*SALBM+ZFC*SALBC+ZFS*SALBS+ZFR*SALBR+ZFU*SALBU)/(1.-ZFV)

      ! vegetation albedo
      IF     ( NINT(ZVEGDOM(JX,JY)) == 3 ) THEN
        ZALBV(JX,JY)=SALBL  ! desert, low vegetation
      ELSEIF ( NINT(ZVEGDOM(JX,JY)) == 4 ) THEN
        ZALBV(JX,JY)=SALBH  ! high vegetation
      ELSE
        ZALBV(JX,JY)=SALBM
      ENDIF

      ! total albedo and emissivity
      ZALB (JX,JY)=(1.-ZFV)*ZALBG(JX,JY)+ZFV*ZALBV(JX,JY)
      ZEMIS(JX,JY)=ZFM*SEMIM+ZFC*SEMIC+ZFS*SEMIS+ZFR*SEMIR+ &
       &           ZFU*SEMIU+ZFV*SEMIV

      ! truncate values to min/max range
      ZALB (JX,JY)=MAX(SALBN,MIN(SALBX,ZALB (JX,JY)))
      ZALBG(JX,JY)=MAX(SALBN,MIN(SALBX,ZALBG(JX,JY)))
      ZALBV(JX,JY)=MAX(SALBN,MIN(SALBX,ZALBV(JX,JY)))
      ZEMIS(JX,JY)=MAX(SEMIN,MIN(SEMIX,ZEMIS(JX,JY)))

    ENDDO
  ENDDO

  ! smooth fields
  IF ( NLISS > 0 ) THEN
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZALBG)
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZALBV)
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZALB )
    CALL SMOOTH(NLISS,NDLON,NDGL,ZLSM,ZEMIS)
  ENDIF

  ! overwrite fields in output FA file
  CALL FAIENC(IREP,JPULFA,'SURF',0,'ALBEDO.SOLNU',ZALBG,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'ALBEDO.COMPL',ZALBG,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'ALBEDO.VEG'  ,ZALBV,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'ALBEDO'      ,ZALB ,LLSPEC)
  CALL FAIENC(IREP,JPULFA,'SURF',0,'EMISSIVITE'  ,ZEMIS,LLSPEC)

ENDIF

! -----
! finalization
! -----

! close output clim file
CALL FAIRME(IREP,JPULFA,'KEEP')

! deallocate FA arrays
DEALLOCATE(INLOPA,INLOPA0,INOZPA)
DEALLOCATE(ZSINLA,ZSINLA0,ZAHYBR,ZBHYBR)

! deallocate data arrays
DEALLOCATE(ZNATURE,ZTOWN,ZLAND,ZLSM)
IF (L_Z0)                          DEALLOCATE(Z0VEG,Z0REL,Z0,Z0H)
IF (L_VEG.OR.L_RAD)                DEALLOCATE(ZVEG0)
IF (L_VEG)                         DEALLOCATE(ZVEG,ZLAI0,ZLAI,ZRSMIN)
IF ((L_VEG.AND.L_VEGDOM).OR.L_RAD) DEALLOCATE(ZVEGDOM)
IF (L_SOIL.OR.L_RAD)               DEALLOCATE(ZCLAY,ZSAND)
IF (L_DEPTH)                       DEALLOCATE(ZDEPTH)
IF (L_RAD)                         DEALLOCATE(ZALB,ZEMIS)

! =====

! -----
! private procedures
! -----

CONTAINS

SUBROUTINE SMOOTH(KLISS,KDLON,KDGL,PLSM,PSMOOTH)

IMPLICIT NONE

INTEGER,INTENT(IN) :: KLISS
INTEGER,INTENT(IN) :: KDLON
INTEGER,INTENT(IN) :: KDGL

REAL,INTENT(IN   ) :: PLSM(KDLON,KDGL)
REAL,INTENT(INOUT) :: PSMOOTH(KDLON,KDGL)

INTEGER :: IX1,IX2,IY1,IY2
INTEGER :: J,JX,JY

REAL :: ZAUX(KDLON,KDGL)

! perform smoothing over the land in C+I zone
DO J=1,KLISS
  ZAUX(:,:)=PSMOOTH(:,:)
  DO JY=NDGUN,NDGUX
    IY1=MAX(NDGUN,JY-1)
    IY2=MIN(NDGUX,JY+1)
    DO JX=NDLUN,NDLUX
      IF ( PLSM(JX,JY) == 1. ) THEN
        IX1=MAX(NDLUN,JX-1)
        IX2=MIN(NDLUX,JX+1)
        PSMOOTH(JX,JY)=(4.*ZAUX(JX,JY)+                             &
         & PLSM(IX1,JY)*ZAUX(IX1,JY)+PLSM(IX2,JY)*ZAUX(IX2,JY)+     &
         & PLSM(JX,IY1)*ZAUX(JX,IY1)+PLSM(JX,IY2)*ZAUX(JX,IY2))/    &
         & (4.+PLSM(IX1,JY)+PLSM(IX2,JY)+PLSM(JX,IY1)+PLSM(JX,IY2))
      ENDIF
    ENDDO
  ENDDO
ENDDO

END SUBROUTINE SMOOTH

! -----

SUBROUTINE HELP

WRITE(0,'(A)') ''
WRITE(0,'(A)') 'USAGE:'
WRITE(0,'(A)') '  fa_sfx2clim [<nam>] <pgd> <sfx> <clim>'
WRITE(0,'(A)') ''
WRITE(0,'(A)') 'ARGUMENTS:'
WRITE(0,'(A)') '  <nam>   namelist file (optional)'
WRITE(0,'(A)') '  <pgd>   PGD file providing constant fields'
WRITE(0,'(A)') '  <sfx>   SURFEX file providing monthly fields'
WRITE(0,'(A)') '  <clim>  e923 clim file to be updated'
WRITE(0,'(A)') ''
WRITE(0,'(A)') 'Example of namelist:'
WRITE(0,'(A)') ''
WRITE(0,'(A)') '  &NAM'
WRITE(0,'(A)') '    FACZ0=0.53,'
WRITE(0,'(A)') '    FACZ0_VEG=1.00,'
WRITE(0,'(A)') '    NLISSZ=1,'
WRITE(0,'(A)') '    NLISSZ_VEG=0,'
WRITE(0,'(A)') '    LZ0THER=.F.,'
WRITE(0,'(A)') '  /'
WRITE(0,'(A)') ''
WRITE(0,'(A)') 'Meaning of namelist variables:'
WRITE(0,'(A)') ''
WRITE(0,'(A)') '  L_FRAC     - update fractions of land and town'
WRITE(0,'(A)') '               (default .F.)'
WRITE(0,'(A)') '  L_Z0       - update surface roughness'
WRITE(0,'(A)') '               (default .F.)'
WRITE(0,'(A)') '  L_VEG      - update vegetation'
WRITE(0,'(A)') '               (default .F.)'
WRITE(0,'(A)') '  L_VEGDOM   - update dominant vegetation index'
WRITE(0,'(A)') '               (suboption of L_VEG, default .F.)'
WRITE(0,'(A)') '  L_SOIL     - update percentage of clay and sand'
WRITE(0,'(A)') '               (default .F.)'
WRITE(0,'(A)') '  L_DEPTH    - update root depth / hydrological soil depth'
WRITE(0,'(A)') '               (default .F.)'
WRITE(0,'(A)') '  L_RAD      - update albedos and emissivity'
WRITE(0,'(A)') '               (default .F.)'
WRITE(0,'(A)') '  NBITS      - output precision (not applied to roughness)'
WRITE(0,'(A)') '               (default 24)'
WRITE(0,'(A)') '  FACZ0      - scaling factor for orographic roughness z0rel'
WRITE(0,'(A)') '               (default 1.00, as in SURFEX)'
WRITE(0,'(A)') '  FACZ0_VEG  - scaling factor for vegetation roughness z0veg'
WRITE(0,'(A)') '               (default 1.00)'
WRITE(0,'(A)') '  NLISS      - number of smoothings of non-roughness fields'
WRITE(0,'(A)') '               (default 0)'
WRITE(0,'(A)') '  NLISSZ     - number of smoothings applied on log(z0rel)'
WRITE(0,'(A)') '               (default 0)'
WRITE(0,'(A)') '  NLISSZ_VEG - number of smoothings applied on log(z0veg)'
WRITE(0,'(A)') '               (default 0)'
WRITE(0,'(A)') '  LZ0THER    - thermal roughness with orographic component'
WRITE(0,'(A)') '               (default .F., as in SURFEX)'
WRITE(0,'(A)') ''

END SUBROUTINE HELP

END PROGRAM FA_SFX2CLIM
