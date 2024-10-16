#!/bin/bash
#PBS -l nodes=1:ppn=128
#PBS -l walltime=00:30:00
#PBS -A 2022_205
#PBS -N testprep

set -x

# 1. SETTINGS
# -----------

# 1a. Runtime settings
# --------------------

NDAYS=1
LEVELS=$(echo $(seq -f %2g, 1 1 46))
YYYY=2019
MM=03

MASTER=/dodrio/scratch/projects/starting_2022_075/accord/pack/test43t2_iimpi/bin/MASTERODB

# 1b. Environment settings
# ------------------------
export PYTHONPATH="" 
export OMP_NUM_THREADS=1
NPROC=$((PBS_NP/OMP_NUM_THREADS))

module swap cluster/dodrio/cpu_milan
module load iimpi/2022a imkl/2022.1.0 vsc-mympirun
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/dodrio/scratch/projects/starting_2022_075/accord/software/iimpi2022a/lib64
ulimit -s unlimited
ulimit -c unlimited

export OMP_STACKSIZE="$((4/OMP_NUM_THREADS))G"
export KMP_STACKSIZE="$((4/OMP_NUM_THREADS))G"
#export KMP_MONITOR_STACKSIZE=1G

export MPL_MBX_SIZE=2048000000
export I_MPI_ADJUST_GATHERV=3   # necessary on hortense, otherwise hangs forever in calculation of spectral norms !?

export DR_HOOK=1
export DR_HOOK_IGNORE_SIGNALS=8   # FPEs in acraneb, acraneb2
#export DR_HOOK_SILENT=1
#export DR_HOOK_OPT=prof

export EC_PROFILE_HEAP=0
export EC_MPI_ATEXIT=0

# 1c. Directory settings
# ----------------------

# directories
BASEDIR=/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/test_run
COUPLINGDIR=/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45263_wout/DYDOCASE/runs/long/level02/run199101/output
NAMEDIR=${BASEDIR}/name
WORKDIR=${BASEDIR}/prep/work${RDATE}
SAVEDIR=${BASEDIR}/prep/coupling

# namelists
NAM927=$NAMEDIR/name.e927
NAM927_SFX=$NAMEDIR/name.e927.sfx
NAM927_LVL=$NAMEDIR/BE40a_46l

# data directories and clim files
CLIM_TRG=/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/outputFiles/pgddomain-BE40a_l.nam__climdomain-BE40a_l.nam__cycle-cy43t2_clim-op8.01__pgdnam-GCO__climnam-GCO__orotrunc-QUADRATIC__othertrunc-LINEAR__v-1.37_gmted30s0_ECOII/Const.Clim.
CLIM_LBC=/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45263_wout/DYDOCASE/runs/long/level02/clim/Const.Clim.
PGDFILE=/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/outputFiles/pgddomain-BE40a_l.nam__climdomain-BE40a_l.nam__cycle-cy43t2_clim-op8.01__pgdnam-GCO__climnam-GCO__orotrunc-QUADRATIC__othertrunc-LINEAR__v-1.37_gmted30s0_ECOII/PGD.fa
DATADIR_RRTM=/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45263_wout/DYDOCASE/runs/data/RRTM
DATADIR_ECOCLIMAP=/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45263_wout/DYDOCASE/runs/data/ECOCLIMAP

# 2. EXECUTION
# ------------

# go to workdir
mkdir -p ${WORKDIR}
cd ${WORKDIR}

# 2a. Coupling files
# ------------------

# Get coupling files for current month
CPLNR=0
DAY=1
HOUR=0
ATTEMPT=1

let CPLNRSTOP=10#$NDAYS*24

while [ $CPLNR -le $CPLNRSTOP ]
do
  CPLNR_FORMATTED=$(printf "%03d" $CPLNR)
  HH=$(printf "%02d" $HOUR)
  DD=$(printf "%02d" $DAY)

# 2a.I Prep
# ---------

if [[ ! -f ${WORKDIR}/ELSCFABOFALBC${CPLNR_FORMATTED} && ! -f ${SAVEDIR}/${YYYY}/${MM}/ELSCFABOFALBC${CPLNR_FORMATTED} ]]
then

if [[ ! -d prep ]] 
then 
mkdir prep
fi

rm prep/*
cd prep

# Bring climate files
if [[ $DD -gt $NDAYS ]]
then
scp ${CLIM_TRG}${NEXTMM} const.clim.ABOF
else
scp ${CLIM_TRG}${MM} const.clim.ABOF
fi

scp ${CLIM_LBC}${MM} Const.Clim

# Bring RRTM files
scp ${DATADIR_RRTM}/RADRRTM .
scp ${DATADIR_RRTM}/RADSRTM .
scp ${DATADIR_RRTM}/MCICA .

### and now we actually run ALADIN:
echo 'echo MONITOR: $* >&2' >monitor.needs
chmod +x monitor.needs
echo $NPROC

# Create namelist file
cat ${NAM927} ${NAM927_LVL} | grep -v '^!' | \
  sed -e "s/!.*//"	\
      -e "s/{nproc}/${NPROC}/g"	\
      -e "s/{domain}/ABOF/g" \
      -e "s/{levels}/${LEVELS}/g"	\
> fort.4

COUPLINGPATH="${COUPLINGDIR}/${YYYY}/${MM}/ICMSHABOF+0${CPLNR_FORMATTED}"
ln -sf ${COUPLINGPATH} ICMSHABOFINIT

# Bring executable binary
ln -sf ${MASTER} ./MASTER

# Run
mympirun -h $((128/OMP_NUM_THREADS)) ./MASTER > log.out 2>log.err

mv PFABOFABOF+0000 ../ELSCFABOFALBC${CPLNR_FORMATTED}

cd ..

fi

# 2aII. prepSurf
# --------------

if [[ $CPLNR_FORMATTED = 000 ]]
then

if [[ ! -f ${WORKDIR}/ICMSHABOFINIT.sfx && ! -f ${SAVEDIR}/${YYYY}/${MM}/ICMSHABOFINIT.sfx ]]
then

if [[ ! -d prepSurf ]] # If directory prepSurf does not exist, then it will be made
then
mkdir prepSurf
fi

rm prepSurf/* # all the contents of prepSurf are deleted
cd prepSurf

# Bring climate files
scp ${CLIM_TRG}${MM} const.clim.ABOF
scp ${CLIM_LBC}${MM} Const.Clim

# Bring data files
scp $PGDFILE const.clim.sfx.ABOF
scp ${DATADIR_ECOCLIMAP}/*.bin .
scp ${DATADIR_RRTM}/RADRRTM .
scp ${DATADIR_RRTM}/RADSRTM .
scp ${DATADIR_RRTM}/MCICA .

# When the monitor.needs file is executed, it will print the string MONITOR: followed by the arguments passed to it (if any), to the standard error stream.
echo 'echo MONITOR: $* >&2' >monitor.needs
chmod +x monitor.needs
echo $NPROC

# Create namelist file
cat ${NAM927_SFX} ${NAM927_LVL} | grep -v '^!' | \
	sed -e "s/!.*//"	\
	    -e "s/{nproc}/${NPROC}/g"	\
	    -e "s/{domain}/ABOF/g" \
	    -e "s/{levels}/${LEVELS}/g"	\
> fort.4

ln -sf $COUPLINGDIR/${YYYY}/${MM}/ICMSHABOF+0000 ICMSHABOFINIT

# Bring executable binary
ln -sf ${MASTER} ./MASTER

# run
mympirun -h $((128/OMP_NUM_THREADS)) ./MASTER > log.out 2>log.err

mv PFABOFABOF+0000.sfx ../ICMSHABOFINIT.sfx
cp const.clim.sfx.ABOF ../Const.Clim.sfx 
cp ecoclimapI_covers_param.bin ../ecoclimapI_covers_param.bin
cp ecoclimapII_eu_covers_param.bin ../ecoclimapII_eu_covers_param.bin

cd ..

fi

fi

# 2aIII. update loop variables if there is output
# -----------------------------------------------

((CPLNR++))
let HOUR=10#$HOUR+1

if [ $HOUR -ge 24 ]
then
  HOUR=0
  ((DAY++))
fi

done

# 2d. Save output
# ---------------

if [ ! -d ${SAVEDIR}/${YYYY}/${MM} ]
then
mkdir -p ${SAVEDIR}/${YYYY}/${MM}
fi

# Copy output from workdir to savedir
cp ELSCFABOFALBC* ICMSHABOFINIT.sfx Const.Clim.sfx ecoclimap* ${SAVEDIR}/${YYYY}/${MM}/

# Delete workdir
rm -rv $WORKDIR