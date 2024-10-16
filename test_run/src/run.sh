#!/bin/bash
#PBS -l nodes=4:ppn=128
#PBS -l walltime=01:00:00
#PBS -A 2022_205
#PBS -N testrun


# 1. SETTINGS
# -----------

# 1a. Runtime settings
# --------------------
TIMESTEP=180

YYYY=2019
MM=03
NDAYS=1
NSTOP=$((NDAYS*24*3600/TIMESTEP))
LEVELS=$(echo $(seq -f %2g, 1 1 46))
NFRHIS=$((3600/TIMESTEP))
LINC=.T. # If false watch out for output handling after run

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
COUPLINGDIR=${BASEDIR}/prep/coupling/${YYYY}/${MM}
NAMEDIR=${BASEDIR}/name
WORKDIR=${BASEDIR}/work
SAVEDIR=${BASEDIR}/output

# namelists
NAM001=$NAMEDIR/name.e001.CY43T2.DYDOCASE.long.level03
NAMSFX=$NAMEDIR/EXSEG1.nam.plus
NAMFAREP=$NAMEDIR/name.FAreplace
NAMFAREPSFX=$NAMEDIR/name.FAreplace.sfx

# 2. EXECUTION
# ------------

# go to workdir
mkdir -p ${WORKDIR}
cd ${WORKDIR}
rm -f * 

# 2a. Coupling files
# ------------------

scp ${COUPLINGDIR}/* .

# 2b. Initial files (FAreplace)
# -----------------------------

rsync -l ELSCFABOFALBC000 ICMSHABOFINIT

# 2c. Run the model
# -----------------

# Bring master
ln -sf ${MASTER} MASTER

# Create namelist file
cat $NAM001 |grep -v '^!'|sed -e "s/!.*//" \
-e "s/{nproc}/${NPROC}/g" \
-e "s/{timestep}/${TIMESTEP}/g" \
-e "s/{nstop}/${NSTOP}/g" \
-e "s/{nfrhis}/${NFRHIS}/g" \
-e "s/{linc}/${LINC}/g" > fort.4
cp $NAMSFX ./EXSEG1.nam 

# RUN!
mympirun -h $((128/OMP_NUM_THREADS)) ./MASTER > log.out 2>log.err

# 2d. Save output
# ---------------

if [ ! -d ${SAVEDIR}/${YYYY}/${MM} ]
then
mkdir -p ${SAVEDIR}/${YYYY}/${MM}
fi

# Copy output from workdir to savedir
cp ICMSHABOF* ${SAVEDIR}/${YYYY}/${MM}/
cp fort.4 ${SAVEDIR}/${YYYY}/${MM}/
cp log* ${SAVEDIR}/${YYYY}/${MM}/
cp NODE* ${SAVEDIR}/${YYYY}/${MM}/
cp ifs* ${SAVEDIR}/${YYYY}/${MM}/
