#!/bin/bash
#PBS -l nodes=1:ppn=8
#PBS -l walltime=12:00:00
#PBS -A 2022_205
#PBS -o log.out
#PBS -e log.err
#PBS -m a
#PBS -N clim2nc

# Define directory where run script is located
SRCDIR=/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake

# Load modules
module swap cluster/dodrio/cpu_milan
module load vsc-mympirun
module load foss/2022a R/4.2.1-foss-2022a
module load Cartopy/0.20.3-foss-2022a
module load dask/2022.10.0-foss-2022a
module load ecCodes/2.27.0-gompi-2022a
module load netcdf4-python/1.6.1-foss-2022a
module load xarray/2022.6.0-foss-2022a
module load rasterio/1.3.4-foss-2022a
module load rioxarray/0.14.0-foss-2022a

export PYTHONPATH="/dodrio/scratch/users/vsc45263/wout/readFA/src:${PYTHONPATH}"
export OMPI_MCA_btl='^uct,ofi'
export OMPI_MCA_pml='ucx'
export OMPI_MCA_mtl='^ofi'
echo $PYTHONPATH

# Go to script and run
cd $SRCDIR
python clim_to_nc.py
