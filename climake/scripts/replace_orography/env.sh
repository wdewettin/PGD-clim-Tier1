module purge
# module swap cluster/dodrio/cpu_milan
# Change module to 2023 version
module load iimpi/2023a imkl/2023.1.0 vsc-mympirun
module list
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/vsc45302_kwinten/software/iimpi2023a/lib64 # Change this to correction location
ulimit -s unlimited
ulimit -c unlimited