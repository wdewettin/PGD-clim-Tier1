from readfa.FA_to_netcdf import FA_to_netcdf_file
from yaml import safe_load

### Define parameters ###

output_dir = f"/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/outputFiles/pgddomain-BE40a_l.nam__climdomain-BE40a_l.nam__cycle-cy43t2_clim-op8.01__pgdnam-namel_buildpgd_noteb__climnam-GCO__orotrunc-QUADRATIC__othertrunc-LINEAR__v-1.37" # The directory containing the FA-files
outputfilename = "PGD.fa" # The base of the FA-file names
store_dir = f"{output_dir}/store" # Directory to store the small netcdf-files with 2D output at single time steps
storefilebase = f"BE40a_l_noTEB"
netcdf_dir = f"{output_dir}/netcdf" # Directory to store the combined raw model output in netcdf-format
rbin = "/readonly/dodrio/apps/RHEL8/zen2-ib/software/R/4.2.1-foss-2022a/lib64/R/bin" # Optional: define the R bin directory to speed up the program
             
### Convert from FA to CF-compliant netcdf format ###
FA_to_netcdf_file(output_dir, outputfilename, store_dir, storefilebase, netcdf_dir, rbin=rbin)