# CLIMAKE on VSC Tier-1

## First to do (if you are not runnning on the VSC Tier-1 machine)

In the directory `climake/e923_update/fa_sfx2clim` you should first compile the fa_sfx2clim executable by following the instructions in the README-file (located in the same directory).

## How to run

- Go to the `climake`-directory. 
- Run the `climake`-script with the following command, where *<path_to_config_file>* needs to be filled in with a path to a config file (see below).

```
./climake -c <path_to_config_file>
``` 
- This script will then launch a series of subsequent jobs to make the clim/PGD files. You will find the output and log files in a directory under `outputFiles`. The directory name consists of the chosen parameters, unless specified otherwise. 

## Common errors

- Oftentimes the 1st and 4th e923-update scripts fail for unexplained reasons. Resubmitting them usually solves the issue. For that you should go into the work directory: `$WORK_DIR/e923_update/script`. Submit the script by running `sbatch $ID_1_lbc` or `$ID_4_clim`, where `$ID` will be filled in with the name of your config file. You can check which script failed by looking at the log files in `$WORK_DIR/e923_update/log`

## More information

More README-files and other documentation can be found in the `doc` directory.

### Configuration file

The configuration file specifies important variables for how to create the clim/PGD files such as the domain, truncation method, etc. An example configuration file is:

```
PGDGEO=geometries/PGD/EUCO25.nam # "NOPGD" geometries/PGD/EUCO25.nam
CLIMGEO=geometries/923/EUCO25.nam
CYCLE=cy43t2_clim-op8.01
PGDNAM=GCO
CLIMNAM=GCO
OROGRAPHY_TRUNCATION=QUADRATIC
OTHERFIELDS_TRUNCATION=LINEAR
AUTOTRUNC=a
CUSTOM_PGD=/dodrio/scratch/users/vsc45263/wout/ALARO-SURFEX/pack/compile_SFX_execs/bin/PGD
CUSTOM_MASTER=/dodrio/scratch/users/vsc45263/wout/ALARO-SURFEX/pack/suborog/bin/MASTERODB
CUSTOM_OUT=test
PGDHACK=/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/hack/overwriteEcoclimap.sh
E923_UPDATE=y
DATA_DIR=/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/PGD-clim-data/data

### e923_update settings
# root directory for e923 update
ROOT="/dodrio/scratch/users/vsc45263/wout/PGD-clim-Tier1/climake/e923_update"

# data directory on scratch file system
DATA="/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/PGD-clim-data/e923_update/m3d"

#####

# roughness configuration
FACZ0='0.53'      # scaling factor for orographic roughness
FACZ0_VEG='1.00'  # scaling factor for vegetation roughness
XMUL_H_TREE='1.5' # scaling factor for tree height
NLISSZ=3          # number of smoothings for orographic roughness
NLISSZ_VEG=3      # number of smoothings for vegetation roughness
```

These variables define the following aspects:
- `PGDGEO`: Path to the domain-specific PGD-namelist. This namelist includes the `NAM_CONF_PROJ` and `NAM_CONF_PROJ_GRID` sections. If you don't want to create a PGD, you should put `"NOPGD"` here.
- `CLIMGEO`: Path to the domain-specific clim-namelist. This namelist includes the `NAMDIM`, `NEMDIM` and `NEMGEO` sections.
- `CYCLE`: Cycle of the model for which clim/PGD-files are created. Currently, we have only transferred information on `cy43t2_clim-op8.01`, so do not change this.
- `PGDNAM`: Path to the namelist for PGD-creation. If you want to use the standard namelist, put `GCO` here. This standard namelist can be found in the shared data directory: `/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/PGD-clim-data/data/cy43t2_clim-bf.01.nam/namel_buildpgd`
- `CLIMNAM`: Path to the namelist for clim-creation. If you want to use the standard namelist, put `GCO` here. This standard namelist can be found in the shared data directory: `/dodrio/scratch/projects/2022_200/project_output/RMIB-UGent/PGD-clim-data/data/cy43t2_clim-bf.01.nam/namel_c923`
- `OROGRAPHY_TRUNCATION`: Truncation method for the orography field. Three values are possibles: `LINEAR`, `QUADRATIC` or `CUBIC`. Best to use `QUADRATIC`.
- `OTHER_FIELDS_TRUNCATION`: Truncation method for the other fields. Three values are possibles: `LINEAR`, `QUADRATIC` or `CUBIC`. Best to use `LINEAR`.
- `AUTOTRUNC`: Method for determining the spectral parameters. Can be `a` for automatic or `m` for manual. If manual then, `NSMAX` and `NMSMAX` will need to be inputted in the terminal. Furthermore if `OROGRAPHY_TRUNCATION` is different from `OTHER_FIELDS_TRUNCATION` then two pairs of spectral parameters will need to be provided.
- `CUSTOM_PGD`: Path to a custom PGD executable. In our implementation this has to be defined!
- `CUSTOM_MASTER`: Path to a custom master for running the model. In our implementation this has to be defined!
- `CUSTOM_OUT`: Custom name for output directory. If not provided, a name will be constructed for all defined variables in the config file.
- `PGDHACK`: Path of a small script to run just after copying the ECOCLIMAP data to the working directory. The main goal of this script is to overwrite the default ECOCLIMAP files with files of your choosing. 
- `E923_UPDATE`: Specifies whether the e923-update will be performed at the end to improve the roughness length fields in the clim files. This step is not necessary when running ALARO with SURFEX. To turn this step on, this variable has to be `y`.
- `DATA_DIR`: Path to directory where databases are stored.

Below this line there are variables that specifiy the e923-update step.
- `ROOT`: Path to root directory with all e923-update related information.
- `DATA`: Path to directory with e923-update related model files. 


### 1_fetch_files

Normally, this script determines the relevant variables based on the `CYCLE` argument from the config file. However, because this versioning is not (yet) implemented these variables have been hard-coded in a txt-file. This txt-file is then simply loaded in by this file. The file can be found in: `climake/variables/variables_${CYCLE}.txt`

### 2_make_pgd

This script makes the (first version of the) PGD-file.

It does so in the work directory defined by `$CLIMAKEWORKDIR`. There, it creates a subdirectory called `0_build_pgd`. The `NEEDED_INPUTS`-variable is a list of necessary databases for the creation of the PGD. You can make manual changes here to other databases (but be careful). This list is converted into a dictionary called `GENVDICT`, based on the information in the file `variables_${CYCLE}.txt`. For every element in the dictionary, the information is copied to the working directory and, if necessary, untarred. 

Next, the namelist is constructed. If `PGDNAM=GCO` then the standard namelist is used from `${GENVDICT[NAMELIST_AROME]}'/arome/namel_buildpgd'` with `GENVDICT[NAMELIST_AROME]="cy43t2_clim-bf.01.nam"`, located in the `data`-directory (but having been copied to the working directory). This namelist is stored as `OPTIONS.nam`. Next, this namelist is updated with variables from the domain-specific namelist `PGDGEO` from the config file. This editing is applied in-place to `OPTIONS.nam` with the tool `/stuff/xpnam`. Subsequently,  the PGD-exectubale is copied to the working directory. If `CUSTOM_PGD=GENERIC` a default binary is copied based on information in the `variables`-file. However, this (probably) does not work as no binary is located in the `data`-directory. Therefore, the custom PGD should be defined. After this step, the environment variables are loaded from the file `/stuff/environmentVariables`. 

Before running the executable, we first overwrite the ECOCLIMAP I files with our own ECOCLIMAP II files. After this step, these new files can be overwritten again if `PGDHACK` is defined. Now, the PGD-executable is run. The PGD-file is copied to the output directory and the listing file to the main working directory. Finally, the following step is submitted as a job. 

### 3_climake

This scripts builds the climatological files.

First the namelist variable `LPGD` is defined based on the previous creation of a PGD or not. Next, we go the working directory. We load in the environment variables from `/stuff/environmentVariables`. The needed inputs are defined as a list and converted to a dictionary `GENVDICT`, similarly to the `2_make_pgd`-script (see above). Subsequently, the relevant databases are copied to the working directory. 

Next, the namelist is loaded from `CLIMNAM`, which is `${GENVDICT[NAMELIST_AROME]}'/arome/namel_c923'` if `CLIMNAM` is `GCO` with `GENVDICT[NAMELIST_AROME]="cy43t2_clim-bf.01.nam"`. This namelist is modified with the domain-specific variables from `CLIMGEO`. Based on the spectral parameters, we need one or two orography steps. The namelists for this/these step(s) are constructed and added to our previous namelist.

Now, we arrive at the actual core of the script. The clim-files are constructed through a series of steps, which are performed subsequently. These steps are defined with the following string: 
```
LOCALDIRECTORYNAME;input1@localname1,input2@localname2,....;commmand1,command2,...
```
Each of these steps are performed in a different subdirectory. At the end of each step the current version of the clim file(s) is moved to the main working directory. For more detailed information, I refer to the script itself.

After all these steps, the monthly clim files are copied to the output directory. The final step, `4_replace_orography`, is submitted.

### 4_replace_orography

This script replaces the orography field (SFX.ZS) in the PGD file with the (modified) geopotential from the clim file(s). This is performed in the output directory. 

If enabled with the variable `E923_UPDATE`, this script will launch the e923-update step. 
