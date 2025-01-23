# CLIMAKE on VSC Tier-1

## 1. How to run

## 2. Where to find output/log files?

## 3. Detailed information

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
```

These variables define the following aspects:
- `PGDGEO`: Path to the domain-specific PGD-namelist. This namelist includes the `NAM_CONF_PROJ` and `NAM_CONF_PROJ_GRID` sections. If you don't want to create a PGD, you should put `"NOPGD"` here.
- `CLIMGEO`: Path to the domain-specific clim-namelist. This namelist includes the `NAMDIM`, `NEMDIM` and `NEMGEO` sections.
- `CYCLE`: Cycle of the model for which clim/PGD-files are created. Currently, we have only transferred information on `cy43t2_clim-op8.01`, so do not change this.
- `PGDNAM`: Path to the namelist for PGD-creation. If you want to use the standard namelist, put `GCO` here. This standard namelist can be found here: **fill in, GCO is passed to 1_fetch_files**
- `CLIMNAM`: Path to the namelist for clim-creation. If you want to use the standard namelist, put `GCO` here. This standard namelist can be found here: **fill in, GCO is passed to 1_fetch_files**
- `OROGRAPHY_TRUNCATION`: Truncation method for the orography field. Three values are possibles: `LINEAR`, `QUADRATIC` or `CUBIC`. Best to use `QUADRATIC`.
- `OTHER_FIELDS_TRUNCATION`: Truncation method for the other fields. Three values are possibles: `LINEAR`, `QUADRATIC` or `CUBIC`. Best to use `LINEAR`.
- `AUTOTRUNC`: Method for determining the spectral parameters. Can be `a` for automatic or `m` for manual. If manual then, `NSMAX` and `NMSMAX` will need to be inputted in the terminal. Furthermore if `OROGRAPHY_TRUNCATION` is different from `OTHER_FIELDS_TRUNCATION` then two pairs of spectral parameters will need to be provided.
- `CUSTOM_PGD`: Path to a custom executable for **making the PGD ?**. If not defined, the `GENERIC` executable will be used. **Which is this?**
- `CUSTOM_MASTER`: Path to a custom master for **running the model ?**. If not defined, the `GENERIC` executable will be used. **Which is this?**
- `CUSTOM_OUT`: Custom name for output directory. If not provided, a name will be constructed for all defined variables in the config file.
- `PGD_HACK`: Lines of code to run just after copying the ECOCLIMAP data to the working directory. The main goal of these lines is to overwrite the default ECOCLIMAP files with files of your choosing. 

**Add arguments for ecoclimap version and gmted version.**


### 1_fetch_files

Normally this script determines the relevant variables based on the `CYCLE` argument from the config file. However, because this versioning is not (yet) implemented these variables have been hard-coded in a txt-file. This txt-file is then simply loaded in by this file. The file can be found in: `climake/scripts/variables_${CYCLE}.txt`

**Move the variable file?**

### 2_make_pgd

This script makes the (first version of the) PGD-file.

It does so in the work directory defined by `$CLIMAKEWORKDIR`. There, it creates a subdirectory called `0_build_pgd`. The `NEEDED_INPUTS`-variable is a list of necessary databases for the creation of the PGD. You can make manual changes here to other databases (but be careful). This list is converted into a dictionary called `GENVDICT`, based on the information in the file `variables_${CYCLE}.txt`. For every element in the dictionary, the information is copied to the working directory and, if necessary, untarred. 

Next, the namelist is constructed. If `PGDNAM=GCO` then the standard namelist is used from `${GENVDICT[NAMELIST_AROME]}'/arome/namel_buildpgd'` with `GENVDICT[NAMELIST_AROME]="cy43t2_clim-bf.01.nam"`, located in the `data`-directory (but having been copied to the working directory). This namelist is stored as `OPTIONS.nam`. Next, this namelist is updated with variables from the domain-specific namelist `PGDGEO` from the config file. This editing is applied in-place to `OPTIONS.nam` with the tool `/stuff/xpnam`. Subsequently,  the PGD-exectubale is copied to the working directory. If `CUSTOM_PGD=GENERIC` a default binary is copied based on information in the `variables`-file. However, this (probably) does not work as no binary is located in the `data`-directory. Therefore, the custom PGD should be defined. After this step, the environment variables are loaded from the file `/stuff/environmentVariables`. 

Before running the executable, we first overwrite the ECOCLIMAP I files with our own ECOCLIMAP II files. After this step, these new files can be overwritten again if `PGDHACK` is defined. Now, the PGD-executable is run. The PGD-file is copied to the output directory and the listing file to the main working directory. Finally, the following step is submitted as a job. 

**Add arguments here for ECOCLIMAP and GMTED**
**Add argument for data directory?**
**Move data directory?**

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

**Expand explanation?**

### 4_replace_orography

This script replaces the orography field (SFX.ZS) in the PGD file with the (modified) geopotential from the clim file(s). This is performed in the output directory. 

**Add a step 5 with e927_update?**
