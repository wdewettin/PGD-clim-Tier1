# CLIMAKE on MF-BELENOS
## 1. BASICS

Clone the climake scripts with
```bash
~suzat/SAVE/cloneClimake
```

What databases will we use?  
In MF these are grouped together in so-called `GENV/GCO`-cycles  
It's a tag corresponding to a version of the set of databases/files needed for the PGD/923 configuration.  
At meteo-france these are linked with the operational cycle.  

You can check the databases with
```bash
/home/mf/dp/marp/martinezs/public/bin/genv
```
For the most recent cycle use `oper-clim`
The climfiles I made for Wout, I used `cy43t2_clim-op8.01`


climake can be run interactively or with config file.  
An example is given in `cofig_example.conf`

New config file for small Belgian 4km domain `config_be40a_l.conf`
```vim
  PGDGEO=geometries/PGD/be40a_l.nam
  CLIMGEO=geometries/923/be40a_l.nam
  CYCLE=cy43t2_clim-op8.01
  PGDNAM=GCO
  CLIMNAM=GCO
  OROGRAPHY_TRUNCATION=QUADRATIC
  OTHERFIELDS_TRUNCATION=LINEAR
  AUTOTRUNC=a
```

So we need to define the geometry namelists.  
We create a `./geometries/PGD/be40a_l.nam` for the creation of the PGD
```vim
 &NAM_CONF_PROJ
   XBETA=0.,
   XLAT0=50.569898649999999,
   XLON0=4.553615160000000,
   XRPK=0.772399999982374,
 /
 &NAM_CONF_PROJ_GRID
   ILATE=11,
   ILONE=11,
   NIMAX=181,
   NJMAX=181,
   XDX=4011.481521874645,
   XDY=4011.481311823738,
   XLATCEN=50.569898649999999,
   XLONCEN=4.553615160000000,
 /
```
If you want to change things for TEB, etc.
You can do it in the _main_ namelist `./namelist/GENERIC.nam`

We also create a `./geometries/923/be40a_l.nam` for the creation of the actual clim-files (FA-files)

```vim
 &NAMDIM
   NDGLG=192,
   NDGUXG=181,
   NDLON=192,
   NDLUXG=181,
   NFLEVG=1,
   NMSMAX=95,
   NSMAX=95,
 /
 &NEMDIM
   NBZONG=8,
   NBZONL=8,
 /
 &NEMGEO
   EDELX=4011.481521874645,
   EDELY=4011.481311823738,
   ELAT0=50.569898649999999,
   ELATC=50.569898649999999,
   ELON0=4.553615160000000,
   ELONC=4.553615160000000,
 /

```

Now we can run climake with
```bash
./climake -c config_be40a_l.conf
```

This script starts a sequence of 4 jobs:
1. Retrieving all the databases
2. Creating the PGD-file
3. Creating the clim-files
4. Plotting all the fields for inspection

Output can be found in `./outputFiles`

## 2. Caveats
### A. Orography
For the orography the Global Multi-resolution Terrain Elevation Data (GMTED2010) database is used.  
This database exists in 7.5, 15 and 30 arcsec resolutions. We can check in the genv:
```bash
/home/mf/dp/marp/martinezs/public/bin/genv cy43t2_clim-op8.01
```
By default the 7.5 arcsec version is used. This we can check in the PGD-creation script.
```bash
scripts/2_make_pgd
```
on line 50
```bash=50
# Input needed in this job
#If you want to change the orography input dabatase and take the GMTED 30s file replace 
#GMTED2010_OROGRAPHY_07S5 with GMTED2010_OROGRAPHY_30S0 in the NEEDED_INPUTS variable above

NEEDED_INPUTS=(HWSD_CLAY HWSD_SAND ECOCLIMAP_I_SURFACE_TYPE GMTED2010_OROGRAPHY_07S5 ECOCLIMAP_COVERS_PARAM MASTER_PGD NAMELIST_AROME)
```

When would you want to do change this database?  
For some regions the 7.5 arcsec database is incorrect or incompatible with your own chosen resolution (e.g. Greenland at 4km resolution with GMTED2010 7.5" gives wrong elevations). 
:::warning
:zap:Always check **ALL** your fields in the clim-files!
:::
To use the GMTED2010 30 arcsec database you need to **manually** change the script to
```bash=50
 Input needed in this job
#If you want to change the orography input dabatase and take the GMTED 30s file replace 
#GMTED2010_OROGRAPHY_07S5 with GMTED2010_OROGRAPHY_30S0 in the NEEDED_INPUTS variable above

NEEDED_INPUTS=(HWSD_CLAY HWSD_SAND ECOCLIMAP_I_SURFACE_TYPE GMTED2010_OROGRAPHY_30S0 ECOCLIMAP_COVERS_PARAM MASTER_PGD NAMELIST_AROME)
```
### B. ECOCLIMAP I vs ECOCLIMAP II
Climake always used `ECOCLIMAPI`, but there exists an `ECOCLIMAPII` **(not to be confused with `ECOCLIMAP-SG`)**.  
From the website:
>ECOCLIMAP-II is an improvement of ECOCLIMAP-I over Europe. The strategy for implementing ECOCLIMAP-II is to depart from prevalent land cover products such as CLC2000 (Corine Land Cover) and GLC2000 (Global Land Cover) by splitting existing classes into new classes that possess a better regional character by virtue of the climatic environment (latitude, proximity to the sea, topography). ...
> [name=https://opensource.umr-cnrm.fr/projects/ecoclimap/wiki] 

So there is no use in using it for domains outside of Europe. If you want to use them you need to overwrite the original files by the new `ECOCLIMAPII` files, which you need to download yourself. _(LINK NEEDED)_

This is done in 
```bash
scripts/2_make_pgd
```
where you can add the lines 119-122
```bash=115
echo '  ------------EXPORTING NAMELIST  ----------------- '
cat OPTIONS.nam
echo '  ------------------------------------------------- '

#>>> overwrite ECOCLIMAP I files by ECOCLIMAP II
chmod 644 ecoclimap.dir ecoclimap.hdr
cp /home/gmap/mrpm/masekj/data/ecoclimap/ECOCLIMAP_II_EUROP_V2.5.dir ecoclimap.dir
cp /home/gmap/mrpm/masekj/data/ecoclimap/ECOCLIMAP_II_EUROP_V2.5.hdr ecoclimap.hdr
```
:::warning
:zap: Always document what database (GMTED2010_7.5/GMTED2010_30.0 and ECOI/ECOII) you used for you clim-files, because there is no trace of it in the listings and name of the output directory.
:::

## 3. Improvement of the clim-files
For high resolution (< 2.5 km), some of the databases used for creation of the clim-files are of to low resolution. Resulting in very low-quality fields (i.e. fields related to roughness lengths).

A fix was provided by J. Masek for CHMI. The scripts can be found on `belenos` under `~masekj/e923_update`  
More information can be found in `~masekj/e923_update/README.e923_update`

So If we want to update our climfiles we can copy the scripts:
```bash
mkdir e923_update
cd e923_update
mkdir script template
cp ~masekj/e923_update/script/run script/
cp ~masekj/e923_update/template/* template/
cd script
```
We can adapt the run script
```bash
# root directory for e923 update
$ROOT = "$HOME/DEMONSTRATION/e923_update";

# data directory on scratch file system
$DATA = "/scratch/work/$user/e923_update/data";

# root directory for climake
$CLIMAKE = "$HOME/DEMONSTRATION/climake";

# climake configuration file for desired domain
$conf = "$CLIMAKE/config_be40a_l.conf";

# directory with climake outputs for desired domain
$CLIMAKEOUT = "$CLIMAKE/outputFiles/pgddomain-be40a_l.nam__climdomain-be40a_l.nam__cycle-cy43t2_clim-op8.01__pgdnam-GCO__climnam-GCO__orotrunc-QUADRATIC__othertrunc-LINEAR__v-1.37";
```
and save it with a decriptive name: `run.be40a_l_0.53_1.00_lis3-3_1.5h`

Lets run it and check the output in `e923_update/data/clim`

### Some remarks
To update the fields in the clim file you need to make an actual forecast (1 timestep) with ALARO/SURFEX with a specific cycle:
```bash
/home/gmap/mrpm/masekj/pack/cy43t2_bf.11_suborog/bin/MASTERODB
```

This means you need to have some LBCs and initial files available.
On BELENOS these are in some default folder of J. Masek , but only works if your domain lies within ~Europe.





