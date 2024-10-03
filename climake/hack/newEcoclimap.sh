#!/bin/bash
#########################################################################
# Correction ecoclimap airports GMME/VILLE  
# ------------------------------------

echo '  ------------ecoclimaps from GMME/VILLE  ------------------- '

rm ecoclimap.dir
rm ecoclimap.hdr
rm ecoclimapI_covers_param.bin
rm ecoclimapII_af_covers_param.bin
rm ecoclimapII_eu_covers_param.bin

tar -zxvf /scratch/climat/CEDRE/data/sfx/ecoclimap_cy46/ecoclimap.covers.param.06_corr.tgz
tar -zxvf /scratch/climat/CEDRE/data/sfx/ecoclimap_cy46/ecoclimap_i.v1_6.01.aude.tgz

#########################################################################
