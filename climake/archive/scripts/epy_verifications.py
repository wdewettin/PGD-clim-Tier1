#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function, absolute_import, division, unicode_literals

import sys
import os
import codecs

epypath = '/home/gmap/mrpe/mary/public/EPyGrAM/1.4.7'
vortexpath = '/home/mf/dp/marp/verolive/vortex/vortex-olive'

sys.path.append(epypath)
sys.path.append(os.path.join(epypath, 'site'))
sys.path.append(vortexpath)
sys.path.append(os.path.join(vortexpath, 'site'))
sys.path.append(os.path.join(vortexpath, 'src'))

#epygram default plots options
epyOpts=dict()

import epygram
epygram.init_env()
filesToCheck=['Const.Clim.'+"%02d" % mm for mm in range(1,13)]
filesToCheck.insert(0, "PGD.fa")
strToReplace="#RESULTS_TABLE#"
rowTmplt="<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>"
strtable=""

bypasslist=['SEA_NBLEVEL','LEVL_OC1','LEVL_OC2','LEVL_OC3','LEVL_OC4','TAU_REL_OC',
'LREL_CUR_OC','LREL_TS_OC','LFLX_NULL_OC','LFLX_CORR_OC',".LDIAPYC_OC",
 "SEAICE_SCHEM","ICENL","ICENT","_FBUF_DIM2","SOC","WRITE_EXT","SPLIT_PATCH",
"ECOSG","SN_VEGP1","ALBEDO","TBUDC","L_BCOEF","L_CONDSAT","L_MPOTSAT","L_REAP_D",
"L_REAP_M","L_SEED_D","L_SEED_M","L_WFC","L_WSAT","L_WWILT","DATE","DATE.DATE",
"SFX.LATMAX","SFX.LATMIN","SFX.LONMAX","SFX.LONMIN","SFX.NLAT","SFX.NLON"]
#first calculate results
for f in filesToCheck:
    epyResource=epygram.formats.resource(filename=f, openmode='r')
    availableFields=epyResource.find_fields_in_resource(fieldtype='H2D')
    availableFields.sort()
    for fname in availableFields:
        if fname in bypasslist:
            continue
        epyFields=epyResource.readfield(fname) 
        sha=epyFields.sha256_checksum()
        statsDict=epyFields.stats()
        identifier=f+'@'+fname
        strtable+=rowTmplt % (f,fname,statsDict['mean'],statsDict['min'],statsDict['max'],statsDict['std'],sha)

        
with codecs.open('results.html', 'r', encoding='utf-8') as file :
  filedata = file.read()

# Replace the target string
filedata = filedata.replace(strToReplace, strtable)

# Write the file out again
with codecs.open('results.html', 'w', encoding='utf-8') as file:
  file.write(filedata)
  

#the make plots
for f in filesToCheck:
    epyResource=epygram.formats.resource(filename=f, openmode='r')
    availableFields=epyResource.find_fields_in_resource(fieldtype='H2D')
    availableFields.sort()
    for fname in availableFields:
        if fname in bypasslist:
            continue
        
        epyFields=epyResource.readfield(fname) 
        identifier=f+'@'+fname
        if hasattr(epyFields, 'spectral'):
            if epyFields.spectral:
                epyFields.sp2gp()
                
                
        (plt,ax)=epyFields.cartoplot(**epyOpts)
        plt.savefig('plots/%s.png' % identifier)
        plt.clear()  

            
            
            
