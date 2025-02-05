#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import print_function, absolute_import, division, unicode_literals
import sys
import os
import argparse

#vortex and epygram dependencies
epypath = '/home/gmap/mrpe/mary/public/EPyGrAM/1.4.7'
vortexpath = '/home/mf/dp/marp/verolive/vortex/vortex-olive'

sys.path.append(epypath)
sys.path.append(os.path.join(epypath, 'site'))
sys.path.append(vortexpath)
sys.path.append(os.path.join(vortexpath, 'site'))
sys.path.append(os.path.join(vortexpath, 'src'))

import vortex
from vortex import toolbox
import common

import epygram
epygram.init_env()

t= vortex.ticket()
sh= t.sh

def main(r, elon1,elon2,elat1,elat2):
    bounds={'lonmin':float(elon1),
            'lonmax':float(elon2),
            'latmin':float(elat1),
            'latmax':float(elat2)}
# vortex algo component mecanism
# it uses the bounds dictionnary passed by argument
    sh.title('Toolbox algo tb01 = tbalgo')
    tb01 = tbalgo = toolbox.algo(
        boundaries     = bounds,
        engine         = 'algo',
        format         = '',
        geometry       = 'customll',
        illustration   = False,
        intent         = '',
        kind           = 'make_domain',
        mode           = 'boundaries',
        resolution     = r,
    )
    for bin in [None]:
        tbalgo.run(bin)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="""Generate geometry namelist deltas for lon lat post processing grids.""")
    parser.add_argument('-r',
                        required=True,
                        dest='resolution',
                        help="""resolution in degree.""")
    parser.add_argument('-elon1',
                        required=True,
                        dest='elon1',
                        help="""minimum longitude.""")
    parser.add_argument('-elon2',
                        required=True,
                        dest='elon2',
                        help="""maximum longitude.""")
    parser.add_argument('-elat1',
                        required=True,
                        dest='elat1',
                        help="""minimum latitude.""")
    parser.add_argument('-elat2',
                        required=True,
                        dest='elat2',
                        help="""maximum latitude.""")
    args = parser.parse_args()

    main(args.resolution, args.elon1,args.elon2,args.elat1,args.elat2)
