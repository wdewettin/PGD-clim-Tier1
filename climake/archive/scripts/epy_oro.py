#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function, absolute_import, division, unicode_literals

import sys
import os
import argparse

epypath = '/home/gmap/mrpe/mary/public/EPyGrAM/1.4.18-eccodes'

vortexpath = '/home/mf/dp/marp/verolive/vortex/vortex-olive'

sys.path.append(epypath)
sys.path.append(os.path.join(epypath, 'site'))
sys.path.append(vortexpath)
sys.path.append(os.path.join(vortexpath, 'site'))
sys.path.append(os.path.join(vortexpath, 'src'))

from bronx.meteo.constants import g0

import epygram
epygram.init_env()

def main(clim, pgd):
    clim = epygram.formats.resource(clim, 'r')
    z = clim.readfield('SURFGEOPOTENTIEL')
    z.operation('/', g0)  # back to metres
    z.fid['FA'] = 'SFX.ZS'  # change name to that of PGD
    clim.close()
    pgd = epygram.formats.resource(pgd, 'a')
    pgd.fieldencoding(z.fid['FA'], update_fieldscompression=True)  # read compression
    pgd.writefield(z, compression=pgd.fieldscompression.get(z.fid['FA'], None))
    pgd.close()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description="""Read SURFGEOPOTENTIEL in Clim, divide by g0, overwrite it in PGD.""")
    parser.add_argument('-c',
                        required=True,
                        dest='clim',
                        help="""Name/path to Clim file.""")
    parser.add_argument('-p',
                        required=True,
                        dest='pgd',
                        help="""Name/path to PGD file.""")
    args = parser.parse_args()

    main(args.clim, args.pgd)
