# -*- coding: utf-8 -*-
# Convert the NLCD Florida file to be more useful.
# Copyright © 2015 Defenders of Wildlife, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

from pprint import pprint as pp
import sys

hab_key = {'Barren Land': 0,
           'Cultivated Crops': 0,
           'Deciduous Forest': 1,
           'Developed; High Intensity': 0,
           'Developed; Low Intensity': 0,
           'Developed; Medium Intensity': 0,
           'Developed; Open Space': 0.5,
           'Emergent Herbaceous Wetlands': 1,
           'Evergreen Forest': 1,
           'Grassland/Herbaceous': 1,
           'Mixed Forest': 1,
           'Open Water': 1,
           'Pasture/Hay': 0.5,
           'Shrub/Scrub': 1,
           'Woody Wetlands': 1}

def main():
    """Convert the NLCD Florida file to be more useful.

    USAGE:
        python prep_NLCD_change_file.py <infile> <outfile>
    ARGS:
        infile, path to a csv of NLCD habitat changes
        outfil, path to a tab'd, well-formed version of NLCD change data
    """
    loaded, habitats = load_data()
    write_data(loaded)

def load_data():
    "Return a dict of form county:from_hab:to_hab:amt."
    res = {}
    all_habs = set()
    for line in open(infile):
        if not line.startswith("LABEL"):
            data = line.replace('"', '').rstrip().split("\t")
            fr_hab, to_hab, all_habs = split_hab(data[0], all_habs)
            data = data[1:]
            for i in range(len(data)):
                cur_dat = data[i]
                cur_cnt = counties[i]
                subd = res[cur_cnt]
                if fr_hab in subd:
                    subd[fr_hab][to_hab] = cur_dat
                else:
                    subd[fr_hab] = {to_hab: cur_dat}
                res[cur_cnt].update(subd)
        else:
            data = line.rstrip().split("\t")
            counties = data[1:]
            for i in counties:
                res[i] = {}
    return res, all_habs

def split_hab(x, ahab):
    "Return the from_hab, to_hab, and updated set of habitats."
    habs = x.split(" to ")
    ahab.add(habs[0])
    ahab.add(habs[1])
    return habs[0], habs[1], ahab

def write_data(d):
    with open(outfil, 'w') as out:
        header = "County\tfrom_hab\tto_hab\tamt\tindex\n"
        out.write(header)
        for i in sorted(d.keys()):
            for j in sorted(d[i].keys()):
                for k in sorted(d[i][j].keys()):
                    idx = str(hab_key[k] - hab_key[j])
                    dat = [i, j, k, d[i][j][k].replace(",", ""), idx]
                    out.write("\t".join(dat) + "\n")

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print main.__doc__
        sys.exit()
    infile = sys.argv[1]
    outfil = sys.argv[2]
    main()
