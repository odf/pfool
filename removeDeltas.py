#!/usr/bin/env python

import sys
import poserFile

if len(sys.argv) < 2:
    print "Usage: %s infile outfile" % sys.argv[0]
    print "  Strips all deltas from a Poser file leaving morph channels empty."
    sys.exit(1)

content = poserFile.PoserFile(file(sys.argv[1]))
content.root.delete('actor', 'channels', 'targetGeom',
                    'indexes|numbDeltas|deltas')
content.writeTo(file(sys.argv[2], 'w'))
