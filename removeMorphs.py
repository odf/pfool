#!/usr/bin/env python

import sys
import poserFile

if len(sys.argv) < 2:
    print "Usage: %s infile outfile" % sys.argv[0]
    print "  Completely removes all morph channels from a Poser file."
    sys.exit(1)

content = poserFile.PoserFile(file(sys.argv[1]))
content.root.delete('actor', 'channels', 'targetGeom')
content.writeTo(file(sys.argv[2], 'w'))
