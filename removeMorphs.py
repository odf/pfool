#!/usr/bin/env python

import sys
import poserFile

content = poserFile.PoserFile(file(sys.argv[1]))
content.root.delete('actor', 'channels', 'targetGeom')
content.writeTo(file(sys.argv[2], 'w'))
