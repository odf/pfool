#!/usr/bin/env python

import sys
from poserFile import *

TEMPLATE_TEXT = """\
targetGeom -
    {
    name -
    initValue 0
    hidden 1
    forceLimits 4
    min -10000
    max 10000
    trackingScale 0.01
    keys
            {
            static  0
            k  0  0
            }
    interpStyleLocked 0
    }
"""
template = PoserFile(TEMPLATE_TEXT.splitlines()).root.firstChild


if __name__ == "__main__":
    content = PoserFile(file(sys.argv[1]))
    
    for channels in content.root.select("actor", "channels"):
        if re.match('^BODY(:\d+)?$', channels.parent.rest):
            continue
        for node in channels.children:
            if not node.firstField in ('{', 'groups'):
                anchor = node
                break
        for name in sys.argv[3:]:
            node = template.clone()
            node.rest = name
            node.select('name').next().rest = name
            anchor.prependSibling(node)

    content.writeTo(file(sys.argv[2], "w"))
