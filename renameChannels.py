#!/usr/bin/env python

import re, sys
from poserFile import *

if __name__ == "__main__":
    content = PoserFile(file(sys.argv[1]))
    pattern = re.compile(sys.argv[3])
    replacement = sys.argv[4]

    for node in content.root.select("actor", "channels",
                                    "targetGeom|valueParm"):
        if pattern.search(node.rest):
            node.rest = pattern.sub(replacement, node.rest)

    for node in content.root.select("actor", "channels", ".*", "valueOp.*"):
        source = node.nextSibling.nextSibling.nextSibling
        if pattern.search(source.firstField):
            source.firstField = pattern.sub(replacement, source.firstField)

    for node in content.root.select("actor", "channels", "groups"):
        for desc in node.descendants:
            if desc.firstField == "parmNode" and pattern.search(desc.rest):
                desc.rest = pattern.sub(replacement, desc.rest)

    for node in content.root.select("figure", "linkParms"):
        parm = node.nextSibling
        if pattern.search(parm.text):
            pattern.sub(replacement, parm.text)
        parm = parm.nextSibling.nextSibling
        if pattern.search(parm.text):
            pattern.sub(replacement, parm.text)

    content.writeTo(file(sys.argv[2], "w"))
