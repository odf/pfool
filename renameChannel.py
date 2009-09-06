#!/usr/bin/env python

import sys
from poserFile import *

if __name__ == "__main__":
    content = PoserFile(file(sys.argv[1]))
    oldName = sys.argv[3]
    newName = sys.argv[4]

    for node in content.root.select("actor", "channels",
                                    "targetGeom|valueParm"):
        if node.rest == oldName:
            node.rest = newName

    for node in content.root.select("actor", "channels", ".*", "valueOp.*"):
        source = node.nextSibling.nextSibling.nextSibling
        if source.firstField == oldName:
            source.firstField = newName

    for node in content.root.select("actor", "channels", "groups"):
        for desc in node.descendants:
            if desc.firstField == "parmNode" and desc.rest == oldName:
                desc.rest = newName

    for node in content.root.select("figure", "linkParms"):
        parm = node.nextSibling
        if parm.text == oldName:
            parm.text = newName
        parm = parm.nextSibling.nextSibling
        if parm.text == oldName:
            parm.text = newName

    content.writeTo(file(sys.argv[2], "w"))
