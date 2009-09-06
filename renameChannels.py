#!/usr/bin/env python

import re, sys
from poserFile import *


class EchoDict(dict):
    def get(self, key):
        if dict.has_key(self, key):
            return dict.get(self, key)
        else:
            return key

    def __getitem__(self, key):
        if dict.has_key(self, key):
            return dict.__getitem__(self, key)
        else:
            return key

if __name__ == "__main__":
    content = PoserFile(file(sys.argv[1]))
    pattern = re.compile(sys.argv[3])
    replacement = sys.argv[4]

    old2new = EchoDict()

    for node in content.root.select("actor", "channels",
                                    "targetGeom|valueParm"):
        if pattern.search(node.rest):
            old = node.rest
            node.rest = old2new.setdefault(old, pattern.sub(replacement, old))

    for node in content.root.select("actor", "channels", ".*", "valueOp.*"):
        source = node.nextSibling.nextSibling.nextSibling
        source.firstField = old2new[source.firstField]

    for node in content.root.select("actor", "channels", "groups"):
        for desc in node.descendants:
            if desc.firstField == "parmNode":
                desc.rest = old2new[desc.rest]

    for node in content.root.select("figure", "linkParms"):
        parm = node.nextSibling
        parm.text = old2new[parm.text]
        parm = parm.nextSibling.nextSibling
        parm.text = old2new[parm.text]

    content.writeTo(file(sys.argv[2], "w"))
