#!/usr/bin/env python

import os.path, re, sys
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
    if len(sys.argv) < 5:
        print re.sub("<prog>", os.path.basename(sys.argv[0]), """\
  Usage: [python] <prog> infile outfile pattern replacement

    Changes internal channel names for targetGeom and valueParm
    channels and updates references to renamed channels.  Regular
    expression syntax is used for pattern and replacement.

    Requires a properly installed Python 2.5 or newer (see
    python.org).

  Examples:

    <prog> a.cr2 b.cr2 CTRL Ctrl-

	Reads the file a.cr2, changes "CTRL" anywhere in a channel
	name to "Ctrl-" and writes the result to the file b.cr2.

    <prog> a.cr2 b.cr2 "^CTRL" Ctrl-

	The same, but only replaces "CTRL" if it appears at the
	beginning of the name.

    <prog> a.cr2 b.cr2 "([a-z])([A-Z])" "\1_\2"

	Inserts an underscore between words in channels names.
""")

        sys.exit(1)

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
