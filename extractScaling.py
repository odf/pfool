#!/usr/bin/env python

import re, sys
from poserFile import *

def scalingData(source, actor_names):
    wrapper_text = """\
        {
        version
                {
                number 4.1
                }
        Figure
                {
                }
        }"""
    content = PoserFile(wrapper_text.splitlines())
    anchor = content.root.select('Figure').next()

    if actor_names:
        actors = list(actor for actor in source.actors
                      if re.sub(r':\d+$', '', actor.name) in actor_names)
    else:
        actors = list(source.actors)

    for actor in actors:
        anchor.prependSibling(actor.extractChannels('scale[XYZ]?'))

    return content

if __name__ == "__main__":
    source = PoserFile(file(sys.argv[1]))
    scalingData(source, None).writeTo(file(sys.argv[2], "w"))
