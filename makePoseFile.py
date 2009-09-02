#!/usr/bin/env python

import sys
from poserFile import *

def makePoseFile(template_text, actor_names):
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
    template = PoserFile(template_text.splitlines()).root.firstChild
    anchor = content.root.select('Figure').next()
    for name in actor_names:
        node = template.clone()
        node.fields[1] = name
        anchor.prependSibling(node)

    return content

if __name__ == "__main__":
    text = """\
        actor -
            {
            castsShadow  0
            includeInDepthCue  0
            visibleInReflections 0
            visibleInRender 0
            }"""
    actors = ['hiphandle', 'lBreast', 'rBreast', 'lHip', 'rHip', 'groin']
    makePoseFile(text, actors).writeTo(file(sys.argv[1], "w"))
