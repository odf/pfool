#!/usr/bin/env python

import sys
from poserFile import PoserFile

def shiftPoint(node, v):
    for i in range(1, len(node.fields)):
        node.fields[i] = str(float(node.fields[i]) + v[i-1])

def shiftChannel(node, v):
    inSpheres = False
    for child in node.children:
        if child.firstField == 'center':
            shiftPoint(child, v)
        elif child.firstField == 'sphereMatsRaw':
            inSpheres = True
        elif inSpheres:
            try:
                w = map(float, child.fields)
            except ValueError:
                inSpheres = False
            else:
                if w and w[-1] == 1:
                    for i in range(len(v)):
                        child.fields[i] = str(float(w[i]) + v[i])

def shiftActor(actor, v):
    parent = actor.parent
    for channels in actor.content.select('channels'):
        for channel in channels.children:
            if channel.get('otherActor') == parent.name:
                shiftChannel(channel, v)
    for channels in parent.content.select('channels'):
        for channel in channels.children:
            if channel.get('otherActor') == actor.name:
                shiftChannel(channel, v)
    for node in actor.content.select('origin'):
        shiftPoint(node, v)
    for node in actor.content.select('endPoint'):
        shiftPoint(node, v)

def shiftActorAndDescendants(actor, v):
    for node in actor.subtree:
        shiftActor(node, v)


if __name__ == "__main__":
    tree = PoserFile(file(sys.argv[1]))

    shiftActorAndDescendants(tree.actor('rForeArm:1'),
                             [-0.013467, -0.001708, 0.000797])
    shiftActorAndDescendants(tree.actor('lForeArm:1'),
                             [0.013467, -0.001708, 0.000797])

    tree.writeTo(file(sys.argv[2], 'w'))
