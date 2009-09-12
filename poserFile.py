import re

def prop(func):
    return property(**func())


class Node(object):
    def __init__(self):
        self._parent = None
        self._children = []

    def insertChild(self, position, child):
        assert child._parent is None
        self._children.insert(position, child)
        child._parent = self

    def appendChild(self, child):
        self.insertChild(len(self._children), child)

    def prependChild(self, child):
        self.insertChild(0, child)

    def appendSibling(self, child):
        parent = self._parent
        parent.insertChild(parent._children.index(self) + 1, child)

    def prependSibling(self, child):
        parent = self._parent
        parent.insertChild(parent._children.index(self), child)

    def unlink(self):
        if self._parent is not None:
            self._parent._children.remove(self)
        self._parent = None

    def cloneSelf(self):
        return Node()

    def clone(self):
        root = self.cloneSelf()
        for node in self.children:
            root.appendChild(node.clone())
        return root

    def cloneSelected(self, elements):
        marked = {}

        def clone(node):
            root = node.cloneSelf()
            for child in node.children:
                if marked.get(child):
                    root.appendChild(clone(child))
            return root

        queue = list(elements)
        while queue:
            node = queue.pop(0)
            if not marked.get(node):
                marked[node] = True
                for child in node.children:
                    if child.firstField in ['{', '}']:
                        marked[child] = True
                if node != self:
                    queue.append(node.parent)

        return clone(self)

    @property
    def parent(self):
        return self._parent

    @property
    def hasChildren(self):
        return len(self._children) > 0

    @property
    def firstChild(self):
        return self._children[0]

    @property
    def children(self):
        for node in self._children:
            yield node

    @property
    def nextSibling(self):
        c = self.parent._children
        i = c.index(self)
        if i < len(c):
            return c[i+1]
        else:
            return None

    @property
    def subtree(self):
        yield self
        for child in self.children:
            for node in child.subtree:
                yield node

    @property
    def descendants(self):
        for child in self.children:
            for node in child.subtree:
                yield node


class Line(Node):
    __counter = [ 0 ]

    def __init__(self, content, useCounter = False):
        Node.__init__(self)
        if useCounter:
            self._nr = self.__counter[0] = self.__counter[0] + 1
        else:
            self._nr = 0
        self.text = content

    def cloneSelf(self):
        return Line(" ".join(self.fields))

    @classmethod
    def startCounter(cls):
        cls.__counter[0] = 0

    @property
    def fields(self):
        return self._fields[:]

    @prop
    def text():
        def fget(self):
            return " ".join(self._fields)

        def fset(self, s):
            self._fields = (s or "").split()

        return locals()

    @prop
    def firstField():
        def fget(self):
            return (self._fields or [None])[0]

        def fset(self, value):
            if value:
                self._fields[0] = value.split()[0]
            else:
                self._fields[0] = None

        return locals()

    @prop
    def rest():
        def fget(self):
            return " ".join(self._fields[1:])

        def fset(self, text):
            self._fields = self._fields[:1] + (text or "").split()

        return locals()

    def select(self, *args):
        if args:
            for node in self.children:
                if re.match('(%s)$' % args[0], node.firstField or ''):
                    for descendant in node.select(*args[1:]):
                        yield descendant
        else:
            yield self

    def extract(self, *args):
        return self.cloneSelected(self.select(*args))

    def delete(self, *args):
        for node in list(self.select(*args)):
            node.unlink()

    def get(self, key):
        matched = list(self.select(key))
        if len(matched) == 1:
            node = matched[0]
            if len(node.fields) == 2 and not node.hasChildren:
                return node.fields[1]

    @property
    def nr(self):
        return self._nr

    def __str__(self):
        if self.parent:
            parent = self.parent.nr
        else:
            parent = 0
        return ("%06d <%06d> %s %s" %
                (self.nr, parent, self.firstField, self.rest))


class Actor(Node):
    def __init__(self, line):
        Node.__init__(self)
        self._content = line

    @property
    def content(self):
        return self._content

    @property
    def name(self):
        return self.content.rest

    def __str__(self):
        return str(self.content)

    def extractChannels(self, pattern):
        return self.content.extract('channels', pattern, 'keys', 'k')


class PoserFile(object):
    def __init__(self, input):
        self._actorsByName = {}
        self.figureRoots = []
        self.parse(input)
        self._actorsByName = dict(
            (node.parent.rest, Actor(node.parent))
            for node in self.root.select('actor|prop|controlProp', 'name'))
        self.makeHierarchy()

    def makeHierarchy(self):
        for figure in self.root.select('figure'):
            lastChild = None
            for node in figure.children:
                if lastChild and node.firstField:
                    actor = self._actorsByName[" ".join(node.fields)]
                    actor.appendChild(lastChild)
                    lastChild = None
                elif node.firstField == 'addChild':
                    lastChild = self._actorsByName[node.rest]
                elif node.firstField == 'root':
                    self.figureRoots.append(self._actorsByName[node.rest])

    def actor(self, name):
        return self._actorsByName.get(name)

    @property
    def actors(self):
        return self._actorsByName.values()

    def parse(self, input):
        Line.startCounter()
        last = self._root = Line(None)
        stack = [ self._root ]

        for line in input:
            node = Line(line, True)
            if node.firstField == '{':
                stack.append(last)
            stack[-1].appendChild(node)
            if node.firstField == '}':
                last = stack.pop()
            else:
                last = node

    @property
    def root(self):
        return self._root

    @property
    def nodes(self):
        for node in self.root.descendants:
            yield node

    def dump(self):
        for node in self.root.subtree:
            print node

    def dumpHierarchy(self):
        def write(actor, level):
            print("  " * level + actor.name)
            for child in actor.children:
                write(child, level + 1)
        for root in self.figureRoots:
            write(root, 0)

    def writeTo(self, output):
        def write(node, level):
            output.write("\t" * level + " ".join(node.fields) + "\n")
            for child in node.children:
                write(child, level + 1)
        for node in self.root.children:
            write(node, 0)
