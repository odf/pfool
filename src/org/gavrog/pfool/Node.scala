package org.gavrog.pfool

import scala.collection.mutable.{HashSet, ListBuffer, Queue}

class Node {
    private var _parent: Node = null
    protected val _children = new ListBuffer[Node]()
    
    def parent = _parent
    
    def children = _children.elements
    
    def firstChild = if (_children.size > 0) _children(0) else null
    
    def nextSibling = {
        val c = parent._children
        val i = c.indexOf(this)
        if (i < c.size) c(i+1) else null
    }
    
    def subtree: Stream[Node] = Stream.cons(this, descendants)

    def descendants: Stream[Node] =
        for (child <- _children.toStream; node <- child.subtree) yield node
    
    def insertChild(position: Int, child: Node) {
        assert(child.parent == null)
        _children.insert(position, child)
    }
    
    def appendChild(child: Node) = _children.append(child)
    
    def prependChild(child: Node) = _children.prepend(child)

    def appendSibling(child: Node) {
        parent.insertChild(parent._children.indexOf(this) + 1, child)
    }

    def prependSibling(child: Node) {
        parent.insertChild(parent._children.indexOf(this), child)
    }
    
    def unlink {
        if (parent != null) {
            val c = parent._children
            c.remove(c.indexOf(this))
        }
        _parent = null
    }
    
    protected def cloneSelf = new Node()

    protected def inheritsMark = false
    
    override def clone: Node = {
        val root = cloneSelf
        for (node <- children) root.appendChild(node.clone)
        root
    }
    
    def cloneSelected(elements: Seq[Node]) = {
        val marked = new HashSet[Node]()
        val queue = new Queue[Node]()

        queue ++= elements
        while (!queue.isEmpty) {
            val node = queue.dequeue
            if (!marked(node)) {
                marked += node
                for (child <- node.children if child.inheritsMark)
                    marked += child
                if (node != this) queue += node.parent
            }
        }
        
        def clone(node: Node): Node = {
            val root = node.cloneSelf
            for (child <- node.children if marked(child))
                root.appendChild(clone(child))
            root
        }
        
        clone(this)
    }
}
