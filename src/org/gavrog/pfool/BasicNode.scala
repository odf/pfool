package org.gavrog.pfool

import scala.collection.mutable.{HashSet, Queue}

trait BasicNode {
    type T <: BasicNode
    
    def parent: T
    
    def children: Stream[T]
    
    def subtree: Stream[T] = Stream.cons(this.asInstanceOf[T], descendants)

    def descendants: Stream[T] =
        for (ch <- children; node <- ch.subtree) yield node.asInstanceOf[T]
    
    def appendChild(child: T): Unit
    
    def unlink: Unit
    
    protected def cloneSelf: T

    protected def inheritsMark = false
    
    override def clone: T = {
        val root = cloneSelf
        for (node <- children) root.appendChild(node.clone.asInstanceOf[root.T])
        root.asInstanceOf[T]
    }
    
    def cloneSelected(elements: Iterable[T]): T = {
        val marked = new HashSet[BasicNode]
        val queue = new Queue[BasicNode]

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
        
        def clone(node: BasicNode): BasicNode = {
            val root = node.cloneSelf
            for (child <- node.children if marked(child))
                root.appendChild(clone(child).asInstanceOf[root.T])
            root
        }
        
        clone(this).asInstanceOf[T]
    }
}
