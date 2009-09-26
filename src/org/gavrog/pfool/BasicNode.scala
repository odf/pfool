package org.gavrog.pfool

import scala.collection.mutable.{HashSet, Queue}

trait BasicNode {
    type T <: BasicNode
    
    def parent: Option[T]
    
    def children: Stream[T]
    
    def subtree: Stream[T] = Stream.cons(this.asInstanceOf[T], descendants)

    def descendants: Stream[T] =
        for (ch <- children; node <- ch.subtree) yield node.asInstanceOf[T]
    
    def appendChild(child: T): Unit
    
    def unlink: Unit
    
    protected def cloneSelf: T

    protected def inheritsMark = false
    
    def cloneIf(f: BasicNode => Boolean): Option[T] = if (f(this)) {
        val root: T = cloneSelf
        for (node <- children; ch <- node.cloneIf(f))
            root.appendChild(ch.asInstanceOf[root.T])
        Some(root)
    } else None
    
    override def clone = cloneIf(_ => true).get
    
    def cloneSelected(elements: Iterable[T]): Option[T] = {
        val marked = new HashSet[BasicNode]
        val queue = new Queue[BasicNode]

        queue ++= elements
        while (!queue.isEmpty) {
            val node = queue.dequeue
            if (!marked(node)) {
                marked += node
                for (child <- node.children if child.inheritsMark)
                    marked += child
                if (node != this) node.parent match {
                    case Some(p) => queue += p
                    case None => ()
                }
            }
        }
        
        cloneIf(marked)
    }
    
    def matches(pattern: String): Boolean
}
