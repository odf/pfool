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
    
    def matches(pattern: String): Boolean
      
    def select(pattern: String*): Stream[T] = {
        if (pattern.isEmpty) Stream.cons(this.asInstanceOf[T], Stream.empty)
        else {
          val candidates =
              if (pattern(0) == "*")
                  subtree
              else if (pattern(0)(0) == '!')
                  children.filter(!_.matches(pattern(0).drop(1)))
              else
                  children.filter(_.matches(pattern(0)))
          for (cand <- candidates; node <- cand.select(pattern.drop(1) :_*))
              yield node.asInstanceOf[T]
        }
    }
    
    def extract(pattern: String*) = cloneSelected(select(pattern :_*))
    
    def delete(pattern: String*) {
        for (node <- select(pattern :_*).toList) node.unlink
    }
}
