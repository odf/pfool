package org.gavrog.pfool

import scala.collection.mutable.ListBuffer

class Actor(line: Line) extends BasicNode {
    type T = Actor
  
    val _content = line
    
    def content = _content
    
    def name = _content.args
    
    private var _parent: T = null
    protected val _children = new ListBuffer[T]
    
    def parent = _parent
    
    def children = _children.toStream
    
    def appendChild(child: T) {
        _children.append(child)
        child._parent = this
    }
    
    def unlink {
        if (parent != null) {
            val c = parent._children
            c.remove(c.indexOf(this))
        }
        _parent = null
    }
    
    def cloneSelf = new Actor(line)
}
