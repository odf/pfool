package org.gavrog.pfool

import scala.collection.mutable.ListBuffer

class Actor(line: Line) extends BasicNode {
    type T = Actor
  
    val _content = line
    
    def content = _content
    
    def name = _content.args
    
    private var _parent: Option[T] = None
    protected val _children = new ListBuffer[T]
    
    def parent = _parent
    
    def children = Stream() ++ _children
    
    def appendChild(child: T) {
        _children.append(child)
        child._parent = Some(this)
    }
    
    def unlink = parent match {
        case Some(n) => {
            val c = n._children
            c.remove(c.indexOf(this))
        }
        case None => ()
    }
    
    def cloneSelf = new Actor(line)
    
    def matches(pattern: String) =
        "(%s)$".format(pattern).r.findPrefixOf(name) != None
}
