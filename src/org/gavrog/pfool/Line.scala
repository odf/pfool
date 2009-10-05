package org.gavrog.pfool

import scala.collection.mutable.ListBuffer

import scala.util.matching.Regex

object Line {
    var counter = 0
    
    def resetCounter { counter = 0 }
}

class Line(content: String, useCounter: Boolean) extends BasicNode {
    type T = Line
  
    def this(content: String) = this(content, false)
    
    private var _parent: Option[T] = None
    protected val _children = new ListBuffer[T]
    
    def parent: Option[T] = _parent
    
    def children = _children.toStream
    
    def nextSibling = parent match {
        case None => None
        case Some(n) => {
            val c = n._children
            val i = c.indexOf(this)
            if (i < c.size) Some(c(i+1)) else None
        }
    }
    
    def insertChild(position: Int, child: T) {
        assert(child.parent == None)
        _children.insert(position, child)
        child._parent = Some(this)
    }
    
    def appendChild(child: T) = insertChild(_children.size, child)
    
    def prependChild(child: T) = insertChild(0, child)

    def appendSibling(child: T) = parent match {
        case Some(n) => n.insertChild(n._children.indexOf(this) + 1, child)
        case None => ()
    }

    def prependSibling(child: T) = parent match {
        case Some(n) => n.insertChild(n._children.indexOf(this), child)
        case None => ()
    }

    def unlink = parent match {
        case Some(n) => {
            val c = n._children
            c.remove(c.indexOf(this))
            _parent = None
        }
        case None => ()
    }
    
    val _nr = if (useCounter) { Line.counter += 1; Line.counter } else 0
    var _key = ""
    var _args = new Array[String](0)

    text = content
    
    protected def cloneSelf = new Line(text)
    
    def text = (key :: _args.toList).mkString(" ")
    
    def text_=(s: String) {
        val fields = s.trim.split("\\s+")
        if (fields.size > 0) {
            key = fields(0)
            _args = fields.drop(1)
        } else {
            key = ""
            _args = new Array(0)
        }
    }
    
    def key = _key
    
    def key_=(s: String) {
        if (s == null || s.trim.length == 0) _key = ""
        else _key = s.trim.split("\\s+")(0)
    }
    
    def args = _args.mkString(" ")
    
    def args_=(s: String) { _args = s.trim.split("\\s+") }
    
    def arg = new Object {
        def apply(i: Int) = _args(i)
        def update(i: Int, s: String) { _args(i) = s }
    }
    
    def nr = _nr
    
    override def toString = {
        val pnr = parent match { case Some(n) => n.nr; case None => 0 }
        "%06d <%06d> %s %s" format (nr, pnr, key, args)
    }
}
