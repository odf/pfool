package org.gavrog.pfool

import scala.util.matching.Regex

object Line {
    var counter = 0
    
    def resetCounter { counter = 0 }
}

class Line(content: String, useCounter: Boolean) extends Node {
    def this(content: String) = this(content, false)
    
    val _nr = if (useCounter) { Line.counter += 1; Line.counter } else 0
    var _key = ""
    var _args = new Array[String](0)

    text = content
    
    override def cloneSelf = new Line(text)
    
    override def inheritsMark = List("{", "}").contains(key)
    
    override def parent = super.parent.asInstanceOf[Line]
    
    override def children = super.children.map(_.asInstanceOf[Line])
    
    override def nextSibling = super.nextSibling.asInstanceOf[Line]
    
    override def subtree = super.subtree.map(_.asInstanceOf[Line])
    
    override def descendants = super.descendants.map(_.asInstanceOf[Line])
    
    override def clone = super.clone.asInstanceOf[Line]
    
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
    
    def nr = _nr
    
    override def toString = {
        val pnr = if (parent != null) parent.nr else 0
        "%06d <%06d> %s %s" format (nr, pnr, key, args)
    }
    
    def select(pattern: String*): Stream[Line] = {
        def matches(node: Line, pattern: String) = {
            val test = if (pattern contains " ") node.text else node.key
            new Regex("(%s)$" format pattern).findPrefixOf(test) != None
        }
      
        if (pattern.isEmpty) Stream.cons(this, Stream.empty)
        else {
          val candidates =
              if (pattern(0) == "*")
                  subtree
              else if (pattern(0)(0) == '!')
                  children.filter(!matches(_, pattern(0).drop(1)))
              else
                  children.filter(matches(_, pattern(0)))
          for { cand <- candidates
                node <- cand.select(pattern.drop(1) :_*) } yield node
        }
    }
    
    def extract(pattern: String*) = cloneSelected(select(pattern :_*))
    
    def delete(pattern: String*) {
        for (node <- select(pattern :_*).toList) node.unlink
    }
}
