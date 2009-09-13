package org.gavrog.pfool

object Line {
    var counter = 0
    
    def resetCounter { counter = 0 }
}

class Line(content: String, useCounter: Boolean) extends Node {
    def this(content: String) = this(content, false)
    
    val _nr = if (useCounter) { Line.counter += 1; Line.counter } else 0
    var key = ""
    var _args = new Array[String](0)

    text = content
    
    override def cloneSelf = new Line(text)
    
    override def parent = super.parent.asInstanceOf[Line]
    
    override def children = super.children.map(_.asInstanceOf[Line])
    
    def text = key + " " + args
    
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
    
    def args = _args.mkString(" ")
    
    def args_=(s: String) { _args = s.trim.split("\\s+") }
    
    def nr = _nr
    
    override def toString = {
        val pnr = if (parent != null) parent.nr else 0
        "%06d <%06d> %s %s" format (nr, pnr, key, args)
    }
    
    def select(pattern: String*): Stream[Line] = {
        def matches(node: Line, pattern: String) = node.key == pattern
      
        if (pattern.size == 0) Stream.cons(this, Stream.empty)
        else for { child <- children if matches(child, pattern(0))
                   node <- child.select(pattern.drop(1) :_*) }
            yield node
    }
    
    def extract(pattern: String*) = cloneSelected(select(pattern :_*))
    
    def delete(pattern: String*) {
        for (node <- select(pattern :_*).toList) node.unlink
    }
}
