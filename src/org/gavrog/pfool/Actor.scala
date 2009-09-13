package org.gavrog.pfool

class Actor(line: Line) extends Node {
    val _content = line
    
    def content = _content
    
    def name = _content.args
    
    override def toString = content.toString
    
    def extractChannelValues(pattern: String) =
        content.extract("channels", pattern, "keys", "k")
}
