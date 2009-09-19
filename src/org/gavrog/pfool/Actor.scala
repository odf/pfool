package org.gavrog.pfool

class Actor(line: Line) extends Node {
    val _content = line
    
    def content = _content
    
    def name = _content.args
    
    override def parent = super.parent.asInstanceOf[Actor]
    
    override def children = super.children.map(_.asInstanceOf[Actor])
    
    override def nextSibling = super.nextSibling.asInstanceOf[Actor]
    
    override def subtree = super.subtree.map(_.asInstanceOf[Actor])
    
    override def descendants = super.descendants.map(_.asInstanceOf[Actor])
    
    override def toString = content.toString
}
