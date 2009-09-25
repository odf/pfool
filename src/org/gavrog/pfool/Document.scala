package org.gavrog.pfool

import java.io.{Writer, FileWriter}
import scala.io.Source
import scala.collection.mutable.Stack

import Document._

object Document {
    def fromString(text: String) = new Document(Source fromString text)
    def fromFile(filename: String) = new Document(Source fromFile filename)
    
    implicit def asSelection(node: Line)    = new Selection(List(node))
    implicit def asSelection(doc: Document) = new Selection(List(doc.root))
    
    implicit def asMatcher(p: String)  = new Matcher[Line](_.matches(p))
	implicit def asMatcher[T](u: Unit) = new Matcher[T](n => true)

    implicit def extract(nodes: Iterable[Line]) = {
	    def root(node: Line): Line = node.parent match {
	        case Some(p) => root(p)
	        case None => node
        }
        val result = new Document
        val anchor = (result \ "Figure")(0)
        for (node <- root(nodes.elements.next).cloneSelected(nodes).children
             if node.key != "{" && node.key != "}"
        )
             anchor.prependSibling(node.clone)
        result
    }
}

class Document(input: Source) {
    private val _root: Line = new Line("")
    
    {
        Line.resetCounter
        var last = _root
        val stack = new Stack[Line]()
        stack.push(last)
        
        for (node <- input.getLines.map(new Line(_, true))) {
            if (node.key == "{") stack.push(last)
            stack.top.appendChild(node)
            last = if (node.key == "}") stack.pop else node
        }
    }
    
    private val _actorsByName = {
        val pattern = "actor" | "prop" | "controlProp"
        val nodes = (this \ pattern \ "name")(_.parent)
        Map(nodes.map(n => n.args -> new Actor(n)) :_*)
    }
    for (c <- this \ "figure" \ "addChild") c.nextSibling match {
        case Some(n) => _actorsByName(n.text).appendChild(_actorsByName(c.args))
        case None => ()
    }

    private val _figureRoots =
        (this \ "figure" \ "root").map(n => _actorsByName(n.args))

    //-- public interface starts here
    
    def this() = this(Source.fromString("{_version_{_number 4.1_}_Figure_{_}_}"
                    split "_" mkString "\n"))

    def root = _root
    
    def actor(name: String) = _actorsByName(name)
    
    def actors = _actorsByName.values
    
    def nodes = root.descendants
    
    def dumpHierarchy {
        def write(actor: Actor, level: Int) {
            println("  " * level + actor.name)
            for (child <- actor.children) write(child, level + 1)
        }
        for (root <- _figureRoots) write(root, 0)
    }
    
    def writeTo(target: Writer) {
        def write(node: Line, level: Int) {
            target.write("\t" * level + node.text + "\n")
            for (child <- node.children) write(child, level + 1)
        }
        for (node <- root.children) write(node, 0)
        target.flush()
    }
    
    def writeTo(filename: String): Unit = new FileWriter(filename) {
        Document.this.writeTo(this)
        close
    }

    def >>(filename: String) = writeTo(filename)
    
    def channelNames: Set[String] = channelNames("targetGeom", "valueParm")
    
    def channelNames(types: String*) =
        Set((this \ "actor" \ "channels" \ types.mkString("|")).map(_.args) :_*)
}
