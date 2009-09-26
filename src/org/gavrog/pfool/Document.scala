package org.gavrog.pfool

import java.io.{Writer, FileWriter}
import scala.io.Source
import scala.collection.mutable.{HashSet, Queue, Stack}

import Document._

object Document {
    def fromString(text: String) = new Document(Source fromString text)
    def fromFile(filename: String) = new Document(Source fromFile filename)
    
    implicit def asMatcher[T](u: Unit) = new Matcher[T](n => true)
    implicit def asMatcher(p: String) = new Matcher[Line](_.matches(p))
    implicit def asMatcher(p: Iterable[String]): Matcher[Line]
        = asMatcher(p.mkString("(", ")|(", ")"))
        
    implicit def asSelector(u: Unit) = new Selector[Line]
    implicit def asSelector(p: String) = new Selector[Line] & (_.matches(p))
    implicit def asSelector(m: Matcher[Line]) = new Selector[Line] & m
    
    implicit def asSelectable(n: Line) = new Object {
        def apply(s: Selector[Line]) = s(n.children)
    }
    implicit def asSelectable(n: Option[Line]) = new Object {
        def apply(s: Selector[Line]) = s(n.toSeq.flatMap(_.children))
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
        val selector = (("actor" | "prop" | "controlProp") \ "name")(_.parent)
        Map(this(selector).map(n => n.args -> new Actor(n)) :_*)
    }
    for (c <- this("figure" \ "addChild")) c.nextSibling match {
        case Some(n) => _actorsByName(n.text).appendChild(_actorsByName(c.args))
        case None => ()
    }

    private val _figureRoots =
        this("figure" \ "root").map(n => _actorsByName(n.args))

    //-- public interface starts here
    
    def this() = this(Source.fromString("{_version_{_number 4.1_}_Figure_{_}_}"
                    split "_" mkString "\n"))

    def root = _root
    
    def apply(s: Selector[Line]) = s(root.children)
    
    def delete(s: Selector[Line]) = this(s).foreach(_.unlink)
    
    def extract(s: Selector[Line]) = {
        val marked = new HashSet[Line]
        val queue = new Queue[Line]
        val roots = new HashSet[Line]

        queue ++= this(s)
        while (!queue.isEmpty) {
            val node = queue.dequeue
            if (!marked(node)) {
                marked += node
                for (child <- node("[{}]")) marked += child
                node.parent match {
                    case Some(p) => queue += p
                    case None => roots += node
                }
            }
        }
        
        val res = new Document
        val anchor = res("Figure")(0)
        for (r <- roots; node <- r.cloneIf(marked)(!"[{}]"))
             anchor.prependSibling(node.clone)
        res
    }
    
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

    def channelNames: Set[String] = channelNames("targetGeom", "valueParm")
    
    def channelNames(types: String*) =
        Set(this("actor" \ "channels" \ types).map(_.args) :_*)
}
