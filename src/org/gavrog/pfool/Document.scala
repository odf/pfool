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


class Document() {
    private def _actorsByName = {
        val selector = (("actor" | "prop" | "controlProp") \ "name")(_.parent)
        val actors = Map(this(selector).map(n => n.args -> new Actor(n)) :_*)
        
        for (c <- this("figure" \ "addChild")) c.nextSibling match {
            case Some(n) => actors(n.text).appendChild(actors(c.args))
            case None => ()
        }
        actors
    }

    private def _figureRoots =
        this("figure" \ "root").map(_.args).map(_actorsByName)

    //-- public interface starts here
    
    val root = new Line("")
    
    def this(input: Source) = this() {
        Line.resetCounter
        var last = root
        val stack = new Stack[Line]()
        stack.push(last)
        
        for (node <- input.getLines.map(new Line(_, true))) {
            if (node.key == "{") stack.push(last)
            stack.top.appendChild(node)
            last = if (node.key == "}") stack.pop else node
        }
    }
    
    override def clone = {
        val original = this
        new Document {
            override val root = original.root.clone
        }
    }
    
    def cloneIf(f: Line => Boolean) = {
        val original = this
        new Document {
            override val root = original.root.cloneIf(f) match {
                case Some(n) => n
                case None => new Line("")
            }
        }
    }
    
    def apply(s: Selector[Line]) = s(root.children)
    
    def delete(s: Selector[Line]) = this(s).foreach(_.unlink)
    
    def extract(s: Selector[Line]) = {
        val marked = new HashSet[Line]
        val queue = new Queue[Line]

        queue ++= this(s)
        queue ++= this("version|figure")
        while (!queue.isEmpty) {
            val node = queue.dequeue
            if (!marked(node)) {
                marked += node
                marked ++= node("[{}]")
                node.parent match {
                    case Some(p) => queue += p
                    case None => ()
                }
            }
        }
        
        cloneIf(marked)
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
