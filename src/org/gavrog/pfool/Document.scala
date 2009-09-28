package org.gavrog.pfool

import java.io.{Writer, FileWriter}
import scala.io.Source

import Document._

object Document {
    def fromString(text: String) = new Document(Source fromString text)
    def fromFile(filename: String) = new Document(Source fromFile filename)
    
    implicit def asSelector(u: Unit) = new Selector[Line]
    implicit def asSelector(p: String) = new Selector[Line] & (_.matches(p))
    implicit def asSelector(p: Iterable[String]): Selector[Line]
        = asSelector(p.mkString("(", ")|(", ")"))
    
    implicit def asSelectable(n: Line) = new Object {
        def apply(s: Selector[Line]) = s(n.children)
    }
    implicit def asSelectable(n: Option[Line]) = new Object {
        def apply(s: Selector[Line]) = s(n.toSeq.flatMap(_.children))
    }
}


class Document(input: Source) {
	val root = new Line("")

	init

	protected def init {
		(List(root) /: input.getLines)((os, line) => {
		  	val node = new Line(line, true)
		  	val ns = if (node.key == "{" || os.tail == Nil) os else os.tail
		  	ns.head.appendChild(node)
		  	if (node.key == "}") ns else node :: ns
		})
	}
    
    private def _actorsByName = {
        val selector = ("actor" | "prop" | "controlProp" \ "name")(_.parent)
        val actors = Map(this(selector).map(n => n.args -> new Actor(n)) :_*)
        
        for (c <- this("figure" \ "addChild")) c.nextSibling match {
            case Some(n) => actors(n.text).appendChild(actors(c.args))
            case None => ()
        }
        actors
    }

    private def _figureRoots =
        this("figure" \ "root").map(_.args).map(_actorsByName)

    private def this() = this(Source fromString "")
    
    //-- public interface starts here
    
    override def clone = {
        val original = this
        new Document() {
            override val root = original.root.clone
            override def init {}
        }
    }
    
    def cloneIf(f: Line => Boolean) = {
        val original = this
        new Document {
            override val root = original.root.cloneIf(f) match {
                case Some(n) => n
                case None => new Line("")
            }
            override def init {}
        }
    }
    
    def apply(s: Selector[Line]) = s(root.children)
    
    def delete(s: Selector[Line]) = this(s).foreach(_.unlink)
    
    def extract(s: Selector[Line]) = {
		def closure(marked: Set[Line], queue: Seq[Line]): Set[Line] =
			if (queue.isEmpty) marked else {
				val n = queue(0)
				val q = queue.drop(1) ++ (if (marked(n)) Nil else n.parent)
				closure(marked + n ++ n("[{}]"), q)
			}
      
		cloneIf(closure(Set(), this(s) ++ this("version|figure")))
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
