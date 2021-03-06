package org.gavrog.pfool

import java.io.{PrintStream, Writer, FileWriter, OutputStreamWriter}

import scala.io.Source
import scala.util.matching.Regex

import Document._

object Document {
    def fromString(text: String) = new Document(Source fromString text)
    def fromFile(filename: String) = new Document(Source fromFile filename)
    
    implicit def filterFromUnit(u: Unit) = Filter[Line](n => true)
    implicit def filterFromString(p: String) =
    	new Filter[Line](n => "(%s)$".format(p).r
    			.findPrefixOf(if (p.contains(" ")) n.text else n.key) != None)
    implicit def filtersFromString(p: String) = new Object {
		private val regex = "(%s)$".format(p).r
    	def matchesKey  = Filter[Line](n => regex.findPrefixOf(n.key)  != None)
    	def matchesArgs = Filter[Line](n => regex.findPrefixOf(n.args) != None)
    	def matchesText = Filter[Line](n => regex.findPrefixOf(n.text) != None)
    }
    implicit def filterFromStrings(p: Iterable[String])
        = filterFromString(p.mkString("(", ")|(", ")"))
    implicit def filtersFromStrings(p: Iterable[String])
        = filtersFromString(p.mkString("(", ")|(", ")"))
    
    implicit def asSelectable(n: Line) = new Object {
        def apply(s: Selector[Line]) = s(n.children)
    }
    implicit def asSelectable(n: Option[Line]) = new Object {
        def apply(s: Selector[Line]) = s(n.toSeq.flatMap(_.children))
    }
}


class Document(theRoot: Line, init: Document => Unit) {
	val root = theRoot
	init(this)

	def this(root: Line) = this(root, doc => {})
 
	def this(input: Source) = this(new Line(""), doc => {
		Line.resetCounter
		for (v <- Line.read(input, true)) doc.root.appendChild(v)
	})
    
	def this(text: String) = this(Source fromString text)

	def this() = this("")
 
	private def _actorsByName = {
		val selector = ("actor" | "prop" | "controlProp") \@ "name"
		val actors = Map(this(selector).map(n => n.args -> new Actor(n)) :_*)
        
		for (c <- this("figure" \ "addChild")) c.nextSibling match {
			case Some(n) => actors(n.text).appendChild(actors(c.args))
			case None => ()
		}
		actors
	}

	private def _figureRoots =
		this("figure" \ "root").map(_.args).map(_actorsByName)

	// -- public interface starts here
    
	override def clone = new Document(root.clone)
    
	def cloneIf(f: Line => Boolean) = root.cloneIf(f) match {
	  case Some(n) => new Document(n)
	  case None    => new Document()
	}
    
	def apply(s: Selector[Line]) = s(root.children)
    
	def delete(s: Selector[Line]) {
		val nodes = this(s).toList
		nodes.foreach(_.unlink)
	}
    
	def extract(s: Selector[Line]) = {
		def closure(marked: Set[Line], queue: Seq[Line]): Set[Line] =
			if (queue.isEmpty) marked else {
				val n = queue(0)
				val q = queue.drop(1) ++ (if (marked(n)) Nil else n.parent)
				closure(marked + n ++ n("[{}]"), q)
			}
      
		cloneIf(closure(Set(), this(s) ++ this("version|figure")))
	}
	
	private def adopt(node: Line, n: Int) {
		if (node.children.isEmpty)
			for (i <- 1 to n) {
				val v = node.nextSibling.get
				v.unlink
				node.appendChild(v)
			}
  }
 
	def fixCommands {
		this("actor" \ "channels" \\ "valueOpDeltaAdd" ).foreach(adopt(_, 4))
    this("actor" \ "channels" \\ "valueOpPlus" ).foreach(adopt(_, 3))
    this("actor" \ "channels" \\ "valueOpMinus" ).foreach(adopt(_, 3))
    this("actor" \ "channels" \\ "valueOpTimes" ).foreach(adopt(_, 3))
    this("actor" \ "channels" \\ "valueOpDivideBy" ).foreach(adopt(_, 3))
    this("actor" \ "channels" \\ "valueOpDivideInto" ).foreach(adopt(_, 3))
		this("figure" \ "addChild").foreach(adopt(_, 1))
    this("figure" \ "weld").foreach(adopt(_, 1))
  }
	
	def actor(name: String) = _actorsByName.get(name)
    
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
		fixCommands
		for (node <- root.children) write(node, 0)
		target.flush()
	}
    
	def writeTo(filename: String): Unit = new FileWriter(filename) {
		Document.this.writeTo(this)
		close
	}

	def writeTo(target: PrintStream): Unit =
		writeTo(new OutputStreamWriter(target))
    
	def channelNames: Set[String] = channelNames("targetGeom", "valueParm")
    
	def channelNames(types: String*) =
		Set(this("actor" \ "channels" \ types).map(_.args) :_*)
}
