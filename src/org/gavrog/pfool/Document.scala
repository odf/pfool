package org.gavrog.pfool

import java.io.{Writer, FileWriter}
import scala.io.Source
import scala.collection.mutable.Stack

class Document(input: Source) {
    def this(filename: String) = this(Source.fromFile(filename))
  
    val _root: Line = new Line("")
    
    parse(input)

    val _actorsByName = Map(actorNodes.map(n => n.args -> new Actor(n)) :_*)
    val _figureRoots =
        root.select("figure", "root").map(_.args).map(_actorsByName)
    for (child <- root.select("figure", "addChild"))
        _actorsByName(child.nextSibling.text)
            .appendChild(_actorsByName(child.args))

    private def parse(input: Source) {
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
    
    def root = _root
    
    def actor(name: String) = _actorsByName(name)
    
    def actors = _actorsByName.values
    
    def nodes = root.descendants
    
    def actorNodes =
        root.select("actor|prop|controlProp").filter(!_.select("name").isEmpty)
    
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
    
    def writeTo(filename: String) {
        val target = new FileWriter(filename)
        writeTo(new FileWriter(filename))
        target.flush
        target.close
    }

    def channelNames: Set[String] = channelNames("targetGeom", "valueParm")
    
    def channelNames(types: String*) = {
        val pattern = types.mkString("|")
        Set(root.select("actor", "channels", pattern).map(_.args) :_*)
    }
}
