package org.gavrog.pfool

import java.io.{Writer, FileWriter}
import scala.io.Source
import scala.collection.mutable.{HashMap, MultiMap, Set, Stack}
import scala.util.matching.Regex

class PoserFile(input: Source) {
    def this(filename: String) = this(Source.fromFile(filename))
  
    val _root: Line = null
    val _actorsByName = new HashMap[String, Actor]
    var _figureRoots = List[Actor]()
    parse(input)
    findActors
    makeHierarchy
  
    private def parse(input: Source) {
        Line.resetCounter
        var last = _root
        val stack = new Stack[Line]()
        
        for (node <- input.getLines.map(new Line(_, true))) {
            if (node.key == "{") stack.push(last)
            stack.top.appendChild(node)
            last = if (node.key == "}") stack.pop else node
        }
    }
    
    private def findActors {
        for (node <- _root.select("actor|prop|controlProp", "name"))
            _actorsByName(node.parent.args) = new Actor(node.parent)
    }
    
    private val nonEmpty = new Regex(".+")
    
    private def makeHierarchy {
        for (figure <- root.select("figure")) {
            var last: Actor = null
            for (node <- figure.children) {
                node.key match {
                    case nonEmpty if last != null => {
                        _actorsByName(node.text).appendChild(last)
                        last = null
                    }
                    case "addChild" => {
                        last = _actorsByName(node.args)
                    }
                    case "root" => {
                        _figureRoots ++= List(_actorsByName(node.args))
                    }
                }
            }
        }
    }

    def root = _root
    
    def actor(name: String) = _actorsByName(name)
    
    def actors = _actorsByName.values
    
    def nodes = root.descendants
    
    def dumpHierarchy {
        def write(actor: Actor, level: Int) {
            println("  " * level + actor.name)
            for (child <- actor.children)
                write(child, level + 1)
        }
        for (root <- _figureRoots)
            write(root, 0)
    }
    
    def writeTo(target: Writer) {
        def write(node: Line, level: Int) {
            target.write("\t" * level + node.text + "\n")
            for (child <- node.children)
                write(child, level + 1)
        }
        for (node <- root.children)
            write(node, 0)
    }
    
    def writeTo(filename: String): Unit = writeTo(new FileWriter(filename))
}
