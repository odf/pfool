package org.gavrog.pfool

import java.io.{Writer, FileWriter}
import scala.io.Source
import scala.collection.mutable.Stack

class Document(input: Source) {
    def this(filename: String) = this(Source.fromFile(filename))
  
    val _root: Line = new Line("")
    
    parse(input)

    val _actorsByName =
        Map() ++ (for (node <- root.select("actor|prop|controlProp", "name"))
                      yield (node.parent.args -> new Actor(node.parent)))
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
    
    def renameChannels(mapping: PartialFunction[String, String]): Unit =
        renameChannels(mapping, List("targetGeom", "valueParm"))
    
    def renameChannels(mapping: PartialFunction[String, String],
                       types: Iterable[String]) {
        val pattern = types.mkString("|")
        def old2new(s: String) = if (mapping.isDefinedAt(s)) mapping(s) else s
      
        //-- change names in channel definitions
        for (node <- root.select("actor", "channels", pattern))
            node.args = old2new(node.args)
      
        //-- change names in dependent parameter (ERC) instructions
        for (node <- root.select("actor", "channels", ".*", "valueOp.*")) {
            val source = node.nextSibling.nextSibling.nextSibling
            source.text = old2new(source.text)
        }
      
        //-- change names in dial groups
        for { node <- root.select("actor", "channels", "groups")
              desc <- node.descendants if desc.key == "parmNode"
        }
            desc.args = old2new(desc.args)
          
        //-- changes names in parameter linking instructions
        for (node <- root.select("figure", "linkParms")) {
            val p1 = node.nextSibling
            val p2 = p1.nextSibling.nextSibling
            p1.text = old2new(p1.text)
            p2.text = old2new(p2.text)
        }
    }
}
