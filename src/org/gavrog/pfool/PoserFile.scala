package org.gavrog.pfool

import scala.io.Source
import scala.collection.mutable.{HashMap, MultiMap, Set, Stack}

class PoserFile(input: Source) {
    val _root: Line = null
    var _actorsByName = new HashMap[String, Set[Actor]]
                                    with MultiMap[String, Actor]
    var _figureRoots = List[Node]()
    parse(input)
    findActors
    //makeHierarchy
  
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
            _actorsByName.add(node.parent.args, new Actor(node.parent))
    }
}
