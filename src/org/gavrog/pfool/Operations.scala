package org.gavrog.pfool

import scala.util.Sorting._

import Document._

object Operations {
    def renameChannels(doc: Document,
                       mapping: PartialFunction[String, String]): Unit = {
        renameChannels(doc, mapping, List("targetGeom", "valueParm"))
    }
    
    def renameChannels(doc: Document, mapping: PartialFunction[String, String],
                       types: Iterable[String]) {
        implicit def grabLine(x: Option[Line]) = new Object {
            def nextSibling = x match {
                case Some(line) => line.nextSibling
                case None => None
            }
            def text = x match {
                case Some(line) => line.text
                case None => ""
            }
            def text_=(s: String) = x match {
                case Some(line) => line.text = s
                case None => ()
            }
        }
  
        def old2new(s: String) = if (mapping.isDefinedAt(s)) mapping(s) else s
      
        //-- change names in channel definitions
        for (node <- doc("actor" \ "channels" \ types))
            node.args = old2new(node.args)
      
        //-- change names in dependent parameter (ERC) instructions
        for (node <- doc("actor" \ "channels" \ () \ "valueOp.*")) {
            val source = node.nextSibling.nextSibling.nextSibling
            source.text = old2new(source.text)
        }
      
        //-- change names in dial groups
        for (node <- doc("actor" \ "channels" \ "groups" \\ "parmNode"))
            node.args = old2new(node.args)
          
        //-- changes names in parameter linking instructions
        for (node <- doc("figure" \ "linkParms")) {
            val p1 = node.nextSibling
            val p2 = p1.nextSibling.nextSibling
            p1.text = old2new(p1.text)
            p2.text = old2new(p2.text)
        }
    }

    def addChannels(actorNode: Line, names: Iterable[String], template: Line) {
        val anchor = actorNode("channels" \ !("[{]" | "groups"))(0)
        for (name <- names) {
            val node = template.clone
            node.args = name
            for (n <- node("name")) n.args = name
            anchor.prependSibling(node)
        }
    }
    
    def addChannels(doc: Document,
                    names: Iterable[String], template: Line): Unit =
        for (node <- doc("actor" & !"BODY(:\\d*)?".matchesArgs \@ "channels"))
            addChannels(node, names, template)
    
    def sortChannels(actorNode: Line) {
        for (base <- actorNode("channels")) {
            val nodes = base("targetGeom" | "valueParm")
            nodes.foreach(_.unlink)
            val anchor = base(!("[{]" | "groups"))(0)
            for (n <- stableSort(nodes, (a: Line, b: Line) => a.args < b.args))
            	anchor.prependSibling(n)
        }
        for (base <- actorNode("channels" \\ "groupNode")) {
            val nodes = base("parmNode")
            nodes.foreach(_.unlink)
            val anchor = base(!("[{]" | "collapsed"))(0)
            for (n <- stableSort(nodes, (a: Line, b: Line) => a.args < b.args))
            	anchor.prependSibling(n)
        }
    }
    
    def sortChannels(doc: Document): Unit =
        doc("actor" \@ "channels").foreach(sortChannels)
    
    private def format(x: Double) = "%.6f" format x
    
    def shiftPoint(node: Line, s: Double*) =
        for (i <- 0 to 2) node.arg(i) = format(node.arg(i).toDouble + s(i))
    
    def shiftChannel(node: Line, s: Double*) {
        for (child <- node("center")) shiftPoint(child, s :_*)

        //-- shift centers of inclusion/exclusion spheres
        for (child <- node("(-?\\d+(\\.\\d+)? ){3}1(\\.\\d+)?")) {
            child.key = format(child.key.toDouble + s(0))
            for (i <- 0 to 1) {
                child.arg(i) = format(child.arg(i).toDouble + s(i))
            }
        }
    }
    
    def shiftActor(actor: Actor, s: Double*) = actor.parent match {
        case Some(parent) => {
            for (node <- actor.content("channels" \ ())) {
                if (node("otherActor " + parent.name) isDefinedAt 0)
                    shiftChannel(node, s :_*)
            }
            for (node <- parent.content("channels" \ ()))
                if (node("otherActor " + actor.name) isDefinedAt 0)
                    shiftChannel(node, s :_*)
            for (node <- actor.content("origin" | "endPoint"))
                shiftPoint(node, s :_*)
        }
        case None => ()
    }
    
    def shiftActorAndDescendants(actor: Actor, s: Double*) =
        for (bone <- actor.subtree) shiftActor(bone, s :_*)
    
    def stripActor(doc: Document, actor: Actor) {
        val names = actor.subtree.map(_.name)
        for (name <- names) {
            doc.delete("actor " + name)
            doc.delete("actor" \ "channels" \ () \@ ("otherActor " + name))
            for (node <- doc("figure" \ ("(addChild|weld) " + name))) {
                node.nextSibling.foreach(_.unlink)
                node.unlink
            }
        }
    }
    
    def stripChildren(doc: Document, actor: Actor) =
        actor.children.foreach(stripActor(doc, _))
}
