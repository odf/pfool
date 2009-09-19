package org.gavrog.pfool

object Operations {
    def renameChannels(doc: Document,
                       mapping: PartialFunction[String, String]): Unit = {
        renameChannels(doc, mapping, List("targetGeom", "valueParm"))
    }
    
    def renameChannels(doc: Document, mapping: PartialFunction[String, String],
                       types: Iterable[String]) {
        val pattern = types.mkString("|")
        def old2new(s: String) = if (mapping.isDefinedAt(s)) mapping(s) else s
      
        //-- change names in channel definitions
        for (node <- doc.root.select("actor", "channels", pattern))
            node.args = old2new(node.args)
      
        //-- change names in dependent parameter (ERC) instructions
        for (node <- doc.root.select("actor", "channels", ".*", "valueOp.*")) {
            val source = node.nextSibling.nextSibling.nextSibling
            source.text = old2new(source.text)
        }
      
        //-- change names in dial groups
        for (node <- doc.root.select("actor", "channels", "groups",
                                     "*", "parmNode"))
            node.args = old2new(node.args)
          
        //-- changes names in parameter linking instructions
        for (node <- doc.root.select("figure", "linkParms")) {
            val p1 = node.nextSibling
            val p2 = p1.nextSibling.nextSibling
            p1.text = old2new(p1.text)
            p2.text = old2new(p2.text)
        }
    }

    def addChannels(actorNode: Line, names: Iterable[String], template: Line) {
        val anchor = actorNode.select("channels", "![{]|groups")(0)
        for (name <- names) {
            val node = template.clone
            node.args = name
            for (n <- node.select("name")) n.args = name
            anchor.prependSibling(node)
        }
    }
    
    def addChannels(doc: Document,
                    names: Iterable[String], template: Line): Unit = {
        for (a <- doc.actors if !a.name.startsWith("BODY:"))
            addChannels(a.content, names, template)
    }
    
    private def format(x: Double) = "%.6f" format x
    
    def shiftPoint(node: Line, s: Double*) =
        for (i <- 0 to 2) node.arg(i) = format(node.arg(i).toDouble + s(i))
    
    def shiftChannel(node: Line, s: Double*) {
        for (child <- node.select("center")) shiftPoint(child, s :_*)

        //-- shift centers of inclusion/exclusion spheres
        for (child <- node.select("(-?\\d+(\\.\\d+)? ){3}1(\\.\\d+)?")) {
            child.key = format(child.key.toDouble + s(0))
            for (i <- 0 to 1) {
                child.arg(i) = format(child.arg(i).toDouble + s(i))
            }
        }
    }
    
    def shiftActor(actor: Actor, s: Double*) {
        for (node <- actor.content.select("channels", ".*"))
            if (!node.select("otherActor " + actor.parent.name).isEmpty)
                shiftChannel(node, s :_*)
        for (node <- actor.parent.content.select("channels", ".*"))
            if (!node.select("otherActor " + actor.name).isEmpty)
                shiftChannel(node, s :_*)
        for (node <- actor.content.select("origin|endPoint"))
            shiftPoint(node, s :_*)
    }
    
    def shiftActorAndDescendants(actor: Actor, s: Double*) =
        for (bone <- actor.subtree) shiftActor(bone, s :_*)
}
