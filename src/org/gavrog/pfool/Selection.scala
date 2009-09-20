package org.gavrog.pfool

object Selection {
    implicit def asSelection(node: Line) = new Selection(node)
    implicit def asSelection(doc: Document) = new Selection(doc.root)
}

class Selection(nodes: Line*) extends Iterable[Line] {
    def elements: Iterator[Line] = nodes.elements
    
    private def extend(f: Line => Iterator[Line]) = {
        val nodes = this.elements
        new Selection {
            override def elements = for (n <- nodes; c <- f(n)) yield c
        }
    }
    
    def \(p: String) = extend(_.children.elements.filter(_.matches(p)))
    
    def \\(p: String) = extend(_.descendants.elements.filter(_.matches(p)))
    
    def \!(p: String) = extend(_.children.elements.filter(!_.matches(p)))
    
    def \\!(p: String) = extend(_.descendants.elements.filter(!_.matches(p)))
}
