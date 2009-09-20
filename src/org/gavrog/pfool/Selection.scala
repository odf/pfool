package org.gavrog.pfool

object Selection {
    implicit def asSelection(node: Line) = Selection(node)
    implicit def asSelection(doc: Document) = Selection(doc.root)
    
    def apply(nodes: Line*) = new Selection(new Selection(null) {
        override def elements = nodes.elements
    })
}

class Selection(base: Selection,
                axis: Line => Iterable[Line],
                filter: Line => Boolean)
    extends Iterable[Line]
{
    def this(base: Selection, axis: Line => Iterable[Line]) =
        this(base, axis, n => true)
  
    def this(base: Selection) = this(base, List(_))
  
    def elements =
        for (n <- base.elements; c <- axis(n).elements if filter(c)) yield c
  
    def \(p: String) = new Selection(this, _.children, _.matches(p))
    
    def \\(p: String) = new Selection(this, _.descendants, _.matches(p))
    
    def \!(p: String) = new Selection(this, _.children, !_.matches(p))
    
    def \\!(p: String) = new Selection(this, _.descendants, !_.matches(p))

    def &(p: String) = new Selection(base, axis, n => filter(n) && n.matches(p))

    def |(p: String) = new Selection(base, axis, n => filter(n) || n.matches(p))

    def &!(p: String) =
        new Selection(base, axis, n => filter(n) && !n.matches(p))

    def |!(p: String) =
        new Selection(base, axis, n => filter(n) || !n.matches(p))
    
    def ! = new Selection(base, axis, n => !filter(n))
}
