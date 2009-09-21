package org.gavrog.pfool

object Selection {
    implicit def asSelection(node: Line) = Selection(node)
    implicit def asSelection(doc: Document) = Selection(doc.root)
    
    def apply(nodes: Line*) = new Selection(new Selection[Line](null) {
        override def elements = nodes.elements
    })
    
    implicit def toMatcher(p: String) = new Matcher((n: Line) => n.matches(p))
}


class Matcher[T](f: T => Boolean) extends (T => Boolean) {
    def apply(arg: T) = f(arg)
    
    def unary_! = new Matcher[T](!this(_))
    
    def & (that: Matcher[T]) = new Matcher[T](n => this(n) && that(n))
    
    def | (that: Matcher[T]) = new Matcher[T](n => this(n) || that(n))
    
    def ^ (that: Matcher[T]) = new Matcher[T](n => this(n) ^ that(n))
}


class Selection[T <: { def children: Iterable[T] }](
                base: Selection[T],
                axis: T => Iterable[T],
                test: T => Boolean)
    extends Iterable[T]
{
    def this(base: Selection[T], axis: T => Iterable[T]) =
        this(base, axis, _ => true)
    
    def this(base: Selection[T]) = this(base, List(_))
  
    def elements = base.elements.flatMap(axis(_).elements).filter(test)

    def children = new Selection[T](this, _.children.toStream)
    
    def descendants = {
        def desc(n: T): Stream[T] =
            Stream.concat(n.children.map(c => Stream.cons(c, desc(c))))
        new Selection[T](this, desc)
    }
    
    def &(m: Matcher[T]) = new Selection[T](base, axis, n => test(n) && m(n))
    
    def \(m: Matcher[T]) = children & m
    
    def \\(m: Matcher[T]) = descendants & m
}
