package org.gavrog.pfool

class Matcher[T](f: T => Boolean) extends (T => Boolean) {
    def apply(arg: T) = f(arg)
    
    def unary_! = new Matcher[T](!this(_))
    
    def & (that: T => Boolean) = new Matcher[T](n => this(n) && that(n))
    def | (that: T => Boolean) = new Matcher[T](n => this(n) || that(n))
    def ^ (that: T => Boolean) = new Matcher[T](n => this(n) ^ that(n))
}


class Selection[T <: { def children: Iterable[T] }](nodes: Iterable[T])
    extends Seq[T]
{
	protected def contents = nodes.toStream
  
	def length = contents.length
    
	def elements = contents.elements
 
	def apply(n: Int) = contents(n)
 
	def apply(axis: T => Iterable[T]) = new Selection(contents.flatMap(axis))

    def children = this(_.children)
    
    def descendants = {
        def desc(n: T): Stream[T] =
            Stream.concat(n.children.map(c => Stream.cons(c, desc(c))))
        this(desc(_))
    }
    
    def &(m: T => Boolean) = new Selection(contents.filter(m))
    
    def \(m: T => Boolean) = children & m
    
    def \\(m: T => Boolean) = descendants & m
}
