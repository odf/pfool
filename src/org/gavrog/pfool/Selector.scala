package org.gavrog.pfool


class Selector[T <: { def children: Iterable[T] }] {
    def apply(roots: Iterable[T]) = roots.toSeq
    
    def apply(f: T => Iterable[T]): Selector[T] = {
        val base = this
        new Selector[T] {
            override def apply(roots: Iterable[T]) = base(roots).flatMap(f)
        }
    }
    
    def children = this(_.children)

    private def desc(n: T): Stream[T] =
        Stream.concat(n.children.map(c => Stream.cons(c, desc(c))))
    
    def descendants = this(desc(_))

    def &(s: Selector[T]) = {
    	val base = this
    	new Selector[T] {
    		override def apply(roots: Iterable[T]) = s(base(roots))
    	}
    }
    
    def \(s: Selector[T]) = children & s
    def \\(s: Selector[T]) = descendants & s
    
    def \@(s: Selector[T]) = this & Filter(n => !s(n.children).isEmpty)
    def \\@(s: Selector[T]) = this & Filter(n => !s(desc(n)).isEmpty)
}

case class Filter[T <: { def children: Iterable[T] }](f: T => Boolean)
	extends Selector[T]
{
  	override def apply(roots: Iterable[T]) = roots.toSeq.filter(f)
  	
  	def unary_! = new Filter[T](!f(_))

  	def |(m: T => Boolean) = new Filter[T](n => f(n) | m(n))
  	def |(s: Filter[T]): Filter[T] = this | s.f
}
