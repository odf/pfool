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

    def descendants = {
        def desc(n: T): Stream[T] =
            Stream.concat(n.children.map(c => Stream.cons(c, desc(c))))
        this(desc(_))
    }

    def &(s: Selector[T]) = {
    	val base = this
    	new Selector[T] {
    		override def apply(roots: Iterable[T]) = s(base(roots))
    	}
    }
    
    def &(m: T => Boolean): Selector[T] = this & Filter(m)
    
    def \(m: T => Boolean) = children & m
    def \(s: Selector[T]) = children & s
    
    def \\(m: T => Boolean) = descendants & m
    def \\(s: Selector[T]) = descendants & s
}

case class Filter[T <: { def children: Iterable[T] }](f: T => Boolean)
	extends Selector[T]
{
  	override def apply(roots: Iterable[T]) = roots.toSeq.filter(f)
  	
  	def unary_! = new Filter[T](!f(_))

  	def |(m: T => Boolean) = new Filter[T](n => f(n) | m(n))
  	def |(s: Filter[T]): Filter[T] = this | s.f
}
