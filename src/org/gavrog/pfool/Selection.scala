package org.gavrog.pfool

object Selection {
	def apply[T <: { def children: Iterable[T]
                  	 def parent: Option[T] }](nodes: T*) =
        new Selection(new Selection[T](null) {
        	private val contents = Stream(nodes :_*)
        })
}

class Matcher[T](f: T => Boolean) extends (T => Boolean) {
    def apply(arg: T) = f(arg)
    
    def unary_! = new Matcher[T](!this(_))
    
    def & (that: T => Boolean) = new Matcher[T](n => this(n) && that(n))
    def | (that: T => Boolean) = new Matcher[T](n => this(n) || that(n))
    def ^ (that: T => Boolean) = new Matcher[T](n => this(n) ^ that(n))
}


class Selection[T <: { def children: Iterable[T]; def parent: Option[T] }](
                base: Selection[T], axis: T => Iterable[T], test: T => Boolean)
    extends Seq[T]
{
    def this(base: Selection[T], axis: T => Iterable[T]) =
        this(base, axis, _ => true)
    
    def this(base: Selection[T]) = this(base, List(_))
  
	private val contents: Stream[T] = base.contents.flatMap(axis).filter(test)
  
	def length = contents.length
    
	def elements = contents.elements
 
	def apply(n: Int) = contents(n)

    def children = new Selection[T](this, _.children.toStream)
    
    def descendants = {
        def desc(n: T): Stream[T] =
            Stream.concat(n.children.map(c => Stream.cons(c, desc(c))))
        new Selection[T](this, desc)
    }
    
    def parent = new Selection[T](this, _.parent.toStream)
    
    def ancestors = {
    	def anc(n: T): Stream[T] = n.parent match {
    		case Some(p) => Stream.cons(p, anc(p))
    		case None => Stream.empty
    	}
    	new Selection[T](this, anc)
    }
    
    def &(m: Matcher[T]) = new Selection[T](base, axis, n => test(n) && m(n))
    
    def \(m: Matcher[T]) = children & m
    
    def \\(m: Matcher[T]) = descendants & m
}
