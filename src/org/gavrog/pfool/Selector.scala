package org.gavrog.pfool


class Matcher[T](f: T => Boolean) extends (T => Boolean) {
    def apply(arg: T) = f(arg)
    
    def unary_! = new Matcher[T](!this(_))
    
    def & (that: T => Boolean) = new Matcher[T](n => this(n) && that(n))
    def | (that: T => Boolean) = new Matcher[T](n => this(n) || that(n))
    def ^ (that: T => Boolean) = new Matcher[T](n => this(n) ^ that(n))
}


trait Selector[T <: { def children: Iterable[T] }] {
    def apply(roots: Iterable[T]): Seq[T]
    
    def apply(f: T => Iterable[T]) = Selector.Mapper(this, f)
    
    def children = this(_.children)

    def descendants = {
        def desc(n: T): Stream[T] =
            Stream.concat(n.children.map(c => Stream.cons(c, desc(c))))
        this(desc(_))
    }

    def &(m: T => Boolean) = Selector.Filter(this, m)
    
    def \(m: T => Boolean) = children & m
    
    def \\(m: T => Boolean) = descendants & m
}


object Selector {
    case class One[T <: { def children: Iterable[T] }]() extends Selector[T]
    {
        def apply(roots: Iterable[T]) = roots.toSeq
    }
    
    case class Filter[T <: { def children: Iterable[T] }](
                    base: Selector[T], test: T => Boolean) extends Selector[T]
    {
        def apply(roots: Iterable[T]) = base(roots).filter(test)
    }
    
    case class Mapper[T <: { def children: Iterable[T] }](
                    base: Selector[T], axis: T => Iterable[T]) extends Selector[T]
    {
        def apply(roots: Iterable[T]) = base(roots).flatMap(axis)
    }
}
