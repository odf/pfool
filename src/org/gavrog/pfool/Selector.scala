package org.gavrog.pfool


class Matcher[T](f: T => Boolean) extends (T => Boolean) {
    def apply(arg: T) = f(arg)
    
    def unary_! = new Matcher[T](!this(_))
    
    def & (that: T => Boolean) = new Matcher[T](n => this(n) && that(n))
    def | (that: T => Boolean) = new Matcher[T](n => this(n) || that(n))
    def ^ (that: T => Boolean) = new Matcher[T](n => this(n) ^ that(n))
}


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

    def &(m: T => Boolean) = {
        val base = this
        new Selector[T] {
            override def apply(roots: Iterable[T]) = base(roots).filter(m)
        }
    }
    
    def \(m: T => Boolean) = children & m
    
    def \\(m: T => Boolean) = descendants & m
}
