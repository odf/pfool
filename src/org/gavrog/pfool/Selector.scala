package org.gavrog.pfool


class Selector[T <: { def children: Iterable[T] }]
               (generate: Iterable[T] => Iterable[T])
{
  def apply = generate
    
  def apply(f: T => Iterable[T]) = new Selector[T](generate(_).flatMap(f))
    
  def children = this(_.children)

  private def desc(n: T): Iterable[T] =
    n.children.flatMap(c => Stream(c) ++ desc(c))
    
  def descendants = this(desc(_))

  def &(s: Selector[T]) = new Selector[T](this.apply andThen s.apply)
  def |(s: Selector[T]) =
    new Selector[T](roots => this(roots).toSet ++ s(roots))
  
  def \(s: Selector[T]) = children & s
  def \\(s: Selector[T]) = descendants & s
    
  def \@(s: Selector[T]) = this & Filter(n => !s(n.children).isEmpty)
  def \\@(s: Selector[T]) = this & Filter(n => !s(desc(n)).isEmpty)
    
  def \!(s: Selector[T]) = this & Filter(n => s(n.children).isEmpty)
  def \\!(s: Selector[T]) = this & Filter(n => s(desc(n)).isEmpty)
}

case class Filter[T <: { def children: Iterable[T] }](f: T => Boolean)
	extends Selector[T](_.filter(f))
{
  def unary_! = new Filter[T](!f(_))

  def |(m: T => Boolean) = new Filter[T](n => f(n) | m(n))
  def |(s: Filter[T]): Filter[T] = this | s.f
}
