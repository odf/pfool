package org.gavrog.pfool

trait BasicNode {
  type T <: BasicNode
    
  def parent: Option[T]
    
  def children: Seq[T]
    
  def subtree: Seq[T] = Stream(this.asInstanceOf[T]) ++ descendants

  def descendants: Seq[T] =
    for (ch <- children; node <- ch.subtree) yield node.asInstanceOf[T]
    
  def appendChild(child: T): Unit
    
  def unlink: Unit
    
  protected def cloneSelf: T

  def cloneIf(f: T => Boolean): Option[T] =
    if (f(this.asInstanceOf[this.T])) {
      val root: T = cloneSelf
      for (nd <- children; ch <- nd.cloneIf(n => f(n.asInstanceOf[T])))
        root.appendChild(ch.asInstanceOf[root.T])
        Some(root)
    } else None
    
  override def clone = cloneIf(_ => true).get
}
