package learnfp.monoid

trait Monoid[A] {
  def mzero:A
  def mappend(lhs:A, rhs:A):A
}

object Monoid {
  def mzero[A](implicit monoid:Monoid[A]):A = monoid.mzero

  implicit class MonoidOps[A](lhs:A)(implicit monoid:Monoid[A]) {
    def |+|(rhs:A):A = monoid.mappend(lhs, rhs)
  }
}

//implicit class MonoidOps[A](lhs:A)(implicit monoid:Monoid[A]) {
//  def |+|(rhs:A):A = monoid.mappend(lhs, rhs)
//}
//
//object MonoidOps {
//  implicit def toMonoidOps[A](x:A)(implicit monoid:Monoid[A]):MonoidOps[A] = new MonoidOps[A](x)
//}

// i was able to remove this by only adding implicit keyword in the class MonoidOps Itself, and changing the imports where it was used (obviously)