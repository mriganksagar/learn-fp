package learnfp.functor

object ListInstance {
  implicit val listInstance:Functor[List] = new Functor[List] {
    override def fmap[A, B](a: List[A])(fx: A => B): List[B] = a match {
      case aa::axs => fx(aa)::fmap(axs)(fx)
      case Nil => Nil
    }
  }
}
