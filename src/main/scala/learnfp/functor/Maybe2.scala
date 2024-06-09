package learnfp.functor

object Maybe2 {
  sealed trait Maybe2[+A]

  case class Just[A](value: A) extends Maybe2[A] {
    def fmap[A, B](a: Just[A])(fx: A => B): Just[B] = Just[B](fx(a.value))
  }

  case class Nothing[A]() extends Maybe2[A] {
    def fmap[A, B](a: Nothing[A])(fx: A => B): Nothing[B] = Nothing[B]()
  }
}

object Maybe2Instance {
  import Maybe2._

  implicit val maybeInstance:Functor[Maybe2] = new Functor[Maybe2] {
    override def fmap[A, B](a: Maybe2[A])(fx: A => B): Maybe2[B] = a match {
      case Just(x) => Just(fx(x))
      case Nothing() => Nothing()
    }
  }
}
