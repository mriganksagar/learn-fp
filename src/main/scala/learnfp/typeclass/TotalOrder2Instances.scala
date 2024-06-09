package learnfp.typeclass

import scala.annotation.tailrec

trait TotalOrder2[A] {
  def less(lhs:A, rhs:A):Boolean
  def equal(lhs: A, rhs: A): Boolean
  def greater(lhs:A, rhs: A): Boolean
}

object TotalOrder2Instances {
  implicit val intInstance:TotalOrder2[Int] = new TotalOrder2[Int] {
    override def less(lhs: Int, rhs: Int): Boolean = lhs < rhs
    override def equal(lhs: Int, rhs: Int): Boolean = lhs == rhs
    override def greater(lhs: Int, rhs: Int): Boolean = lhs > rhs
  }

  implicit val stringInstance:TotalOrder2[String] = new TotalOrder2[String] {
    override def less(lhs: String, rhs: String): Boolean = lhs < rhs
    override def equal(lhs: String, rhs: String): Boolean = lhs == rhs
    override def greater(lhs: String, rhs: String): Boolean = lhs > rhs
  }

  implicit def listInstance[T](implicit suborder:TotalOrder2[T]):TotalOrder2[List[T]] = new TotalOrder2[List[T]] {
    @tailrec
    override def less(lhs: List[T], rhs: List[T]): Boolean = {
      (lhs, rhs) match {
        case (_, Nil) => false
        case (Nil, r::_) => true
        case (l::llhs, r::rrhs) => suborder.less(l, r) && less(llhs, rrhs)
      }
    }

    @tailrec
    override def equal(lhs: List[T], rhs: List[T]): Boolean = {
      (lhs, rhs) match {
        case(Nil, Nil) => true
        case(l::llhs, r::rrhs) => suborder.equal(l, r) && equal(llhs, rrhs)
        case _ => false
      }
    }

    @tailrec
    override def greater(lhs: List[T], rhs: List[T]): Boolean = {
      (lhs, rhs) match {
        case(l::llhs, Nil ) => true
        case(Nil, _) => false
        case(l::llhs, r::rrhs) => suborder.greater(l, r) && greater(llhs, rrhs)
      }
    }
  }
}

object Comparator2 {
  @annotation.implicitNotFound("No instance of TotalOrder2 found")
  def less[A](lhs:A, rhs:A)(implicit order:TotalOrder2[A]): Boolean = {
    order.less(lhs, rhs)
  }
  def equal[A](lhs:A, rhs: A)(implicit order:TotalOrder2[A]):Boolean = {
    order.equal(lhs, rhs)
  }
  def greater[A](lhs:A, rhs: A)(implicit order:TotalOrder2[A]):Boolean = {
    order.greater(lhs, rhs)
  }
}