package fps.chapter3

import scala.annotation.tailrec

object Exercise6 {
  def init[A](as: List[A]): List[A] =
    @tailrec
    def go(bs: List[A], acc: List[A]): List[A] = bs match
      case Nil => Nil
      case _ :: Nil => acc
      case b :: bss => go(bss, acc :+ b)

    go(as, Nil)

  @main def initTest(): Unit =
    println(init(List()))
    println(init(List(1)))
    println(init(List(1,2,3)))
}
