package fps.chapter3

import scala.annotation.tailrec

object Exercise5 {
  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil => Nil
    case _ => if f(as.head) then dropWhile(as.tail, f) else as

  @main def dropWhileTest(): Unit =
    println(dropWhile(List(1,2,3,4), _ == 1))
    println(dropWhile(List(1,2,3,4), _ > 5))
    println(dropWhile(List(1,2,3,4), _ < 5))
}
