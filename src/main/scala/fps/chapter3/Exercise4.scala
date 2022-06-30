package fps.chapter3

import scala.annotation.tailrec
import fps.chapter2.Exercise5.*

object Exercise4 {
  def drop[A](as: List[A], n: Int): List[A] =
    @tailrec
    def go(m: Int, acc: List[A]): List[A] =
      if n == m then acc
      else go(m + 1, acc.tail)
    
    go(0, as)

  @main def dropTest(): Unit =
    println(drop(List(1, 2, 3, 4, 5), 2))
}
