package fps.chapter3

import LazyList._

object Exercise1 {
  val x = LazyList(1, 2, 3, 4, 5) match
    case cons(x, cons(2, cons(4, _))) => x
    case LazyList() => 42
    case cons(x: Int, cons(y: Int, cons(3, cons(4, _)))) => x + y
    case cons(h: Int, t) => h + t.sum
    case _ => 101


  @main def xTest(): Unit =
    println(x)
}

