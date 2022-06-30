package fps.chapter3

object Exercise2 {
  def tail[A](xs: List[A]): Option[List[A]] = xs match
    case List() => None
    case _ :: ys => Some(ys)

  @main def tailTest(): Unit =
    println(tail(List(1, 2, 3, 4)))
    println(tail(List(1)))
    println(tail(List()))
}

