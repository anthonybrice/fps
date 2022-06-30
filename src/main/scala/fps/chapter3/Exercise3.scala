package fps.chapter3

object Exercise3 {
  def setHead[A](x: A, xs: List[A]): List[A] = xs match
    case List() => x :: List()
    case _ => x :: xs.tail
}
