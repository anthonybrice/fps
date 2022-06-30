package fps.chapter3

object Exercise8 {
  @main def ex8(): Unit =
    val x = List(1,2,3).foldRight(Nil: List[Int], (_: Int) :: (_: List[Int]))
    println(x)
}
