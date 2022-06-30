package fps.chapter2

object Exercise2 {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = as match
    case Array() => true
    case Array(_) => true
    case _ => as.sliding(2).forall { case Array(m,n) => ordered(m,n) }


  @main def isSortedTest(): Unit =
    println(isSorted(Array(1,2,3,4,4,5), _ <= _))
}
