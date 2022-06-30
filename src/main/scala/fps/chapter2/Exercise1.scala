package fps.chapter2

object Exercise1 {
  
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibPrime(m: Int, mMinus1: Int, mMinus2: Int): Int = 
      if (m == n) mMinus1 + mMinus2
      else fibPrime(m+1, mMinus1 + mMinus2, mMinus1)

    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibPrime(2, 1, 0)
    }
  }

  @main def fibTest: Unit = {
    println(fib(10))
  }
}
