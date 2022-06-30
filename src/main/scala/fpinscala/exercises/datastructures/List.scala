package fpinscala.exercises.datastructures

import scala.annotation.tailrec

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // this one
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  // No, foldRight cannot short circuit. One could write the given op such that
  // it is a noop after 0.0 is encountered, but foldRight must still traverse
  // the entire list applying the op.
  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("Empty list")
    case Cons(_, ls) => ls

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("Empty list")
    case _ => Cons(h, tail(l))

  def drop[A](l: List[A], n: Int): List[A] =
    if n < 0 then return l
    @tailrec
    def go(m: Int, acc: List[A]): List[A] = acc match
      case Nil => Nil
      case _ =>
        if n == m then acc
        else go(m + 1, tail(acc))

    go(0, l)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Nil => Nil
    case Cons(a, as) => if f(a) then dropWhile(as, f) else l

  def init[A](l: List[A]): List[A] =
    @tailrec
    def go(bs: List[A], acc: List[A]): List[A] = bs match
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => acc
      case Cons(b, bss) => go(bss, append(acc, Cons(b, Nil)))

    go(l, Nil)

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(a, as) => foldLeft(as, f(acc, a), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, (_: Int) + (_: Int))

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1, (_: Double) * (_: Double))

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A], (acc, a) => Cons(a, acc))

  def foldRightViaFoldLeft[A,B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(l), acc, (b,a) => f(a,b))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A], foldRight(_, _, Cons(_, _)))

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (a, acc) => Cons(a+1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (a, acc) => Cons(a.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B], (a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(
      as,
      Nil: List[A],
      (a, acc) => if f(a)
        then Cons(a, acc)
        else acc
    )

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if f(a) then Cons(a, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    @tailrec
    def go(xs: List[Int], ys: List[Int], acc: List[Int]): List[Int] = (xs, ys) match
      case (Nil, Nil) => acc
      case (_, Nil) => acc
      case (Nil, _) => acc
      case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(x + y, acc))

    reverse(go(a, b, Nil))

//    foldLeft(
//      a,
//      (Nil: List[Int], b),
//      (acc: (List[Int], List[Int]), x: Int) => acc._2 match
//        case Nil => (acc._1, Nil)
//        case Cons(y, ys) => (Cons(x+y, acc._1), ys)
//    )._1


  // def zipWith - TODO determine signature
  def zipWith[A,B](a: List[A], b: List[A], f: (A,A) => B): List[B] =
    @tailrec
    def go(xs: List[A], ys: List[A], acc: List[B]): List[B] = (xs, ys) match
      case (Nil, Nil) => acc
      case (_, Nil) => acc
      case (Nil, _) => acc
      case (Cons(x, xs), Cons(y, ys)) => go(xs, ys, Cons(f(x,y), acc))

    reverse(go(a, b, Nil))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    if sub == Nil then return true
    
    def subsequenceHere[A](sup: List[A], sub: List[A]): Boolean =
      val zipped: List[(A,A)] = zipWith(sup, sub, (a,b) => (a,b))
      length(filter(zipped)((t: (A, A)) => t._1 == t._2)) == length(zipped)

    sup match
      case Nil => false
      case _ => if subsequenceHere(sup, sub)
        then true
        else hasSubsequence(tail(sup), sub)
