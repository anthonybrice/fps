package fpinscala.exercises.laziness

import scala.annotation.tailrec
import fpinscala.exercises.laziness.LazyList.*

enum LazyList[+A]:

  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(a, as) => a() :: as().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(a, as) if n > 1 => cons(a(), as().take(n-1))
    case Cons(a, _) if n == 1 => cons(a(), empty)
    case _ => empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, as) if n > 0 => as().drop(n-1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] =
   foldRight(empty: LazyList[A])((a, acc) => if p(a) then cons(a, acc) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty: LazyList[B])((a, acc) => cons(f(a), acc))

  def filter(p: A => Boolean): LazyList[A] =
    foldRight(empty: LazyList[A])((a, acc) => if p(a) then cons(a, acc) else acc)

  def append[B >: A](that: => LazyList[B]): LazyList[B] =
    foldRight(that)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty: LazyList[B])((a, acc) => f(a).foldRight(acc)((b, acc2) => cons(b, acc2)))

  def startsWith[B >: A](s: LazyList[B]): Boolean =
    this.zipAll(s).forAll {
      case (None, None) => true
      case (Some(_), None) => true
      case (None, Some(_)) => false
      case (Some(x), Some(y)) => x == y
    }

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), m) if m > 1 => Some(h(), (t(), m - 1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)) {
      case (Cons(x, xs), Cons(y, ys)) => Some(f(x(), y()), (xs(), ys()))
      case _ => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this, that) {
      case (Cons(x, xs), Cons(y, ys)) => Some((Some(x()), Some(y())), (xs(), ys()))
      case (Cons(x, xs), Empty) => Some((Some(x()), None), (xs(), empty))
      case (Empty, Cons(y, ys)) => Some((None, Some(y())), (empty, ys()))
      case _ => None
    }

  def tails: LazyList[LazyList[A]] =
    unfold(this) {
      case Cons(x, xs) => Some(cons(x(), xs()), xs())
      case Empty => None
    }.append(cons(empty, empty))

  def scanRight[B](s: B)(f: (A,B) => B): LazyList[B] =
//    unfold(this) {
//      case Empty => None
//      case xs => Some(xs.foldRight(s)(f(_,_)), xs.drop(1))
//    }.append(cons(s, empty))
    foldRight(cons(s, empty)) { (a, acc) =>
      cons(f(a, acc.headOption.get), acc)
    }

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] =
    def go(m: Int, n: Int): LazyList[Int] =
      cons(m, go(n, m + n))

    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty

  lazy val fibsViaUnfold: LazyList[Int] =
    cons(0, cons(1, unfold((0,1))((m,n) => Some((m + n, (n, m + n))))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    cons(n, unfold(n)(s => Some(s+1, s+1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(s => Some(s, s))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(1)(s => Some(s, s))
