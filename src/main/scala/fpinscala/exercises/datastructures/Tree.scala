package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => (1 + l.depth).max(1 + r.depth) 

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(x) => f(x)
    case Branch(l, r) => g(l.fold(f,g), r.fold(f,g))
  
  def sizeViaFold: Int =
    fold(_ => 1, (a,b) => 1 + a + b)
  
  def depthViaFold: Int =
    fold(_ => 0, (a,b) => (1 + a).max(1 + b))
  
  def mapViaFold[B](f: A => B): Tree[B] =
    fold(x => Leaf(f(x)), (a,b) => Branch(a, b))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match
    case Leaf(i) => if i > 0 then Some(i) else None
    case Branch(l, r) => l.firstPositive orElse r.firstPositive


  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => l.maximum.max(r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(i => i, (a,b) => a.max(b))
