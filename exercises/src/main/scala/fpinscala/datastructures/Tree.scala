package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.29

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n)             => n
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)             => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a)             => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B, g: (B, B) => B): B = t match {
    case Leaf(a)             => f(a)
    case Branch(left, right) => g(fold(left)(f, g), fold(right)(f, g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold[A, Int](t)(a => 1, (b1, b2) => 1 + b1 + b2)

  def maximumViaFold(t: Tree[Int]): Int =
    fold[Int, Int](t)(a => a, (b1, b2) => b1 max b2)

  def depthViaFold[A](t: Tree[A]): Int =
    fold[A, Int](t)(a => 0, (b1, b2) => 1 + (b1 max b2))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(a => Leaf(f(a)), (b1, b2) => Branch(b1, b2))

}
