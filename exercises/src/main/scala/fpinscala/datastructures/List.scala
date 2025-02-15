package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil
    extends List[
      Nothing
    ] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](
      as: List[A],
      z: B
  )(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil        => Nil
    case Cons(h, t) => t
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil        => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil        => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil        => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  // 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  // 3.10
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 3.11
  def sumViaFoldLeft(l: List[Int]): Int = foldLeft(l, 0)((acc, a) => acc + a)
  def productViaFoldLeft(l: List[Int]): Int =
    foldLeft(l, 1)((acc, a) => acc * a)
  def lengthViaFoldLeft(l: List[Int]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] = {
    foldRight(l, Nil: List[A])((a, acc) => Cons(a, acc))
  }

  // 3.13
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => (b: B) => g(f(b, a)))(z)
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => (b: B) => g(f(a, b)))(z)

  // 3.14
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a, acc) => Cons(a, acc))
  }

  // 3.15
  def concat[A](ll: List[List[A]]): List[A] = {
    foldRight(ll, Nil: List[A])((l, acc) => appendViaFoldLeft(l, acc))
  }

  // 3.16
  def add1(l: List[Int]): List[Int] = l match {
    case Nil        => Nil
    case Cons(h, t) => Cons(h + 1, add1(t))
  }

  // 3.17
  def intToString(l: List[Int]): List[String] = l match {
    case Nil        => Nil
    case Cons(h, t) => Cons(h.toString, intToString(t))
  }

  // 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil        => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil        => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }
  def filterEven(l: List[Int]): List[Int] = filter(l)(n => n % 2 == 0)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))
  }

  // 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def add(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1, t2))
  }

  // 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(a, t1), Cons(b, t2)) => Cons(f(a, b), zipWith(t1, t2)(f))
    }

  // 3.24
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = {
      (l, prefix) match {
        case (_, Nil)                                   => true
        case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
        case _                                          => false
      }
    }
    startsWith(sup, sub) || (sup match {
      case Nil        => false
      case Cons(h, t) => hasSubsequence(t, sub)
    })
  }

}
