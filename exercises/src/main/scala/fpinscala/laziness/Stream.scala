package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  // Ex 5.1
  def toList: List[A] = foldRight(Nil: List[A])((a, b) => a :: b)

  // Ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // Ex 5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  // Ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
    case Empty => empty
  }

  // Ex 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  // Ex 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else empty)

  // 5.6
  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  // Ex 5.14
  def startsWith[B >: A](s: Stream[B]): Boolean = {
    zipAll(s).foldRight(true){ (a, b) =>
      a match {
        case (Some(a1), Some(a2)) => if (a1 == a2) b else false
        case (Some(a1), None) => b
        case (None, Some(_)) => false
        case _ => true
      }
    }
  }

  // Ex 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = 
    foldRight(Stream(z)){ (a, b) =>
      b match {
        case b @ Cons(h, t) => cons(f(a, h()), b)
      }
    }

  // Ex 5.15
  def tails: Stream[Stream[A]] = 
    unfold(this){ s =>
      s match {
        case Empty => None
        case Cons(h, t) => Some((s, t()))
      }
    }.append(Stream(empty))

  // Ex 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){ s => 
      s match {
        case Empty => None
        case Cons(h, t) => Some((f(h()), t()))
      }
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)){ s =>
      s._1 match {
        case Cons(h, t) if s._2 > 0 => Some((h(), (t(), s._2 - 1)))
        case _ => None
      }
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this){ s =>
      s match {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }
    }

  def zipWithViaUnfold[B](s: Stream[B]): Stream[(A, B)] =
    unfold((this, s)){ s =>
      s match {
        case (Cons(ha, ta), Cons(hb, tb)) => Some((ha(), hb()), (ta(), tb()))
        case _ => None
      }
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)){ s =>
      s match {
        case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
        case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
        case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
        case _ => None
      }
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  // Ex 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  // Ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Ex 5.10
  val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  // Ex 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  // Ex 5.12
  val fibsViaUnfold: Stream[Int] = 
    unfold[Int, (Int, Int)]((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def fromViaUnfold(n: Int): Stream[Int] = 
    unfold[Int, Int](n)(s => Some(s, s + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold[A, A](a)(a => Some(a, a))

  def onesViaUnfold = constantViaUnfold(1)


}
