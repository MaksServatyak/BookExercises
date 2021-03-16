package exercises_5_1_to_5_15

import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  // Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, r) => p(h) && r)

  // Exercise 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  // Exercise 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  // Exercise 5.12
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), i) if i > 1 => Some((h(), (t(), i - 1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWithViaUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }



  // Exercise 5.14
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(_, t) => Some((s, t()))
      case Empty => None
    } append Stream(empty)

  // Exercise 5.15
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val lb = b
      val x = f(a, lb._1)
      (x, cons(x, lb._2))
    })._2
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
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.7
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.8
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.9
  def fibs: Stream[Int] = {
    def loop(previous: Int, current: Int): Stream[Int] =
      cons(previous, loop(current, previous + current))
    loop(0, 1)
  }

  // Exercise 5.10
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // Exercise 5.11
  def fibsViaUnfold: Stream[Int] = unfold((0, 1)) {
    case (i, j) => Some((i, (j, i + j)))
  }
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some((i, i + 1)))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))
}