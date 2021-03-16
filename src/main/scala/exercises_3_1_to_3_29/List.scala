package exercises_3_1_to_3_29

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  // Exercise 3.1
  // Result: 3 (1 + 2)
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sumLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
  def productLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, el) => Cons(el, acc))


  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((b: B, a: A) => f(a, b))

  // Exercise 3.14
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
  // foldLeft(reverse(a1), a2)((b: List[A], a: A) => Cons(a, b))
    foldRight(a1, a2)((a: A, b: List[A]) => Cons(a, b))

  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)
  // foldRight(l, List[A]())((el: List[A], acc: List[A]) => append(el, acc))

  // Exercise 3.16
  def increment(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((el: Int, acc: List[Int]) => Cons(el + 1, acc))

  // Exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])(
      (el: Double, acc: List[String]) => Cons(el.toString, acc)
    )

  // Exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((el: A, acc: List[B]) => Cons(f(el), acc))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((el: A, acc: List[A]) =>
      if (f(el)) Cons(el, acc)
      else acc
    )

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))


  // Exercise 3.22
  def addList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addList(t1, t2))
  }

  // Exercise 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}
