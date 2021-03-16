package exercises_4_1_to_4_8

sealed trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None


  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob


  def filter(f: A => Boolean): Option[A] =
    flatMap((a: A) => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(va => b.map(vb => f(va, vb)))



  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
