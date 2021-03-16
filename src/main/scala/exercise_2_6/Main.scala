package exercise_2_6

object Main {
  def main(args: Array[String]): Unit = {
    def compose[A, B, C](f: B => C, g: A => B): A => C =
      (a: A) => f(g(a))
  }

}
