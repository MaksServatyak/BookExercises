package exersice_2_2

object Main {
  def main(args: Array[String]): Unit = {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @annotation.tailrec
      def loop(n: Int): Boolean =
        if (n + 1 >= as.length) true
        else if (!ordered(as(n), as(n + 1))) false
        else loop(n + 1)
      loop(0)
    }

  }

}
