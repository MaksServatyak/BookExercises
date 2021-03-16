package exersice_2_1

object Main {
  def main(args: Array[String]): Unit = {

    def fib(n: Int): Int = {
      @annotation.tailrec
      def loop(previous: Int, current: Int, n: Int): Int =
        if (n == 0) previous
        else loop(current, previous + current, n - 1)

      loop(0, 1, n)
    }
  }
}
