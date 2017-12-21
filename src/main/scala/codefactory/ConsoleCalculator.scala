package codefactory

import scala.io.StdIn

object ConsoleCalculator {
  def main(args: Array[String]): Unit = {
    val a = number()
    val b = number()

    println(s"a + b = ${a + b}")
  }

  // Delay effects by using a procedure.
  private def number(): Int = {
    println("Enter number: ")
    StdIn.readInt()
  }
}
