package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    val int = 42

    def not(b: Boolean): Boolean = {
      if b then false
      else true
    } // here is my greatest solution

    def and(left: Boolean, right: Boolean): Boolean = {
      if (left) {
        if right then true
        else false
      }
      else false
    }

    def or(left: Boolean, right: Boolean): Boolean = {
      if left then true
      else {
        if right then true
        else false
      }
    }

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) => {

      @tailrec
      def multRec(a: BigInt, b: BigInt, acc: BigInt): BigInt = {
        if (b == 0) acc
        else multRec(a, b - 1, acc + a)
      }

      if (b > a) {
        multRec(b, a, 0)
      } else {
        multRec(a, b, 0)
      }
    }

    val power: (BigInt, BigInt) => BigInt = (a, b) => {

      @tailrec
      def powerRec(a: BigInt, b: BigInt, acc: BigInt): BigInt = {
        if (b == 0) acc
        else powerRec(a, b - 1, multiplication(a, acc))
      }

      powerRec(a, b, 1)
    }

    val fermatNumber: Int => BigInt = a => {
      power(2, power(2, a)) + 1
    }

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = n => {

      @tailrec
      def lookAndSayHelper(s: String, acc: String): String = {
        if (s.isEmpty) acc
        else {
          val (lead, tail) = s.span(_ == s.head)
          lookAndSayHelper(tail, acc + lead.length + lead.head)
        }
      }

      BigInt(lookAndSayHelper(n.toString, ""))
    }

  end `Look-and-say Sequence`

  object LookAndSay {

    val lookAndSayTest: Int => BigInt = n => {
      BigInt(lookandsay(n.toString))
    }

    loop(10, "1")

    @tailrec
    private def loop(n: Int, num: String): Unit = {
      if (n <= 0) () else loop(n - 1, lookandsay(num))
    }

    private def lookandsay(number: String): String = {
      val result = new StringBuilder

      @tailrec
      def loop(numberString: String, repeat: Char, times: Int): String =
        if (numberString.isEmpty) result.toString()
        else if (numberString.head != repeat) {
          result.append(times).append(repeat)
          loop(numberString.tail, numberString.head, 1)
        } else loop(numberString.tail, numberString.head, times + 1)

      loop(number.tail + " ", number.head, 1)
    }

  }

end Homework