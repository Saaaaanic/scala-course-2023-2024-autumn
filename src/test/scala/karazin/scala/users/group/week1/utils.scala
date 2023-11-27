package karazin.scala.users.group.week1

import scala.annotation.tailrec

object utils:
  val modelFactorial: Int => BigInt =
    n => (0 to n).foldLeft(BigInt(1)) { (acc, n) =>
      if n == 0 then acc * 1 else acc * n
    }

  val modelSum: (BigInt => BigInt) => (Int, Int) => BigInt =
    f => (left, right) => (left to right).foldLeft(BigInt(0)) { (acc, v) =>
      acc + f(v)
    }

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