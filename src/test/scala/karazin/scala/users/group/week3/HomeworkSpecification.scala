package karazin.scala.users.group.week3

import scala.math.*
import org.scalacheck.*
import Prop.{forAll, propBoolean, throws}
import Homework.{Nat, *}

object HomeworkSpecification extends Properties("Homework"):

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):
  import arbitraries.given Arbitrary[Zero]

  property("isZero") = forAll{ (zero: Zero) =>
    zero.isZero == true
  }

  property("successor") = forAll { (n: Zero) =>
    n.successor - n == Zero.fromInt(1)
  }

  property("toInt") = Zero.toInt == 0

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):
  import arbitraries.given Arbitrary[Succ]

  property("toInt") = forAll { (n: Succ) =>
    var acc: Nat = Zero
    for(i <- 1 to n.toInt)
      acc = Succ(acc)

    n == acc
  }

  property("successor") = forAll { (n: Succ) =>
    n.successor - n == Zero.fromInt(1)
  }

  property("successor") = forAll { (n: Succ) =>
    n - n.predecessor == Zero.fromInt(1)
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
  import arbitraries.given Arbitrary[Nat]

  property("addition") = forAll { (left: Nat, right: Nat) =>
    (left + right).toInt == left.toInt + right.toInt
  }

  property("subtraction") = forAll { (left: Nat, right: Nat) =>
    val (greater, lesser) = if (left.toInt >= right.toInt) (left, right) else (right, left)
    (greater - lesser).toInt == greater.toInt - lesser.toInt
  }

end NatSpecification
  