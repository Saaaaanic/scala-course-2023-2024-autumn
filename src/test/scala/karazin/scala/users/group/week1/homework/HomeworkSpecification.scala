package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndAaSequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    and(left, right) == (left && right)
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair
    
    or(left, right) == left || right
  }

  property("eagerAnd") = propBoolean {
    and(false, throw new Exception("And is right operand")) == (false && true)
  }

  property("eagerOr") = propBoolean {
    or(true, throw new Exception("Or in right operand")) == (true || false)
  }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    fermatNumber(n) == BigInt(2).pow(BigInt(2).pow(n).toInt) + 1
  }  

end FermatNumbersSpecification

object LookAndAaSequenceSpecification extends Properties("Look-and-say Sequence"):
  import karazin.scala.users.group.week1.utils.LookAndSay._
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  property("lookAndSaySequenceElement") = forAll { (n: Int) =>
    lookAndSaySequenceElement(n) == lookAndSayTest(n)
  }  

end LookAndAaSequenceSpecification
