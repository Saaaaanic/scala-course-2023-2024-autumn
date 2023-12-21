package karazin.scala.users.group.week2.homework

import scala.math.*
import org.scalacheck.*
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework.{Rational, *}
import karazin.scala.users.group.week2.arbitraries.restricted.{PositiveInteger, Zero}
import utils.*
import scala.language.implicitConversions

object HomeworkSpecification extends Properties("Homework"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[Rational]}

  property("throw exception due to zero denominator") = forAll { (numer: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, 0)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, kindaDenom: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, -abs(kindaDenom))
    }
  }

  property("check that rational number is simplified") = forAll { (numer: Int, int: Int) ⇒
    val denom = abs(int) + 1
    val rational = Rational(numer, denom)

    rational.numer == (numer / gcd(abs(numer), denom)) && rational.denom == (denom / gcd(abs(numer), denom))
  }

  property("check equals") = forAll { (left: Rational, right: Rational) ⇒
    (left == right) == (left.numer == right.numer && left.denom == right.denom)
  }

  property("less then") = forAll { (left: Rational, right: Rational) =>
    (left < right) == (left.numer * right.denom < right.numer * left.denom)
  }

  property("less or equal") = forAll { (left: Rational, right: Rational) =>
    (left <= right) == ( left < right || left == right)
  }

  property("greater") = forAll { (left: Rational, right: Rational) =>
    (left > right) == !(left <= right)
  }

  property("greater or equal") = forAll { (left: Rational, right: Rational) =>
    (left >= right) == ( left > right || left == right)
  }

  property("negation") = forAll { (rational: Rational) =>
    -rational == Rational(-rational.numer, rational.denom)
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    left + right == Rational(left.numer * right.denom + right.numer * left.denom, left.denom * right.denom)
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    left - right == Rational(left.numer * right.denom - right.numer * left.denom, left.denom * right.denom)
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    left * right == Rational(left.numer * right.numer, left.denom * right.denom)
  }

  property("division") = forAll { (left: Rational, numer: PositiveInteger, denom: Int) =>
    val right = Rational(numer, abs(denom) + 1)
    left / right == Rational(left.numer * right.denom * signum(right.numer), left.denom * abs(right.numer))
  }

  property("division numer 0") = forAll { (left: Rational, denom: Int) =>
    val right = Rational(1, abs(denom) + 1)
    left / right == Rational(left.numer * right.denom * signum(right.numer), left.denom * abs(right.numer))
  }

  property("division by zero") = forAll { (left: Rational, int: Int) =>
    val right = Rational(int, 1)
    left / right == Rational(left.numer * right.denom * signum(right.numer), left.denom * abs(right.numer))
  }

  property("equals hash code contract") = forAll { (left: Rational, right: Rational) ⇒
    (left == right) == (left.hashCode() == right.hashCode())
  }

end HomeworkSpecification