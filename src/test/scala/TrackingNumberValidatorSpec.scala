import org.scalatest.WordSpec

import scala.util.{Failure, Try}

class TrackingNumberValidatorSpec extends WordSpec {

  "The TrackingNumber validator" must {
    "validate a correct number" in {
      val number = TrackingNumber("NY819217301GB")
      assert(number.isRight)
    }
    "validate an incorrect number" in {
      val number = TrackingNumber("NY819217302GB")
      assert(number.isLeft)
    }
    "compute a check digit" in {
      val number = TrackingNumber("NY", "81921731".toArray.map(_.toInt), "GB")
      assert(number.isRight)
      assert(number.exists(_.checkDigit === 5))
    }
  }

  "The TrackingNumber validator using a few other numbers and edge cases" must {
    List(
      ("NY819217301GB", true),
      ("NY819217302GB", false),
      ("NY81921S7302GB", false),
      ("NY12345678901GB", false),
      ("gibberish", false),
      ("NY819217292GB", true),
      ("pY819217292GB", false),
      ("NY819217292GBs", false),
      ("NY819217315GB", true),
      ("NYC19217315GB", false),
      ("NY8192173153B", false),
      ("NY000000080GB", true), // edge case when checkDigit of 10 is replaced by 0
      ("NY000000155GB", true), // edge case when checkDigit of 11 is replaced by 5
      (null, false),
      ("", false)
    ) foreach {
      case (theNumber, valid) =>
        s"'$theNumber' is ${Option.when(!valid)("in").mkString}valid" in {
          TrackingNumber(theNumber) match {
            case Right(_)  => assert(valid, "Should be valid")
            case Left(err) => assert(!valid, s"Should be valid but is not: ${err.reason}")
            case _         => assert(false, s"expectation mismatch")
          }
        }
    }
  }

  "The TrackingNumber.checkDigit validator" must {
    "work for all possible digits" in {
      1.toLong to 99999999 take 20 map { idx => f"$idx%08d".toArray.map(_.toInt) } foreach { digits =>
        val check = Try(TrackingNumber.checkDigit(digits)) match {
          case Failure(err) => assert(false, s"digits: ${digits.map(_.toChar).mkString} failed with ${err.getMessage}")
          case _            => ()
        }
      }
    }
  }

}
