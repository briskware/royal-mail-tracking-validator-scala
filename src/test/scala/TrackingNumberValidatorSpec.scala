import org.scalatest.WordSpec

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

}
