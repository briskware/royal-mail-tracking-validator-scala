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
      ("gibberish", false),
      ("NY819217292GB", true),
      ("NY819217315GB", true),
      ("NYC19217315GB", false),
      ("NY8192173153B", false),
      (null, false),
      ("", false)
    ) foreach {
      case (theNumber, valid) =>
        s"'$theNumber' is ${if (valid) "valid" else "invalid"}" in {
          val _ = TrackingNumber(theNumber) match {
            case Right(_)  => assert(valid, "Should be valid")
            case Left(err) => assert(!valid, s"Should be valid but is not: ${err.message}")
            case _         => System.err.println(s"$theNumber Test FAILURE")
          }
        }
    }
  }

}
