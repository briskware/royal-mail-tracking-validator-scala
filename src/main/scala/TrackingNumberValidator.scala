import scala.util.Try
import scala.util.matching.Regex
import cats.syntax.all._

import scala.annotation.tailrec

/**
  * UK Royal Mail Tracking Number Validator
  *
  * For reference see: https://www.royalmail.com/sites/default/files/Royal_Mail_Tracked_Standard_COSS_V2_1.pdf
  */
case class TrackingNumber(prefix: String, digits: Array[Int], checkDigit: Int, suffix: String = "GB") {
  override def toString =
    s"$prefix${digits.mkString}$checkDigit$suffix"
}

case class TrackingNumberError(reason: String)

object TrackingNumber {
  private val TNP: Regex = "([A-Z]{2})([0-9]{8})([0-9])([A-Z]{2})".r

  @tailrec
  private def checkDigit(digits: Array[Int], weighting: Array[Int] = Array(8, 6, 4, 2, 3, 5, 9, 7), sum: Int = 0): Int =
    if (digits.isEmpty) {
      val result = 11 - sum % 11
      assert(result < 10)
      result
    } else
      checkDigit(digits.tail, weighting.tail, sum + digits.head * weighting.head)

  private def validateCheckDigit(digits: Array[Int], digit: Int): Try[Unit] =
    Try {
      assert(digits.length === 8, "invalid item identifier")
      assert(checkDigit(digits) === digit, "incorrect check digit")
    }

  def apply(string: String): Either[TrackingNumberError, TrackingNumber] =
    string match {
      case TNP(prefix, digits, checkDigit, suffix) =>
        for {
          ds <- Try(digits.map(_.toInt).toArray).toEither.left.map(e => TrackingNumberError(e.getMessage))
          cd <- Try(checkDigit.toInt).toEither.left.map(e => TrackingNumberError(e.getMessage))
          _  <- validateCheckDigit(ds, cd).toEither.left.map(e => TrackingNumberError(e.getMessage))
        } yield TrackingNumber(prefix, ds, cd, suffix)
      case _ =>
        Left(TrackingNumberError(s"invalid tracking number syntax: $string"))
    }

  def apply(prefix: String, digits: Array[Int], suffix: String): Either[TrackingNumberError, TrackingNumber] =
    apply(s"$prefix${digits.map(_.toChar).mkString}${checkDigit(digits)}$suffix")

  def unapply(trackingNumber: TrackingNumber): Some[(String, Array[Int], Int, String)] =
    Some((trackingNumber.prefix, trackingNumber.digits, trackingNumber.checkDigit, trackingNumber.suffix))
}
