import scala.language.{implicitConversions, postfixOps}

package object models {

  implicit final class BitcoinLong(private val long: Long) extends AnyVal {

    def btc: Double = long / 1e8
  }

  implicit final class ZFill(private val that: String) extends AnyVal {

    def zfill: String =
      String.format("%65s", that).replace(" ", "0")
  }
}
