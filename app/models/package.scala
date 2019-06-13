import scala.language.{implicitConversions, postfixOps}

package object models {

  implicit val Coin: Int = 100000000
  implicit val MaxMoney: Int = Coin * 21000000

  implicit val MaxScriptElementSize: Int = 520
  implicit val LockTimeThreshold: Long = 500000L

  implicit final class BitcoinLong(private val long: Long) extends AnyVal {

    def btc: Double = long / 1e8
  }

  implicit final class ZFill(private val that: String) extends AnyVal {

    def zfill: String =
      String.format("%65s", that).replace(" ", "0")
  }

}
