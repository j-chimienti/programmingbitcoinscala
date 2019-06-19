import scala.language.{implicitConversions, postfixOps}

package object models {

  /*
   * see https://en.bitcoin.it/wiki/Base58Check_encoding
   *
   * Why base-58 instead of standard base-64 encoding?
   * <ul>
   * <li>Don't want 0OIl characters that look the same in some fonts and could be used to create visually identical
   * looking account numbers.</li>
   * <li>A string with non-alphanumeric characters is not as easily accepted as an account number.</li>
   * <li>E-mail usually won't line-break if there's no punctuation to break at.</li>
   * <li>Doubleclicking selects the whole number as one word if it's all alphanumeric.</li>
   */
  object Base58 {

    object Prefix {
      val PubkeyAddress: Byte = 0.toByte
      val ScriptAddress: Byte = 5.toByte
      val SecretKey: Byte = 128.toByte
      val PubkeyAddressTestnet: Byte = 111.toByte
      val ScriptAddressTestnet: Byte = 196.toByte
      val SecretKeyTestnet: Byte = 239.toByte
      val PubkeyAddressSegnet: Byte = 30.toByte
      val ScriptAddressSegnet: Byte = 50.toByte
      val SecretKeySegnet: Byte = 158.toByte
    }

  }

  final val Coin: Int = 100000000
  final val MaxMoney: Int = Coin * 21000000

  final val MaxScriptElementSize: Int = 520
  final val LockTimeThreshold: Long = 500000L

  val SIGHASH_ALL = 1
  val SIGHASH_NONE = 2
  val SIGHASH_SINGLE = 3

  implicit final class BitcoinLong(private val long: Long) extends AnyVal {

    def btc: Double = long / 1e8
  }

  implicit final class ZFill(private val that: String) extends AnyVal {

    def zfill: String =
      String.format("%65s", that).replace(" ", "0")
  }

}
