package object models {

  implicit final class BitcoinLong(private val long: Long) extends AnyVal {
    def btc = long / 1e8
  }
}
