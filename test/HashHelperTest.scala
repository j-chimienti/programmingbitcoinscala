import java.nio.charset.StandardCharsets

import fr.acinq.bitcoin.{Base58Check, Crypto}
import models.{ByteSwapper, HashHelper}
import org.scalatest.{FlatSpec, FunSuite}
import scodec.bits.ByteVector

class HashHelperTest extends FlatSpec {

  import models.HashHelper._

  behavior of "FlatSpec"

  def assertEqual[T](a: T, b: T): Unit = assert(a == b)

  it should "bytes" in {

    val b = "hello world".getBytes(StandardCharsets.US_ASCII)
    val s = "hello world"
    assertEqual(b.deep, str_to_bytes(s).deep)
    assert(BigInt(b) == BigInt(str_to_bytes(s)))
    assert(s == bytes_to_str(b))
  }

  it should "littleEndianToInt" in {

    val h = "99c3980000000000"
    val want = BigInt(10011545)
    val result = littleEndianToInt(h)
    assert(result == want)
    val h1 = "a135ef0100000000"
    val want1 = BigInt(32454049)
    assert(littleEndianToInt(h1) == want1)
  }

  it should "base58" in {
    val addr = "mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf"
    val (version, h160) = base58(addr)
    val want = "507b27411ccf7f16f10297de6cef3f291623eddf"
    assert(h160.toHex == want)
    val got = Base58Check.encode(version, h160)
    assert(got == addr)
  }

  it should "p2pkh_address" in {
    val h160 =
      ByteVector.fromValidHex("74d691da1574e6b3c192ecfb52cc8984ee7b6c56")
    val want = "1BenRpVUFK65JFWcQSuHnJKzc4M8ZP8Eqa"
    assert(h1602p2pkh(h160, testnet = false) == want)
    val want1 = "mrAjisaT4LXL5MzE81sfcDYKU3wqWSvf9q"
    assert(h1602p2pkh(h160, testnet = true) == want1)
  }

  it should "p2sh_address" in {
    val h160 =
      "74d691da1574e6b3c192ecfb52cc8984ee7b6c56"
    val want = "3CLoMMyuoDQTPRD3XYZtCvgvkadrAdvdXh"
    assert(h1602p2sh(h160, testnet = false) == want)
    val want1 = "2N3u1R6uwQfuobCqbCgBkpsgBxvr1tZpe7B"
    assert(h1602p2sh(h160, testnet = true) == want1)
  }
}
