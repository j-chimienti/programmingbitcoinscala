package models

import scodec.bits.ByteVector
import java.io.{ByteArrayInputStream, InputStream}
import scala.language.postfixOps
import scala.concurrent.duration._
import play.api.libs.ws._
import akka.actor.ActorSystem
import scala.collection.mutable.Map
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import play.api.libs.json.Json
import play.api.libs.ws.ahc.AhcWSClient

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  *
  * Nodes must verify that it's not spending bitcoins that don't exist
  *
  * @param prevTx
  * @param prevIdx
  * @param scriptSig
  * @param sequence
  */
case class TxIn(prevTx: ByteVector,
                prevIdx: Long = 0,
                scriptSig: ByteVector = ByteVector.empty,
                sequence: Long = 0) {
  implicit val system: ActorSystem = ActorSystem("TxIn")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  val client: AhcWSClient = AhcWSClient()

  override def toString: String = s"TxIn(${prevTx.toHex}, $prevIdx)"

//  def serialize = {
//    var result = prevTx.reverse
//    result += ""
//    val rawScriptSig = scriptSig.serialize
//    result += HashHelper.encodeVarint(rawScriptSig.length)
//    result += rawScriptSig
//    result += HashHelper.intToLittleEndian(sequence, 4)
//    result
//
//  }

  val cache = Map.empty[String, Transaction]

  def uri(testnet: Boolean = false) =
    if (testnet)
      "http://client:pleasedonthackme@tbtc.programmingblockchain.com:18332"
    else "http://pbclient:ecdsaisawesome@btc.programmingblockchain.com:8332"

  def fetchTx(testnet: Boolean = false): Future[Transaction] = {

    if (cache.contains(prevTx.toHex))
      FastFuture.successful[Transaction](cache(prevTx.toHex))
    else {
      val url = uri(testnet)
      val data = Json.obj(
        "jsonrpc" -> "2.0",
        "method" -> "getrawtransaction",
        "params" -> prevTx.toHex,
        "id" -> "0"
      )
      client
        .url(url)
        .addHttpHeaders("accept" -> "application/json")
        .withRequestTimeout(1 second)
        .post(data)
        .map(response => {
          val j = response.json
          val HEX = (j \ "result").as[String]
          val raw = ByteVector.fromValidHex(HEX)
          val stream = new ByteArrayInputStream(raw.toArray)
          val tx = Transaction.parse(stream)
          cache(prevTx.toHex) = tx
          tx

        })

    }
  }

//  def value(testnet: Boolean = false): Long = {
//
//    val tx = fetchTx(testnet)
//    tx.outputs(prevIdx).amount
//  }
//  def scriptPubKey(testnet: Boolean = false) = {
//
//    val tx = fetchTx(testnet)
//    tx.outputs(prevIdx).scriptPubKey
//  }
//
//  def derSignature(idx: Int = 0) = {
//
//    val signature = scriptSig.signature(idx)
//    signature.dropRight(1)
//  }
//
//  def hashType(idx: Int = 0) =
//    scriptSig.signature(idx).last
//
//  def secPubKey(idx: Int = 0) = scriptSig.secPubKey(idx)
//
//  def redeemScript = scriptSig.redeemScript

}

object TxIn {

  def apply(stream: InputStream): TxIn = TxIn.parse(stream)
  def apply(hash: String): TxIn = TxIn.parse(hash)

  def parse(hash: String): TxIn =
    TxIn.parse(new ByteArrayInputStream(ByteVector.fromValidHex(hash).toArray))

  /**
    * Takes a byte stream and parses the tx_input at the start
    * return a TxIn object
    *
    * @param stream
    * @return
    */
  def parse(stream: InputStream): TxIn = {
    val prevTxBuf = Array[Byte](32)
    stream.read(prevTxBuf)
    val prevTx = prevTxBuf.reverse
    val buff = Array[Byte](4)
    val prevIdx = HashHelper.littleEndianToInt(stream.read(buff))
    val scriptSigLength = HashHelper.readVarint(stream)
    val bufff = new Array[Byte](scriptSigLength)
    stream.read(bufff)
    val buf4 = Array[Byte](4)
    val sequence = HashHelper.littleEndianToInt(stream.read(buf4))
    TxIn(ByteVector.view(prevTx), prevIdx, ByteVector.empty, sequence)
  }

}
