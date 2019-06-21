package services

import java.nio.ByteOrder

import models.{
  HashHelper,
  OP_PUSHDATA,
  PrivateKey,
  S256Point,
  Script,
  ScriptElt,
  Signature,
  Transaction
}
import play.api.libs.json.{JsArray, Json, OFormat}

import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import javax.inject._
import models.HashHelper.{hash256, writeUInt32}
import play.api.Logging
import play.api.cache.AsyncCacheApi
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.ahc.AhcWSClient
import scodec.bits.ByteVector

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.language.implicitConversions

@Singleton
class TransactionService @Inject()(cache: AsyncCacheApi,
                                   applicationLifeCycle: ApplicationLifecycle)
    extends Logging {

  implicit val system: ActorSystem = ActorSystem("TxIn")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  val client: AhcWSClient = AhcWSClient()

  applicationLifeCycle.addStopHook(() => {
    Future.successful(client.close())
  })

  def baseUri(testnet: Boolean = false): String =
    if (testnet) "https://blockstream.info/testnet/api"
    else "https://blockstream.info/api"

  /**
    * Get the tx hex and parse the tx. Store in cache
    * @param txId Transaction id
    * @param testnet
    * @return Transaction
    */
  def fetch(txId: String, testnet: Boolean = false): Future[Transaction] = {

    cache
      .getOrElseUpdate[Transaction](txId) {
        fromService(txId, testnet)
      }

  }

  def fromService(txId: String, testnet: Boolean): Future[Transaction] = {

    val url = baseUri(testnet) + s"/tx/$txId/hex"
    for {
      response <- client
        .url(url)
        .withRequestTimeout(3 seconds)
        .get()
    } yield {
      response.status match {
        case 200 =>
          val hexStr = response.body
          val tx = Transaction.parse(hexStr)
          tx
        case _ =>
          val e =
            s"fetch($txId, $testnet): status = ${response.status}, response = ${response.body}"
          println(e)
          throw new RuntimeException(e)
      }
    }
  }

  def post(tx: String, testnet: Boolean = false): Future[String] = {

    val url = baseUri(testnet) + s"/tx"

    client
      .url(url)
      .withRequestTimeout(5 seconds)
      .post(tx)
      .map(response => {

        println(response.body)
        response.body
      })
  }

  def value(txId: String,
            index: Int,
            testnet: Boolean = false): Future[Long] = {

    for {
      tx <- fetch(txId, testnet)
    } yield tx.txOut(index).amount

  }

  def scriptPubKey(tx: Transaction, index: Int): Future[ByteVector] =
    scriptPubKey(tx.txId.toHex, index, tx.testnet)

  /**
    *   Get the scriptPubKey by looking up tx hash on server
    * @param testnet bitcoin network
    * @return the binary scriptpubkey
    */
  def scriptPubKey(txId: String,
                   index: Int,
                   testnet: Boolean = false): Future[ByteVector] = {

    for {
      tx <- fetch(txId, testnet)
    } yield tx.txOut(index).scriptPubKey

  }

  /**
    *
    * @param index
    * @param hashType
    * @return the integer of the hash that needs to get signed for index input_index
    */
  def sigHash(tx: Transaction, index: Int, hashType: Int) = {

    val altTxIns = for (trans <- tx.txIn)
      yield trans.copy(scriptSig = ByteVector.empty)
    val input = altTxIns(index)
    for {
      scriptPubKey <- scriptPubKey(tx, index)
    } yield {
      val txIns =
        altTxIns.updated(index, input.copy(scriptSig = scriptPubKey))
      val tx1 = tx.copy(txIn = txIns)
      val result = Transaction.serialize(tx1) ++ writeUInt32(hashType)
      val hash = hash256(result)
      (tx1, hash)

    }
  }

  def fee(tx: Transaction): Future[Long] = fee(tx, tx.testnet)

  /**
    *   Fee of the tx in satoshi
    * @param testnet
    * @return
    */
  def fee(tx: Transaction, testnet: Boolean = false): Future[Long] = {

    val list =
      tx.txIn.map(input => value(input.prevTxId.toHex, input.prevIdx, testnet))
    for {
      txInFound <- Future.sequence(list)

    } yield {
      if (tx.txIn.length != txInFound.length)
        throw new RuntimeException(s"Error fetching tx fee")
      val txInSum = txInFound.sum
      val txOutSum = tx.txOut.map(_.amount).sum
      txInSum - txOutSum
    }
  }

  def verifyInput(transaction: Transaction, index: Int): Future[Boolean] = {

    val txIn = transaction.txIn(index)
    val pubKey = txIn.secPubKey()
    val point = S256Point.parse(pubKey)
    val der = txIn.derSignature()
    val signature = Signature.parse(der)
    val ht = txIn.hashType()

    // fixme: combined = tx_in.script_sig + script_pubkey
    //        # evaluate the combined script
    //        return combined.evaluate(z)
    sigHash(transaction, index, ht)
      .map(result => point.verify(result._2, signature))

  }

  def verify(tx: Transaction) = {

    fee(tx) map { fee =>
      if (fee < 0) FastFuture.successful(false)
      val who = tx.txIn.indices.map(idx => verifyInput(tx, idx))
      for {
        isVerifiedList <- Future.sequence(who)
      } yield {
        val foundUnverified: Option[Boolean] =
          isVerifiedList.find(isVerified => !isVerified)
        if (foundUnverified.isDefined) false else true
      }

    }

  }

  /** fixme p2sh
    * Sign the input w/ the Private Key
    * @param index
    * @param privateKey
    * @param hashType
    * @return
    */
  def signInput(tx: Transaction,
                index: Int,
                privateKey: PrivateKey,
                hashType: Int): Future[Future[Transaction]] = {

    sigHash(tx, index, hashType).map(
      result => {
        val z = result._2
        val tx1 = result._1
        val signature = privateKey.sign(z)
        val der = signature.der
        val sig = ByteVector(der :+ hashType.toByte)
        val publicKey = privateKey.publicKey.sec(compressed = true)
        val scriptSig: Seq[ScriptElt] = OP_PUSHDATA(sig) :: OP_PUSHDATA(
          publicKey
        ) :: Nil
        val ss = Script.serialize(scriptSig)
        val tx2 = tx1.copy(
          txIn = tx.txIn
            .updated(index, tx.txIn(index).copy(scriptSig = ss))
        )
        for {

          isVerified <- verifyInput(tx2, index)
        } yield {

          println(s"is verified = $isVerified")
          // require(isVerified)
          tx2
        }

      }
    )
  }

  def coinbaseHeight(tx: Transaction) = {

//    if (!tx.isCoinbase) {
//      false
//    }
//    val foo = tx.txIn.head.scriptSig.head
//    HashHelper.uint32(foo, ByteOrder.LITTLE_ENDIAN)
  }

}

/// JSON obj for tx

case class Prevout(scriptpubkey: String,
                   scriptpubkey_asm: String,
                   scriptpubkey_address: String,
                   scriptpubkey_type: String,
                   value: Double)
case class Vin(txid: String,
               vout: Double,
               prevout: Prevout,
               scriptsig: String,
               scriptsig_asm: String,
               witness: Option[String],
               is_coinbase: Boolean,
               sequence: Double)
case class Status(confirmed: Boolean,
                  block_height: Double,
                  block_hash: String,
                  block_time: Double)

case class TransactionJson(txid: String,
                           version: Double,
                           locktime: Double,
                           vin: List[Vin],
                           vout: List[Prevout],
                           size: Double,
                           weight: Double,
                           fee: Double,
                           status: Status)

object TransactionJson {

  implicit val rwtx: OFormat[Prevout] = Json.format[Prevout]
  implicit val rwtx1: OFormat[Vin] = Json.format[Vin]
  implicit val rwtx2: OFormat[Status] = Json.format[Status]
  implicit val rwtx3: OFormat[TransactionJson] = Json.format[TransactionJson]
}
