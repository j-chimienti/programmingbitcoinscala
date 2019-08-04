package services

import models._
import play.api.libs.json.{Json, OFormat}

import scala.concurrent.duration._
import akka.http.scaladsl.util.FastFuture
import javax.inject._
import models.HashHelper.{ByteVector32, hash256, writeUInt32}
import play.api.Logging
import play.api.cache.AsyncCacheApi
import play.api.libs.ws.WSClient
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.language.implicitConversions

@Singleton
class TransactionService @Inject()(cache: AsyncCacheApi, client: WSClient)(
  implicit ec: ExecutionContext
) extends Logging {

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
      .getOrElseUpdate[Transaction](txId, 30 seconds) {
        fromService(txId, testnet)
      }

  }

  def block(blockHex: String,
            testnet: Boolean = false): Future[BlockstreamBlock] = {
    cache.getOrElseUpdate[BlockstreamBlock](blockHex, Duration.Inf) {
      fetchBlock(blockHex, testnet)
    }
  }

  /**
    *
    *
    * The response from this endpoint can be cached indefinitely.
    *
    * @param str
    * @param testnet
    * @return Returns information about a block.
    */
  def fetchBlock(str: String, testnet: Boolean): Future[BlockstreamBlock] = {

    val url = baseUri(testnet) + s"/block/$str"
    for {
      response <- client
        .url(url)
        //.withRequestTimeout(3 seconds)
        .get()
    } yield
      response.status match {
        case 200 =>
          response.json.asOpt[BlockstreamBlock] match {
            case Some(block) =>
              block
            case _ =>
              throw new RuntimeException("Invalid Request")
          }
        case _ =>
          throw new RuntimeException("Invalid Response")
      }

  }

  def fromService(txId: String, testnet: Boolean): Future[Transaction] = {

    val url = baseUri(testnet) + s"/tx/$txId/hex"
    for {
      response <- client
        .url(url)
        //.withRequestTimeout(3 seconds)
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
      //.withRequestTimeout(5 seconds)
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

  def scriptPubKey(txIn: TxIn, testnet: Boolean): Future[ByteVector] =
    scriptPubKey(txIn.prevTxId.toHex, txIn.prevIdx, testnet)

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
    * Returns the integer representation of the hash that needs to get
    * signed for index input_index
    * @param index
    * @param hashType
    * @return the integer of the hash that needs to get signed for index input_index
    */
  def sigHash(tx: Transaction,
              index: Int,
              hashType: ByteVector,
              redeemScript: Option[String] = None): Future[ByteVector32] = {

    for {
      scriptPubKey <- scriptPubKey(tx.txIn(index), tx.testnet)
    } yield {
      val modTxIns = for ((tIn, i) <- tx.txIn.zipWithIndex)
        yield
          tIn.copy(
            scriptSig = if (index == i) scriptPubKey else ByteVector.empty
          )
      val modTx = tx.copy(txIn = modTxIns)
      HashHelper.hash256(modTx.serialize ++ hashType)
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
    sigHash(transaction, index, mapHashType(ht))
      .map(result => point.verify(result, signature))

  }

  def verify(tx: Transaction): Future[Boolean] = {

    fee(tx) map { fee =>
      if (fee < 0) FastFuture.successful(false)
      val who = tx.txIn.indices.map(idx => verifyInput(tx, idx))
      for {
        isVerifiedList <- Future.sequence(who)
      } yield {
        val foundUnverified: Option[Boolean] =
          isVerifiedList.find(isVerified => !isVerified)
        foundUnverified.isEmpty
      }

    } flatMap { result =>
      result
    }

  }

  /** fixme p2sh
    * Sign the input w/ the Private Key
    * @param index
    * @param privateKey
    * @param hashType
    * @return
    */
//  def signInput(tx: Transaction,
//                index: Int,
//                privateKey: PrivateKey,
//                hashType: Int): Future[Transaction] = {
//
//    sigHash(tx, index, hashType)
//      .map(result => {
//        val z = result._2
//        val tx1 = result._1
//        val signature = privateKey.sign(z)
//        val der = signature.der
//        val sig = ByteVector(der :+ hashType.toByte)
//        val publicKey = privateKey.publicKey.sec(compressed = true)
//        val scriptSig: Seq[ScriptElt] = OP_PUSHDATA(sig) :: OP_PUSHDATA(
//          publicKey
//        ) :: Nil
//        val ss = Script.serialize(scriptSig)
//        val tx2 = tx1.copy(
//          txIn = tx.txIn
//            .updated(index, tx.txIn(index).copy(scriptSig = ss))
//        )
//        for {
//
//          isVerified <- verifyInput(tx2, index)
//        } yield {
//
//          println(s"is verified = $isVerified")
//          // require(isVerified)
//          tx2
//        }
//
//      })
//      .flatMap(result => result)
//  }

  def coinbaseHeight(tx: Transaction): Unit = {

//    if (!tx.isCoinbase) {
//      false
//    }
//    val foo = tx.txIn.head.scriptSig.head
//    HashHelper.uint32(foo, ByteOrder.LITTLE_ENDIAN)
  }

}

object BlockstreamBlock {

  implicit val fm: OFormat[BlockstreamBlock] = Json.format[BlockstreamBlock]
}

/**
  * JSON from blockstream GET /block/:hash
  * @param id
  * @param height
  * @param version
  * @param timestamp
  * @param tx_count
  * @param size
  * @param weight
  * @param merkle_root
  * @param previousblockhash
  * @param nonce
  * @param bits
  */
case class BlockstreamBlock(id: String,
                            height: Double,
                            version: Double,
                            timestamp: Double,
                            tx_count: Double,
                            size: Double,
                            weight: Double,
                            merkle_root: String,
                            previousblockhash: String,
                            nonce: Double,
                            bits: Double)

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
