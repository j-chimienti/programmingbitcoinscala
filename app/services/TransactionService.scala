package services

import models.Transaction
import play.api.libs.json.{JsArray, Json, OFormat}

import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import akka.stream.ActorMaterializer
import play.api.Logging
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.collection.mutable

//
//@Singleton
//class TransactionService @Inject()(cache: AsyncCacheApi) extends Logging {

object TransactionService extends Logging {

  implicit val system: ActorSystem = ActorSystem("TxIn")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  val client: AhcWSClient = AhcWSClient()

  val LOG = logger.logger

  var cache: mutable.Map[String, Transaction] =
    mutable.Map.empty[String, Transaction]

  def baseUri(testnet: Boolean = false): String =
    if (testnet) "https://blockstream.info/testnet/api"
    else "https://blockstream.info/api"

  /**
    * Get the tx hex and parse the tx. Store in cache
    * @param txId Transaction id
    * @param testnet
    * @return Transaction
    */
  def fetch(txId: String, testnet: Boolean = false) = {

    cache.get(txId) match {
      case Some(tx) =>
        LOG.info(s"Cached $txId")
        FastFuture.successful(tx)
      case None =>
        fromService(txId, testnet).map { tx =>
          cache(tx.txId.toHex) = tx
          tx
        }
    }

  }

  def fromService(txId: String, testnet: Boolean) = {

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
          // note: to set expiry duration add duration as 3rd param
          tx
        case _ =>
          val e =
            s"fetch($txId, $testnet): status = ${response.status}, response = ${response.body}"
          LOG.error(e)
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
//        result.status match {
//
//          case
//        }
      })
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
