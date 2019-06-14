package models

import scodec.bits.ByteVector
import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  InputStream,
  OutputStream
}

import fr.acinq.bitcoin.{ByteVector32, MaxScriptElementSize, OutPoint}
import services.TransactionService

import HashHelper._
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//case class PrevTx(hash: ByteVector32, index: Long = 0) {
//
//  require(index >= -1)
//
//  val txId = hash.reverse
//  def write(input: OutPoint, out: OutputStream, protocolVersion: Long) = {
//    out.write(input.hash.toArray)
//    writeUInt32(input.index.toInt, out)
//  }
//}
//
//object PrevTx {}

/**
  *
  * Transaction input
  *
  * @param prevTx Previous output transaction reference
  * @param prevIdx Previous index of transaction output reference
  * @param scriptSig Signature script which should match the public key script of the output that we want to spend
  * @param sequence Transaction version defined by sender.
  */
case class TxIn(prevTx: ByteVector32,
                prevIdx: Int = 0,
                scriptSig: ByteVector,
                sequence: Long = 0xFFFFFFFFL) {

  require(prevIdx >= -1)

  lazy val txId: ByteVector = prevTx.reverse

  override def toString: String = s"TxIn($txId, $prevIdx)"

  def validate(input: TxIn): Unit =
    require(
      scriptSig.length <= MaxScriptElementSize,
      s"signature script is ${input.scriptSig.length} bytes, limit is $MaxScriptElementSize bytes"
    )

  // fixme
  def serialize(out: OutputStream) = {

    out.write(prevTx.toArray)
    writeUInt32(prevIdx, out)
    writeScript(scriptSig.toArray, out)
    writeUInt32(sequence.toInt, out)

  }
  def value(testnet: Boolean = false): Future[Option[Long]] = {

    for {
      txOpt <- TransactionService.fetch(txId.toHex, testnet)
    } yield
      txOpt match {
        case None     => None
        case Some(tx) => Some(tx.outputs(prevIdx).amount)

      }

  }
  def scriptPubKey(testnet: Boolean = false): Future[Option[ByteVector]] = {

    for {
      txOpt <- TransactionService.fetch(prevTx.toHex, testnet)
    } yield
      txOpt match {
        case Some(tx) => Some(tx.outputs(prevIdx).scriptPubKey)
        case None     => None
      }

  }

//  def derSignature(idx: Int = 0) = {
//
//    scriptSig.signature(idx) match {
//
//      case signature: OP_PUSHDATA => signature.data.dropRight(1)
//
//    }
//
//  }
//
//  def hashType(idx: Int = 0) = {
//    scriptSig.signature(idx) match {
//      case op: OP_PUSHDATA => op.data.last
//    }
//  }
//
  //def secPubKey(idx: Int = 0) = scriptSig.secPubkey(idx)

  //def redeemScript = scriptSig.redeemScript

}

object TxIn {

  def apply(prevTx: String, index: Int, sequence: Long): TxIn = new TxIn(
    ByteVector32(ByteVector(ByteVector.fromValidHex(prevTx).toArray.reverse)),
    index,
    ByteVector.empty,
    sequence
  )
  def apply(stream: InputStream): TxIn = TxIn.parse(stream)

  /**
    * Takes a byte stream and parses the tx_input at the start
    * return a TxIn object
    *
    * @param stream
    * @return
    */
  def parse(stream: InputStream): TxIn = {
    val prevTxId = hash(stream)
    val prevTxIdx = uint32(stream)
    val sigScript = script(stream)
    val sequence = uint32(stream)
    TxIn(prevTxId, prevTxIdx.toInt, sigScript, sequence)

  }

}
