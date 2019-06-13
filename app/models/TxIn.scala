package models

import scodec.bits.ByteVector
import java.io.{ByteArrayInputStream, InputStream}
import services.TransactionService
import scala.language.postfixOps
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  *
  * Transaction input
  *
  * @param prevTx Previous output transaction reference
  * @param prevIdx Previous index of transaction output reference
  * @param scriptSig Signature script which should match the public key script of the output that we want to spend
  * @param sequence Transaction version defined by sender.
  */
case class TxIn(prevTx: ByteVector,
                prevIdx: Int = 0,
                scriptSig: Script,
                sequence: Long = 0xFFFFFFFFL) {

  require(prevIdx >= -1)

  override def toString: String = s"TxIn(${prevTx.toHex}, $prevIdx)"

  def serialize = {
    var result = prevTx.reverse.toArray
    result = result ++ ByteVector.empty.toArray
    val rawScriptSig = scriptSig.serialize.toArray
    result = result ++ HashHelper.encodeVarint(rawScriptSig.length)
    result = result ++ rawScriptSig
    result ++ HashHelper.intToLittleEndian(sequence.toInt, 4)

  }
  def value(testnet: Boolean = false): Future[Long] = {

    for {
      tx <- TransactionService.fetch(prevTx.toHex, testnet)
    } yield tx.outputs(prevIdx).amount

  }
  def scriptPubKey(testnet: Boolean = false): Future[Script] = {

    for {
      tx <- TransactionService.fetch(prevTx.toHex, testnet)
    } yield {

      tx.outputs(prevIdx).scriptPubKey
    }
  }

  def derSignature(idx: Int = 0) = {

    scriptSig.signature(idx) match {

      case signature: OP_PUSHDATA => signature.data.dropRight(1)

    }

  }

  def hashType(idx: Int = 0) = {
    scriptSig.signature(idx) match {
      case op: OP_PUSHDATA => op.data.last
    }
  }

  def secPubKey(idx: Int = 0) = scriptSig.secPubkey(idx)

  //def redeemScript = scriptSig.redeemScript

}

object TxIn {

  def apply(stream: InputStream): TxIn = TxIn.parse(stream)
  def apply(hash: String): TxIn = TxIn.parse(hash)

  def apply(hash: String, index: Int, script: Script, locktime: Long): TxIn =
    TxIn(ByteVector.fromValidHex(hash), index, script, locktime)

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
    // prev tx id is 32 bytes in little endian order
    val prevTxBuf = new Array[Byte](32)
    stream.read(prevTxBuf)
    val prevTx = ByteVector(prevTxBuf).reverse

    // tx index 4 bytes
    val prevIdxArr = new Array[Byte](4)
    stream.read(prevIdxArr)
    val prevIdx: Long = HashHelper.littleEndianToInt(prevIdxArr)
    // script sig is a varible length
    val scriptSigLength = HashHelper.readVarint(stream)
    val scriptSigBuf = new Array[Byte](scriptSigLength.toInt)
    stream.read(scriptSigBuf)
    val scriptSig = Script(scriptSigBuf)

    // sequence is 4 bytes, little-endian, as int
    val seqBuf = new Array[Byte](4)
    stream.read(seqBuf)
    val sequence = HashHelper.littleEndianToInt(seqBuf)
    TxIn(prevTx, prevIdx.toInt, scriptSig, sequence)
  }

}
