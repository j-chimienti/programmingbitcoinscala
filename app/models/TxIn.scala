package models

import scodec.bits.ByteVector
import java.io.{ByteArrayInputStream, InputStream}

import services.TransactionService

import scala.language.postfixOps
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
                prevIdx: Int = 0,
                scriptSig: Script,
                sequence: Long = 0) {

  override def toString: String = s"TxIn(${prevTx.toHex}, $prevIdx)"

  def serialize = {
    var result = prevTx.reverse.toArray
    result = result ++ ByteVector.empty.toArray
    val rawScriptSig = scriptSig.serialize.toArray
    result = result ++ HashHelper.encodeVarint(rawScriptSig.length)

    // fixme
    //result = result ++ rawScriptSig
    result = result ++ HashHelper.intToLittleEndian(sequence.toInt, 4)
    result

  }
  def value(testnet: Boolean = false): Future[Long] = {

    for {
      tx <- TransactionService.fetchTx(prevTx.toHex, testnet)
    } yield tx.outputs(prevIdx).amount

  }
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
    // prev tx id is 32 bytes in little endian order
    val prevTxBuf = new Array[Byte](32)
    stream.read(prevTxBuf)
    val prevTx = ByteVector(prevTxBuf).reverse

    // tx index 4 bytes
    val prevIdxArr = new Array[Byte](4)
    stream.read(prevIdxArr)
    val prevIdx: Int = HashHelper.littleEndianToInt(prevIdxArr)
    // script sig is a varible length
    val scriptSigLength = HashHelper.readVarint(stream)
    val scriptSigBuf = new Array[Byte](scriptSigLength.toInt)
    stream.read(scriptSigBuf)
    val scriptSig = Script(scriptSigBuf)

    // sequence is 4 bytes, little-endian, as int
    val seqBuf = new Array[Byte](4)
    stream.read(seqBuf)
    val sequence = HashHelper.littleEndianToInt(seqBuf)
    TxIn(prevTx, prevIdx, scriptSig, sequence)
  }

}
