package models

import java.io.{InputStream, OutputStream}

import models.HashHelper._
import scodec.bits.ByteVector
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

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
  * @param scriptSig (unlocks scriptPubKey) Signature script which should match the public key script of the output that we want to spend
  * @param sequence Transaction version defined by sender.
  */
case class TxIn(prevTx: ByteVector32,
                prevIdx: Int = 0,
                scriptSig: ByteVector = ByteVector.empty,
                sequence: Long = 0xFFFFFFFFL)
    extends BtcSerializable[TxIn] {

  require(prevIdx >= -1)

  override def serializer: BtcSerializer[TxIn] = TxIn
  def prevTxId: ByteVector = prevTx.reverse

  override def toString: String = s"TxIn(${prevTxId.toHex}, $prevIdx)"

  def derSignature(idx: Int = 0): ByteVector =
    Script(scriptSig).signature(idx).dropRight(1)

  def hashType(idx: Int = 0): Byte =
    Script(scriptSig).signature(idx).last

  /**
    *
    * @param idx
    * @return SEC format public if the scriptSig has one
    */
  def secPubKey(idx: Int = 0): ByteVector = {

    val script = Script(scriptSig)
    Script.secPubkey(script, idx)
  }

  def redeemScript =
    Script(scriptSig).redeemScript

  def serialize: ByteVector = TxIn.serialize(this)

}

object TxIn extends BtcSerializer[TxIn] {

  def apply(prevTx: String, index: Int): TxIn =
    TxIn(
      ByteVector32(ByteVector(ByteVector.fromValidHex(prevTx).toArray.reverse)),
      index
    )

  def apply(prevTx: String,
            index: Int,
            scriptSig: ByteVector,
            sequence: Long): TxIn =
    TxIn(
      ByteVector32(ByteVector(ByteVector.fromValidHex(prevTx).toArray.reverse)),
      index,
      scriptSig,
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

  override def serialize(tx: TxIn, out: OutputStream): Unit = {
    out.write(tx.prevTx.toArray)
    writeUInt32(tx.prevIdx, out)
    writeScript(tx.scriptSig.toArray, out)
    writeUInt32(tx.sequence.toInt, out)
  }

  override def validate(input: TxIn): Unit =
    require(
      input.scriptSig.length <= MaxScriptElementSize,
      s"signature script is ${input.scriptSig.length} bytes, limit is $MaxScriptElementSize bytes"
    )

}
