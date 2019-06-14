package models

import java.io.{InputStream, OutputStream}

import fr.acinq.bitcoin.Protocol.writeUInt64
import HashHelper._
import scodec.bits.ByteVector

/**
  *
  * @param amount amount in Satoshis
  * @param scriptPubKey public key script which sets the conditions for spending this output
  */
case class TxOut(amount: Long, scriptPubKey: ByteVector) {

  def serialize(out: OutputStream) = {

    writeUInt64(amount, out)
    writeScript(scriptPubKey.toArray, out)
  }
  override def toString = s"TxOut($amount, $scriptPubKey)"

  def validate = {

    require(amount >= 0, s"Invalid Tx amount = $amount")
    require(amount < MaxMoney, s"Invalid Tx amout = $amount")
//    require(
//      scriptPubKey.length > MaxScriptElementSize,
//      s"Invalid scriptPubkey script length = ${scriptPubKey.length} limit  = $MaxScriptElementSize bytes"
//    )

  }
}

object TxOut {

  def apply(amount: Long, scriptPubKey: Seq[ScriptElt]): TxOut =
    new TxOut(amount, Script.write(scriptPubKey))

  def parse(stream: InputStream): TxOut = {

    val amt = uint64(stream)
    val elems = script(stream)
    TxOut(amt, elems)
  }
}
