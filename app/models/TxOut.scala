package models

import java.io.{InputStream, OutputStream}

import fr.acinq.bitcoin.Protocol.writeUInt64
import models.HashHelper._
import scodec.bits.ByteVector

/**
  *
  * @param amount amount in Satoshis
  * @param scriptPubKey (lockbox) public key script which sets the conditions for spending this output
  *                     tells where btc should go
  */
case class TxOut(amount: Long, scriptPubKey: ByteVector)
    extends BtcSerializable[TxOut] {

  override def toString = s"TxOut($amount, $scriptPubKey)"

  override def serializer: BtcSerializer[TxOut] = TxOut

}

object TxOut extends BtcSerializer[TxOut] {

  def apply(amount: Long, scriptPubKey: Seq[ScriptElt]): TxOut =
    new TxOut(amount, Script.serialize(scriptPubKey))

  def parse(stream: InputStream): TxOut = {
    val amt = uint64(stream)
    val elems = script(stream)
    TxOut(amt, elems)
  }

  override def serialize(tx: TxOut, out: OutputStream) = {
    writeUInt64(tx.amount, out)
    writeScript(tx.scriptPubKey.toArray, out)
  }

  override def validate(tx: TxOut) = {

    val amount = tx.amount

    require(amount >= 0, s"Invalid Tx amount = $amount")
    require(amount < MaxMoney, s"Invalid Tx amout = $amount")
    //    require(
    //      scriptPubKey.length > MaxScriptElementSize,
    //      s"Invalid scriptPubkey script length = ${scriptPubKey.length} limit  = $MaxScriptElementSize bytes"
    //    )

  }
}
