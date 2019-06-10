package models

import java.io.InputStream

import scodec.bits.ByteVector

case class TxOut(amount: Long, scriptPubKey: ByteVector) {

  def hex = ""

  override def toString = s"TxOut($amount, $scriptPubKey)"

  def validate = {

    require(amount >= 0, s"Invalid Tx amount = $amount")
    require(amount < MaxMoney, s"Invalid Tx amout = $amount")
    require(
      scriptPubKey.length > MaxScriptElementSize,
      s"Invalid scriptPubkey script length = ${scriptPubKey.length} limit  = $MaxScriptElementSize bytes"
    )

  }
}

object TxOut {

  def parse(stream: InputStream): TxOut = {

    val buf = Array[Byte](8)
    val amt = HashHelper.littleEndianToInt(stream.read(buf))
    val scriptPubkeyLength = HashHelper.readVarint(stream)
    val buf2 = new Array[Byte](scriptPubkeyLength)
    val scriptPubKey = stream.read(buf2)
    TxOut(amt, ByteVector.fromLong(scriptPubKey))
  }
}
