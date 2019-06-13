package models

import java.io.InputStream

/**
  *
  * @param amount amount in Satoshis
  * @param scriptPubKey public key script which sets the conditions for spending this output
  */
case class TxOut(amount: Long, scriptPubKey: Script) {

  def serialize = {

    var result = HashHelper.intToLittleEndian(amount.toInt, 8)
    val rawScriptPubKey = scriptPubKey.serialize.toArray
    result = result ++ rawScriptPubKey
    result = result ++ HashHelper.encodeVarint(rawScriptPubKey.length)
    result ++ rawScriptPubKey

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

  def parse(stream: InputStream): TxOut = {

    val amtBuf = new Array[Byte](8)
    stream.read(amtBuf)
    val amt = HashHelper.littleEndianToInt(amtBuf)
    val scriptPubkeyLength = HashHelper.readVarint(stream)
    val scriptPubKey = new Array[Byte](scriptPubkeyLength.toInt)
    stream.read(scriptPubKey)
    val script = Script(scriptPubKey)
    TxOut(amt, script)
  }
}
