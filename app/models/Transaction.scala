package models

import scodec.bits.ByteVector
import java.io.{ByteArrayInputStream, InputStream}

case class Transaction(version: Long,
                       inputs: Seq[TxIn],
                       outputs: Seq[TxOut],
                       locktime: Long) {

  def validate = {

    require(inputs.nonEmpty, "Inputs cannot be empty")
    require(outputs.nonEmpty, "Outputs cannot be empty")
    require(
      outputs.map(_.amount).sum < MaxMoney,
      "Sum of outputs amount is invalid"
    )

  }

  override def toString: String = {
    var tx_ins = ""
    for (tx_in <- tx_ins) tx_ins += tx_in.toString + '\n'
    var tx_outs = ""
    for (tx_out <- tx_outs) tx_outs += tx_out.toString + '\n'
    s"version: $version\ntx_ins:\n$inputs\ntx_outs:\n$outputs\nlocktime: $locktime\n"
  }

  def isCoinbase = inputs.length == 1 && inputs.head.prevIdx == 0xffffffff
  //def hash = HashHelper.hash256(serialize).reverse

//  def serialize = {
//    var result = HashHelper.intToLittleEndian(version, 4)
//    result += HashHelper.encodeVarint(inputs.length)
//    for (txIn <- inputs) {
//      result += txIn.serialize()
//    }
//    result += HashHelper.encodeVarint(outputs.length)
//    for (txOut <- outputs) {
//      result += txOut.serialize
//    }
//    result += HashHelper.intToLittleEndian(locktime)
//
//  }

  /**
    *   Fee of the tx in satoshi
    * @param testnet
    * @return
    */
//  def fee(testnet: Boolean = false): Int = {
//
//    val inputSum = inputs.map(_.value(testnet = testnet)).sum
//    val outputSum = outputs.map(_.amount).sum
//    inputSum - outputSum
//  }

//  def coinbaseHeight =
//    HashHelper.littleEndianToInt(inputs.head.scriptPubKey)

}

object Transaction {

  // if lockTime >= LOCKTIME_THRESHOLD it is a unix timestamp otherwise it is a block height
  val LOCKTIME_THRESHOLD = 500000000L

  def parse(hexStr: String): Transaction = {

    val stream = new ByteArrayInputStream(
      ByteVector.fromValidHex(hexStr).toArray
    )
    parse(stream)
  }
  def parse(stream: InputStream): Transaction = {
    val version = HashHelper.littleEndianToInt(stream)
    val numInputs = HashHelper.readVarint(stream)
    val inputs: Seq[TxIn] = for (_ <- 1 to numInputs) yield TxIn.parse(stream)
    val numOutputs = HashHelper.readVarint(stream)
    val outputs: Seq[TxOut] = for (_ <- 1 to numOutputs)
      yield TxOut.parse(stream)
    val lockTime = HashHelper.littleEndianToInt(stream.read())
    Transaction(version, inputs, outputs, lockTime)
  }
}
