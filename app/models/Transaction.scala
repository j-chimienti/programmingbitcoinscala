package models

import fr.acinq.bitcoin.{TxIn, TxOut}
import scodec.bits.ByteVector

case class Transaction(version: Int,
                       input: List[TxIn],
                       outputs: List[TxOut],
                       locktime: Long) {

  override def toString: String = {

    var tx_ins = ""
    for (tx_in <- tx_ins) tx_ins += tx_in.toString + '\n'
    var tx_outs = ""
    for (tx_out <- tx_outs) tx_outs += tx_out.toString + '\n'
    s"version: ${version}\ntx_ins:\n${tx_ins}\ntx_outs:\n${tx_outs}\nlocktime: ${locktime}\n"
  }
  //def hash = HashHelper.hash256(serialize).reverse
}

object Transaction {

  // if lockTime >= LOCKTIME_THRESHOLD it is a unix timestamp otherwise it is a block height
  val LOCKTIME_THRESHOLD = 500000000L
//  def parse(ser: InputStream) = {
//
//    val version = HashHelper.littleEndianToInt(ser.take(4).toHex)
//
//    val numInputs = HashHelper
//
//    Transaction(version, )
//  }
}
