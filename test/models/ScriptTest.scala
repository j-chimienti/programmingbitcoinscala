package models

import java.io.ByteArrayInputStream

import org.scalatest.FlatSpec
import scodec.bits.ByteVector

import scala.util.Random

class ScriptTest extends FlatSpec {

  behavior of "ScriptTest"

  it should "parse" in {}

  it should "secPubkey" in {}

  it should "p2pkh" in {

    val script_pubkey_raw = "76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac"

    val stream = new ByteArrayInputStream(
      ByteVector
        .fromValidHex(script_pubkey_raw)
        .toArray
    )
    val script_pubkey: Script = Script(stream)
    assert(script_pubkey.`type` == "p2pkh")
    assert(script_pubkey.serialize.toHex == script_pubkey_raw)

    val script_sig_raw =
      "483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"

    val stream2 = new ByteArrayInputStream(
      ByteVector
        .fromValidHex(script_sig_raw)
        .toArray
    )
    val script_sig = Script(stream2)

    assert(script_sig.`type` == "p2pkh sig")
    assert(script_sig.serialize.toHex == script_sig_raw)

    val s = script_sig.signature(0)
    val sig = s.asInstanceOf[OP_PUSHDATA].data
    assert(
      sig.toHex == "3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01"
    )
    assert(
      script_sig
        .secPubkey()
        .asInstanceOf[OP_PUSHDATA]
        .data
        .toHex == "0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
    )

  }

  it should "p2sh" in {

    val h = "a91474d691da1574e6b3c192ecfb52cc8984ee7b6c5687"
    val stream = new ByteArrayInputStream(ByteVector.fromValidHex(h).toArray)
    val script = Script(stream)
    assert(script.`type` == "p2sh")
    assert(script.serialize.toHex == h)

    val h2 =
      "00483045022100dc92655fe37036f47756db8102e0d7d5e28b3beb83a8fef4f5dc0559bddfb94e02205a36d4e4e6c7fcd16658c50783e00c341609977aed3ad00937bf4ee942a8993701483045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8eef53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e75402201475221022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb702103b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb7152ae"
    val stream2 = new ByteArrayInputStream(ByteVector.fromValidHex(h2).toArray)
    val script2 = Script(stream2)
    assert(script2.`type` == "p2sh sig")
    assert(script2.serialize.toHex == h2)

    val sig = script2.signature(0).asInstanceOf[OP_PUSHDATA].data.toHex
    val sig2 = script2.signature(1).asInstanceOf[OP_PUSHDATA].data.toHex
    assert(
      sig == "3045022100dc92655fe37036f47756db8102e0d7d5e28b3beb83a8fef4f5dc0559bddfb94e02205a36d4e4e6c7fcd16658c50783e00c341609977aed3ad00937bf4ee942a8993701"
    )
    assert(
      sig2 == "3045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8eef53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e75402201"
    )

    assert(
      script2
        .secPubkey()
        .asInstanceOf[OP_PUSHDATA]
        .data
        .toHex == "022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb70"
    )
    assert(
      script2
        .secPubkey(1)
        .asInstanceOf[OP_PUSHDATA]
        .data
        .toHex == "03b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb71"
    )
  }

}
