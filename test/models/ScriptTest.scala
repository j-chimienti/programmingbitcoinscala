package models

import java.io.ByteArrayInputStream

import org.scalatest.FlatSpec
import scodec.bits.ByteVector

import scala.util.Random

class ScriptTest extends FlatSpec {

  behavior of "ScriptTest"

  it should "parse" in {}

  it should "secPubkey" in {}

//  it should "p2pkh" in {
//
//    val script_pubkey_raw = new ByteArrayInputStream(
//      ByteVector
//        .fromValidHex("76a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac")
//        .toArray
//    )
//    val script_pubkey = Script.parse(script_pubkey_raw)
//    assert(script_pubkey.`type` == "p2pkh")
//    assert(script_pubkey.serialize() == script_pubkey_raw)
//
//    val script_sig_raw = new ByteArrayInputStream(
//      ByteVector
//        .fromValidHex(
//          "483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
//        )
//        .toArray
//    )
//    val script_sig = Script.parse(script_sig_raw)
//    assert(script_sig.`type` == "p2pkh sig")
//    assert(script_sig.serialize() == script_sig_raw)
//    assert(
//      script_sig
//        .signature() == hex"3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01"
//    )
//    assert(
//      script_sig
//        .sec_pubkey() == hex"0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a"
//    )
//
//  }

}
