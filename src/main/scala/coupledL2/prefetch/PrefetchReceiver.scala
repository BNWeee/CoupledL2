/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package coupledL2.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import coupledL2._
import freechips.rocketchip.diplomacy.{BundleBridgeSink, BundleBridgeSource, LazyModule, LazyModuleImp}
import utility.{Pipeline, RegNextN}

import scala.collection.Seq

// TODO: PrefetchReceiver is temporarily used since L1&L2 do not support Hint.
// TODO: Delete this after Hint is accomplished.
class PrefetchRecv extends Bundle {
  val addr = UInt(64.W)
  val addr_valid = Bool()
  val pf_en = Bool()
}
class l2PrefetchRecv extends PrefetchRecv {
}
class l2PrefetchSend extends PrefetchRecv {
}

case class PrefetchReceiverParams(n: Int = 32) extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val inflightEntries: Int = n
}

class PrefetchReceiver()(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO())
  // just ignore train reqs
  io.train.ready := true.B
  io.resp.ready := true.B

  io.req.bits.tag := parseFullAddress(io.recv_addr.bits)._1
  io.req.bits.set := parseFullAddress(io.recv_addr.bits)._2
  io.req.bits.needT := false.B
  io.req.bits.isBOP := false.B
  io.req.bits.source := 0.U // TODO: ensure source 0 is dcache
  io.req.valid := io.recv_addr.valid
  io.evict := DontCare
}

class PrefetchReceiver_llc()(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO())
  io.train:=DontCare
  io.resp:=DontCare

  io.req.valid :=RegNextN(io.recv_addr.valid,3)
  io.req.bits.tag := RegNextN(parseAddress(io.recv_addr.bits)._1,3)
  io.req.bits.set := RegNextN(parseAddress(io.recv_addr.bits)._2,3)
  io.req.bits.needT := RegNextN(false.B,3)
  io.req.bits.isBOP := RegNextN(false.B,3)
  io.req.bits.source := RegNextN(1056.U,3) //FIXME: ensure source id is l3cache mshrIdx
  io.req.ready := DontCare
}

class PrefetchReceiverXbar(val clientNum:Int=2)(implicit p: Parameters) extends LazyModule{
  val inNode = Seq.fill(clientNum)(BundleBridgeSink(Some(() => new coupledL2.prefetch.l2PrefetchSend())))
  val outNode = Seq.fill(1)(BundleBridgeSource(Some(() => new huancun.prefetch.l3PrefetchRecv())))
  lazy val module = new LazyModuleImp(this){
    val arbiter = Module(new Arbiter(new coupledL2.prefetch.l2PrefetchSend(), clientNum))
    arbiter.suggestName(s"pf_l3recv_node_arb")
    for (i <- 0 until clientNum) {
      arbiter.io.in(i).valid := inNode(i).in.head._1.addr_valid
      arbiter.io.in(i).bits.addr_valid := inNode(i).in.head._1.addr_valid
      arbiter.io.in(i).bits.addr := inNode(i).in.head._1.addr
      arbiter.io.in(i).bits.pf_en := inNode(i).in.head._1.pf_en
      arbiter.io.in(i).ready := DontCare
    }
    arbiter.io.out.valid := DontCare
    outNode.head.out.head._1.addr_valid := arbiter.io.out.bits.addr_valid
    outNode.head.out.head._1.addr := arbiter.io.out.bits.addr
    outNode.head.out.head._1.pf_en := arbiter.io.out.bits.pf_en
    arbiter.io.out.ready := true.B
  }
}