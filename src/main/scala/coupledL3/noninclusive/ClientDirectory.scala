/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package coupledL3.noninclusive

import chisel3._
import chisel3.util._
import coupledL3._
import coupledL3.utils._
import coupledL3.MetaData._
import chipsalliance.rocketchip.config.Parameters
import utility.ParallelPriorityMux


trait HasClientInfo { this: HasCoupledL3Parameters =>
  // assume all clients have same params
  // TODO: check client params to ensure they are same
  val clientCacheParams = cacheParams.clientCaches.head

  val clientSets = clientCacheParams.sets
  val clientWays = clientCacheParams.ways
  val clientSetBits = log2Ceil(clientSets)
  val clientWayBits = log2Ceil(clientWays)
  val clientTagBits = addressBits - clientSetBits - offsetBits
}

class ClientMetaEntry(implicit p: Parameters) extends L3Bundle {
  val state = UInt(stateBits.W)

  def =/=(entry: ClientMetaEntry): Bool = {
    this.asUInt =/= entry.asUInt
  }
}

class ClientDirRead(implicit p: Parameters) extends L3Bundle with HasClientInfo {
  val tag = UInt(tagBits.W) // we will do address transformation in client directory for simplicity
  val set = UInt(setBits.W)

  val replacerInfo = new ReplacerInfo()
}

class ClientDirResult(implicit p: Parameters) extends L3Bundle with HasClientInfo {
  val tag = UInt(clientTagBits.W)
  val set = UInt(clientSetBits.W)
  val way = UInt(clientWayBits.W)  // hit way or victim way
  val hits = Vec(clientBits, Bool())
  val metas = Vec(clientBits, new ClientMetaEntry())
  val error = Bool()
  val replacerInfo = new ReplacerInfo()

  def tagMatch() = {
    Cat(this.hits).orR
  }
}

trait HasClientDirResult extends L3Bundle with HasClientInfo {
  val client = Output(new ClientDirResult()) // TODO: lower case name
}

class ClientMetaWrite(implicit p: Parameters) extends L3Bundle with HasClientInfo {
  val set = UInt(clientSetBits.W)
  val wayOH = UInt(clientWays.W)
  val wmeta = Vec(clientBits, new ClientMetaEntry)
}

class ClientTagWrite(implicit p: Parameters) extends L3Bundle with HasClientInfo {
  val set = UInt(clientSetBits.W)
  val way = UInt(clientWayBits.W)
  val tag = UInt(clientTagBits.W)

  def apply(lineAddr: UInt, way: UInt) = {
    this.set := lineAddr(clientSetBits - 1, 0)
    this.way := way
    this.tag := lineAddr(clientSetBits + clientTagBits - 1, clientSetBits)
  }
}

class ClientDirectory(implicit p: Parameters) extends L3Module with DontCareInnerLogic with HasClientInfo {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new ClientDirRead))
    val resp = Output(new ClientDirResult)
    // val metaWReq = Flipped(ValidIO(new ClientMetaWrite))
    // val tagWReq = Flipped(ValidIO(new ClientTagWrite))
    val metaWReq = Flipped(DecoupledIO(new ClientMetaWrite))
    val tagWReq = Flipped(DecoupledIO(new ClientTagWrite))
  })

  println(s"clientInfo:\n" +
          s"\tclientWays:${clientWays}\tclientWayBits:${clientWayBits}\n" +
          s"\tclientSets:${clientSets}\tclientSetBits:${clientSetBits}\n" +
          s"\tclientTagBits:${clientTagBits}\tclientMetaBits:${(new ClientMetaEntry).getWidth}\tclientBits:${clientBits}")

  def client_invalid_way_fn(metaVec: Vec[Vec[ClientMetaEntry]], repl: UInt): (Bool, UInt) = {
    val invalid_vec = Cat(metaVec.map(states => Cat(states.map(_.state === INVALID)).andR).reverse)
    val has_invalid_way = Cat(invalid_vec).orR
    val way = ParallelPriorityMux(invalid_vec.asBools.zipWithIndex.map(x => x._1 -> x._2.U(clientWayBits.W)))
    (has_invalid_way, way)
  }

  //  ClientDirectory parameters declaration
  val sets = clientSets
  val ways = clientWays
  val banks = cacheParams.dirNBanks

  val tagArray  = Module(new BankedSRAM(UInt(clientTagBits.W), sets, ways, banks, singlePort = true, enableClockGate = enableClockGate))
  val metaArray = Module(new BankedSRAM(Vec(clientBits, new ClientMetaEntry), sets, ways, banks, singlePort = true, enableClockGate = enableClockGate))
  val tagRead = Wire(Vec(ways, UInt(clientTagBits.W)))
  val metaRead = Wire(Vec(ways, Vec(clientBits, new ClientMetaEntry)))

  val tagWen  = io.tagWReq.valid
  val metaWen = io.metaWReq.valid
  val replacerWen = RegInit(false.B)

  tagArray.io.r <> DontCare
  tagArray.io.w <> DontCare
  metaArray.io.r <> DontCare
  metaArray.io.w <> DontCare


  def restoreAddr(set: UInt, tag: UInt) = {
    (set << offsetBits).asUInt + (tag << (setBits + offsetBits)).asUInt
  }
  val req = io.read.bits
  val debugAddr = restoreAddr(req.set, req.tag)
  dontTouch(debugAddr)

  // ---------------------------------------------------------------------------
  //  Stage1: latch request, access Tag/Meta
  // --------------------------------------------------------------------------
  // Address transformation
  val addr = Cat(io.read.bits.tag, io.read.bits.set)
  val rdSet = addr(clientSetBits - 1, 0)
  val rdTag = addr >> clientSetBits

  require((clientTagBits + clientSetBits) == (tagBits + setBits))

  // Tag R/W
  tagRead := tagArray.io.r(io.read.fire, rdSet).resp.data
  tagArray.io.w(
    tagWen,
    io.tagWReq.bits.tag,
    io.tagWReq.bits.set,
    UIntToOH(io.tagWReq.bits.way)
  )

  // Meta R/W
  metaRead := metaArray.io.r(io.read.fire, rdSet).resp.data
  metaArray.io.w(
    metaWen,
    io.metaWReq.bits.wmeta,
    io.metaWReq.bits.set,
    io.metaWReq.bits.wayOH
  )

  // val metaArrayDiv = 8
  // val metaArrayDivBits = log2Ceil(metaArrayDiv)
  // val metaArray = Module(new BankedSRAM(Vec(clientBits, new ClientMetaEntry), sets / metaArrayDiv, ways * metaArrayDiv, banks / 2, singlePort = true, enableClockGate = enableClockGate))
  // val metaReadVec = Wire(Vec(ways * metaArrayDiv, Vec(clientBits, new ClientMetaEntry)))
  // val metaRdSet = rdSet(clientSetBits - 1, metaArrayDivBits)
  // val metaRdWayOH = RegEnable(UIntToOH(rdSet(metaArrayDivBits -1, 0)), 0.U(metaArrayDiv.W), io.read.fire)
  // dontTouch(metaRdSet)
  // dontTouch(metaReadVec)
  // dontTouch(metaRead)
  // dontTouch(metaRdWayOH)
  // metaReadVec := metaArray.io.r(io.read.fire, metaRdSet).resp.data
  // metaRead := Mux1H(metaRdWayOH.asBools,
  //   metaReadVec.asTypeOf(Vec(metaArrayDiv, Vec(ways, Vec(clientBits, new ClientMetaEntry)))
  // ))

  // val metaWrSet = io.metaWReq.bits.set(clientSetBits - 1, metaArrayDivBits)
  // val metaWrWayOH = UIntToOH(io.metaWReq.bits.set(metaArrayDivBits - 1, 0))
  // val metaWrWayMask = WireInit(0.U((ways * metaArrayDiv).W)).asTypeOf(Vec(metaArrayDiv, UInt(ways.W)))
  // dontTouch(metaWrSet)
  // dontTouch(metaWrWayOH)
  // dontTouch(metaWrWayMask)
  // metaWrWayMask.zip(metaWrWayOH.asBools).foreach{
  //   case (m, en) =>
  //     m := Mux(en, io.metaWReq.bits.wayOH, 0.U(ways.W))
  // }
  // metaArray.io.w(
  //   metaWen,
  //   io.metaWReq.bits.wmeta,
  //   metaWrSet,
  //   metaWrWayMask.asUInt
  // )

  dontTouch(io)
  dontTouch(metaArray.io)
  dontTouch(tagArray.io)


  // ---------------------------------------------------------------------------
  //  Stage2: get Tag/Meta, calculate hit/way
  // --------------------------------------------------------------------------
  val req_s2 = RegEnable(io.read.bits, 0.U.asTypeOf(io.read.bits), enable = io.read.fire)
  val set_s2 = RegEnable(rdSet, 0.U(clientSetBits.W), enable = io.read.fire)
  val tag_s2 = RegEnable(rdTag, 0.U(clientTagBits.W), enable = io.read.fire)
  val hit_s2 = Wire(Bool())
  val way_s2 = Wire(UInt(clientWayBits.W))
  val valid_s2 = RegNext(io.read.fire, false.B)

  // Replacer
  val repl = ReplacementPolicy.fromString("random", clientWays)

  val tagMatchVec = tagRead.map(_(clientTagBits - 1, 0) === tag_s2)
  val metaValidVec = metaRead.map( way => Cat(way.map( _.state =/= INVALID )).orR )
  val hitVec = tagMatchVec.zip(metaValidVec).map(x => x._1 && x._2)
  val hitWay = OHToUInt(hitVec)
  val replaceWay = repl.get_replace_way(0.U)
  val (inv, invalidWay) = client_invalid_way_fn(metaRead, replaceWay)
  val chosenWay = Mux(inv, invalidWay, replaceWay)

  hit_s2 := Cat(hitVec).orR
  way_s2 := Mux(hit_s2, hitWay, chosenWay)


  // ---------------------------------------------------------------------------
  //  Stage3: output latched hit/way and chosen meta/tag by way
  // --------------------------------------------------------------------------
  val hit_s3 = RegEnable(hit_s2, false.B, valid_s2)
  val way_s3 = RegEnable(way_s2, 0.U, valid_s2)
  val metaAll_s3 = RegEnable(metaRead, 0.U.asTypeOf(metaRead), valid_s2)
  val tagAll_s3 = RegEnable(tagRead, 0.U.asTypeOf(tagRead), valid_s2)
  val meta_s3 = metaAll_s3(way_s3)
  val tag_s3 = tagAll_s3(way_s3) 
  val set_s3 = RegEnable(set_s2, 0.U, valid_s2)
  val replacerInfo_s3 = RegEnable(req_s2.replacerInfo, 0.U.asTypeOf(req_s2.replacerInfo), valid_s2)

  io.resp.hits.zip(meta_s3).foreach{
    case(hit, meta) =>
      hit := meta.state =/= INVALID && hit_s3
  }
  io.resp.way := way_s3
  io.resp.metas := meta_s3
  io.resp.tag := tag_s3 // TODO: 
  io.resp.set := set_s3
  io.resp.error := false.B // TODO:
  io.resp.replacerInfo := replacerInfo_s3  

  io.read.ready := (tagArray.io.r.req.ready && metaArray.io.r.req.ready)


  // ---------------------------------------------------------------------------
  //  HalfFreq blocking logic
  // --------------------------------------------------------------------------
  if(enableHalfFreq) {
    val readFull = RegInit(false.B)
    val writeFullMeta = RegInit(false.B)
    val writeFullTag = RegInit(false.B)

    io.read.ready := tagArray.io.r.req.ready && metaArray.io.r.req.ready && !readFull
    when(io.read.fire) {
      readFull := true.B
    }.otherwise{
      readFull := false.B
    }

    io.metaWReq.ready := !writeFullMeta
    io.tagWReq.ready := !writeFullTag

    when(io.metaWReq.fire) {
      writeFullMeta := true.B
    }.otherwise{
      writeFullMeta := false.B
    }

    when(io.tagWReq.fire) {
      writeFullTag := true.B
    }.otherwise{
      writeFullTag := false.B
    }

    when(RegNext(!reset.asBool)) {
      assert(!(RegNext(io.read.fire) && io.read.fire), "[half freq] Consecutively read!")
      assert(!(RegNext(io.metaWReq.fire) && io.metaWReq.fire), "[half freq] Consecutively metaW!")
      assert(!(RegNext(io.tagWReq.fire) && io.tagWReq.fire), "[half freq] Consecutively tagW!")
    }
  } else {
    io.read.ready := tagArray.io.r.req.ready && metaArray.io.r.req.ready
    io.metaWReq.ready := true.B
    io.tagWReq.ready := true.B 
  }

}