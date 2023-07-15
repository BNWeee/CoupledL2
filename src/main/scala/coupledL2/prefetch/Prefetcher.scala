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

import chisel3._
import chisel3.util._
import utility._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import coupledL2._
import coupledL2.utils.{XSPerfAccumulate, XSPerfHistogram}

class PrefetchReq(implicit p: Parameters) extends PrefetchBundle {
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  val isBOP = Bool()
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchEvict(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchResp(implicit p: Parameters) extends PrefetchBundle {
  // val id = UInt(sourceIdBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchTrain(implicit p: Parameters) extends PrefetchBundle {
  // val addr = UInt(addressBits.W)
  val tag = UInt(fullTagBits.W)
  val set = UInt(setBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  // prefetch only when L2 receives a miss or prefetched hit req
  // val miss = Bool()
  // val prefetched = Bool()
  def addr = Cat(tag, set, 0.U(offsetBits.W))
}

class PrefetchIO(implicit p: Parameters) extends PrefetchBundle {
  val train = Flipped(DecoupledIO(new PrefetchTrain))
  val req = DecoupledIO(new PrefetchReq)
  val resp = Flipped(DecoupledIO(new PrefetchResp))
  val evict = Flipped(DecoupledIO(new PrefetchEvict))
  val recv_addr = Flipped(ValidIO(UInt(64.W)))
}

class PrefetchQueue(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(new PrefetchReq))
    val deq = DecoupledIO(new PrefetchReq)
  })
  /*  Here we implement a queue that
   *  1. is pipelined  2. flows
   *  3. always has the latest reqs, which means the queue is always ready for enq and deserting the eldest ones
   */
  val queue = RegInit(VecInit(Seq.fill(inflightEntries)(0.U.asTypeOf(new PrefetchReq))))
  val valids = RegInit(VecInit(Seq.fill(inflightEntries)(false.B)))
  val idxWidth = log2Up(inflightEntries)
  val head = RegInit(0.U(idxWidth.W))
  val tail = RegInit(0.U(idxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last

  when(!empty && io.deq.ready) {
    valids(head) := false.B
    head := head + 1.U
  }

  when(io.enq.valid) {
    queue(tail) := io.enq.bits
    valids(tail) := !empty || !io.deq.ready // true.B
    tail := tail + (!empty || !io.deq.ready).asUInt
    when(full && !io.deq.ready) {
      head := head + 1.U
    }
  }

  io.enq.ready := true.B
  io.deq.valid := !empty || io.enq.valid
  io.deq.bits := Mux(empty, io.enq.bits, queue(head))

  // The reqs that are discarded = enq - deq
  XSPerfAccumulate(cacheParams, "prefetch_queue_enq", io.enq.fire())
  XSPerfAccumulate(cacheParams, "prefetch_queue_fromL1_enq", io.enq.fire() && !io.enq.bits.isBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_fromL2_enq", io.enq.fire() && io.enq.bits.isBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_deq", io.deq.fire())
  XSPerfAccumulate(cacheParams, "prefetch_queue_fromL1_deq", io.deq.fire() && !io.enq.bits.isBOP)
  XSPerfAccumulate(cacheParams, "prefetch_queue_fromL2_enq", io.deq.fire() && io.enq.bits.isBOP)
  XSPerfHistogram(cacheParams, "prefetch_queue_entry", PopCount(valids.asUInt),
    true.B, 0, inflightEntries, 1)
}

class Prefetcher(implicit p: Parameters) extends PrefetchModule {
  val io = IO(new PrefetchIO)
  val io_pf_en = IO(Input(Bool()))
  val io_llc = if(prefetchSendOpt.nonEmpty) Some(IO(Output(new l2PrefetchSend))) else None
  //configSwitch
  //L2-->1.l2prefetchRecv/l2prefetch 3.l3prefetchSend
  val configTuple = (prefetchOpt.nonEmpty, prefetchSendOpt.nonEmpty)
  configTuple match {
    case(true ,false) => {
      prefetchOpt.get match {
        case bop: BOPParameters =>
          val pft = Module(new BestOffsetPrefetch)
          val pftQueue = Module(new PrefetchQueue)
          val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
          pft.io.train <> io.train
          pft.io.resp <> io.resp
          pftQueue.io.enq <> pft.io.req
          pipe.io.in <> pftQueue.io.deq
          io.req <> pipe.io.out
      }
    }
    case(true ,true) => {
      val pftQueue = Module(new PrefetchQueue)
      val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
      val l2_pf_en = RegNextN(io_pf_en,2,Some(true.B))

      prefetchOpt.get match {
        case pf: BOPParameters =>
          println(s"${cacheParams.name} prefetcher: BestOffsetPrefetch+PrefetchSender")
          val bop = Module(new BestOffsetPrefetch)
          bop.io.train <> io.train
          bop.io.resp <> io.resp
          pftQueue.io.enq <> bop.io.req
          pipe.io.in <> pftQueue.io.deq
          io.req <> pipe.io.out

          // send to prq
          pftQueue.io.enq.valid :=l2_pf_en && bop.io.req.valid
          pftQueue.io.enq.bits := bop.io.req.bits

          bop.io.req.ready := true.B
          pipe.io.in <> pftQueue.io.deq
          io.req <> pipe.io.out

          //llc prefetchSend
          val sent2llc_valid=Counter(io.req.valid,10)._2
          io_llc.get.pf_en := true.B
          io_llc.get.addr_valid := sent2llc_valid
          io_llc.get.addr := Cat(io.req.bits.tag, io.req.bits.set, 0.U(offsetBits.W))

        case branch2: PrefetchBranchV2Params =>
          val hybrid_pfts = Module(new PrefetchBranchV2())
          val pftQueue = Module(new PrefetchQueue)
          val pipe = Module(new Pipeline(io.req.bits.cloneType, 1))
          hybrid_pfts.io.train <> io.train
          hybrid_pfts.io.resp <> io.resp
          hybrid_pfts.io.recv_addr := ValidIODelay(io.recv_addr, 2)
          hybrid_pfts.io.evict <> io.evict
          pftQueue.io.enq <> hybrid_pfts.io.req
          pipe.io.in <> pftQueue.io.deq
          io.req <> pipe.io.out

      }
    }
    case(_,_) => {
      io := DontCare
      io_pf_en := DontCare
    }
  }
}
