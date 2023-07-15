package coupledL2.prefetch

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

import coupledL2.HasCoupledL2Parameters
import coupledL2.utils.XSPerfAccumulate
import coupledL2._

case class PrefetchBranchV2Params(
  fTableEntries: Int = 32,
  pTableQueueEntries: Int = 2,
  fTableQueueEntries: Int = 256
)
    extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 32
}

trait HasPrefetchBranchV2Params extends HasCoupledL2Parameters {
  val prefetchBranchV2Params = prefetchOpt.get.asInstanceOf[PrefetchBranchV2Params]

  val pageAddrBits = fullAddressBits - pageOffsetBits
  val blkOffsetBits = pageOffsetBits - offsetBits

  val fTableEntries = prefetchBranchV2Params.fTableEntries
  val fTagBits = pageAddrBits - log2Up(fTableEntries)
  val pTableQueueEntries = prefetchBranchV2Params.pTableQueueEntries
  val fTableQueueEntries = prefetchBranchV2Params.fTableQueueEntries
}

abstract class PrefetchBranchV2Module(implicit val p: Parameters) extends Module with HasPrefetchBranchV2Params
abstract class PrefetchBranchV2Bundle(implicit val p: Parameters) extends Bundle with HasPrefetchBranchV2Params

class FilterV2(implicit p: Parameters) extends PrefetchBranchV2Module {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new PrefetchReq))
    val resp = DecoupledIO(new PrefetchReq)
    val evict = Flipped(DecoupledIO(new PrefetchEvict))
  })

  def idx(addr:      UInt) = addr(log2Up(fTableEntries) - 1, 0)
  def tag(addr:      UInt) = addr(pageAddrBits - 1, log2Up(fTableEntries))

  def fTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(fTagBits.W)
    val bitMap = Vec(64, Bool())
  }

  val fTable = RegInit(VecInit(Seq.fill(fTableEntries)(0.U.asTypeOf(fTableEntry()))))
  val q = Module(new ReplaceableQueueV2(UInt(fullAddressBits.W), fTableQueueEntries))

  val oldAddr = io.req.bits.addr
  val blkAddr = oldAddr(fullAddressBits - 1, offsetBits)
  val pageAddr = oldAddr(fullAddressBits - 1, pageOffsetBits)
  val blkOffset = oldAddr(pageOffsetBits - 1, offsetBits)

  //read fTable
  val hit = Wire(Bool())
  val readResult = Wire(fTableEntry())
  readResult := fTable(idx(pageAddr))
  hit := readResult.valid && tag(pageAddr) === readResult.tag
  val hitForMap = hit && readResult.bitMap(blkOffset)

  io.resp.valid := io.req.fire() && !hitForMap
  io.resp.bits := io.req.bits

  val wData = Wire(fTableEntry())
  val newBitMap = readResult.bitMap.zipWithIndex.map{ case (b, i) => Mux(i.asUInt === blkOffset, true.B, false.B) }
  
  wData.valid := true.B
  wData.tag := tag(pageAddr)
  wData.bitMap := newBitMap
  when(io.req.fire) {
    when(hit) {
      fTable(idx(pageAddr)).bitMap(blkOffset) := true.B
    } .otherwise {
      fTable(idx(pageAddr)) := wData
    }
  }

  q.io.enq.valid := io.req.fire && !hitForMap
  q.io.enq.bits := io.req.bits.addr
  q.io.deq.ready := q.io.full && q.io.enq.fire

  val evictAddr = q.io.deq.bits
  val evictPageAddr = evictAddr(fullAddressBits - 1, pageOffsetBits)
  val evictBlkOffset = evictAddr(pageOffsetBits - 1, offsetBits)
  val evictBlkAddr = evictAddr(fullAddressBits - 1, offsetBits)
  val readEvict = Wire(fTableEntry())
  val hitEvict = Wire(Bool())
  val conflict = io.req.fire && blkAddr === evictBlkAddr
  readEvict := fTable(idx(evictPageAddr))
  hitEvict := q.io.deq.fire && readEvict.valid && tag(evictPageAddr) === readEvict.tag && readEvict.bitMap(evictBlkOffset) && !conflict
  when(hitEvict) {
    fTable(idx(evictPageAddr)).bitMap(evictBlkOffset) := false.B
  }

  /*
  val evictAddr = io.evict.bits.addr
  val evictPageAddr = evictAddr(fullAddressBits - 1, pageOffsetBits)
  val evictBlkOffset = evictAddr(pageOffsetBits - 1, offsetBits)
  val evictBlkAddr = evictAddr(fullAddressBits - 1, offsetBits)
  val readEvict = Wire(fTableEntry())
  val hitEvict = Wire(Bool())
  val conflict = io.req.fire && blkAddr === evictBlkAddr
  readEvict := fTable(idx(evictPageAddr))
  hitEvict := io.evict.valid && readEvict.valid && tag(evictPageAddr) === readEvict.tag && readEvict.bitMap(evictBlkOffset) && !conflict
  when(hitEvict) {
    fTable(idx(evictPageAddr)).bitMap(evictBlkOffset) := false.B
  }*/

  io.req.ready := true.B
  io.evict.ready := true.B
}

//Only used for hybrid spp and bop
class PrefetchBranchV2()(implicit p: Parameters) extends PrefetchBranchV2Module {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val evict = Flipped(DecoupledIO(new PrefetchEvict))
    val recv_addr = Flipped(ValidIO(UInt(64.W)))
  })

  val fTable = Module(new FilterV2)

  val spp = Module(new SignaturePathPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(SPPParameters()))
  })))
  val bop = Module(new BestOffsetPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(BOPParameters()))
  })))
  val sms = Module(new PrefetchReceiver()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(PrefetchReceiverParams()))
  })))

  val q_bop = Module(new ReplaceableQueueV2(chiselTypeOf(bop.io.req.bits), pTableQueueEntries))
  q_bop.io.enq <> bop.io.req
  q_bop.io.deq.ready := !sms.io.req.valid
  val bop_req = q_bop.io.deq.bits

  val q_spp = Module(new ReplaceableQueueV2(chiselTypeOf(spp.io.req.bits), pTableQueueEntries))
  q_spp.io.enq <> spp.io.req
  q_spp.io.deq.ready := !q_bop.io.deq.fire && !sms.io.req.valid
  val spp_req = q_spp.io.deq.bits

  spp.io.train.valid := io.train.valid
  spp.io.train.bits := io.train.bits

  val train_for_bop = Reg(new PrefetchTrain)
  val train_for_bop_valid = RegInit(false.B)
  
  when(io.train.valid && !bop.io.train.ready) {
    train_for_bop := io.train.bits
    train_for_bop_valid := true.B
  }
  bop.io.train.valid := io.train.valid || train_for_bop_valid
  bop.io.train.bits := Mux(io.train.valid, io.train.bits, train_for_bop)
  when(bop.io.train.fire() && !io.train.valid) {
    train_for_bop_valid := false.B
  }

  bop.io.resp <> io.resp
  spp.io.resp.bits.tag := 0.U
  spp.io.resp.bits.set := 0.U
  spp.io.resp.valid := false.B

  spp.io.req.ready := true.B
  bop.io.req.ready := true.B

  sms.io.recv_addr.valid := io.recv_addr.valid
  sms.io.recv_addr.bits := io.recv_addr.bits
  sms.io.req.ready := true.B

  fTable.io.req.valid := q_spp.io.deq.fire || q_bop.io.deq.fire || sms.io.req.valid
  fTable.io.req.bits := Mux(sms.io.req.valid, sms.io.req.bits, 
                          Mux(q_bop.io.deq.fire, bop_req, spp_req))

  io.req <> fTable.io.resp

  fTable.io.evict.valid := io.evict.valid
  fTable.io.evict.bits := io.evict.bits
  io.evict.ready := fTable.io.evict.ready

  io.train.ready := true.B

  XSPerfAccumulate(cacheParams, "sms_send2_queue", fTable.io.resp.fire && sms.io.req.valid)
  XSPerfAccumulate(cacheParams, "bop_send2_queue", fTable.io.resp.fire && q_bop.io.deq.fire)
  XSPerfAccumulate(cacheParams, "spp_send2_queue", fTable.io.resp.fire && q_spp.io.deq.fire)
}