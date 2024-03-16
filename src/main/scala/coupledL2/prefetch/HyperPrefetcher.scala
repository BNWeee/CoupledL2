package coupledL2.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.{HasCoupledL2Parameters,L2ParamKey}
import coupledL2._
import xs.utils.perf.HasPerfLogging
import xs.utils.sram.SRAMTemplate
import xs.utils.Pipeline
import xs.utils.SignExt

class ReplaceableQueueV2[T <: Data](val gen: T, val entries: Int) extends Module(){
  object EnqIOV2 {
    def apply[T <: Data](gen: T): DecoupledIO[T] = Decoupled(gen)
  }
  object DeqIOV2 {
    def apply[T <: Data](gen: T): DecoupledIO[T] = Flipped(Decoupled(gen))
  }

  val io = IO(new Bundle{
    val enq = Flipped(EnqIOV2(gen))
    val deq = Flipped(DeqIOV2(gen))
    val empty = Output(Bool())
    val full = Output(Bool())
    val flush = Input(Bool())
  })
  /*  Here we implement a queue that
   *  1. is pipelined  2. flows
   *  3. always has the latest reqs, which means the queue is always ready for enq and deserting the eldest ones
   */
  val queue = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(gen))))
  val valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val idxWidth = log2Up(entries)
  val head = RegInit(0.U(idxWidth.W))
  val tail = RegInit(0.U(idxWidth.W))
  val empty = head === tail && !valids.last
  val full = head === tail && valids.last
  io.empty := empty
  io.full := full

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
  when(io.flush){
    valids.foreach(x => x := false.B)
    tail := 0.U
    head := 0.U
  }

  io.enq.ready := true.B
  io.deq.valid := !empty || (io.enq.valid && !io.flush)
  io.deq.bits := Mux(empty, io.enq.bits, queue(head))

  //XSPerfHistogram(cacheParams, "nrWorkingPfQueueEntries", PopCount(valids), true.B, 0, inflightEntries, 1)
}

case class HyperPrefetchParams(
  fTableEntries: Int = 32,
  pTableQueueEntries: Int = 2,
  fTableQueueEntries: Int = 256
)
    extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 32
}

trait HasHyperPrefetcherParams extends HasCoupledL2Parameters {
  val hyperPrefetchParams = HyperPrefetchParams()

  val pageAddrBits = fullAddressBits - pageOffsetBits
  val blkOffsetBits = pageOffsetBits - offsetBits

  val fTableEntries = hyperPrefetchParams.fTableEntries
  val fTagBits = pageAddrBits - log2Up(fTableEntries)
  val pTableQueueEntries = hyperPrefetchParams.pTableQueueEntries
  val fTableQueueEntries = hyperPrefetchParams.fTableQueueEntries
}

abstract class PrefetchBranchV2Module(implicit val p: Parameters) extends Module with HasHyperPrefetcherParams with HasPerfLogging
abstract class PrefetchBranchV2Bundle(implicit val p: Parameters) extends Bundle with HasHyperPrefetcherParams

class FilterV2(implicit p: Parameters) extends PrefetchBranchV2Module {
  val io = IO(new Bundle() {
    val req = Flipped(DecoupledIO(new PrefetchReq))
    val resp = DecoupledIO(new PrefetchReq)
    val evict = Flipped(DecoupledIO(new PrefetchEvict))
    val from_bop = Input(Bool())
    val spp2llc = Input(Bool())
    val hint2llc = if(sppMultiLevelRefillOpt.nonEmpty)  Some(Output(Bool())) else None
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
  io.resp.valid := io.req.fire && (!hitForMap || io.from_bop)
  io.resp.bits := io.req.bits

  io.hint2llc match{
    case Some(hint2llc) => hint2llc := io.req.fire && !hitForMap && io.spp2llc
    case None => 
  }
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

  q.io.enq.valid := io.req.fire && !hitForMap && !io.spp2llc // if spp2llc , don't enq
  q.io.enq.bits := io.req.bits.addr
  q.io.deq.ready := q.io.full && q.io.enq.fire
  q.io.flush := false.B
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
class HyperPrefetcher(parentName:String = "Unknown")(implicit p: Parameters) extends PrefetchBranchV2Module {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val evict = Flipped(DecoupledIO(new PrefetchEvict))
    val recv_addr = Flipped(ValidIO(UInt(64.W)))
    val hint2llc = if(sppMultiLevelRefillOpt.nonEmpty)  Some(Output(Bool())) else None
    val db_degree = Flipped(ValidIO(UInt(2.W)))
    val queue_used = Input(UInt(6.W))
  })

  val fTable = Module(new FilterV2)

  val spp = Module(new SignaturePathPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(SPPParameters()))
  })))
//   val bop = Module(new BestOffsetPrefetch()(p.alterPartial({
//         case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(BOPParameters()))
//   })))
//   val sms = Module(new PrefetchReceiver()(p.alterPartial({
//         case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(PrefetchReceiverParams()))
//   })))

//   val q_sms = Module(new ReplaceableQueueV2(chiselTypeOf(sms.io.req.bits), pTableQueueEntries))
//   q_sms.io.enq <> sms.io.req
//   q_sms.io.deq.ready := !bop.io.req.valid
//   val sms_req = q_sms.io.deq.bits

  val q_spp = Module(new ReplaceableQueueV2(new PrefetchReq, pTableQueueEntries))
  val q_spp_hint2llc = Module(new ReplaceableQueueV2(Bool(), pTableQueueEntries))
  q_spp.io.enq <> spp.io.req
  q_spp.io.deq.ready := true.B //!q_sms.io.deq.fire && !bop.io.req.valid
  q_spp.io.flush := false.B
  q_spp_hint2llc.io.enq.valid := spp.io.req.valid
  q_spp_hint2llc.io.enq.bits := spp.io.req_hint2llc.valid
  q_spp_hint2llc.io.deq.ready := true.B //!q_sms.io.deq.fire && !bop.io.req.valid
  q_spp_hint2llc.io.flush := false.B
  val spp_req = q_spp.io.deq.bits
  val spp_hint2llc = q_spp_hint2llc.io.deq.bits
  spp.io.req_hint2llc.ready := false.B
  spp.io.train <> io.train

  val train_for_bop = Reg(new PrefetchTrain)
//   val train_for_bop_valid = RegInit(false.B)
  
//   when(io.train.valid && !bop.io.train.ready) {
//     train_for_bop := io.train.bits
//     train_for_bop_valid := true.B
//   }
//   bop.io.train.valid := io.train.valid || train_for_bop_valid
//   bop.io.train.bits := Mux(io.train.valid, io.train.bits, train_for_bop)
//   when(bop.io.train.fire() && !io.train.valid) {
//     train_for_bop_valid := false.B
//   }

//   bop.io.resp <> io.resp
  io.resp.ready := true.B

  // spp.io.resp.bits.tag := DontCare
  // spp.io.resp.bits.set := DontCare
  // spp.io.resp.valid := false.B
  // spp.io.resp.bits.pfVec := DontCare

  // spp.io.req.ready := true.B
//   bop.io.req.ready := true.B

//   sms.io.recv_addr.valid := io.recv_addr.valid
//   sms.io.recv_addr.bits := io.recv_addr.bits
//   sms.io.req.ready := true.B
//   sms.io.resp := DontCare
//   sms.io.train := DontCare

  fTable.io.req.valid := q_spp.io.deq.fire //|| q_sms.io.deq.fire || bop.io.req.valid
  fTable.io.req.bits := spp_req//Mux(bop.io.req.valid, bop.io.req.bits, 
                        //   Mux(q_sms.io.deq.fire, sms_req, spp_req))
  fTable.io.spp2llc := false.B//Mux(bop.io.req.valid, false.B, 
                        //   Mux(q_sms.io.deq.fire, false.B, spp_hint2llc)) 
  io.req <> fTable.io.resp
  io.hint2llc match{
    case Some(hint2llc) => hint2llc := fTable.io.spp2llc
    case _ =>
  }

  fTable.io.evict.valid := io.evict.valid
  fTable.io.evict.bits := io.evict.bits
  io.evict.ready := fTable.io.evict.ready

  fTable.io.from_bop := false.B//bop.io.req.valid

  io.train.ready := true.B
  // spp.io.db_degree.valid := io.db_degree.valid
  // spp.io.db_degree.bits := io.db_degree.bits
  // spp.io.queue_used := io.queue_used

//   XSPerfAccumulate(cacheParams, "bop_send2_queue", fTable.io.resp.fire && bop.io.req.valid)
//   XSPerfAccumulate(cacheParams, "sms_send2_queue", fTable.io.resp.fire && q_sms.io.deq.fire)
//   XSPerfAccumulate(cacheParams, "spp_send2_queue", fTable.io.resp.fire && q_spp.io.deq.fire)
//   XSPerfAccumulate(cacheParams, "prefetcher_has_evict", io.evict.fire())
}