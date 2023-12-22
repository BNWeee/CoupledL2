package coupledL2.prefetch

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import coupledL2.{HasCoupledL2Parameters,L2ParamKey}
import coupledL2.prefetch.{PrefetchParameters,PrefetchTrain,PrefetchReq,PrefetchResp,PrefetchEvict}
import coupledL2.prefetch.{BestOffsetPrefetch,BOPParameters,PrefetchReceiver,PrefetchReceiverParams}
import coupledL2.prefetch.AccessState
import coupledL2.prefetch.PrefetchQueue
import xs.utils.perf.HasPerfLogging
import xs.utils.sram.SRAMTemplate
import xs.utils.Pipeline
import xs.utils.SRAMQueue
import xs.utils.OneHot
import xs.utils.HighestBit
import xs.utils.ParallelPriorityMux
import xs.utils.RegNextN
import xs.utils.CircularShift

object PfCtrlConst{
  object Switch{
    val SMS = 0
    val BOP = 1
    val SPP = 2

    val bits = 3
    def get_fields(x:UInt):UInt = {
      val fields = WireInit(x(bits-1,0));dontTouch(fields)
      fields
    }
  }
  object SppConfig{
    val bits = 5
    def get_fields(x:UInt):UInt = {
      val fields = WireInit(x(Switch.bits+bits-1,Switch.bits));dontTouch(fields)
      fields
    }
  }
  object FitlerTableConfig{
    val bits = 3
    def get_fields(x:UInt):UInt = {
      val fields = WireInit(x(Switch.bits+SppConfig.bits+bits-1,Switch.bits+SppConfig.bits));dontTouch(fields)
      fields
    }
  }
}

object PfSource extends Enumeration {
  val bits = 3
  val NONE    = "b000".U(bits.W)
  val SMS     = "b001".U(bits.W)
  val BOP     = "b010".U(bits.W)
  val SPP     = "b100".U(bits.W)
  val BOP_SPP = "b110".U(bits.W)
}
object PfVectorConst extends {
  val SMS = 0
  val BOP = 1
  val SPP = 2

  val bits = 3
  val DEFAULT = 0.U(bits.W)
}

object PfcovState {
  val bits = 2

  def p_0     = 0.U(bits.W)
  def p_25    = 1.U(bits.W)
  def p_50    = 2.U(bits.W)
  def p_75    = 3.U(bits.W)
}

object PfaccState {
  val bits = 2

  def p_0     = 0.U(bits.W)
  def p_25    = 1.U(bits.W)
  def p_50    = 2.U(bits.W)
  def p_75    = 3.U(bits.W)
}

object EnqIOV2 {
  def apply[T <: Data](gen: T): DecoupledIO[T] = Decoupled(gen)
}

/** Consumer - drives (outputs) ready, inputs valid and bits.
  * @param gen The type of data to dequeue
  */
object DeqIOV2 {
  def apply[T <: Data](gen: T): DecoupledIO[T] = Flipped(Decoupled(gen))
}

/** An I/O Bundle for Queues
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue.
  * @param hasFlush A boolean for whether the generated Queue is flushable
  * @groupdesc Signals The hardware fields of the Bundle
  */
class QueueIOV2[T <: Data](
  private val gen: T,
  val entries:     Int)
    extends Bundle { 
  /** I/O to enqueue data (client is producer, and Queue object is consumer), is [[chisel3.util.DecoupledIO]] flipped.
    * @group Signals
    */
  val enq = Flipped(EnqIOV2(gen))

  /** I/O to dequeue data (client is consumer and Queue object is producer), is [[chisel3.util.DecoupledIO]]
    * @group Signals
    */
  val deq = Flipped(DeqIOV2(gen))

  val empty = Output(Bool())
  val full = Output(Bool())
}

/** A hardware module implementing a Queue
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  * @param pipe True if a single entry queue can run at full throughput (like a pipeline). The ''ready'' signals are
  * combinationally coupled.
  * @param flow True if the inputs can be consumed on the same cycle (the inputs "flow" through the queue immediately).
  * The ''valid'' signals are coupled.
  * @param useSyncReadMem True uses SyncReadMem instead of Mem as an internal memory element.
  * @param hasFlush True if generated queue requires a flush feature
  * @example {{{
  * val q = Module(new Queue(UInt(), 16))
  * q.io.enq <> producer.io.out
  * consumer.io.in <> q.io.deq
  * }}}
  */
class ReplaceableQueueV2[T <: Data](
  val gen:            T,
  val entries:        Int) extends Module()
{
  val io = IO(new QueueIOV2(gen, entries))
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

  io.enq.ready := true.B
  io.deq.valid := !empty || io.enq.valid
  io.deq.bits := Mux(empty, io.enq.bits, queue(head))

  //XSPerfHistogram(cacheParams, "nrWorkingPfQueueEntries", 
  //  PopCount(valids), true.B, 0, inflightEntries, 1)
}


case class SPPParameters(
  sTableEntries: Int = 256,
  bpTableEntries: Int = 64,
  pTableEntries: Int = 512,
  pTableDeltaEntries: Int = 4,
  signatureBits: Int = 12,
  fTableEntries: Int = 32,
  enable_bp: Boolean =true,
  enable_nextline: Boolean = false,
)
    extends PrefetchParameters {
  override val hasPrefetchBit:  Boolean = true
  override val inflightEntries: Int = 32
}

trait HasSPPParams extends HasCoupledL2Parameters {
  val sppParams = SPPParameters()

  val sTableEntries = sppParams.sTableEntries
  val bpTableEntries = sppParams.bpTableEntries
  val pTableEntries = sppParams.pTableEntries
  val inflightEntries = sppParams.inflightEntries
  val pTableDeltaEntries = sppParams.pTableDeltaEntries
  val signatureBits = sppParams.signatureBits
  val pTableQueueEntries = 4
  val unpackQueueEntries = 2
  val fTableEntries = sppParams.fTableEntries
  val lookCountBits = 6
  val hin2llcQueueThreshold = 30

  val pageAddrBits = fullAddressBits - pageOffsetBits
  val blkAddrBits = fullAddressBits - offsetBits
  val blkOffsetBits = pageOffsetBits - offsetBits
  val sTagBits = signatureBits - log2Up(sTableEntries)
  val pTagBits = signatureBits - log2Up(pTableEntries)
  val fTagBits = pageAddrBits - log2Up(fTableEntries)
  val deltaBits = blkOffsetBits + 1

  def strideMap(a: SInt) : UInt = {
    val out = WireInit(0.U(3.W))
    when(a <= -5.S) {
      out := "b100".U
    } .elsewhen(a >= 5.S) {
      out := "b011".U
    } .elsewhen(-4.S<=a && a <= -3.S) {
      out := "b101".U
    } .elsewhen(3.S <= a && a <= 4.S) {
      out := "b000".U
    } .otherwise {
      out := a.asUInt(2, 0)
    }
    out
  }
  def makeSign(old_sig:UInt,new_delta:SInt):UInt={
    // (old_sig << 3) ^ new_delta.asUInt
    (old_sig << 3) ^ strideMap(new_delta)
  }

  val ENABLE_BP = sppParams.enable_bp
  val ENABLE_NL = sppParams.enable_nextline
}

abstract class SPPBundle(implicit val p: Parameters) extends Bundle with HasSPPParams
abstract class SPPModule(implicit val p: Parameters) extends Module with HasSPPParams with HasPerfLogging

class GhrForSpp(implicit p:  Parameters) extends  SPPBundle{
    val l2_deadCov_state = UInt(PfcovState.bits.W)
    val l2_hitAcc_state = UInt(PfaccState.bits.W)
    val bop_hitAcc_state = UInt(PfaccState.bits.W) 
    val spp_hitAcc_state = UInt(PfaccState.bits.W)
    val shareBO = SInt(6.W)
    val global_queue_used = (UInt(6.W))   
}
class SignatureTableReq(implicit p: Parameters) extends SPPBundle {
  val blkAddr = UInt(blkAddrBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  // val isBP = Bool()
  val fromGHR_shareBO = SInt(6.W)
  def get_pageAddr = blkAddr >> blkOffsetBits
  def get_blkOff = blkAddr(blkOffsetBits-1,0)
  def get_accessAddr = Cat(blkAddr,0.U(offsetBits.W))
}

class BreakPointReq(implicit p: Parameters) extends SPPBundle{
  val blkAddr = UInt(blkAddrBits.W)
  val parent_sig = Vec(1,UInt(signatureBits.W))
  def get_pageAddr = blkAddr >> blkOffsetBits
}
class SignatureTableResp_s0(implicit p: Parameters) extends SPPBundle {
  val blkAddr = UInt((pageAddrBits+blkOffsetBits).W)
  // from sram
  val oldSig = UInt(signatureBits.W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
  def get_pageAddr = blkAddr >> blkOffsetBits
  def get_blkOff = blkAddr(blkOffsetBits-1,0)
}
class SignatureTableResp_s1(implicit p: Parameters) extends SPPBundle {
  //read from sram
  val st_hit = Bool()
  val newDelta = SInt((blkOffsetBits + 1).W)
  val newSig = UInt(signatureBits.W)
  // bp data package
  val bp_use_valid = Bool()
  val bp_redirect_blk = UInt(blkAddrBits.W)
  val bp_sig = UInt(signatureBits.W)
}

class PatternTableResp(implicit p: Parameters) extends SPPBundle {
  val deltas = Vec(pTableDeltaEntries, SInt((blkOffsetBits + 1).W))
  val block = UInt((blkAddrBits).W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
}

class UnpackResp(implicit p: Parameters) extends SPPBundle {
  val prefetchBlock = UInt((blkAddrBits).W)
  val needT = Bool()
  val source = UInt(sourceIdBits.W)
}

class DeltaEntry(implicit p: Parameters) extends SPPBundle {
  val delta = SInt((blkOffsetBits + 1).W)
  val cDelta = UInt(4.W)

  def apply(delta: SInt, cDelta: UInt) = {
    val entry = WireInit(0.U.asTypeOf(this.cloneType))
    entry.delta := delta
    entry.cDelta := cDelta
    entry
  }
}

class SignatureTable(parentName: String = "Unknown")(implicit p: Parameters) extends SPPModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new SignatureTableReq))
    val s0_toPtReq = DecoupledIO(new SignatureTableResp_s0) //output old sig and delta to write PT
    val s1_toPtReq = DecoupledIO(new SignatureTableResp_s1) 
    val s0_bp_update = Flipped(ValidIO(new BreakPointReq))
    val ctrl = new Bundle {
      val en_bp_recovery = Input(Bool())
    }
  })
  def hash1(addr:    UInt) = addr(log2Up(sTableEntries) - 1, 0)
  def hash2(addr:    UInt) = addr(2 * log2Up(sTableEntries) - 1, log2Up(sTableEntries))
  def get_idx(addr:      UInt) = hash1(addr) ^ hash2(addr)
  def get_bpIdx(addr: UInt) = addr(log2Up(bpTableEntries) - 1, 0) ^ addr(2 * log2Up(bpTableEntries) - 1, log2Up(bpTableEntries))
  def get_tag(addr:      UInt) = addr(signatureBits - 1, log2Up(sTableEntries))
  def sTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(sTagBits.W)
    val sig = UInt(signatureBits.W)
    val last_blkOff = UInt(blkOffsetBits.W)
  }
  def breakPointEntry() = new Bundle() {
    val valid = Bool()
    val pre_blkAddr = UInt(blkAddrBits.W)
    val parent_sig = Vec(1, UInt(signatureBits.W))
  }
  
  println(s"fullAddressBits: ${fullAddressBits}")
  println(s"pageOffsetBits: ${pageOffsetBits}")
  println(s"sTagBits: ${sTagBits}")
  
  val sTable = RegInit(VecInit(Seq.fill(sTableEntries)(0.U.asTypeOf(sTableEntry()))))
  // val sTable = Module(
  //   new SRAMTemplate(sTableEntry(), set = sTableEntries, way = 1, 
  //     bypassWrite = true, 
  //     shouldReset = true, 
  //     hasMbist = cacheParams.hasMbist, 
  //     hasShareBus = cacheParams.hasShareBus,
  //     hasClkGate = enableClockGate, 
  //     parentName = parentName
  //   ))
  val bpTable = if(ENABLE_BP) Some(RegInit(VecInit(Seq.fill(bpTableEntries)(0.U.asTypeOf(breakPointEntry()))))) else None
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  //1. read sTable
  //2. write bpTable
  val s0_valid =  WireInit(false.B);dontTouch(s0_valid)
  val s0_req =  WireInit(0.U.asTypeOf(new SignatureTableReq))
  s0_valid := io.req.fire
  s0_req := io.req.bits

  if(bpTable.isDefined){
    val s0_bp_page = WireInit(io.s0_bp_update.bits.get_pageAddr)
    val s0_bp_wIdx = WireInit(get_bpIdx(s0_bp_page));dontTouch(s0_bp_wIdx)
    //caution: not include power gating there
    bpTable.get(s0_bp_wIdx).valid := io.s0_bp_update.valid
    bpTable.get(s0_bp_wIdx).pre_blkAddr := io.s0_bp_update.bits.blkAddr
    for( i <- 0 until(io.s0_bp_update.bits.parent_sig.length)){
        bpTable.get(s0_bp_wIdx).parent_sig(i) := io.s0_bp_update.bits.parent_sig(i)
    }

  }

  val s0_rEntryData = WireInit(sTable(get_idx(s0_req.get_pageAddr)));dontTouch(s0_rEntryData)
  // pressure cacalute
  val s0_hit = WireInit(s0_rEntryData.tag === get_tag(s0_req.get_pageAddr));dontTouch(s0_hit)
  val s0_delta  = WireInit(s0_req.get_blkOff.asSInt - s0_rEntryData.last_blkOff.asSInt);dontTouch(s0_delta)
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  //1. update sTable & req pTable
    //- we should write  biggest_blkAddr for solving spp timely problems, let spp prefetching farther!!!
  //2. delta BP from filterTable
    // redirect accessed blockAddr when use bp
    // calculate probeDelta for avoiding biased train signal delta, not make origin good sig overrided
    // calculate probe delata
  val s1_valid        = RegNext(s0_valid,false.B);dontTouch(s1_valid)
  val s1_req          = RegNext(s0_req,0.U.asTypeOf(new SignatureTableReq));dontTouch(s1_req)
  val s1_hit          = RegNext(s0_hit,false.B);dontTouch(s1_hit)
  val s1_delta        = RegNext(s0_delta,0.S);dontTouch(s1_delta)
  val s1_rEntryData   = RegNext(s0_rEntryData);dontTouch(s1_rEntryData)
  val s1_oldSig       = WireInit(s1_rEntryData.sig)
  val s1_newBlkAddr   = s1_req.blkAddr
  // pressure cacalute
  val s1_cal_newSig = WireInit(makeSign(s1_oldSig,s1_delta));dontTouch(s1_cal_newSig)

  //bp read
  val s1_bp_rIdx = WireInit(get_bpIdx(s1_req.get_pageAddr))
  val s1_bp_hit = WireInit(false.B)
  val s1_bp_mask = WireInit(VecInit(Seq.fill(4)(false.B)))
  val s1_bp_redirect_blk = WireInit(0.U(blkAddrBits.W))
  val s1_bp_matched_sig = WireInit(0.U(signatureBits.W))
  val s1_rotate_sig = VecInit(Seq.fill(4)(0.U(signatureBits.W)));dontTouch(s1_rotate_sig)
  if(bpTable.isDefined){
    for (i <- 0 until (4)) {
      s1_rotate_sig(i) := CircularShift(bpTable.get(s1_bp_rIdx).parent_sig.head).left(3 * i)
      s1_bp_mask(i) := s1_rotate_sig(i) === s1_rEntryData.sig
    }
    s1_bp_redirect_blk := bpTable.get(s1_bp_rIdx).pre_blkAddr
    s1_bp_hit := ENABLE_BP.asBool && s1_valid && s1_bp_mask.reduce(_ || _)
    //TODO: there should set offset for matchedIndex?
    val s1_bp_matchedIdx = WireInit(OneHot.OH1ToUInt(HighestBit(s1_bp_mask.asUInt,4)));dontTouch(s1_bp_matchedIdx)
    s1_bp_matched_sig := s1_rotate_sig(s1_bp_matchedIdx)
  }
  // io.resp.bits.isBP := s1_req.isBP

  // io.req.ready := sTable.io.r.req.ready

  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  val s2_valid  = RegNext(s1_valid,false.B)
  val s2_oldSig = RegNext(s1_oldSig,0.U)
  val s2_req    = RegNext(s1_req,0.U.asTypeOf(new SignatureTableReq))
  val s2_entryData = RegNext(s1_rEntryData,0.U.asTypeOf(sTableEntry()))

  // sTable.io.r.req.valid       := s0_valid
  // sTable.io.r.req.bits.setIdx := get_idx(s0_req.get_pageAddr)
  // sTable.io.w.req.valid := s2_valid && s2_delta =/= 0.S
  // sTable.io.w.req.bits.setIdx := get_idx(s2_req.get_pageAddr)
  // sTable.io.w.req.bits.data(0).valid := true.B
  // sTable.io.w.req.bits.data(0).tag := get_tag(s2_req.get_pageAddr)
  // //TODO: there should hold strideMap -> delta signal!! fuck!!!
  // //TODO: there should hold origin delta signal!!
  // // sTable.io.w.req.bits.data(0).sig := makeSign(s1_oldSig,strideMap(s1_newDelta))
  // sTable.io.w.req.bits.data(0).sig := makeSign(s2_oldSig,s2_delta)
  // sTable.io.w.req.bits.data(0).last_blkOff := s2_req.get_blkOff //(blkOffsetBits-1,0)

  when(s1_valid && s1_delta =/= 0.S){
    sTable(get_idx(s1_req.get_pageAddr)).valid := true.B
    sTable(get_idx(s1_req.get_pageAddr)).tag := get_tag(s1_req.get_pageAddr)
    sTable(get_idx(s1_req.get_pageAddr)).sig := makeSign(s1_oldSig,s1_delta)
    sTable(get_idx(s1_req.get_pageAddr)).last_blkOff := s1_req.get_blkOff
  }
  // s0 send response to paternTable
  // caution : just send data pack to table ,not calculate for serious timing problem
  io.s0_toPtReq.valid := s0_valid
  io.s0_toPtReq.bits.blkAddr := s0_req.blkAddr
  io.s0_toPtReq.bits.oldSig := s0_rEntryData.sig
  io.s0_toPtReq.bits.source := s0_req.source
  io.s0_toPtReq.bits.needT := s0_req.needT
  // s2 send response to paternTable
  io.s1_toPtReq.valid := s1_valid
  io.s1_toPtReq.bits.st_hit := s1_hit
  io.s1_toPtReq.bits.newDelta := s1_delta
  io.s1_toPtReq.bits.newSig := s1_cal_newSig
  io.s1_toPtReq.bits.bp_use_valid := io.ctrl.en_bp_recovery && s1_bp_hit
  io.s1_toPtReq.bits.bp_redirect_blk := s1_bp_redirect_blk
  io.s1_toPtReq.bits.bp_sig := s1_bp_matched_sig

  io.req.ready := io.s0_toPtReq.ready

  XSPerfAccumulate("spp_st_req_nums",io.s0_toPtReq.valid)
  if(ENABLE_BP){
    XSPerfAccumulate("spp_st_bp_req", s0_valid && s1_bp_hit)
    XSPerfAccumulate("spp_st_bp_update",io.s0_bp_update.valid)
  }
}

class PatternTable(parentName:String="Unkown")(implicit p: Parameters) extends SPPModule {
  val io = IO(new Bundle {
    val fromStReq_s0 = Flipped(DecoupledIO(new SignatureTableResp_s0))
    val fromStReq_s1 = Flipped(DecoupledIO(new SignatureTableResp_s1))
    val resp = DecoupledIO(new PatternTableResp)
    val from_ghr = Flipped(ValidIO(new GhrForSpp))
    val pt2st_bp = ValidIO(new BreakPointReq)
    val ctrl = new Bundle {
      val en_Nextline_Agreesive = Input(Bool())
      val en_bp_recovery = Input(Bool())
      val en_shareBO = Input(Bool())
      val en_slowLookUp = Input(Bool())
    }
  })
  dontTouch(io.from_ghr)
  def get_idx(addr:      UInt) = addr(log2Up(pTableEntries) - 1, 0)
  def get_tag(addr:      UInt) = addr(signatureBits - 1, log2Up(pTableEntries))
  class DeltaEntry(implicit p: Parameters) extends SPPBundle {
    val delta = SInt((blkOffsetBits + 1).W)
    val cDelta = UInt(4.W)

    def apply(delta: SInt, cDelta: UInt) = {
        val entry = WireInit(0.U.asTypeOf(this))
        entry.delta := delta
        entry.cDelta := cDelta
        entry
    }
  }
  class pt_SignatureTableResp(implicit p: Parameters) extends SPPBundle {
    val sig = UInt(signatureBits.W)
    val delta = SInt((blkOffsetBits + 1).W)
    val block = UInt((blkAddrBits).W)
    val needT = Bool()
    val source = UInt(sourceIdBits.W)
  }
  def is_samePage(addr:UInt, originBlockAddr:UInt):Bool = addr(blkAddrBits - 1, blkOffsetBits) === originBlockAddr(blkAddrBits - 1, blkOffsetBits)
  class pTableEntry() extends  Bundle {
    val valid = Bool()
    val tag = UInt(pTagBits.W)
    val deltaEntries = Vec(pTableDeltaEntries, new DeltaEntry())
    val count = UInt(4.W)
  }

  val pTable = Module(
    new SRAMTemplate(new pTableEntry(), set = pTableEntries, way = 1, 
      bypassWrite = true, 
      shouldReset = true, 
      hasMbist = cacheParams.hasMbist, 
      hasShareBus = cacheParams.hasShareBus,
      hasClkGate = enableClockGate, 
      parentName = parentName
    ))
  val s0_ghr_shareBO = RegNext(io.from_ghr.bits.shareBO,0.S)
  val s1_ghr_shareBO = WireInit(1.S)
  s1_ghr_shareBO := Mux(io.ctrl.en_shareBO,Mux(s0_ghr_shareBO > 0.S, s0_ghr_shareBO + 1.S, s0_ghr_shareBO - 1 .S),1.S)
  // --------------------------------------------------------------------------------
  // stage 0
  // --------------------------------------------------------------------------------
  //read pTable
  // val q = Module(new Queue(chiselTypeOf(io.req.bits), pTableQueueEntries, flow = true, pipe = false))
  val s0_fire = WireInit(io.fromStReq_s0.fire);dontTouch(s0_fire)
  val s1_ready = WireInit(false.B)
  val s2_ready = WireInit(false.B)

  val s0_pageAddr = WireInit(io.fromStReq_s0.bits.get_pageAddr)
  val s0_block = WireInit(io.fromStReq_s0.bits.blkAddr);dontTouch(s0_block)
  val s0_sig = WireInit(io.fromStReq_s0.bits.oldSig);dontTouch(s0_sig)

  io.fromStReq_s0.ready := s1_ready
  // --------------------------------------------------------------------------------
  // stage 1
  // --------------------------------------------------------------------------------
  //read pTable
  //1. calculate sram rindex and send read  sram Table requrest
  // when state_s2s1 == s_lookahead 
    //2. when lookcount bigger than sig folding length , start bp update operation
    //3. caculate s1_miniCount, miniCount has been designed 3 strategies for determing need to start next lookahead round
      //-1 slowLookTable(s1_lookCount), use slowLook , relatively conservative query
      //-2 s1_lookCount,  medium level
      //-3 s0_lookCOunt >> 2, very aggressive
      //-4 Mux(q.io.empty, slowLookTable(s1_lookCount), s1_lookCount) ,considering receive queue used situation
    //4. calculate s2_child new data entry
  // s1_child_sig should aligned to s1_parent_sig when first lookahead
  val s2_readResult = WireInit(0.U.asTypeOf(new pTableEntry));dontTouch(s2_readResult)
  val s2_child = WireInit(0.U.asTypeOf(new pt_SignatureTableResp));dontTouch(s2_child)
  val s2_valid = WireInit(false.B)
  val s2_first_flag = WireInit(false.B)
  val s2_bp_use_valid = WireInit(false.B)
  val s2_bp_blk = WireInit(0.U(blkAddrBits.W))

  val s1_fromStReq = io.fromStReq_s1
  val s_idle :: s_lookahead0 :: s_lookahead :: Nil = Enum(3)
  val state_s2s1 = RegInit(s_idle)

  val s2_s1_hit = WireInit(false.B)

  val s1_first_flag = RegNext(s0_fire,false.B)
  val s1_continue = WireInit(false.B);dontTouch(s1_continue)
  val s1_valid = WireInit(s1_first_flag || (!s2_valid && s1_continue))

  // sig and block hold from s0
  val s1_parent = WireInit(0.U.asTypeOf(new pt_SignatureTableResp));dontTouch(s1_parent)
  val s1_child = WireInit(0.U.asTypeOf(new pt_SignatureTableResp));dontTouch(s1_child)

  val s1_bp_use_valid = WireInit(io.fromStReq_s1.bits.bp_use_valid);dontTouch(s1_bp_use_valid)
  val s1_bp_blk = WireInit(io.fromStReq_s1.bits.bp_redirect_blk);dontTouch(s1_bp_blk)

  s1_parent.block := RegEnable(s0_block,0.U,s0_fire)
  s1_parent.sig := RegEnable(s0_sig,0.U,s0_fire)
  s1_parent.delta := DontCare

  // s1_delta from st s2 pipe
  val s1_delta = WireInit(s1_fromStReq.bits.newDelta);dontTouch(s1_delta)

  val s1_lookCount = RegInit(0.U(lookCountBits.W));dontTouch(s1_lookCount)
  val s1_miniCount = WireInit(0.U(lookCountBits.W));dontTouch(s1_miniCount)

  //forward hold dequeue data
  val s2_s1_bypass_result = RegNext(s2_readResult,0.U.asTypeOf(new pTableEntry))
  val s2_s1_bypass_sig = Mux(s1_first_flag||s2_first_flag, s1_parent.sig, RegNext(s2_child.sig,0.U))
  val s2_s1_bypass_block = RegNext(Mux(s2_bp_use_valid, s2_bp_blk, s2_child.block),0.U)

  val s1_cal_maxEntry = WireInit(s2_s1_bypass_result.deltaEntries.reduce((a, b) => Mux(a.cDelta >= b.cDelta, a, b)))
  val s1_bypass_valid = WireInit(s2_s1_bypass_result.valid)
  val s1_bypass_tag = WireInit(s2_s1_bypass_result.tag)
  //temporary calculate only for machine control
  val s2_s1_testBlock = WireInit((s2_s1_bypass_block.asSInt + s1_cal_maxEntry.delta).asUInt);dontTouch(s2_s1_testBlock)
  s2_s1_hit := s1_bypass_valid && (get_tag(s2_s1_bypass_sig) === s1_bypass_tag)
  s1_continue := state_s2s1 === s_lookahead && s1_cal_maxEntry.cDelta >= s1_miniCount
  //| sig | delta | block |
  when(state_s2s1 === s_idle){
    s1_lookCount := 0.U
  }.elsewhen(s1_valid){
    s1_lookCount := s1_lookCount + 1.U
  }.otherwise{
    s1_lookCount := s1_lookCount
  }
  
  when(state_s2s1 === s_lookahead){
    s1_child.sig := makeSign(s2_s1_bypass_sig,s1_cal_maxEntry.delta)
    s1_child.block := s2_s1_testBlock
    s1_child.delta := s1_cal_maxEntry.delta
  }.otherwise{
    s1_child.sig := s1_fromStReq.bits.newSig
    s1_child.block := Mux(s1_bp_use_valid, s1_bp_blk, s1_parent.block)
    s1_child.delta := s1_fromStReq.bits.newDelta
  }

  def slowLookTable(lc: UInt): UInt = {
    Mux(lc >= 1.U && lc <= 5.U, (lc >> 1.U) + 1.U, lc)
  }
  //TODO: need to be optimized !!! some error here
  // val s1_miniCount = slowLookTable(s1_lookCount) // test1
  // s1_miniCount := s1_lookCount // test2
  s1_miniCount := Mux(io.ctrl.en_slowLookUp, slowLookTable(s1_lookCount), s1_lookCount) // test3
  s1_ready := state_s2s1 === s_idle
  s1_fromStReq.ready := true.B
  val s1_can_write = WireInit(state_s2s1 === s_lookahead0);dontTouch(s1_can_write)
  // --------------------------------------------------------------------------------
  // stage 2
  // --------------------------------------------------------------------------------
  //1. calculate value for next update
  //2. calculate lookcount when sram read finished
  //caution: do not need check cross page in s1
  s2_valid := RegNext(s1_valid ,false.B)
  s2_child := RegNext(s1_child ,0.U.asTypeOf(new pt_SignatureTableResp))
  s2_first_flag := RegNext(s1_first_flag,false.B)

  //directly calculate from sram 
  s2_readResult := Mux(s2_valid, pTable.io.r.resp.data(0), 0.U.asTypeOf(new pTableEntry))

  //pressure calculate, there should not take complex logics
  val s2_is_crossPage = WireInit(is_samePage(s2_child.block,s1_parent.block));dontTouch(s2_is_crossPage)
  //TODO: > or >= ?
  
  //FSM
  switch(state_s2s1) {
    is(s_idle) {
      when(s0_fire) {
        state_s2s1 := s_lookahead0
      }
    }
    is(s_lookahead0) {
        state_s2s1 := s_lookahead
    }
    is(s_lookahead) {
        when(s1_valid || s2_valid) {
            state_s2s1 := s_lookahead
        }.otherwise{
            state_s2s1 := s_idle
        }
    }
  }

  // --------------------------------------------------------------------------------
  // stage 3
  // --------------------------------------------------------------------------------
  //calculate update paternTable's data
  //hold needed write sig when fisrt read sram index
  //1. when leave lookahead0,hold needed writing data
  //2. sendout ptable request
  val s3_valid = RegNext(s2_valid,false.B)
  val s3_state = RegNext(state_s2s1,s_idle)
  val s3_first_flag = RegNext(s2_first_flag,false.B)
  val s3_write_valid = RegNextN(s1_can_write,2,Some(false.B))
  val s3_miniCount = RegNextN(s1_miniCount,2,Some(0.U))
  val s3_lookCount = RegNextN(s1_lookCount,2,Some(0.U))
  
  val s3_enprefetch = WireInit(false.B)
  val s3_enprefetchnl = WireInit(false.B)
  //these should hold
  val s3_current = RegEnable(s2_child,0.U.asTypeOf(new pt_SignatureTableResp),s2_valid)
  val s3_readResult = RegEnable(s2_readResult,0.U.asTypeOf(new pTableEntry),s2_valid)

  //TODO : need tag match ???
  val s3_hit = WireInit(s3_readResult.valid && get_tag(s3_current.sig) === s3_readResult.tag)
  //pressure calculate
  // assert(s3_current.delta === 0.S,"s3_current.delta sould equal 0")
  val s3_smallest: SInt = s3_readResult.deltaEntries.reduce((a, b) => Mux(a.cDelta < b.cDelta, a, b)).delta
  val s3_replaceIdx: UInt = s3_readResult.deltaEntries.indexWhere(a => a.delta === s3_smallest)
  val s3_exist = s3_readResult.deltaEntries.map(_.delta === s3_current.delta).reduce(_ || _)
  val s3_wTemp = s3_readResult.deltaEntries.map(x => Mux(x.delta === s3_current.delta, (new DeltaEntry).apply(s3_current.delta, x.cDelta + 1.U), x))
  val s3_wdeltaEntries = WireInit(VecInit(Seq.fill(pTableDeltaEntries)(0.U.asTypeOf(new DeltaEntry()))));dontTouch(s3_wdeltaEntries)
  val s3_wEntry = WireInit(0.U.asTypeOf(new pTableEntry()))

  //set output
  val s3_delta_list = s3_readResult.deltaEntries.map(x => 
    Mux(x.cDelta >= s3_miniCount,
      Mux(io.ctrl.en_shareBO,
        ParallelPriorityMux(
          Seq(
            (x.delta === 0.S) -> 0.S,
            (x.delta > 0.S && s1_ghr_shareBO > 0.S )-> (x.delta + s1_ghr_shareBO),
            (x.delta < 0.S && s1_ghr_shareBO < 0.S) -> (x.delta + s1_ghr_shareBO),
            (x.delta > 0.S && s1_ghr_shareBO < 0.S) -> (x.delta - s1_ghr_shareBO),
            (x.delta < 0.S && s1_ghr_shareBO > 0.S) -> (x.delta - s1_ghr_shareBO),
        )),x.delta),
      0.S)
  )

  val s3_delta_list_checked = WireInit(VecInit(Seq.fill(pTableDeltaEntries)(0.S((blkOffsetBits + 1).W))))
  val s3_delta_list_nl = WireInit(VecInit(Seq.fill(pTableDeltaEntries)(0.S((blkOffsetBits + 1).W))));dontTouch(s3_delta_list_nl)
  
  val s3_NL_blkAddr =WireInit(VecInit(Seq.fill(pTableDeltaEntries)(0.U(blkAddrBits.W))))
  // pressure calculate
  // normal lookahead
  s3_delta_list_checked := s3_delta_list.map(x => Mux(is_samePage((s3_current.block.asSInt + x).asUInt, s3_current.block), x, 0.S))
  // nextline
  s3_delta_list_nl := s3_delta_list.zipWithIndex.map(d => Mux(s1_ghr_shareBO > 0.S, s1_ghr_shareBO + d._2.S, s1_ghr_shareBO - d._2.S))
  s3_NL_blkAddr := s3_delta_list_nl.map(x => (s3_current.block.asSInt + x).asUInt)

  val s3_issued = s3_delta_list_checked.map(a => Mux(a =/= 0.S, 1.U, 0.U)).reduce(_ +& _)
  //
  s3_enprefetch := s3_valid && s3_hit && s3_issued =/= 0.U
  when(s3_valid && s3_first_flag && (!s3_enprefetch || io.ctrl.en_Nextline_Agreesive)) {
    s3_enprefetchnl := ENABLE_NL.B && is_samePage(s3_NL_blkAddr.head, s3_current.block)
  }

  // calculate needed writing delta counters
  when(s3_write_valid && s3_hit) {
    when(s3_exist) {
      //counter overflow --- only considering count overflow
      when(s3_readResult.count + 1.U === ((1.U << s3_readResult.count.getWidth).asUInt - 1.U)) {
        s3_wdeltaEntries := s3_wTemp.map(x => (new DeltaEntry).apply(x.delta, x.cDelta >> 1.asUInt))
      } .otherwise {
        s3_wdeltaEntries := s3_wTemp
      }
    } .otherwise {
      //to do replacement
      s3_wdeltaEntries := VecInit.tabulate(s3_readResult.deltaEntries.length) { i =>
        Mux((i.U === s3_replaceIdx), (new DeltaEntry).apply(s3_current.delta, 1.U), s3_readResult.deltaEntries(i))
      }
    }
    //to consider saturate here
  } .otherwise {
    s3_wdeltaEntries(0).delta := s1_parent.delta
    s3_wdeltaEntries(0).cDelta := 1.U
  }
  s3_wEntry.tag := get_tag(s1_parent.sig)
  s3_wEntry.valid := true.B
  s3_wEntry.deltaEntries := s3_wdeltaEntries
  s3_wEntry.count := DontCare
  // --------------------------------------------------------------------------------
  // stage 4
  // --------------------------------------------------------------------------------
  // write ptable
  val s4_valid = RegNext(s3_valid, false.B)
  val s4_write_valid = RegNext(s3_first_flag,false.B)
  val s4_wEntry = RegNext(s3_wEntry,0.U.asTypeOf(new pTableEntry))
  val s4_sig = RegNext(s3_current.sig,0.U)
  val s4_block = RegNext(s3_current.block,0.U)
  val s4_delta_list_checked = RegNext(s3_delta_list_checked,0.U.asTypeOf((s3_delta_list_checked.cloneType)))
  val s4_NL_blkAddr_longest = RegNext(s3_NL_blkAddr.last, 0.U)
  val s4_enprefetch = RegNext(s3_enprefetch,false.B)
  val s4_enprefetchnl = RegNext(s3_enprefetchnl,false.B)


  val s4_bp_update = WireInit(s4_valid && (s4_enprefetch || s4_enprefetchnl) && io.ctrl.en_bp_recovery);dontTouch(s4_bp_update)
  //pressure caculate
  val s4_wCount_accum = WireInit(s4_wEntry.deltaEntries.map(_.cDelta).reduce(_ + _))
  val s4_delta_longest = WireInit(s4_delta_list_checked.reduce((a,b) => Mux(a > b, a, b)));dontTouch(s4_delta_longest)
  // --------------------------------------------------------------------------------
  // pTable operation
  // --------------------------------------------------------------------------------
  pTable.io.r.req.valid := s0_fire || s1_valid
  when(s0_fire){
    pTable.io.r.req.bits.setIdx := get_idx(s0_sig)
  }.otherwise{
    pTable.io.r.req.bits.setIdx := get_idx(s1_child.sig)
  }

  pTable.io.w.req.valid := s4_valid && s4_write_valid
  pTable.io.w.req.bits.setIdx := RegEnable(get_idx(s1_parent.sig),0.U,s3_write_valid)
  pTable.io.w.req.bits.data(0) := s4_wEntry
  pTable.io.w.req.bits.data(0).count := s4_wCount_accum
  
  //update bp
  io.pt2st_bp.valid := ENABLE_BP.asBool && s4_bp_update
  io.pt2st_bp.bits.blkAddr := Mux(s4_enprefetchnl, s4_NL_blkAddr_longest, (s4_block.asSInt + s4_delta_longest.asSInt).asUInt)
  io.pt2st_bp.bits.parent_sig(0) := s4_sig
  dontTouch(io.pt2st_bp)

  // output
  io.resp.valid := s3_enprefetch || s3_enprefetchnl
  io.resp.bits.block := RegNext(s3_current.block)
  when(s3_enprefetchnl) {
    io.resp.bits.deltas := s3_delta_list_nl
  }.otherwise{
    io.resp.bits.deltas := s3_delta_list_checked
  }
  // io.resp.bits.degree := s1_lookCount
  io.resp.bits.source := RegEnable(s1_parent.source, state_s2s1 === s_lookahead0)
  io.resp.bits.needT := RegEnable(s1_parent.needT, state_s2s1 === s_lookahead0)

  //perf
  XSPerfAccumulate("spp_pt_bp_nums",io.pt2st_bp.valid)
  XSPerfAccumulate("spp_pt_hit",s3_state === s_lookahead && s3_hit)
  XSPerfAccumulate("spp_pt_lookaheadX",state_s2s1 === s_lookahead && s1_valid)
  for (i <- 2 until 7){
      XSPerfAccumulate(s"spp_pt_lookahead${i}",state_s2s1 === s_lookahead && s1_lookCount === i.U)
  }
  XSPerfAccumulate("spp_pt_enpf",state_s2s1 === s_lookahead && s3_enprefetch)
  XSPerfAccumulate("spp_pt_nextLine",state_s2s1 === s_lookahead && s3_enprefetchnl)
  XSPerfAccumulate("spp_pt_cross_page",state_s2s1 === s_lookahead && s2_valid && 
    s3_delta_list.map(x => is_samePage((s3_current.block.asSInt + x).asUInt,s3_current.block)).reduce(_||_))
  for (i <- 0 until pTableEntries) {
    XSPerfAccumulate(s"spp_pt_touched_entry_onlyset_${i.toString}", pTable.io.r.req.bits.setIdx === i.U(log2Up(pTableEntries).W)
    )
  }
}

class Unpack(implicit p: Parameters) extends SPPModule {
  val io = IO(new Bundle {
    val req = Flipped(DecoupledIO(new PatternTableResp))
    val resp = DecoupledIO(new UnpackResp)
  })

  def idx(addr:      UInt) = addr(log2Up(fTableEntries) - 1, 0)
  def tag(addr:      UInt) = addr(fullAddressBits - offsetBits - 1, log2Up(fTableEntries))

  def fTableEntry() = new Bundle {
    val valid = Bool()
    val tag = UInt(fTagBits.W)
  }
  val fTable = RegInit(VecInit(Seq.fill(fTableEntries)(0.U.asTypeOf(fTableEntry()))))

  val inProcess = RegInit(false.B)
  val endeq = WireInit(false.B)

  val q = Module(new ReplaceableQueueV2(chiselTypeOf(io.req.bits), unpackQueueEntries))
  q.io.enq <> io.req //change logic to replace the tail entry

  val req = RegEnable(q.io.deq.bits,0.U.asTypeOf(new PatternTableResp), q.io.deq.fire)
  val req_deltas = Reg(Vec(pTableDeltaEntries, SInt((blkOffsetBits + 1).W)))
  val issue_finish = req_deltas.map(_ === 0.S).reduce(_ && _)
  q.io.deq.ready := !inProcess || issue_finish || endeq
  when(q.io.deq.fire) {
    req_deltas := q.io.deq.bits.deltas
  }
  
  val enresp = WireInit(false.B)
  val extract_delta = req_deltas.reduce((a, b) => Mux(a =/= 0.S, a, b))
  val prefetchBlock = (req.block.asSInt + extract_delta).asUInt

  val hit = WireInit(false.B)
  val s1_result = WireInit(0.U.asTypeOf(fTableEntry()))
  s1_result := fTable(idx(prefetchBlock))
  hit := s1_result.valid && tag(prefetchBlock) === s1_result.tag

  when(enresp && !hit) {
    fTable(idx(prefetchBlock)).valid := true.B
    fTable(idx(prefetchBlock)).tag := tag(prefetchBlock)
  }

  io.resp.valid := RegNext(enresp && !hit,false.B)
  io.resp.bits.prefetchBlock := RegNext(prefetchBlock,0.U((blkAddrBits).W))
  io.resp.bits.source := 0.U
  io.resp.bits.needT := false.B

  when(inProcess) {
    when(!issue_finish) {
      val cnt: UInt = req_deltas.count(_ =/= 0.S)
      enresp := true.B
      // req_deltas := req_deltas.map(a => Mux(a === extract_delta, 0.S, a))
      when(cnt === 1.U) {
        endeq := true.B
        when(!q.io.deq.fire) {
          req_deltas := req_deltas.map(a => Mux(a === extract_delta, 0.S, a))
        }
      } .otherwise {
        req_deltas := req_deltas.map(a => Mux(a === extract_delta, 0.S, a))
      }
    } .otherwise {
      when(!q.io.deq.fire) {
        inProcess := false.B
      }
    }
  } .otherwise {
    when(q.io.deq.fire) {
      inProcess := true.B
    }
  }
}

class SignaturePathPrefetch(implicit p: Parameters) extends SPPModule {
  val io = IO(new Bundle() {
    val train = Flipped(DecoupledIO(new PrefetchTrain)) 
    val req = DecoupledIO(new PrefetchReq)
    val req_hint2llc = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val from_ghr = Flipped(ValidIO(new GhrForSpp))
    val sppCtrl = Input(UInt(PfCtrlConst.SppConfig.bits.W))
  })

  val sppCtrl = WireInit(io.sppCtrl);dontTouch(sppCtrl)
  //sppConfig[0] -> enable hint2llc   
  //sppConfig[1] -> enable Nextline Agreesive
  //sppConfig[2] -> enable bp recovery
  //sppConfig[3] -> enable shareBO
  //sppConfig[4] -> enable slowLookUp  
  val ctrl_hint2llc_en = WireInit(false.B);//WireInit(sppCtrl(0));dontTouch(ctrl_hint2llc_en)
  val ctrl_nl_agressive = WireInit(false.B);//WireInit(sppCtrl(1));dontTouch(ctrl_nl_agressive)
  val ctrl_bp_recovery = WireInit(sppCtrl(2));dontTouch(ctrl_bp_recovery)
  val ctrl_shareBO = WireInit(sppCtrl(3));dontTouch(ctrl_shareBO)
  val ctrl_slowLookup = WireInit(sppCtrl(4));dontTouch(ctrl_slowLookup)

  val sTable = Module(new SignatureTable)
  val pTable = Module(new PatternTable)
  val unpack = Module(new Unpack)

  sTable.io.req.valid := io.train.valid
  sTable.io.req.bits.blkAddr := io.train.bits.blkAddr
  sTable.io.req.bits.needT := io.train.bits.needT
  sTable.io.req.bits.source := io.train.bits.source
  sTable.io.s0_bp_update <> pTable.io.pt2st_bp
  sTable.io.ctrl.en_bp_recovery := ctrl_bp_recovery
  sTable.io.req.bits.fromGHR_shareBO := DontCare
  io.train.ready := sTable.io.req.ready

  pTable.io.fromStReq_s0 <> sTable.io.s0_toPtReq
  pTable.io.fromStReq_s1 <> sTable.io.s1_toPtReq
  pTable.io.resp <> unpack.io.req
  pTable.io.from_ghr <> RegNext(io.from_ghr,0.U.asTypeOf(io.from_ghr.cloneType))
  pTable.io.ctrl.en_Nextline_Agreesive := ctrl_nl_agressive
  pTable.io.ctrl.en_bp_recovery := ctrl_bp_recovery
  pTable.io.ctrl.en_shareBO := ctrl_shareBO
  pTable.io.ctrl.en_slowLookUp := ctrl_slowLookup

  val req = WireInit(0.U.asTypeOf(new PrefetchReq))

  val pf_newAddr = WireInit(Cat(unpack.io.resp.bits.prefetchBlock, 0.U(offsetBits.W)))
  req.tag := parseFullAddress(pf_newAddr)._1
  req.set := parseFullAddress(pf_newAddr)._2
  req.needT := unpack.io.resp.bits.needT
  req.source := unpack.io.resp.bits.source
  req.pfVec := PfSource.SPP

  io.req.valid := unpack.io.resp.valid && io.from_ghr.bits.global_queue_used <= hin2llcQueueThreshold.U
  io.req.bits := req
  unpack.io.resp.ready := io.req.ready

  dontTouch(io.req_hint2llc)
  dontTouch(io.from_ghr)
  io.req_hint2llc.valid := ctrl_hint2llc_en && unpack.io.resp.valid && io.from_ghr.bits.global_queue_used > hin2llcQueueThreshold.U
  io.req_hint2llc.bits := Mux(io.req_hint2llc.valid,req,0.U.asTypeOf(new PrefetchReq))

  io.resp.ready := true.B

  XSPerfAccumulate("recv_train", io.train.fire)
  XSPerfAccumulate("recv_pt", Mux(pTable.io.resp.fire, pTable.io.resp.bits.deltas.map(a => Mux(a =/= 0.S, 1.U, 0.U)).reduce(_ +& _), 0.U))
  XSPerfAccumulate("recv_up", unpack.io.resp.fire)
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

trait HasHyperPrefetchDev2Params extends HasCoupledL2Parameters {
  val hyperParams = HyperPrefetchParams()

  val blkAddrBits = fullAddressBits - offsetBits
  val pageAddrBits = fullAddressBits - pageOffsetBits
  val blkOffsetBits = pageOffsetBits - offsetBits
  val blkNums = 1<<blkOffsetBits //64

  val fTableEntries = hyperParams.fTableEntries
  val fTagBits = pageAddrBits - log2Up(fTableEntries)
  val pTableQueueEntries = hyperParams.pTableQueueEntries
  val fTableQueueEntries = hyperParams.fTableQueueEntries
  val bop_pfReqQueueEntries = 4
  val spp_pfReqQueueEntries = 16
  val sms_pfReqQueueEntries = 16
  def get_blockAddr(x:UInt) = x(fullAddressBits-1,offsetBits)
}

abstract class HyperPrefetchDev2Module(implicit val p: Parameters) extends Module with HasHyperPrefetchDev2Params with HasPerfLogging
abstract class HyperPrefetchDev2Bundle(implicit val p: Parameters) extends Bundle with HasHyperPrefetchDev2Params

class FilterTable(parentName:String = "Unknown")(implicit p: Parameters) extends HyperPrefetchDev2Module {
  val io = IO(new Bundle() {
    val in_smsReq = Flipped(DecoupledIO(new PrefetchReq))
    val in_bopReq = Flipped(DecoupledIO(new PrefetchReq))
    val in_sppReq = Flipped(DecoupledIO(new PrefetchReq))
    val in_trainReq = Flipped(DecoupledIO(new PrefetchReq))

    val out_smsReq = DecoupledIO(new PrefetchReq)
    val out_bopReq = DecoupledIO(new PrefetchReq)
    val out_sppReq = DecoupledIO(new PrefetchReq)
    //
    val evict = Flipped(DecoupledIO(new PrefetchEvict))
    val is_hint2llc = Input(Bool())
    val hint2llc_out = ValidIO(new PrefetchReq)
    val ctrl = Input(UInt(PfCtrlConst.FitlerTableConfig.bits.W))
  })
  val ctrl_filter_sms = WireInit(io.ctrl(PfVectorConst.SMS));dontTouch(ctrl_filter_sms)
  val ctrl_filter_bop = WireInit(io.ctrl(PfVectorConst.BOP));dontTouch(ctrl_filter_bop)
  val ctrl_filter_spp = WireInit(io.ctrl(PfVectorConst.SPP));dontTouch(ctrl_filter_spp)
  def get_idx(addr:      UInt) = addr(log2Up(fTableEntries) - 1, 0)
  def get_tag(addr:      UInt) = addr(pageAddrBits - 1, log2Up(fTableEntries))
  object FitlerVecState {
    val bits = 3

    def toN = 0.U(bits.W)
    def toB = 1.U(bits.W)
    def toS = 2.U(bits.W)
    def toC = 3.U(bits.W)

    def None          = 0.U(bits.W)
    def SMS           = 1.U(bits.W)
    def BOP           = 2.U(bits.W)
    def SPP           = 4.U(bits.W)

    def COMMON        = 7.U(bits.W)

    //TODO: further study, is need bop update filterTable?
    // def getVecState(isHit:Bool, originV:UInt, trigerId:UInt) = (originV | trigerId) & ~(BOP)
    def getVecState(isHit:Bool, originV:UInt, trigerId:UInt) = (originV | trigerId)
    def checkOne(v:UInt) = v === BOP || v === SPP
    def checkTwo(v:UInt) = v === COMMON
    def hasMyself(v:UInt,originV:UInt) = (v & originV) === v
    def hasMerged(v:UInt,originV:UInt) = v =/= originV
    def is_SPPchase(v:UInt,originV:UInt) = hasMyself(v,SPP) && (originV === BOP || originV === (BOP | SMS))
  }
  def has_sms(x:UInt): Bool=x(FitlerVecState.SMS)
  def has_bop(x:UInt): Bool=x(FitlerVecState.BOP)
  def has_spp(x:UInt): Bool=x(FitlerVecState.SPP)
  val dupNums = 4
  val dupOffsetBits = log2Up(fTableEntries/dupNums)
  val dupBits = log2Up(dupNums)
  val req_dups = RegInit(VecInit(Seq.fill(dupNums)(0.U.asTypeOf(Valid(new PrefetchReq)))))
  // val req_issue = WireInit(0.U.asTypeOf(DecoupledIO(new PrefetchReq())));dontTouch(req_issue)
  dontTouch(io)
  // --------------------------------------------------------------------------------
  // consensus Table cTable
  // --------------------------------------------------------------------------------
  // | valid | tag | cVec[[pfVec],[pfVec],...,[pfVec]] |
  // | valid | tag | cVec[[pfVec],[pfVec],...,[pfVec]] |
  // | valid | tag | cVec[[pfVec],[pfVec],...,[pfVec]] |
  // | valid | tag | cVec[[pfVec],[pfVec],...,[pfVec]] |
  // | valid | tag | cVec[[001], [100] , ..., [111]]   |
  //                               ^
  //                               |
  //                            archored_value
    def fTableEntry() = new Bundle {
      val valid = Bool()
      val tag = UInt(fTagBits.W)
      val cVec = Vec(blkNums, UInt(FitlerVecState.bits.W))
    }
    // val consensusTable = Mem(fTableEntries,fTableEntry())
    val consensusTable = RegInit(VecInit(Seq.fill(fTableEntries)(0.U.asTypeOf(fTableEntry()))))
    // val evict_q = Module(new Queue(UInt(fullAddressBits.W), fTableQueueEntries, flow = false, pipe = true))
    val evict_q = Module(new SRAMQueue(UInt(31.W),entries = fTableQueueEntries, flow = false, 
        hasMbist = cacheParams.hasMbist, hasClkGate=enableClockGate, hasShareBus = cacheParams.hasShareBus, parentName=parentName+"filterDelayQ"))

    // --------------------------------------------------------------------------------
    // stage 0
    // --------------------------------------------------------------------------------
    val s0_bop_tagHit = WireInit(false.B)
    val s0_spp_tagHit = WireInit(false.B)
    // read filterTable
    val s0_valid = WireInit(false.B);dontTouch(s0_valid)
    val s0_req = WireInit(0.U.asTypeOf(new PrefetchReq));dontTouch(s0_req)
    val s0_result = WireInit(0.U.asTypeOf(fTableEntry()));dontTouch(s0_result)
    val s0_isHint2llc = WireInit(io.is_hint2llc)
    val s0_fromTrain = WireInit(io.in_trainReq.fire)
    val s0_train_pfVec = WireInit(VecInit(Seq.fill(blkNums)(0.U(FitlerVecState.bits.W))))
    val s0_train_pageTag = WireInit(0.U((pageAddrBits - log2Up(fTableEntries)).W));dontTouch(s0_train_pageTag)

    //l1 pf need go normal pipeline
    s0_bop_tagHit := get_tag(io.in_bopReq.bits.get_pageAddr) === s0_train_pageTag
    s0_spp_tagHit := get_tag(io.in_sppReq.bits.get_pageAddr) === s0_train_pageTag

    val s0_skip_filter = WireInit(false.B)

    val replay_Q0 = Module(new Queue(new PrefetchReq, 4, flow = false, pipe = false))
    val replay_Q1 = Module(new Queue(new PrefetchReq, 4, flow = false, pipe = false))
    val replay_Q2 = Module(new Queue(new PrefetchReq, 4, flow = false, pipe = true))

    val quiteUpdateQ = Module(new Queue(new PrefetchReq, 4, flow = true, pipe = false))
    val quiteUpdateQ_pfVec = Module(new Queue(s0_train_pfVec.cloneType, 4, flow = true, pipe = false))

    def get_stall(x:DecoupledIO[PrefetchReq]):Bool = x.valid && !x.ready
    val s0_sppReq_stall = WireInit(get_stall(io.in_smsReq))
    val s0_smsReq_stall = WireInit(get_stall(io.in_smsReq))

    replay_Q0.io.enq <> io.in_bopReq
    replay_Q1.io.enq <> io.in_sppReq
    replay_Q2.io.enq <> io.in_smsReq
    replay_Q0.io.enq.valid := io.in_bopReq.valid && !s0_bop_tagHit
    replay_Q1.io.enq.valid := io.in_sppReq.valid && !s0_spp_tagHit

    //only l2 pf req need go quiteUpdateQ
    quiteUpdateQ.io.enq.valid := (io.in_bopReq.valid && s0_bop_tagHit) || (io.in_sppReq.valid && s0_spp_tagHit)
    quiteUpdateQ.io.enq.bits := ParallelPriorityMux(
      Seq(
        io.in_bopReq.valid -> io.in_bopReq.bits,
        io.in_sppReq.valid -> io.in_sppReq.bits,
       // q_hint2llc.io.deq.valid -> q_hint2llc.io.deq.bits,
      )
    )
    quiteUpdateQ_pfVec.io.enq.valid := quiteUpdateQ.io.enq.valid
    quiteUpdateQ_pfVec.io.enq.bits := s0_train_pfVec.zipWithIndex.map(x => 
      Mux(x._2.U === quiteUpdateQ.io.enq.bits.get_blockOff,
       x._1 | quiteUpdateQ.io.enq.bits.pfVec, x._1))
    s0_valid := quiteUpdateQ.io.deq.valid || replay_Q0.io.deq.valid || replay_Q1.io.deq.valid  || replay_Q2.io.deq.valid || io.in_trainReq.valid //|| q_hint2llc.io.deq.valid
    s0_req := ParallelPriorityMux(
      Seq(
        replay_Q2.io.deq.valid -> replay_Q2.io.deq.bits,
        replay_Q1.io.deq.valid -> replay_Q1.io.deq.bits,
        replay_Q0.io.deq.valid -> replay_Q0.io.deq.bits,
        quiteUpdateQ.io.deq.valid -> quiteUpdateQ.io.deq.bits,
        io.in_trainReq.valid -> io.in_trainReq.bits,
       // q_hint2llc.io.deq.valid -> q_hint2llc.io.deq.bits,
      )
    )

    replay_Q2.io.deq.ready := true.B
    replay_Q1.io.deq.ready := !replay_Q2.io.deq.valid
    replay_Q0.io.deq.ready := !replay_Q2.io.deq.valid && !replay_Q1.io.deq.valid
    quiteUpdateQ.io.deq.ready := !replay_Q2.io.deq.valid && !replay_Q1.io.deq.valid && !replay_Q0.io.deq.valid
    quiteUpdateQ_pfVec.io.deq.ready := quiteUpdateQ.io.deq.ready
    io.in_trainReq.ready := true.B
    // wait_issueQ.io.deq.ready := !io.in_smsReq.valid

    s0_result := consensusTable(get_idx(s0_req.get_pageAddr))
    
    for(i <- 0 until(blkNums)){
      s0_train_pfVec(i) := RegEnable(s0_result.cVec(i), 0.U(FitlerVecState.bits.W), s0_valid && s0_fromTrain)
    }
    s0_train_pageTag := RegEnable(s0_result.tag, 0.U, s0_valid && s0_fromTrain)
    s0_skip_filter := quiteUpdateQ.io.deq.fire

    val s0_fwd_wBitMap = WireInit(VecInit.fill(blkNums)(0.U(PfVectorConst.bits.W)));dontTouch(s0_fwd_wBitMap)
    for (j <- 0 until blkNums){
        s0_fwd_wBitMap(j) := quiteUpdateQ_pfVec.io.deq.bits(j)
    }
    
    // --------------------------------------------------------------------------------
    // stage 1
    // --------------------------------------------------------------------------------
    // calculate
    // send out prefetch request
    val s1_valid = VecInit.fill(dupNums)(RegNext(s0_valid,false.B));dontTouch(s1_valid)
    val s1_req = VecInit.fill(dupNums)(RegEnable(s0_req,0.U.asTypeOf(new PrefetchReq),s0_valid));dontTouch(s1_req)
    val s1_result = VecInit.fill(dupNums)(RegEnable(s0_result,0.U.asTypeOf(fTableEntry()),s0_valid));dontTouch(s1_result)
    val s1_isHint2llc = RegNext(s0_isHint2llc,false.B)
    val s1_fromTrain = RegNext(s0_fromTrain,false.B);dontTouch(s1_fromTrain)
    val s1_skip_filter = RegNext(s0_skip_filter,false.B)
    val s1_fwd_wBitMap = RegNext(s0_fwd_wBitMap)

    val s1_oldAddr = WireInit(s1_req(1).addr);dontTouch(s1_oldAddr)
    val s1_dup_offset = WireInit(s1_req(1).set(dupOffsetBits-1+dupBits-1,dupOffsetBits-1));dontTouch(s1_dup_offset)

    val s1_pageAddr = WireInit(s1_req(1).get_pageAddr);dontTouch(s1_pageAddr)
    val s1_blkOffset = WireInit(s1_req(1).get_blockOff);dontTouch(s1_blkOffset)
    val s1_hit = WireInit(VecInit.fill(dupNums)(false.B))
    
    val s1_hitForMap_filtedpfVec = WireInit(VecInit.fill(dupNums)(0.U(PfSource.bits.W)));dontTouch(s1_hitForMap_filtedpfVec)
    val s1_hitForMap_bitVec = WireInit(VecInit.fill(dupNums)(VecInit.fill(blkNums)(false.B)));dontTouch(s1_hitForMap_bitVec)
    //val hitForMap_needDrop = WireInit(VecInit.fill(dupNums)(false.B));dontTouch(hitForMap_needDrop)
    val s1_can_send2_pfq = WireInit(VecInit.fill(dupNums)(false.B));dontTouch(s1_can_send2_pfq)
    //val s1_anchored_longest_blkOff = WireInit(VecInit.fill(dupNums)(0.U(blkOffsetBits.W)));dontTouch(s1_anchored_longest_blkOff)
    val s1_next_VecState = WireInit(VecInit.fill(dupNums)(0.U(PfVectorConst.bits.W)))

    val s1_wBitMap = WireInit(VecInit.fill(dupNums)(VecInit.fill(blkNums)(0.U(FitlerVecState.bits.W))))
    val s1_wData = WireInit(VecInit.fill(dupNums)(0.U.asTypeOf(fTableEntry())));dontTouch(s1_wData)

    for(i <- 0 until(dupNums)) {
      val trigerId = s1_req(i).pfVec
      val anchored_cVec = s1_result(i).cVec
      val anchored_value = s1_result(i).cVec(s1_blkOffset)
      s1_next_VecState(i) := Mux(s1_hit(i),FitlerVecState.getVecState(true.B, originV = anchored_value, trigerId = s1_req(i).pfVec),s1_req(i).pfVec)

      s1_hit(i) := s1_result(i).valid && get_tag(s1_pageAddr) === s1_result(i).tag
      //hitForMap_needDrop(i) := hit(i) && FitlerVecState.hasMerged(s1_req(i).pfVec, anchored_value)
      for (j <- 0 until s1_result(i).cVec.length){
          when(s1_hit(i)){       
              s1_hitForMap_bitVec(i)(j) := anchored_cVec(j) =/= PfSource.NONE
              s1_hitForMap_filtedpfVec(i) := s1_result(i).cVec(s1_blkOffset)
          }.otherwise{
              s1_hitForMap_bitVec(i)(j) := false.B
              s1_hitForMap_filtedpfVec(i) := PfSource.NONE
          }
      }
      //s1_anchored_longest_blkOff(i) := OneHot.OH1ToUInt(HighestBit(s1_hitForMap_bitVec(i).asUInt,blkNums))
      // should filter when any other prefetchBitVec existed expected prefetch from bop
      s1_can_send2_pfq(i) := !s1_skip_filter && (!s1_hit(i) || (s1_hit(i) && (anchored_value === PfSource.NONE ||  s1_req(i).hasBOP)))//anchored_value === PfSource.NONE //|| anchored_value === PfSource.BOP || anchored_value === PfSource.SMS

      when(!s1_fromTrain){
        for (j <- 0 until blkNums){
            when(s1_skip_filter){
              s1_wBitMap(i) := s1_fwd_wBitMap
            }.elsewhen(s1_hit(i)){
              s1_wBitMap(i)(j) := Mux(j.asUInt === s1_blkOffset, s1_next_VecState(i), anchored_cVec(j))
            }.otherwise{
              s1_wBitMap(i)(j) := Mux(j.asUInt === s1_blkOffset, s1_next_VecState(i), PfSource.NONE)
          }
        }
        s1_wData(i).valid := true.B
        s1_wData(i).tag := get_tag(s1_pageAddr)
        s1_wData(i).cVec := s1_wBitMap(i)
      }
    }
    val s1_need_write = WireInit(!s1_fromTrain);dontTouch(s1_need_write)
    // --------------------------------------------------------------------------------
    // stage 2
    // --------------------------------------------------------------------------------
    // update consensusTable
    val s2_valid = RegNext(s1_valid(s1_dup_offset),false.B)
    val s2_hit = RegEnable(s1_hit(s1_dup_offset), false.B, s1_valid(s1_dup_offset))
    val s2_can_send2_pfq = RegNext(s1_can_send2_pfq(3))
    val s2_isHint2llc = RegNext(s1_isHint2llc)
    val s2_need_write = RegNext(s1_need_write,false.B)
    val s2_req = RegEnable(s1_req(s1_dup_offset), 0.U.asTypeOf(new PrefetchReq), s1_valid(s1_dup_offset))
    val s2_wData = RegEnable(s1_wData(s1_dup_offset), 0.U.asTypeOf(fTableEntry()), s1_valid(s1_dup_offset));dontTouch(s2_wData)
    val s2_widx = WireInit(get_idx(s2_req.addr(fullAddressBits - 1, pageOffsetBits)(log2Up(fTableEntries)-1,0)));dontTouch(s2_widx)
    val s2_evictQ_enq_valid = WireInit(s2_valid && s2_can_send2_pfq && !s2_isHint2llc)

    val s2_write = WireInit(s2_valid && s2_need_write)
    when(RegNext(s2_write)) {
        when(RegNext(s2_hit)){
            consensusTable(RegNext(s2_widx)).cVec := RegNext(s2_wData.cVec)
        }.otherwise{
            consensusTable(RegNext(s2_widx)) := RegNext(s2_wData)
        }
    }
    
    // --------------------------------------------------------------------------------
    // evict operation
    // --------------------------------------------------------------------------------
    val evict_valid = RegNext(s2_valid,false.B)
    val s1_req_evict = RegNext(s2_req,0.U.asTypeOf(new PrefetchReq))

    evict_q.io.enq.valid := s2_evictQ_enq_valid // if spp2llc , don't enq
    evict_q.io.enq.bits := get_blockAddr(s2_req.addr)
    val isqFull = RegNextN(evict_q.io.count === (fTableQueueEntries-2).U, 2, Some(false.B))
    evict_q.io.deq.ready := isqFull;
    
    val evict_q_s1_fire = RegNext(evict_q.io.deq.fire)
    val evict_q_s1 = RegNext(evict_q.io.deq.bits)

    val s1_evictAddr = WireInit(Cat(evict_q_s1,0.U(offsetBits.W)))
    val s1_evictPageAddr = s1_evictAddr(fullAddressBits - 1, pageOffsetBits)
    val s1_evictBlkOffset = s1_evictAddr(pageOffsetBits - 1, offsetBits)
    val s1_evictBlkAddr = s1_evictAddr(fullAddressBits - 1, offsetBits)
    val s1_readEvict = WireInit(0.U.asTypeOf(fTableEntry()))
    val s1_hitEvict =  WireInit(false.B)

    val oldAddr = s1_req_evict.addr
    val blkAddr = oldAddr(fullAddressBits - 1, offsetBits)
    val conflict = evict_valid && blkAddr === s1_evictBlkAddr
    s1_readEvict := consensusTable(get_idx(s1_evictPageAddr))
    s1_hitEvict := evict_q_s1_fire && s1_readEvict.valid && get_tag(s1_evictPageAddr) === s1_readEvict.tag && !conflict

    when(s1_hitEvict) {
      // consensusTable(get_idx(evictPageAddr)).cVec(evictBlkOffset) := consensusTable(get_idx(evictPageAddr)).cVec(evictBlkOffset) & FitlerVecState.BOP
      consensusTable(get_idx(s1_evictPageAddr)).cVec(s1_evictBlkOffset) := FitlerVecState.None //TODO: need further study
    }
    io.evict.ready := true.B
  
  //send out
    when(!ctrl_filter_sms){
      io.out_smsReq <> io.in_smsReq
    }.otherwise{
      io.out_smsReq.valid := s1_valid(s1_dup_offset) && s1_can_send2_pfq(s1_dup_offset) && s1_req(s1_dup_offset).hasSMS
      io.out_smsReq.bits := s1_req(s1_dup_offset)
      io.in_smsReq.ready := true.B
    }

    when(true.B){
      io.out_bopReq <> io.in_bopReq
    }.otherwise{
      io.out_bopReq.valid := s1_valid(0) && !s1_fromTrain && !s1_skip_filter && s1_can_send2_pfq(s1_dup_offset) && s1_req(0).hasBOP
      io.out_bopReq.bits := s1_req(s1_dup_offset)
      io.in_bopReq.ready := io.out_bopReq.ready
    }
    when(!ctrl_filter_spp){
      io.out_sppReq <> io.in_sppReq
    }.elsewhen(s0_spp_tagHit){
      io.out_sppReq.valid := io.in_sppReq.fire && s0_train_pfVec(io.in_sppReq.bits.get_blockOff) === PfSource.NONE
      io.out_sppReq.bits := io.in_sppReq.bits
      io.in_sppReq.ready := io.out_sppReq.ready
    }.otherwise{
      io.out_sppReq.valid := s1_valid(1) && !s1_fromTrain && !s1_skip_filter && s1_can_send2_pfq(s1_dup_offset) && s1_req(1).hasSPP
      io.out_sppReq.bits := s1_req(s1_dup_offset)
      io.in_sppReq.ready := io.out_sppReq.ready
    }

    dontTouch(io.hint2llc_out)
    io.hint2llc_out.valid := s1_valid(s1_dup_offset) && s1_can_send2_pfq(s1_dup_offset) && s1_isHint2llc
    io.hint2llc_out.bits := s1_req(s1_dup_offset)

  
    XSPerfAccumulate("hyper_filter_nums",s1_valid(s1_dup_offset) && s1_can_send2_pfq(s1_dup_offset))
    XSPerfAccumulate("hyper_filter_input",io.in_smsReq.valid||io.in_bopReq.valid||io.in_sppReq.valid)
    XSPerfAccumulate("hyper_filter_input_sms",io.in_smsReq.valid)
    XSPerfAccumulate("hyper_filter_input_bop",io.in_bopReq.valid)
    XSPerfAccumulate("hyper_filter_input_spp",io.in_sppReq.valid)
    // XSPerfAccumulate("hyper_filter_input_hint2llc",io.req.valid && io.is_hint2llc)
    XSPerfAccumulate("hyper_filter_output",io.out_bopReq.valid||io.out_bopReq.valid||io.out_bopReq.valid)
    XSPerfAccumulate("hyper_filter_output_sms",io.out_smsReq.valid)
    XSPerfAccumulate("hyper_filter_output_bop",io.out_bopReq.valid)
    XSPerfAccumulate("hyper_filter_output_spp",io.out_sppReq.valid)
    XSPerfAccumulate("hyper_filter_ouput_hint2llc",io.hint2llc_out.valid)
    XSPerfAccumulate("hyper_filter_evict_fomMshr",io.evict.fire)
    XSPerfAccumulate("hyper_filter_evict_fromQ",s1_hitEvict)
}

//Only used for hybrid spp and bop
class HyperPrefetchDev2(parentName:String = "Unknown")(implicit p: Parameters) extends HyperPrefetchDev2Module {
  val io = IO(new Bundle() {
    val l2_pf_en = Input(Bool())
    val l2_pf_ctrl = Input(UInt(Csr_PfCtrlBits.W))
    val train = Flipped(DecoupledIO(new PrefetchTrain))
    val req = DecoupledIO(new PrefetchReq)
    val resp = Flipped(DecoupledIO(new PrefetchResp))
    val evict = Flipped(DecoupledIO(new PrefetchEvict))
    val recv_addr = Flipped(ValidIO(UInt(64.W)))
    val hint2llc = ValidIO(new PrefetchReq)
  })
  // --------------------------------------------------------------------------------
  // csr pf ctrl
  // --------------------------------------------------------------------------------
  // ctrl[15,14,13,12,11,10,9,8,  7, 6, 5, 4, 3,  2, 1, 0]
  //                    |     |   |           |  |      |
  //                    ------    ------------   ------
  //                      |            |           |
  //                  FTConfig    sppConfig     ctrlSwitch
  // default 0000_0101_1110_0111
  // hex:    0    5    e    7
  // default value: 0x07e3
  //switchConfig[0,1,2]
    //| ctrl[0] -> enable sms
    //| ctrl[1] -> enable bop
    //| ctrl[2] -> enable spp
  //sppConfig[3,4,5,6,7]
    //| sppConfig[0] -> enable hint2llc   
    //| sppConfig[1] -> enable Nextline Agreesive
    //| sppConfig[2] -> enable bp recovery
    //| sppConfig[3] -> enable shareBO
    //| sppConfig[4] -> enable slowLookUp
  // filterTableConfig[8,9,10]
    // fTConfig[0]  -> enable fitter sms
    // fTConfig[1]  -> enable filter bop
    // fTConfig[2]  -> enable fitter spp
  val ctrlSwitch = WireInit(PfCtrlConst.Switch.get_fields(io.l2_pf_ctrl));dontTouch(ctrlSwitch)
  val ctrl_SMSen = WireInit(ctrlSwitch(PfCtrlConst.Switch.SMS));dontTouch(ctrl_SMSen)
  val ctrl_BOPen = WireInit(ctrlSwitch(PfCtrlConst.Switch.BOP));dontTouch(ctrl_BOPen)
  val ctrl_SPPen = WireInit(ctrlSwitch(PfCtrlConst.Switch.SPP));dontTouch(ctrl_SPPen)

  val ctrl_sppConfig = WireInit(PfCtrlConst.SppConfig.get_fields(io.l2_pf_ctrl));dontTouch(ctrl_sppConfig)
  val ctrl_fitlerTableConfig = WireInit(PfCtrlConst.FitlerTableConfig.get_fields(io.l2_pf_ctrl));dontTouch(ctrl_fitlerTableConfig)
  
  // --------------------------------------------------------------------------------
  // instance each algorithm moudle
  // --------------------------------------------------------------------------------
  val fTable = Module(new FilterTable)

  val spp = Module(new SignaturePathPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(SPPParameters()))
  })))
  val bop = Module(new BestOffsetPrefetch()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(BOPParameters()))
  })))
  val sms = Module(new PrefetchReceiver()(p.alterPartial({
        case L2ParamKey => p(L2ParamKey).copy(prefetch = Some(PrefetchReceiverParams()))
  })))

  val train_spp_q = Module(new Queue(new PrefetchTrain, entries = 4, flow = true, pipe = false))

  val q_bop = Module(new ReplaceableQueueV2(new PrefetchReq, bop_pfReqQueueEntries))
  val q_spp = Module(new ReplaceableQueueV2(new PrefetchReq, spp_pfReqQueueEntries))
  val q_sms = Module(new ReplaceableQueueV2(new PrefetchReq, sms_pfReqQueueEntries))
  val q_hint2llc = Module(new ReplaceableQueueV2(new PrefetchReq, spp_pfReqQueueEntries))
  // val pftQueue = Module(new PrefetchQueue(inflightEntries = hyperParams.inflightEntries))
  // --------------------------------------------------------------------------------
  // global counter 
  // --------------------------------------------------------------------------------
  // seperate eache prefetcher perf counter
  def get_perfState(perfCounter:UInt, allIssued:UInt, state_s2s1: UInt)={
    when((perfCounter << 2) > allIssued + allIssued + allIssued) {
      state_s2s1 := 3.U
    } .elsewhen((perfCounter << 1) > allIssued) {
      state_s2s1 := 2.U
    } .elsewhen((perfCounter << 2) > allIssued) {
      state_s2s1 := 1.U
    } .otherwise {
      state_s2s1 := 0.U
    }
  }
  class globalCounter extends HyperPrefetchDev2Bundle{
    val l1pf_hitAcc = UInt(8.W)
    val l1pf_issued = UInt(8.W)
    val l2pf_hitAcc = UInt(8.W)
    val l2pf_issued = UInt(8.W)
    val bop_hitAcc = UInt(8.W)
    val bop_issued = UInt(8.W)
    val spp_hitAcc = UInt(8.W)
    val spp_issued = UInt(8.W)
    val shareBO = SInt(6.W)

    val l1pf_hitAccState = UInt(PfaccState.bits.W)
    val l2pf_hitAccState = UInt(PfaccState.bits.W) 
  }
  val ghr = RegInit(0.U.asTypeOf(new  globalCounter()));dontTouch(ghr)
  val ghr_last = RegInit(0.U.asTypeOf(new  globalCounter()));dontTouch(ghr_last)
  val ghrCounter = Counter(true.B, 512)
  val ghr_roundReset = WireInit(false.B);dontTouch(ghr_roundReset)
  val ghr_roundCnt = ghrCounter._1
  ghr_roundReset := ghrCounter._2
  val bop_roundMax = 50
  val shareBO_reset = ghr_roundCnt === (bop_roundMax*2).U

  val deadPfEviction = RegInit(0.U(13.W))
  val issued = RegInit(0.U(16.W))
  val pf_deadCov_state = WireInit(0.U(PfcovState.bits.W));dontTouch(pf_deadCov_state)
  when(io.evict.valid && io.evict.bits.is_prefetch) {
    deadPfEviction := deadPfEviction + 1.U
  }
  when(io.req.fire){
    issued := issued + 1.U
  }
  when(ghr_roundReset) {
    deadPfEviction := 0.U
    issued := 0.U
    get_perfState(deadPfEviction,issued,pf_deadCov_state) 
  }
  // global acc state
  val ghrTrain = io.train.bits
  when((io.train.valid && io.train.bits.state === AccessState.PREFETCH_HIT)){
    when(ghrTrain.is_l1pf){
      ghr.l1pf_hitAcc := ghr.l1pf_hitAcc + 1.U
    }
    when(ghrTrain.is_l2pf){
      ghr.l2pf_hitAcc := ghr.l2pf_hitAcc + 1.U
    }
    when(ghrTrain.hasBOP){
      ghr.bop_hitAcc := ghr.bop_hitAcc + 1.U
    }
    when(ghrTrain.hasSPP){
      ghr.spp_hitAcc := ghr.spp_hitAcc + 1.U
    }
  }
  when(io.req.fire){
    when(io.req.bits.is_l1pf){
      ghr.l1pf_issued := ghr.l1pf_issued + 1.U
    }
    when(io.req.bits.is_l2pf){
      ghr.l2pf_issued := ghr.l2pf_issued + 1.U
    }
    when(io.req.bits.hasBOP){
      ghr.bop_issued := ghr.bop_issued + 1.U
    }
    when(io.req.bits.hasSPP){
      ghr.spp_issued := ghr.spp_issued + 1.U
    }
  }

  when(ghr_roundReset){
    ghr := 0.U.asTypeOf(new globalCounter())
    ghr.shareBO := bop.io.shareBO
    ghr_last := ghr
    get_perfState(ghr.l1pf_hitAcc,ghr.l1pf_issued,ghr.l1pf_hitAccState) 
    get_perfState(ghr.l2pf_hitAcc,ghr.l2pf_issued,ghr.l2pf_hitAccState) 
  }

  when(shareBO_reset){
    ghr.shareBO := bop.io.shareBO
  }
  // --------------------------------------------------------------------------------
  // spp train diverter queue
  // --------------------------------------------------------------------------------
  //devert
  train_spp_q.io.enq <> io.train
  spp.io.train.valid := ctrl_SPPen && train_spp_q.io.deq.valid
  spp.io.train.bits := train_spp_q.io.deq.bits
  train_spp_q.io.deq.ready := spp.io.train.ready
  //
  bop.io.train.valid := ctrl_BOPen && io.train.valid
  bop.io.train.bits := io.train.bits
  //TODO: need respALL ?
  bop.io.resp.valid := io.resp.valid //&& io.resp.bits.hasBOP
  bop.io.resp.bits := io.resp.bits
  io.resp.ready := bop.io.resp.ready

  spp.io.resp := DontCare
  spp.io.from_ghr.valid := ghr_roundReset
  spp.io.from_ghr.bits.l2_deadCov_state := pf_deadCov_state
  spp.io.from_ghr.bits.l2_hitAcc_state := ghr.l2pf_hitAccState
  spp.io.from_ghr.bits.bop_hitAcc_state := ghr.bop_hitAcc
  spp.io.from_ghr.bits.spp_hitAcc_state := ghr.spp_hitAcc
  spp.io.from_ghr.bits.shareBO := RegNext(ghr.shareBO)
  spp.io.from_ghr.bits.global_queue_used := q_spp.io.full //RegNext(pftQueue.io.queue_used)
  spp.io.sppCtrl := ctrl_sppConfig

  sms.io.recv_addr.valid := ctrl_SMSen && io.recv_addr.valid
  sms.io.recv_addr.bits := io.recv_addr.bits
  sms.io.req.ready := true.B

  q_hint2llc.io.enq <> spp.io.req_hint2llc
  // qurry fTable
  fTable.io.in_smsReq <> sms.io.req
  fTable.io.in_bopReq <> bop.io.req
  fTable.io.in_sppReq <> spp.io.req
  fTable.io.in_trainReq.valid := io.train.valid
  fTable.io.in_trainReq.bits := io.train.bits
  // 
  q_sms.io.enq <> fTable.io.out_smsReq
  q_bop.io.enq <> fTable.io.out_bopReq
  q_spp.io.enq <> fTable.io.out_sppReq

  fTable.io.is_hint2llc := q_hint2llc.io.deq.fire
  fTable.io.ctrl := ctrl_fitlerTableConfig

  //send to prefetchQueue
  // val req_pipe = Module(new Pipeline(new PrefetchReq, 1, pipe = true))
  // pftQueue.io.enq.valid := q_sms.io.deq.valid || q_bop.io.deq.valid || q_spp.io.deq.valid
  // pftQueue.io.enq.bits := ParallelPriorityMux(
  //   Seq(
  //     q_sms.io.deq.valid -> q_sms.io.deq.bits,
  //     q_bop.io.deq.valid -> q_bop.io.deq.bits,
  //     q_spp.io.deq.valid -> q_spp.io.deq.bits,
  //     q_hint2llc.io.deq.valid -> q_hint2llc.io.deq.bits,
  //   )
  // )
  // pftQueue.io.enq <> bop.io.req
  // q_sms.io.deq.ready := true.B
  // q_bop.io.deq.ready := true.B && !q_sms.io.deq.valid
  // q_spp.io.deq.ready := true.B && !q_sms.io.deq.valid && !q_bop.io.deq.valid
  // q_hint2llc.io.deq.ready := true.B && !q_sms.io.deq.valid && !q_bop.io.deq.valid && !q_spp.io.deq.valid
  //fTable.io.resp.ready := io.req.ready //cannot back pressure
  // req_pipe.io.in <> pftQueue.io.deq

  // io.req.valid := req_pipe.io.out.valid && io.l2_pf_en
  // io.req.bits := req_pipe.io.out.bits
  // req_pipe.io.out.ready := io.req.ready
  // io.req <> req_pipe.io.out
  val s1_ready = WireInit(false.B)
  val s0_req = WireInit(0.U.asTypeOf(ValidIO(new PrefetchReq)))
  s0_req.valid := q_sms.io.deq.valid || q_bop.io.deq.valid || q_spp.io.deq.valid
  s0_req.bits := ParallelPriorityMux(
    Seq(
      q_sms.io.deq.valid -> q_sms.io.deq.bits,
      q_bop.io.deq.valid -> q_bop.io.deq.bits,
      q_spp.io.deq.valid -> q_spp.io.deq.bits,
      q_hint2llc.io.deq.valid -> q_hint2llc.io.deq.bits,
    )
  )
  q_sms.io.deq.ready := s1_ready
  q_bop.io.deq.ready := s1_ready && !q_sms.io.deq.valid
  q_spp.io.deq.ready := s1_ready && !q_sms.io.deq.valid && !q_bop.io.deq.valid
  q_hint2llc.io.deq.ready := s1_ready && !q_sms.io.deq.valid && !q_bop.io.deq.valid && !q_spp.io.deq.valid
  //hint to llc
  io.hint2llc.valid := RegNextN(fTable.io.hint2llc_out.valid, 2, Some(false.B))
  io.hint2llc.bits := RegNextN(fTable.io.hint2llc_out.bits, 2 , Some(0.U.asTypeOf(new PrefetchReq)))
  fTable.io.evict.valid := false.B//io.evict.valid
  fTable.io.evict.bits := io.evict.bits
  io.evict.ready := fTable.io.evict.ready
  io.train.ready := true.B
  // --------------------------------------------------------------------------------
  // stage 1 send out 
  // --------------------------------------------------------------------------------
  val s1_valid = RegNext(s0_req.valid, false.B)
  val s1_req = RegEnable(s0_req.bits, 0.U.asTypeOf(new PrefetchReq), s0_req.valid)
  
  io.req.valid := s1_valid
  io.req.bits := s1_req
  s1_ready := io.req.ready

  XSPerfAccumulate("spp_deq_blocked", q_spp.io.deq.valid && !q_spp.io.deq.ready)
  XSPerfAccumulate("bop_deq_blocked", q_bop.io.deq.valid && !q_bop.io.deq.ready)
  XSPerfAccumulate("sms_deq_blocked", q_sms.io.deq.valid && !q_sms.io.deq.ready)
  XSPerfAccumulate("bop_resp", bop.io.resp.valid)
}