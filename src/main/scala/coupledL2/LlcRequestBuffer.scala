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

package coupledL2

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters

class LlcRequestBuffer(implicit p : Parameters) extends L2Module{
  val io = IO(new Bundle(){
    val llcRequest = Flipped(ValidIO(new MSHRRequest))
    val llcReq = DecoupledIO(new SourceAReq)
  })

  val msTask   = io.llcRequest.bits.task
  val a_task = {
    val oa = Wire(new SourceAReq)
    oa := DontCare
    oa.tag := msTask.tag
    oa.set := msTask.set
    oa.off := msTask.off
    oa.opcode := msTask.opcode
    oa.size := msTask.size
    oa.reqSource := msTask.reqSource
    oa
  }

  val reqQueue = Module(new Queue(new SourceAReq, 16, pipe=true, flow=true))
  reqQueue.io.enq.valid := io.llcRequest.valid && msTask.hint2llc.getOrElse(false.B)
  reqQueue.io.enq.bits := a_task
  io.llcReq <> reqQueue.io.deq
}