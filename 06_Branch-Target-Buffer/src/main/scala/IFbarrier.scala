// ADS I Class Project
// Pipelined RISC-V Core - IF Barrier
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
IF-Barrier: pipeline register between Fetch and Decode stages

Internal Registers:
    instrReg: holds instruction between pipeline stages, initialized to 0

Inputs:
    inInstr: fetched instruction from IF stage

Outputs:
    outInstr: instruction to ID stage

Functionality:
    Save all input signals to a register and output them in the following clock cycle
*/

package core_tile

import chisel3._

// -----------------------------------------
// IF-Barrier
// -----------------------------------------
class IFBarrier extends Module {
  val io = IO(new Bundle {
    val inInstr = Input(UInt(32.W)) // instruction from if stage
    val inPC = Input(UInt(32.W)) //pc from if stage

    val inPredictTaken = Input(Bool())   //branch prediction result from btb
    val inPredictedTarget = Input(UInt(32.W))  //predicted branch tgt adress from btb
    val flush = Input(Bool())  //flush signal to flush after misprediction

    val outInstr = Output(UInt(32.W))  //pass reg instruction to decode stage
    val outPC = Output(UInt(32.W))  //pass to id stage
    val outPredictTaken = Output(Bool())  //branch prediction passing to id stage
    val outPredictedTarget = Output(UInt(32.W)) // pass thge predicted tgt to next stage
  })
  val instrReg = RegInit(0.U(32.W))  //reg to store fetched instruction
  val pcReg = RegInit(0.U(32.W))  //reg to store pc
  val predictTakenReg = RegInit(false.B)  //reg to store branch prediction 
  val predictedTargetReg = RegInit(0.U(32.W)) //reg to store tgt addr

  //clear pipeline registers when flush occcursssss
  when(io.flush) {
    instrReg := 0.U; pcReg := 0.U; predictTakenReg := false.B; predictedTargetReg := 0.U  //insert nop (clearing all stored values)
  }.otherwise {
    instrReg := io.inInstr; pcReg := io.inPC  //latch if stage output into pipeline registers
    predictTakenReg := io.inPredictTaken; predictedTargetReg := io.inPredictedTarget //store the branch prediction info
  }
  io.outInstr := instrReg; io.outPC := pcReg  //send the reg instrn and pc to id stage
  io.outPredictTaken := predictTakenReg; io.outPredictedTarget := predictedTargetReg  //Send registered prediction information to the ID stage.
}

