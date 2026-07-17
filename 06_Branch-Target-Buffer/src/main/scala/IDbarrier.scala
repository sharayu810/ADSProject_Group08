// ADS I Class Project
// Pipelined RISC-V Core - ID Barrier
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
ID-Barrier: pipeline register between Decode and Execute stages

Internal Registers:
    uop: micro-operation code (from uopc enum)
    rd: destination register index, initialized to 0
    operandA: first source operand, initialized to 0
    operandB: second operand/immediate, initialized to 0

Inputs:
    inUOP: micro-operation code from ID stage
    inRD: destination register from ID stage
    inOperandA: first operand from ID stage
    inOperandB: second operand/immediate from ID stage
    inXcptInvalid: exception flag from ID stage

Outputs:
    outUOP: micro-operation code to EX stage
    outRD: destination register to EX stage
    outOperandA: first operand to EX stage
    outOperandB: second operand to EX stage
    outXcptInvalid: exception flag to EX stage
Functionality:
    Save all input signals to a register and output them in the following clock cycle
*/

package core_tile

import chisel3._
import uopc._

// -----------------------------------------
// ID-Barrier
// -----------------------------------------

//ToDo: Add your implementation according to the specification above here 
class IDBarrier extends Module {
  val io = IO(new Bundle {
    val flush         = Input(Bool())

  //I/O Declaration
    val inUOP         = Input(uopc())
    val inRD          = Input(UInt(5.W))
    val inRS1         = Input(UInt(5.W))
    val inRS2         = Input(UInt(5.W))
    val inOperandA    = Input(UInt(32.W))
    val inOperandB    = Input(UInt(32.W))
    val inXcptInvalid = Input(Bool())
    val inPC          = Input(UInt(32.W))

    val inImmI        = Input(UInt(32.W))
    val inImmB        = Input(UInt(32.W))
    val inImmJ        = Input(UInt(32.W))
    val inPredictTaken = Input(Bool()) //Carry the BTB's prediction from the ID stage to the EX stage
    val inPredictedTarget = Input(UInt(32.W))

    val outUOP         = Output(uopc())
    val outRD          = Output(UInt(5.W))
    val outRS1         = Output(UInt(5.W))
    val outRS2         = Output(UInt(5.W))
    val outOperandA    = Output(UInt(32.W))
    val outOperandB    = Output(UInt(32.W))
    val outXcptInvalid = Output(Bool())
    val outPC          = Output(UInt(32.W))

    val outImmI        = Output(UInt(32.W))
    val outImmB        = Output(UInt(32.W))
    val outImmJ        = Output(UInt(32.W))
    val outPredictTaken = Output(Bool())
    val outPredictedTarget = Output(UInt(32.W))
  })

//Pipeline Register Declarations
  val uopReg         = RegInit(uopc.NOP)
  val rdReg          = RegInit(0.U(5.W))
  val rs1Reg         = RegInit(0.U(5.W))
  val rs2Reg         = RegInit(0.U(5.W))
  val operandAReg    = RegInit(0.U(32.W))
  val operandBReg    = RegInit(0.U(32.W))
  val xcptInvalidReg = RegInit(false.B)
  val pcReg          = RegInit(0.U(32.W))

  val immIReg        = RegInit(0.U(32.W))
  val immBReg        = RegInit(0.U(32.W))
  val immJReg        = RegInit(0.U(32.W))
  val predictTakenReg = RegInit(false.B)  //store the prediction info for one clock cycle
  val predictedTargetReg = RegInit(0.U(32.W))

//Flush Logic
  when(io.flush) {
    uopReg         := uopc.NOP
    rdReg          := 0.U
    rs1Reg         := 0.U
    rs2Reg         := 0.U
    operandAReg    := 0.U
    operandBReg    := 0.U
    xcptInvalidReg := false.B
    pcReg          := 0.U

    immIReg        := 0.U
    immBReg        := 0.U
    immJReg        := 0.U
    predictTakenReg := false.B  //clear the prediction when pipeline is flushed
    predictedTargetReg := 0.U
  }.otherwise {
    uopReg         := io.inUOP
    rdReg          := io.inRD
    rs1Reg         := io.inRS1
    rs2Reg         := io.inRS2
    operandAReg    := io.inOperandA
    operandBReg    := io.inOperandB
    xcptInvalidReg := io.inXcptInvalid
    pcReg          := io.inPC

    immIReg        := io.inImmI
    immBReg        := io.inImmB
    immJReg        := io.inImmJ
    predictTakenReg := io.inPredictTaken  //save the btb prediction into the pipeline register
    predictedTargetReg := io.inPredictedTarget
  }

//Control Signal Outputs
  io.outUOP         := uopReg
  io.outRD          := rdReg
  io.outRS1         := rs1Reg
  io.outRS2         := rs2Reg
  //Data Signal Outputs
  io.outOperandA    := operandAReg
  io.outOperandB    := operandBReg
  io.outXcptInvalid := xcptInvalidReg
  io.outPC          := pcReg

//Immediate Outputs
  io.outImmI        := immIReg
  io.outImmB        := immBReg
  io.outImmJ        := immJReg
  io.outPredictTaken := predictTakenReg  //Forward the prediction information to the EX stage so it can verify whether the prediction was correct.
  io.outPredictedTarget := predictedTargetReg
}
