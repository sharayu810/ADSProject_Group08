// ADS I Class Project
// Pipelined RISC-V Core - EX Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Instruction Execute (EX) Stage: ALU operations and exception detection

Instantiated Modules:
    ALU: Integrate your module from Assignment02 for arithmetic/logical operations

ALU Interface:
    alu.io.operandA: first operand input
    alu.io.operandB: second operand input
    alu.io.operation: operation code controlling ALU function
    alu.io.aluResult: computation result output

Internal Signals:
    Map uopc codes to ALUOp values

Functionality:
    Map instruction uop to ALU operation code
    Pass operands to ALU
    Output results to pipeline

Outputs:
    aluResult: computation result from ALU
    exception: pass exception flag
    branchTarget: calculated branch target address for conditional branch instructions
    flush: control signal to flush pipeline on mispredicted branches
*/

package core_tile

import chisel3._
import chisel3.util._
import Assignment02.{ALU, ALUOp}
import uopc._

// -----------------------------------------
// Execute Stage
// -----------------------------------------

//ToDo: Add your implementation according to the specification above here 
class EX extends Module {
  val io = IO(new Bundle {
    val inUOP = Input(uopc())
    val inRD = Input(UInt(5.W))
    val inOperandA = Input(UInt(32.W))
    val inOperandB = Input(UInt(32.W))
    val inXcptInvalid = Input(Bool())
    val inPC = Input(UInt(32.W))
    val inImmB = Input(UInt(32.W))
    val inImmJ = Input(UInt(32.W))
    val inImmI = Input(UInt(32.W))
    val inPredictTaken = Input(Bool()) // Input branch prediction from the BTB.
    val inPredictedTarget = Input(UInt(32.W)) // Input predicted branch target from the BTB.

    val outRD = Output(UInt(5.W))
    val aluResult = Output(UInt(32.W))
    val outXcptInvalid = Output(Bool())
    val redirect = Output(Bool()) //output signal when the branch redirect is incorrect
    val redirectPC = Output(UInt(32.W))  //o/p the correct PC for pipeline redirection
    val btbUpdate = Output(Bool())  //signal to update the Branch tgt buffer
    val btbPC = Output(UInt(32.W)) //o/p branch PC used for BTB Update
    val btbTarget = Output(UInt(32.W)) //actual branch tgt address for btb
    val btbMispredicted = Output(Bool()) //o/p wether the btb prediction was incorrect
  })

  val alu = Module(new ALU)
  alu.io.operandA := io.inOperandA
  alu.io.operandB := io.inOperandB
  alu.io.operation := ALUOp.ADD

  switch(io.inUOP) {
    is(ADD) { alu.io.operation := ALUOp.ADD }; is(ADDI) { alu.io.operation := ALUOp.ADD }
    is(SUB) { alu.io.operation := ALUOp.SUB }
    is(AND) { alu.io.operation := ALUOp.AND }; is(ANDI) { alu.io.operation := ALUOp.AND }
    is(OR) { alu.io.operation := ALUOp.OR }; is(ORI) { alu.io.operation := ALUOp.OR }
    is(XOR) { alu.io.operation := ALUOp.XOR }; is(XORI) { alu.io.operation := ALUOp.XOR }
    is(SLL) { alu.io.operation := ALUOp.SLL }; is(SLLI) { alu.io.operation := ALUOp.SLL }
    is(SRL) { alu.io.operation := ALUOp.SRL }; is(SRLI) { alu.io.operation := ALUOp.SRL }
    is(SRA) { alu.io.operation := ALUOp.SRA }; is(SRAI) { alu.io.operation := ALUOp.SRA }
    is(SLT) { alu.io.operation := ALUOp.SLT }; is(SLTI) { alu.io.operation := ALUOp.SLT }
    is(SLTU) { alu.io.operation := ALUOp.SLTU }; is(SLTIU) { alu.io.operation := ALUOp.SLTU }
    is(NOP) { alu.io.operation := ALUOp.PASSB }
  }

// Check whether the current instruction is a conditional branch
  val isBranch = io.inUOP === BEQ || io.inUOP === BNE || io.inUOP === BLT ||
    io.inUOP === BGE || io.inUOP === BLTU || io.inUOP === BGEU

// Store the actual branch outcome
  val actualTaken = WireDefault(false.B)

  // Evaluate the branch condition.
  switch(io.inUOP) {
    is(BEQ) { actualTaken := io.inOperandA === io.inOperandB }
    is(BNE) { actualTaken := io.inOperandA =/= io.inOperandB }
    is(BLT) { actualTaken := io.inOperandA.asSInt < io.inOperandB.asSInt }
    is(BGE) { actualTaken := io.inOperandA.asSInt >= io.inOperandB.asSInt }
    is(BLTU) { actualTaken := io.inOperandA < io.inOperandB }
    is(BGEU) { actualTaken := io.inOperandA >= io.inOperandB }
  }

  // Calculate the actual branch target address
  val branchTarget = io.inPC + io.inImmB

  // Check whether the predicted branch direction was incorrect
  val directionWrong = io.inPredictTaken =/= actualTaken

  //Check whether predicted branch target address was incorrect
  val targetWrong = actualTaken && io.inPredictTaken && io.inPredictedTarget =/= branchTarget

  // Determine whether a branch misprediction occurred
  val branchMispredicted = isBranch && (directionWrong || targetWrong)

//Generate a redirect on branch misprediction or jump instructions
  io.redirect := branchMispredicted || io.inUOP === JAL || io.inUOP === JALR
  // Select the correct next PC after branch resolution
  io.redirectPC := Mux(actualTaken, branchTarget, io.inPC + 4.U)

  //// Compute the jump target for JAL
  when(io.inUOP === JAL) {
    io.redirectPC := io.inPC + io.inImmJ
  }.elsewhen(io.inUOP === JALR) {
    io.redirectPC := (io.inOperandA + io.inImmI) & "hfffffffe".U
  }

  io.btbUpdate := isBranch //// Enable BTB update for conditional branch instructions
  io.btbPC := io.inPC ///provide the branch pc for btb update
  io.btbTarget := branchTarget  //provide actual pc for btb update
  // The counter learns direction only... a target mismatch still redirects but
  // must not be interpreted as a not-taken outcome by the predictor

  // Update the BTB predictor only when the branch direction was mispredicted
  io.btbMispredicted := isBranch && directionWrong

  // Output PC+4 for jumps; otherwise output the ALU result
  io.aluResult := Mux(io.inUOP === JAL || io.inUOP === JALR, io.inPC + 4.U, alu.io.aluResult)
  // Forward the destination register
  io.outRD := io.inRD
  // Forward the exception flag
  io.outXcptInvalid := io.inXcptInvalid
}
