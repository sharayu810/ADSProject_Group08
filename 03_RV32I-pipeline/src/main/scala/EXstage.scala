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
    val uop             = Input(uopc())
    val operandA        = Input(UInt(32.W))
    val operandB        = Input(UInt(32.W))
    val rd              = Input(UInt(5.W))
    val xcptInvalid     = Input(Bool())
    val aluResult       = Output(UInt(32.W))
    val rd_out          = Output(UInt(5.W))
    val xcptInvalid_out = Output(Bool())
  })

  val alu   = Module(new ALU)
  val aluOp = WireDefault(ALUOp.ADD)

  switch(io.uop) {
    is(uopc.isADD,  uopc.isADDI)  { aluOp := ALUOp.ADD }
    is(uopc.isSUB)                { aluOp := ALUOp.SUB }
    is(uopc.isAND,  uopc.isANDI)  { aluOp := ALUOp.AND }
    is(uopc.isOR,   uopc.isORI)   { aluOp := ALUOp.OR }
    is(uopc.isXOR,  uopc.isXORI)  { aluOp := ALUOp.XOR }
    is(uopc.isSLL,  uopc.isSLLI)  { aluOp := ALUOp.SLL }
    is(uopc.isSRL,  uopc.isSRLI)  { aluOp := ALUOp.SRL }
    is(uopc.isSRA,  uopc.isSRAI)  { aluOp := ALUOp.SRA }
    is(uopc.isSLT,  uopc.isSLTI)  { aluOp := ALUOp.SLT }
    is(uopc.isSLTU, uopc.isSLTIU) { aluOp := ALUOp.SLTU }
    // isNOP falls through to ADD; result is discarded (rd = x0)
  }

  alu.io.operandA  := io.operandA
  alu.io.operandB  := io.operandB
  alu.io.operation := aluOp

  io.aluResult       := alu.io.aluResult
  io.rd_out          := io.rd
  io.xcptInvalid_out := io.xcptInvalid
}