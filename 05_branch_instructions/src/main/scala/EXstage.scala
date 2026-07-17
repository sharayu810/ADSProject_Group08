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
    val inUOP         = Input(uopc())
    val inRD          = Input(UInt(5.W))
    val inOperandA    = Input(UInt(32.W))
    val inOperandB    = Input(UInt(32.W))
    val inXcptInvalid = Input(Bool())
    val inPC          = Input(UInt(32.W))

    val inImmB        = Input(UInt(32.W))
    val inImmJ        = Input(UInt(32.W))
    val inImmI        = Input(UInt(32.W))

    val outRD          = Output(UInt(5.W))
    val aluResult      = Output(UInt(32.W))
    val outXcptInvalid = Output(Bool())

    val redirect       = Output(Bool())
    val redirectPC     = Output(UInt(32.W))
  })

  val alu = Module(new ALU)

  alu.io.operandA := io.inOperandA
  alu.io.operandB := io.inOperandB
  alu.io.operation := ALUOp.ADD

  io.redirect := false.B
  io.redirectPC := 0.U

  switch(io.inUOP) {
    is(uopc.ADD)  { alu.io.operation := ALUOp.ADD }
    is(uopc.ADDI) { alu.io.operation := ALUOp.ADD }

    is(uopc.SUB)  { alu.io.operation := ALUOp.SUB }

    is(uopc.AND)  { alu.io.operation := ALUOp.AND }
    is(uopc.ANDI) { alu.io.operation := ALUOp.AND }

    is(uopc.OR)   { alu.io.operation := ALUOp.OR }
    is(uopc.ORI)  { alu.io.operation := ALUOp.OR }

    is(uopc.XOR)  { alu.io.operation := ALUOp.XOR }
    is(uopc.XORI) { alu.io.operation := ALUOp.XOR }

    is(uopc.SLL)  { alu.io.operation := ALUOp.SLL }
    is(uopc.SLLI) { alu.io.operation := ALUOp.SLL }

    is(uopc.SRL)  { alu.io.operation := ALUOp.SRL }
    is(uopc.SRLI) { alu.io.operation := ALUOp.SRL }

    is(uopc.SRA)  { alu.io.operation := ALUOp.SRA }
    is(uopc.SRAI) { alu.io.operation := ALUOp.SRA }

    is(uopc.SLT)  { alu.io.operation := ALUOp.SLT }
    is(uopc.SLTI) { alu.io.operation := ALUOp.SLT }

    is(uopc.SLTU)  { alu.io.operation := ALUOp.SLTU }
    is(uopc.SLTIU) { alu.io.operation := ALUOp.SLTU }

    is(uopc.BEQ) {
      when(io.inOperandA === io.inOperandB) {
        io.redirect := true.B
        io.redirectPC := io.inPC + io.inImmB
      }
    }

    is(uopc.BNE) {
      when(io.inOperandA =/= io.inOperandB) {
        io.redirect := true.B
        io.redirectPC := io.inPC + io.inImmB
      }
    }

    is(uopc.BLT) {
      when(io.inOperandA.asSInt < io.inOperandB.asSInt) {
        io.redirect := true.B
        io.redirectPC := io.inPC + io.inImmB
      }
    }

    is(uopc.BGE) {
      when(io.inOperandA.asSInt >= io.inOperandB.asSInt) {
        io.redirect := true.B
        io.redirectPC := io.inPC + io.inImmB
      }
    }

    is(uopc.BLTU) {
      when(io.inOperandA < io.inOperandB) {
        io.redirect := true.B
        io.redirectPC := io.inPC + io.inImmB
      }
    }

    is(uopc.BGEU) {
      when(io.inOperandA >= io.inOperandB) {
        io.redirect := true.B
        io.redirectPC := io.inPC + io.inImmB
      }
    }

    is(uopc.JAL) {
      io.redirect := true.B
      io.redirectPC := io.inPC + io.inImmJ
    }

    is(uopc.JALR) {
      io.redirect := true.B
      io.redirectPC := (io.inOperandA + io.inImmI) & "hfffffffe".U
    }

    is(uopc.NOP) {
      alu.io.operation := ALUOp.PASSB
    }
  }

  io.aluResult      := alu.io.aluResult
  io.outRD          := io.inRD
  io.outXcptInvalid := io.inXcptInvalid
}