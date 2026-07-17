// ADS I Class Project
// Pipelined RISC-V Core
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/15/2023 by Tobias Jauch (@tojauch)

/*
The goal of this task is to implement a 5-stage pipeline that features a subset of RV32I (all R-type and I-type instructions). 

    Instruction Memory:
        The CPU has an instruction memory (IMem) with 4096 words, each of 32 bits.
        The content of IMem is loaded from a binary file specified during the instantiation of the MultiCycleRV32Icore module.

    CPU Registers:
        The CPU has a program counter (PC) and a register file (regFile) with 32 registers, each holding a 32-bit value.
        Register x0 is hard-wired to zero.

    Microarchitectural Registers / Wires:
        Various signals are defined as either registers or wires depending on whether they need to be used in the same cycle or in a later cycle.

    Processor Stages:
        The FSM of the processor has five stages: fetch, decode, execute, memory, and writeback.
        All stages are active at the same time and process different instructions simultaneously.

        Fetch Stage:
            The instruction is fetched from the instruction memory based on the current value of the program counter (PC).

        Decode Stage:
            Instruction fields such as opcode, rd, funct3, and rs1 are extracted.
            For R-type instructions, additional fields like funct7 and rs2 are extracted.
            Control signals (isADD, isSUB, etc.) are set based on the opcode and funct3 values.
            Operands (operandA and operandB) are determined based on the instruction type.

        Execute Stage:
            Arithmetic and logic operations, including branch target calculation, are performed based on the control signals and operands.
            The result is stored in the aluResult register.

        Memory Stage:
            No memory operations are implemented in this basic CPU.

        Writeback Stage:
            The result of the operation (writeBackData) is written back to the destination register (rd) in the register file.

    Check Result:
        The final result (writeBackData) is output to the io.check_res signal.
        The exception signal is also passed to the wrapper module. It indicates whether an invalid instruction has been encountered.
        In the fetch stage, a default value of 0 is assigned to io.check_res.
*/

package core_tile

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
//import Assignment02.{ALU, ALUOp}
import uopc._


class PipelinedRV32Icore (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    //ToDo: Add I/O ports
    //These go outside to the testbench.
    val check_res = Output(UInt(32.W))
    val exception = Output(Bool())
  })

//ToDo: Add your implementation according to the specification above here 
  val ifStage    = Module(new IF(BinaryFile))
  val ifBarrier  = Module(new IFBarrier)
  val idStage    = Module(new ID)
  val idBarrier  = Module(new IDBarrier)
  val exStage    = Module(new EX)
  val exBarrier  = Module(new EXBarrier)
  val memStage   = Module(new MEM)
  val memBarrier = Module(new MEMBarrier)
  val wbStage    = Module(new WB)
  val wbBarrier  = Module(new WBBarrier)
  val forwardingUnit = Module(new ForwardingUnit)
  val registerFile = Module(new regFile)

  // IF -> IF Barrier
  ifBarrier.io.inInstr := ifStage.io.instr
  ifBarrier.io.inPC    := ifStage.io.pc
  ifBarrier.io.flush := exStage.io.redirect

  // temporary: no branch/jump redirect yet
  ifStage.io.redirect   := exStage.io.redirect
  ifStage.io.redirectPC := exStage.io.redirectPC

  // IF Barrier -> ID
  idStage.io.instr := ifBarrier.io.outInstr
  idStage.io.pc := ifBarrier.io.outPC
  // Register file read connections
  registerFile.io.req_1 <> idStage.io.regFileReq_A
  idStage.io.regFileResp_A <> registerFile.io.resp_1

  registerFile.io.req_2 <> idStage.io.regFileReq_B
  idStage.io.regFileResp_B <> registerFile.io.resp_2

  // ID -> ID Barrier
  idBarrier.io.flush         := exStage.io.redirect
  idBarrier.io.inUOP         := idStage.io.uop
  idBarrier.io.inRD          := idStage.io.rd
  idBarrier.io.inRS1         := idStage.io.rs1
  idBarrier.io.inRS2         := idStage.io.rs2
  idBarrier.io.inOperandA    := idStage.io.operandA
  idBarrier.io.inOperandB    := idStage.io.operandB
  idBarrier.io.inPC := idStage.io.outPC
  idBarrier.io.inXcptInvalid := idStage.io.XcptInvalid
  idBarrier.io.inImmI        := idStage.io.immI
  idBarrier.io.inImmB        := idStage.io.immB
  idBarrier.io.inImmJ        := idStage.io.immJ

  // Forwarding Unit connections
forwardingUnit.io.rs1_EX := idBarrier.io.outRS1
forwardingUnit.io.rs2_EX := idBarrier.io.outRS2

forwardingUnit.io.rd_MEM := exBarrier.io.outRD
forwardingUnit.io.rd_WB  := memBarrier.io.outRD

forwardingUnit.io.wrEn_MEM := (exBarrier.io.outRD =/= 0.U) && (exBarrier.io.outXcptInvalid === false.B)
forwardingUnit.io.wrEn_WB  := (memBarrier.io.outRD =/= 0.U) && (memBarrier.io.outXcptInvalid === false.B)

// Forwarding muxes for ALU operands
val forwardedOperandA = Wire(UInt(32.W))
val forwardedOperandB = Wire(UInt(32.W))

forwardedOperandA := idBarrier.io.outOperandA
forwardedOperandB := idBarrier.io.outOperandB

when(forwardingUnit.io.forwardA === 2.U) {
  forwardedOperandA := exBarrier.io.outAluResult
} .elsewhen(forwardingUnit.io.forwardA === 1.U) {
  forwardedOperandA := memBarrier.io.outAluResult
}

when(forwardingUnit.io.forwardB === 2.U) {
  forwardedOperandB := exBarrier.io.outAluResult
} .elsewhen(forwardingUnit.io.forwardB === 1.U) {
  forwardedOperandB := memBarrier.io.outAluResult
}

  // ID Barrier -> EX
  exStage.io.inUOP         := idBarrier.io.outUOP
  exStage.io.inRD          := idBarrier.io.outRD
  exStage.io.inOperandA := forwardedOperandA
  exStage.io.inOperandB := forwardedOperandB
  exStage.io.inXcptInvalid := idBarrier.io.outXcptInvalid
  exStage.io.inPC := idBarrier.io.outPC

  // EX -> EX Barrier
  exBarrier.io.inAluResult   := exStage.io.aluResult
  exBarrier.io.inRD          := exStage.io.outRD
  exBarrier.io.inXcptInvalid := exStage.io.outXcptInvalid
  exStage.io.inImmI := idBarrier.io.outImmI
  exStage.io.inImmB := idBarrier.io.outImmB
  exStage.io.inImmJ := idBarrier.io.outImmJ

  // EX Barrier -> MEM Barrier
  memBarrier.io.inAluResult   := exBarrier.io.outAluResult
  memBarrier.io.inRD          := exBarrier.io.outRD
  memBarrier.io.inXcptInvalid := exBarrier.io.outXcptInvalid

  // MEM Barrier -> WB
  wbStage.io.aluResult     := memBarrier.io.outAluResult
  wbStage.io.rd            := memBarrier.io.outRD
  wbStage.io.inXcptInvalid := memBarrier.io.outXcptInvalid

  // WB -> Register file write
  registerFile.io.req_3 <> wbStage.io.regFileReq

  // WB -> WB Barrier -> core output
  wbBarrier.io.inCheckRes    := wbStage.io.check_res
  wbBarrier.io.inXcptInvalid := wbStage.io.exception

  io.check_res := wbBarrier.io.outCheckRes
  io.exception := wbBarrier.io.outXcptInvalid


}

